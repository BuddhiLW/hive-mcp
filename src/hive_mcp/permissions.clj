(ns hive-mcp.permissions
  "Tiered permission system for agent tool execution.
   
   Implements escalation chain:
   - Tier 1: Auto-approve (test files, generated code)
   - Tier 2: Coordinator review (src files, returns diff)
   - Tier 3: Human approval (destructive ops, DB writes, security)
   
   Usage:
     (require '[hive-mcp.permissions :as perm])
     
     ;; Check what tier a tool call falls into
     (perm/get-tier \"file_edit\" {:file_path \"test/foo_test.clj\"})
     ;; => :tier-1
     
     ;; Escalate and get approval
     (perm/escalate! agent-id :tier-2 \"file_edit\" {:file_path \"src/core.clj\"})
     ;; => {:approved true :reviewer :coordinator}
   
   Architecture:
     Ling ──hivemind.ask!──> Coordinator ──channel──> Human
                                │                        │
                          (tier 1,2)              (tier 3 only)"
  (:require [hive-mcp.hivemind :as hivemind]
            [hive-mcp.channel :as channel]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;;; ============================================================
;;; Permission Matrix Configuration
;;; ============================================================

;; TODO: Implement - define the tier rules
(def ^:private tier-1-rules
  "Auto-approve rules: test files, mocks, generated code"
  {:tools #{"file_write" "file_edit" "clojure_edit"}
   :path-patterns [#".*_test\.(clj|cljs|cljc|go|js|ts)$"
                   #"^test/.*"
                   #".*_mock\..*"
                   #".*/generated/.*"
                   #".*/fixtures/.*"]})

(def ^:private tier-2-rules
  "Coordinator review: source files"
  {:tools #{"file_write" "file_edit" "clojure_edit"}
   :path-patterns [#"^src/.*"
                   #"^lib/.*"
                   #"^elisp/.*"]})

(def ^:private tier-3-tools
  "Human approval required: destructive/security operations"
  #{"bash" ; arbitrary shell commands
    "magit_commit" ; git history modification
    "magit_push" ; remote state change
    "eval_elisp" ; arbitrary Emacs code
    "cider_eval_explicit" ; arbitrary Clojure code
    "preset_delete" ; config removal
    "swarm_kill" ; process termination
    "mcp_memory_cleanup_expired"})

(defn dangerous-tool?
  "Check if a tool is in tier-3 (requires human approval).
   Use this for simple boolean checks without path context."
  [tool-name]
  (contains? tier-3-tools tool-name)) ; data deletion

(def ^:private tier-3-operations
  "Operations that always require human approval regardless of tool"
  #{:delete-file
    :drop-table
    :truncate-table
    :write-database
    :permission-change
    :remove-mcp-server})

;;; ============================================================
;;; Tier Detection
;;; ============================================================

;; TODO: Implement
(defn- matches-path-pattern?
  "Check if path matches any of the given regex patterns."
  [path patterns]
  (when path
    (some #(re-find % path) patterns)))

;; TODO: Implement
(defn- extract-path
  "Extract file path from tool arguments."
  [arguments]
  (or (get arguments "file_path")
      (get arguments "path")
      (get arguments "file")))

;; TODO: Implement
(defn- detect-operation
  "Detect if arguments indicate a tier-3 operation (delete, DB write, etc)."
  [tool-name arguments]
  (when (= tool-name "bash")
    (let [cmd (get arguments "command" "")]
      (cond
        (str/includes? cmd "rm ") :delete-file
        (str/includes? cmd "DROP ") :drop-table
        (str/includes? cmd "TRUNCATE ") :truncate-table
        (str/includes? cmd "DELETE FROM") :write-database
        :else nil))))

(defn get-tier
  "Determine permission tier for a tool call.
   
   Returns :tier-1, :tier-2, :tier-3, or :allowed"
  [tool-name arguments]
  (let [path (extract-path arguments)
        operation (detect-operation tool-name arguments)]
    (cond
      ;; Tier 3: Dangerous tools or operations
      (contains? tier-3-tools tool-name) :tier-3
      (contains? tier-3-operations operation) :tier-3

      ;; Tier 1: Auto-approve for test files etc
      (and (contains? (:tools tier-1-rules) tool-name)
           (matches-path-pattern? path (:path-patterns tier-1-rules)))
      :tier-1

      ;; Tier 2: Coordinator review for source files
      (and (contains? (:tools tier-2-rules) tool-name)
           (matches-path-pattern? path (:path-patterns tier-2-rules)))
      :tier-2

      ;; Default: allowed
      :else :allowed)))

;;; ============================================================
;;; Escalation Handlers
;;; ============================================================

;; TODO: Implement
(defn- handle-tier-1
  "Auto-approve for tier-1 (test files, etc)."
  [agent-id tool-name arguments]
  {:approved true
   :tier :tier-1
   :reviewer :auto})

;; TODO: Implement
(defn- handle-tier-2
  "Request coordinator review for tier-2.
   Returns diff preview, coordinator decides."
  [agent-id tool-name arguments]
  (let [path (extract-path arguments)
        ;; Show content preview for coordinator
        content-preview (when-let [content (or (get arguments "new_string")
                                               (get arguments "content"))]
                          (let [lines (str/split-lines content)]
                            (if (> (count lines) 3)
                              (str (str/join "\n" (take 3 lines)) "\n... (" (count lines) " lines)")
                              content)))
        question (if content-preview
                   (format "TIER-2: Agent %s wants to %s on %s\n\nPreview:\n%s\n\nApprove?"
                           agent-id tool-name path content-preview)
                   (format "TIER-2: Agent %s wants to %s on %s. Approve?"
                           agent-id tool-name path))
        response (hivemind/ask! agent-id question ["yes" "no"])]
    {:approved (= "yes" (:decision response))
     :tier :tier-2
     :reviewer :coordinator
     :reason (when (not= "yes" (:decision response))
               "Coordinator denied")}))

;; TODO: Implement
(defn- handle-tier-3
  "Request human approval for tier-3.
   Blocks until human responds via channel."
  [agent-id tool-name arguments]
  (let [path (extract-path arguments)
        ;; Format arguments for human review (truncate if too long)
        args-preview (let [code (or (get arguments "code")
                                    (get arguments "command")
                                    (get arguments "content"))
                           preview (if code
                                     (let [lines (str/split-lines code)
                                           truncated (if (> (count lines) 5)
                                                       (str (str/join "\n" (take 5 lines)) "\n...")
                                                       code)]
                                       (if (> (count truncated) 200)
                                         (str (subs truncated 0 200) "...")
                                         truncated))
                                     (pr-str arguments))]
                       (if (> (count preview) 300)
                         (str (subs preview 0 300) "...")
                         preview))
        question (format "TIER-3 APPROVAL REQUIRED\n\nAgent: %s\nTool: %s\nPath: %s\n\nArguments:\n%s\n\nApprove this action?"
                         agent-id tool-name (or path "N/A") args-preview)]
    ;; Emit channel event for UI notification
    (channel/emit-event! :approval-request
                         {:agent-id agent-id
                          :tool tool-name
                          :arguments arguments
                          :tier :tier-3})
    ;; Block waiting for human decision
    (let [response (hivemind/ask! agent-id question ["approve" "deny"])]
      {:approved (= "approve" (:decision response))
       :tier :tier-3
       :reviewer :human
       :reason (when (not= "approve" (:decision response))
                 "Human denied")})))

(defn escalate!
  "Escalate tool execution based on permission tier.
   
   Returns map with:
     :approved - boolean
     :tier     - which tier was applied
     :reviewer - :auto, :coordinator, or :human
     :reason   - explanation if denied"
  [agent-id tool-name arguments]
  (let [tier (get-tier tool-name arguments)]
    (log/debug "Permission check:" tool-name "-> tier:" tier)
    (case tier
      :allowed {:approved true :tier :allowed :reviewer :none}
      :tier-1 (handle-tier-1 agent-id tool-name arguments)
      :tier-2 (handle-tier-2 agent-id tool-name arguments)
      :tier-3 (handle-tier-3 agent-id tool-name arguments)
      ;; Unknown tier = deny
      {:approved false :tier tier :reviewer :error :reason "Unknown tier"})))

;;; ============================================================
;;; Integration Helpers
;;; ============================================================

(defn approved?
  "Convenience: check if escalation result was approved."
  [result]
  (:approved result))

(defn requires-escalation?
  "Check if a tool call needs escalation (tier > allowed)."
  [tool-name arguments]
  (not= :allowed (get-tier tool-name arguments)))
