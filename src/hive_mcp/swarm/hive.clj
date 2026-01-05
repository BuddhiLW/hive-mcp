(ns hive-mcp.swarm.hive
  "Hive knowledge injection for swarm lings.

   Queries project memory and formats it for injection into ling system context.
   Enables collective learning: each new ling starts with the hive's accumulated
   knowledge rather than starting cold.

   Memory types injected:
   - conventions: Coding standards, patterns, anti-patterns discovered
   - decisions: Recent architectural/technical decisions with rationale
   - notes: Important context about the project

   Usage:
   (build-hive-context {:conventions 10 :decisions 5})
   => \"== CONVENTIONS ==\\n- Use specs for validation...\\n\\n== DECISIONS ==\\n...\""
  (:require [hive-mcp.emacsclient :as ec]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; =============================================================================
;; Memory Query (Direct elisp call, bypasses MCP tool layer)
;; =============================================================================

(defn- query-memory
  "Query project memory directly via elisp.
   Returns parsed JSON or nil on error."
  [type limit]
  (let [elisp (format "(json-encode (hive-mcp-api-memory-query \"%s\" nil %d))"
                      (name type) limit)
        {:keys [success result]} (ec/eval-elisp-with-timeout elisp 3000)]
    (when success
      (try
        (json/read-str result :key-fn keyword)
        (catch Exception e
          (log/warn "Failed to parse memory query result:" (.getMessage e))
          nil)))))

(defn- format-convention
  "Format a convention entry for context injection."
  [{:keys [content tags]}]
  (let [tags-str (when (seq tags) (str " [" (str/join ", " tags) "]"))]
    (str "- " content tags-str)))

(defn- format-decision
  "Format a decision entry for context injection."
  [{:keys [content created]}]
  (let [date (when created (subs created 0 10))] ; YYYY-MM-DD
    (str "- [" (or date "?") "] " content)))

(defn- format-note
  "Format a note entry for context injection."
  [{:keys [content]}]
  (str "- " content))

;; =============================================================================
;; Context Building
;; =============================================================================

(defn build-hive-context
  "Build hive knowledge context for ling injection.

   Options:
   - :conventions - Number of conventions to include (default: 10)
   - :decisions   - Number of recent decisions (default: 5)
   - :notes       - Number of notes (default: 0, opt-in)
   - :include-empty? - Include sections even if empty (default: false)

   Returns formatted string suitable for system prompt injection."
  ([] (build-hive-context {}))
  ([{:keys [conventions decisions notes include-empty?]
     :or {conventions 10 decisions 5 notes 0 include-empty? false}}]
   (let [sections (atom [])]

     ;; Conventions section
     (when (pos? conventions)
       (when-let [convs (seq (query-memory :convention conventions))]
         (swap! sections conj
                (str "== TEAM CONVENTIONS ==\n"
                     (str/join "\n" (map format-convention convs)))))
       (when (and include-empty? (nil? (query-memory :convention 1)))
         (swap! sections conj "== TEAM CONVENTIONS ==\n(none established yet)")))

     ;; Decisions section
     (when (pos? decisions)
       (when-let [decs (seq (query-memory :decision decisions))]
         (swap! sections conj
                (str "== RECENT DECISIONS ==\n"
                     (str/join "\n" (map format-decision decs)))))
       (when (and include-empty? (nil? (query-memory :decision 1)))
         (swap! sections conj "== RECENT DECISIONS ==\n(none recorded yet)")))

     ;; Notes section (opt-in)
     (when (pos? notes)
       (when-let [ns (seq (query-memory :note notes))]
         (swap! sections conj
                (str "== PROJECT NOTES ==\n"
                     (str/join "\n" (map format-note ns))))))

     (let [context (str/join "\n\n" @sections)]
       (when (seq context)
         (str "# HIVE KNOWLEDGE\n"
              "The following is accumulated knowledge from previous sessions.\n"
              "Follow these conventions and respect these decisions.\n\n"
              context))))))

(defn build-spawn-context
  "Build complete context for ling spawn.
   Combines hive knowledge with any task-specific context.

   Arguments:
   - task-context: Optional string with task-specific instructions
   - opts: Options for build-hive-context

   Returns context string for injection, or nil if nothing to inject."
  ([task-context] (build-spawn-context task-context {}))
  ([task-context opts]
   (let [hive (build-hive-context opts)]
     (cond
       (and hive task-context) (str hive "\n\n" task-context)
       hive hive
       task-context task-context
       :else nil))))

;; =============================================================================
;; Integration with Swarm Spawn
;; =============================================================================

(defn wrap-prompt-with-context
  "Wrap a dispatch prompt with hive context.
   Used when dispatching to inject knowledge without modifying spawn.

   Returns: {:prompt wrapped-prompt :context-injected? bool}"
  [original-prompt opts]
  (if-let [context (build-hive-context opts)]
    {:prompt (str context "\n\n---\n\n" original-prompt)
     :context-injected? true
     :context-size (count context)}
    {:prompt original-prompt
     :context-injected? false}))

(defn get-hive-stats
  "Get statistics about available hive knowledge."
  []
  {:conventions (count (or (query-memory :convention 100) []))
   :decisions (count (or (query-memory :decision 100) []))
   :notes (count (or (query-memory :note 100) []))})

(comment
  ;; Development REPL examples

  ;; Check what knowledge is available
  (get-hive-stats)

  ;; Build context with defaults
  (println (build-hive-context))

  ;; Build with custom limits
  (println (build-hive-context {:conventions 5 :decisions 3}))

  ;; Wrap a prompt
  (wrap-prompt-with-context "Fix the bug in core.clj" {}))
