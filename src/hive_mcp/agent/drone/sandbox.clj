(ns hive-mcp.agent.drone.sandbox
  "Drone sandbox for safe execution environment enforcement."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def blocked-tools
  "Tools that drones should never have access to."
  #{"bash"
    "mcp__emacs__bash"
    ;; Agent spawning
    "swarm_spawn"
    "swarm_dispatch"
    "swarm_broadcast"
    "agent_delegate"
    "delegate_drone"
    ;; Memory writes (drones are read-only)
    "mcp_memory_add"
    "mcp__emacs__mcp_memory_add"
    "mcp_mem_kanban_create"
    "mcp__emacs__mcp_mem_kanban_create"
    "mcp_mem_kanban_move"
    "mcp__emacs__mcp_mem_kanban_move"
    ;; Direct file writes (must use propose_diff)
    "file_write"
    "mcp__emacs__file_write"
    "file_edit"})

(def blocked-patterns
  "File patterns that should never be accessed by drones."
  [#"\.env$"
   #"\.env\."
   #"credentials"
   #"secret"
   #"password"
   #"\.pem$"
   #"\.key$"
   #"\.crt$"
   #"id_rsa"
   #"id_ed25519"
   #"token"
   #"\.gpg$"])

(def file-tools
  "Tools that operate on specific files."
  #{"read_file"
    "mcp__emacs__read_file"
    "propose_diff"
    "kondo_lint"
    "mcp__emacs__kondo_lint"
    "kondo_analyze"
    "mcp__emacs__kondo_analyze"})

(def directory-tools
  "Tools that operate on directories."
  #{"grep"
    "mcp__emacs__grep"
    "glob_files"
    "mcp__emacs__glob_files"})

(defn- parent-dir
  "Get parent directory of a file path."
  [file-path]
  (when file-path
    (let [f (io/file file-path)
          parent (.getParent f)]
      (or parent "."))))

(defn- normalize-path
  "Normalize a file path for comparison using canonical path resolution."
  [path]
  (when path
    (try
      (.getCanonicalPath (io/file path))
      (catch Exception _
        (-> path
            (str/replace #"^\./" "")
            (str/replace #"/+" "/")
            (str/replace #"/$" ""))))))

(defn validate-path-containment
  "Validate that a path resolves within an allowed root directory."
  [path root-dir]
  (when (and path root-dir)
    (try
      (let [root-canonical (.getCanonicalPath (io/file root-dir))
            file (if (.isAbsolute (io/file path))
                   (io/file path)
                   (io/file root-dir path))
            path-canonical (.getCanonicalPath file)]
        (if (str/starts-with? path-canonical root-canonical)
          {:valid? true :canonical-path path-canonical}
          {:valid? false
           :error (str "Path '" path "' escapes allowed directory '" root-dir "'. "
                       "Canonical: " path-canonical)}))
      (catch Exception e
        {:valid? false
         :error (str "Path validation failed: " (.getMessage e))}))))

(defn create-sandbox
  "Create a sandbox specification for drone execution."
  ([files] (create-sandbox files nil))
  ([files project-root]
   (let [effective-root (or project-root (System/getProperty "user.dir"))
         validations (for [f files]
                       (let [result (validate-path-containment f effective-root)]
                         (assoc result :original-path f)))
         valid-paths (->> validations
                          (filter :valid?)
                          (map :canonical-path))
         rejected (->> validations
                       (remove :valid?)
                       (map (fn [v] {:path (:original-path v)
                                     :error (:error v)})))]
     (when (seq rejected)
       (log/warn "Sandbox rejected paths escaping project directory"
                 {:project-root effective-root
                  :rejected-count (count rejected)
                  :rejected rejected}))
     (let [normalized (set (map normalize-path valid-paths))
           dirs (set (map parent-dir valid-paths))]
       {:allowed-files normalized
        :allowed-dirs dirs
        :blocked-patterns blocked-patterns
        :blocked-tools blocked-tools
        :rejected-files (vec rejected)}))))

(defn- matches-blocked-pattern?
  "Check if a path matches any blocked pattern."
  [path patterns]
  (when path
    (let [normalized (normalize-path path)]
      (some #(re-find % normalized) patterns))))

(defn- file-in-scope?
  "Check if a file path is within the allowed files set."
  [path allowed-files]
  (when path
    (let [normalized (normalize-path path)]
      (contains? allowed-files normalized))))

(defn- path-in-allowed-dirs?
  "Check if a path is within any of the allowed directories."
  [path allowed-dirs]
  (when path
    (let [canonical (normalize-path path)
          canonical-allowed (set (keep normalize-path allowed-dirs))]
      (or (contains? canonical-allowed canonical)
          (some (fn [allowed-dir]
                  (and allowed-dir
                       (str/starts-with? canonical (str allowed-dir "/"))))
                canonical-allowed)))))

(defn- extract-path-from-args
  "Extract file/directory path from tool arguments."
  [args]
  (or (:file_path args)
      (:path args)
      (:file-path args)
      (get args "file_path")
      (get args "path")))

(defn sandbox-allows?
  "Check if sandbox allows a tool call with given arguments."
  [sandbox tool-name args]
  (let [{:keys [allowed-files allowed-dirs blocked-patterns blocked-tools]} sandbox
        path (extract-path-from-args args)]

    (cond
      (contains? blocked-tools tool-name)
      {:allowed? false
       :reason (str "Tool '" tool-name "' is blocked for drones")}

      (and path (matches-blocked-pattern? path blocked-patterns))
      {:allowed? false
       :reason (str "Path matches blocked pattern (sensitive file): " path)}

      (and (contains? file-tools tool-name)
           path
           (not (file-in-scope? path allowed-files)))
      {:allowed? false
       :reason (str "File not in drone's allowed scope: " path)}

      (and (contains? directory-tools tool-name)
           path
           (not (path-in-allowed-dirs? path allowed-dirs)))
      {:allowed? false
       :reason (str "Directory not in drone's allowed scope: " path)}

      :else
      {:allowed? true})))

(defn violation-message
  "Generate helpful error message for sandbox violation."
  [tool-name reason sandbox]
  (str "SANDBOX VIOLATION [" tool-name "]: " reason "\n\n"
       "You are a drone with restricted access. Your allowed files are:\n"
       (str/join "\n" (map #(str "  - " %) (:allowed-files sandbox)))
       "\n\n"
       "To modify a file not in your scope, the parent ling must delegate "
       "a new drone task with that file included."))

(defn- log-tool-call!
  "Log a drone tool call for audit purposes."
  [drone-id tool-name args allowed?]
  (let [path (extract-path-from-args args)
        log-data {:drone-id drone-id
                  :tool tool-name
                  :path path
                  :allowed? allowed?
                  :timestamp (System/currentTimeMillis)}]
    (if allowed?
      (log/debug "Drone tool call" log-data)
      (log/warn "DRONE SANDBOX VIOLATION" log-data))))

(defn create-audit-fn
  "Create an audit function for a specific drone."
  [drone-id]
  (fn [tool-name args allowed?]
    (log-tool-call! drone-id tool-name args allowed?)))

(defn wrap-tool-for-sandbox
  "Wrap a tool handler with sandbox enforcement."
  [tool sandbox audit-fn]
  (let [tname (:name tool)]
    (if (contains? (:blocked-tools sandbox) tname)
      nil
      (update tool :handler
              (fn [orig-handler]
                (fn [args]
                  (let [result (sandbox-allows? sandbox tname args)
                        allowed? (:allowed? result)]
                    (when audit-fn
                      (audit-fn tname args allowed?))
                    (if allowed?
                      (orig-handler args)
                      {:type "text"
                       :text (violation-message tname (:reason result) sandbox)
                       :isError true}))))))))

(defn filter-tools-for-sandbox
  "Apply sandbox restrictions to a list of tools."
  [tools sandbox audit-fn]
  (->> tools
       (map #(wrap-tool-for-sandbox % sandbox audit-fn))
       (remove nil?)))

(defn enforce-sandbox
  "Create sandbox and wrap tools for a drone."
  [drone-id files tools]
  (let [sandbox (create-sandbox files)
        audit-fn (create-audit-fn drone-id)]
    (log/info "Drone sandbox created"
              {:drone-id drone-id
               :allowed-files (count files)
               :tools-before (count tools)})
    (let [sandboxed (filter-tools-for-sandbox tools sandbox audit-fn)]
      (log/info "Sandbox enforcement applied"
                {:drone-id drone-id
                 :tools-after (count sandboxed)
                 :blocked (- (count tools) (count sandboxed))})
      sandboxed)))

(defn sandbox-status
  "Get current sandbox configuration for diagnostics."
  []
  {:blocked-tools blocked-tools
   :blocked-patterns (map str blocked-patterns)
   :file-tools file-tools
   :directory-tools directory-tools})
