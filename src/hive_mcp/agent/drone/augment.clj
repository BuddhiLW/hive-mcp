(ns hive-mcp.agent.drone.augment
  "Task augmentation with context and file contents for drone execution."
  (:require [hive-mcp.agent.registry :as registry]
            [hive-mcp.agent.drone.sandbox :as sandbox]
            [hive-mcp.agent.drone.context :as ctx]
            [hive-mcp.agent.drone.kg-context :as kg-ctx]
            [hive-mcp.agent.drone.kg-priming :as kg-priming]
            [hive-mcp.agent.drone.unified-context :as unified-ctx]
            [hive-mcp.agent.context-envelope :as context-envelope]
            [hive-mcp.context.budget :as budget]
            [hive-mcp.knowledge-graph.disc :as kg-disc]
            [hive-mcp.tools.diff :as diff]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

(defn prepare-context
  "Gather catchup data (conventions, decisions, snippets) for drone context."
  []
  (try
    (let [catchup-handler (registry/get-tool "mcp_get_context")
          context (when (:handler catchup-handler)
                    ((:handler catchup-handler) {}))]
      (if (and context (:text context))
        (let [parsed (json/read-str (:text context) :key-fn keyword)]
          {:conventions (get-in parsed [:memory :conventions] [])
           :decisions (get-in parsed [:memory :decisions] [])
           :snippets (get-in parsed [:memory :snippets] [])
           :project (get parsed :project {})})
        {}))
    (catch Exception e
      (log/warn e "Failed to gather ling context")
      {})))

(defn format-context-str
  "Format context data as string for task augmentation."
  [context]
  (when (seq context)
    (let [sections (cond-> ""
                     (seq (:conventions context))
                     (str "### Conventions\n"
                          (str/join "\n" (map :content (:conventions context)))
                          "\n\n")

                     (seq (:decisions context))
                     (str "### Decisions\n"
                          (str/join "\n" (map :content (:decisions context)))
                          "\n\n"))]
      (when (seq sections)
        (str "## Project Context\n" sections)))))

(defn format-file-contents
  "Pre-read file contents so drone has exact content for propose_diff."
  [files project-root]
  (when (seq files)
    (let [effective-root (or project-root (diff/get-project-root) "")
          contents (for [f files]
                     (let [validation (sandbox/validate-path-containment f effective-root)]
                       (if (:valid? validation)
                         (try
                           (let [content (slurp (:canonical-path validation))]
                             (future
                               (try
                                 (kg-disc/touch-disc! (:canonical-path validation))
                                 (catch Exception e
                                   (log/debug "Disc touch failed (non-fatal):" (.getMessage e)))))
                             (str "### " f "\n```\n" content "```\n"))
                           (catch Exception e
                             (str "### " f "\n(File not found or unreadable: " (.getMessage e) ")\n")))
                         (do
                           (log/warn "Path validation failed in format-file-contents"
                                     {:file f :error (:error validation)})
                           (str "### " f "\n(BLOCKED: " (:error validation) ")\n")))))]
      (str "## Current File Contents\n"
           "IMPORTANT: Use this EXACT content as old_content in propose_diff.\n"
           "Do NOT guess or assume file content - use what is provided below.\n\n"
           (str/join "\n" contents)))))

(defn build-smart-context
  "Build smart context (imports, related functions, lint warnings) for target files."
  [files task project-root project-id]
  (when (seq files)
    (let [contexts (for [f files]
                     (try
                       (let [ctx-data (ctx/build-drone-context
                                       {:file-path f
                                        :task task
                                        :project-root project-root
                                        :project-id project-id})]
                         (when (:formatted ctx-data)
                           (str "## Smart Context for " f "\n"
                                (:formatted ctx-data))))
                       (catch Exception e
                         (log/debug "Could not build smart context for" f (.getMessage e))
                         nil)))
          non-nil-contexts (remove nil? contexts)]
      (when (seq non-nil-contexts)
        (str/join "\n\n" non-nil-contexts)))))

(defn augment-task
  "Augment task with context and file contents using compressed, unified, or legacy paths."
  [task files & [{:keys [project-root project-id use-kg-first return-metadata seeds
                         use-unified token-budget ctx-refs kg-node-ids]
                  :or {use-kg-first true
                       use-unified  true
                       token-budget budget/default-total-budget}}]]
  (let [effective-root (or project-root (diff/get-project-root) "")
        effective-project-id (or project-id "hive-mcp")

        ;; Compressed path: pre-resolved context refs from wave dispatch
        compressed-ctx-str
        (when (seq ctx-refs)
          (try
            (let [envelope (context-envelope/enrich-context
                            ctx-refs
                            (or kg-node-ids [])
                            effective-project-id
                            {:mode :inline})]
              (when (seq envelope)
                (log/info "augment-task using COMPRESSED context path (ctx-refs)"
                          {:project-id effective-project-id
                           :ref-count (count ctx-refs)
                           :kg-seeds (count (or kg-node-ids []))
                           :envelope-chars (count envelope)})
                envelope))
            (catch Exception e
              (log/debug "Compressed context (ctx-refs) failed, falling back:" (.getMessage e))
              nil)))

        ;; Unified path: single KG traversal for conventions + domain
        use-unified? (and (not compressed-ctx-str)
                          use-unified
                          (unified-ctx/unified-context-available?))

        unified-raw
        (when use-unified?
          (try
            (unified-ctx/prepare-drone-context
             {:task       task
              :seeds      seeds
              :project-id effective-project-id})
            (catch Exception e
              (log/debug "Unified context failed, falling back to legacy:" (.getMessage e))
              nil)))

        ;; Entry-level budget allocation before formatting
        unified-ctx-str
        (when unified-raw
          (if token-budget
            (let [kg-budget (long (Math/floor (* (long token-budget) 0.6)))
                  selected (budget/allocate-unified-entries
                            {:total-budget  kg-budget
                             :conventions   (:conventions unified-raw)
                             :decisions     (:decisions unified-raw)
                             :snippets      (:snippets unified-raw)
                             :domain        (:domain unified-raw)})
                  selected-ctx (assoc unified-raw
                                      :conventions (:conventions selected)
                                      :decisions   (:decisions selected)
                                      :snippets    (:snippets selected)
                                      :domain      (get selected :domain []))]
              (log/info "augment-task unified entry-level budget"
                        {:kg-budget kg-budget
                         :budget-used (:budget-used selected)
                         :budget-remaining (:budget-remaining selected)
                         :metadata (:metadata selected)})
              (unified-ctx/format-unified-context selected-ctx))
            (unified-ctx/format-unified-context unified-raw)))

        ;; Legacy path: separate pipelines (fallback)
        context-str (when-not (or compressed-ctx-str unified-ctx-str)
                      (format-context-str (prepare-context)))

        smart-ctx-str (when-not (or compressed-ctx-str unified-ctx-str)
                        (build-smart-context files task effective-root effective-project-id))

        primed-ctx-str (when (and (not compressed-ctx-str) (not unified-ctx-str) (seq seeds))
                         (try
                           (let [primed (kg-priming/prime-context
                                         {:task task
                                          :seeds seeds
                                          :project-id effective-project-id
                                          :token-budget 1500})]
                             (when (seq primed) primed))
                           (catch Exception e
                             (log/debug "Domain priming failed (non-fatal):" (.getMessage e))
                             nil)))

        ;; File contents: KG-first approach
        {:keys [context files-read kg-skipped summary]}
        (if (and use-kg-first (seq files))
          (kg-ctx/format-files-with-kg-context files {:project-root effective-root})
          {:context (format-file-contents files project-root)
           :files-read files
           :kg-skipped []
           :summary {:kg-known 0 :needs-read (count files) :stale 0}})

        file-contents-str context

        ;; Fixed sections (always injected)
        project-dir-str
        (when (seq effective-root)
          (str "## Project Directory\n"
               "IMPORTANT: When calling propose_diff, you MUST include:\n"
               "  directory: \"" effective-root "\"\n"
               "This ensures paths are validated against YOUR project, not the MCP server.\n\n"))

        task-str (str "## Task\n" task
                      (when (seq files)
                        (str "\n\n## Files to modify\n"
                             (str/join "\n" (map #(str "- " %) files)))))

        ;; Merge context sections: compressed > unified > legacy
        kg-ctx-str (cond
                     compressed-ctx-str compressed-ctx-str
                     unified-ctx-str    unified-ctx-str
                     :else              (str context-str
                                             (when primed-ctx-str (str "\n" primed-ctx-str "\n"))
                                             (when smart-ctx-str (str "\n" smart-ctx-str "\n"))))

        ;; Budget allocation
        {:keys [kg-context-out file-contents-out budget-metadata]}
        (if token-budget
          (let [allocated (budget/allocate-drone-context
                           {:total-budget token-budget
                            :preset       (str project-dir-str task-str)
                            :kg-context   (or kg-ctx-str "")
                            :file-contents (or file-contents-str "")})]
            {:kg-context-out    (:kg-context allocated)
             :file-contents-out (:file-contents allocated)
             :budget-metadata   (:metadata allocated)})
          {:kg-context-out    kg-ctx-str
           :file-contents-out file-contents-str
           :budget-metadata   nil})

        augmented (str
                   (when (seq kg-context-out)
                     (str kg-context-out "\n"))
                   project-dir-str
                   task-str
                   (when (seq file-contents-out)
                     (str "\n\n" file-contents-out)))]

    (cond
      compressed-ctx-str
      (log/info "augment-task used COMPRESSED context path (ctx-refs)"
                {:project-id effective-project-id
                 :compressed-chars (count compressed-ctx-str)
                 :ref-count (count ctx-refs)
                 :budget-managed? (some? token-budget)})

      unified-ctx-str
      (log/info "augment-task used UNIFIED context path"
                {:project-id effective-project-id
                 :unified-chars (count unified-ctx-str)
                 :budget-managed? (some? token-budget)})

      :else
      (when primed-ctx-str
        (log/info "Domain context primed"
                  {:seeds seeds
                   :primed-chars (count primed-ctx-str)
                   :priming-available? (kg-priming/priming-available?)})))

    (when budget-metadata
      (log/info "augment-task budget allocation"
                {:total-budget (:total-budget budget-metadata)
                 :total-tokens (:total-tokens budget-metadata)
                 :remaining (:remaining budget-metadata)
                 :layers (mapv (fn [l] (select-keys l [:id :budget :tokens :truncated?]))
                               (:layers budget-metadata))}))

    (when (seq kg-skipped)
      (log/info "KG-first augment-task saved file reads"
                {:kg-skipped (count kg-skipped)
                 :files-read (count files-read)
                 :summary summary}))

    (if return-metadata
      {:task augmented
       :files-read files-read
       :kg-skipped kg-skipped
       :summary summary
       :compressed? (boolean compressed-ctx-str)
       :unified? (boolean unified-ctx-str)
       :budget budget-metadata}
      augmented)))
