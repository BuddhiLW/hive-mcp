(ns hive-mcp.tools.consolidated.project
  "Consolidated Project CLI tool."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.result-bridge :as rb]
            [hive-mcp.tools.projectile :as projectile-handlers]
            [hive-mcp.project.tree :as tree]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.dns.result :as result]
            [taoensso.timbre :as log]))

;; ── Pure Result-returning functions ───────────────────────────────────────────

(defn- scan*
  [{:keys [directory max_depth force]}]
  (let [effective-dir (or directory (ctx/current-directory) ".")
        opts (cond-> {}
               max_depth (assoc :max-depth max_depth)
               force (assoc :force force))]
    (log/info "project scan" {:directory effective-dir :opts opts})
    (result/ok (tree/scan-project-tree! effective-dir opts))))

(defn- tree*
  [{:keys [project_id]}]
  (log/info "project tree" {:project-id project_id})
  (let [all-projects (tree/query-all-projects)
        tree-data (tree/build-project-tree all-projects)]
    (if project_id
      (let [descendants (tree/get-descendants tree-data project_id)
            ancestors (tree/get-ancestors tree-data project_id)
            project (tree/query-project-by-id project_id)]
        (result/ok {:project project
                    :ancestors ancestors
                    :descendants descendants
                    :children (get (:children tree-data) project_id [])}))
      (result/ok {:roots (:roots tree-data)
                  :total-projects (count all-projects)
                  :children (:children tree-data)
                  :projects (mapv #(select-keys % [:project/id :project/path :project/type :project/parent-id])
                                  all-projects)}))))

(defn- staleness*
  [{:keys [directory]}]
  (let [effective-dir (or directory (ctx/current-directory) ".")]
    (log/info "project staleness" {:directory effective-dir})
    (result/ok {:stale (tree/tree-stale? effective-dir)
                :directory effective-dir})))

;; ── Public handlers (MCP boundary) ────────────────────────────────────────────

(defn handle-project-scan
  "Scan filesystem for .hive-project.edn files and build project hierarchy."
  [params]
  (rb/result->mcp (rb/try-result :project/scan-failed #(scan* params))))

(defn handle-project-tree
  "Query the cached project tree structure from DataScript."
  [params]
  (rb/result->mcp (rb/try-result :project/tree-failed #(tree* params))))

(defn handle-project-staleness
  "Check if project tree needs re-scanning."
  [params]
  (rb/result->mcp (rb/try-result :project/staleness-failed #(staleness* params))))

(def handlers
  {:info      projectile-handlers/handle-projectile-info
   :files     projectile-handlers/handle-projectile-files
   :search    projectile-handlers/handle-projectile-search
   :find      projectile-handlers/handle-projectile-find-file
   :recent    projectile-handlers/handle-projectile-recent
   :list      projectile-handlers/handle-projectile-list-projects
   :scan      handle-project-scan
   :tree      handle-project-tree
   :staleness handle-project-staleness})

(def handle-project
  (make-cli-handler handlers))

(def tool-def
  {:name "project"
   :consolidated true
   :description "Projectile project operations: info (project details), files (list files), search (content search), find (find by filename), recent (recently visited), list (all projects), scan (discover .hive-project.edn hierarchy), tree (query cached hierarchy), staleness (check if rescan needed). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["info" "files" "search" "find" "recent" "list" "scan" "tree" "staleness" "help"]
                                         :description "Project operation to perform"}
                              "pattern" {:type "string"
                                         :description "Glob pattern to filter files or search pattern"}
                              "filename" {:type "string"
                                          :description "Filename to search for"}
                              "directory" {:type "string"
                                           :description "Root directory for scan/staleness check"}
                              "max_depth" {:type "integer"
                                           :description "Maximum scan depth (default: 5)"}
                              "force" {:type "boolean"
                                       :description "Force rescan even if fresh"}
                              "project_id" {:type "string"
                                            :description "Project ID to query tree for"}}
                 :required ["command"]}
   :handler handle-project})

(def tools [tool-def])
