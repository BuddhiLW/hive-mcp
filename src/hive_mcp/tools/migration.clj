(ns hive-mcp.tools.migration
  "MCP tool for unified migration operations.

   Commands:
   - status     - Current backend info and data counts
   - backup     - Create timestamped backup (scope-aware)
   - restore    - Restore from backup (with cross-project guard)
   - export     - Export with optional adapter transform
   - import     - Import with optional adapter transform
   - switch     - Switch backend with auto-migration
   - sync       - Mirror to secondary backend
   - list       - List available backups (filterable by project)
   - validate   - Validate backup integrity
   - adapters   - List available adapters

   Project Scope Awareness:
   - backup: Includes :backup/project-id derived from :directory param
   - restore: Checks scope compatibility, blocks cross-project by default
   - list: Supports :project-id filtering

   CLARITY-T: All operations logged with full context."
  (:require [hive-mcp.migration.core :as core]
            [hive-mcp.migration.adapter :as adapter]
            [hive-mcp.knowledge-graph.connection :as kg-conn]
            [hive-mcp.knowledge-graph.migration :as kg-mig]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.chroma :as chroma]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Status Command
;; =============================================================================

(defn- get-kg-status
  "Get current KG backend status and counts."
  []
  (let [backend (kg-mig/detect-current-backend)
        counts (try
                 {:edges (count (kg-conn/query '[:find ?e :where [?e :kg-edge/id]]))
                  :disc (count (kg-conn/query '[:find ?e :where [?e :disc/path]]))
                  :synthetic (count (kg-conn/query '[:find ?e :where [?e :kg-synthetic/id]]))}
                 (catch Exception e
                   {:error (.getMessage e)}))]
    {:backend backend
     :counts counts
     :temporal? (kg-conn/temporal-store?)}))

(defn cmd-status
  "Get migration status for all scopes.

   Returns:
     {:kg       {:backend :datalevin, :counts {...}, :temporal? false}
      :backups  {:count N, :latest {...}}}"
  [_params]
  (log/info "Migration status check")
  (let [kg-status (get-kg-status)
        backups (core/list-backups {:limit 5})
        latest-kg (core/latest-backup :kg)]
    {:kg kg-status
     :backups {:count (count (core/list-backups {:limit 1000}))
               :latest latest-kg
               :recent (take 3 backups)}}))

;; =============================================================================
;; Memory Export Helper
;; =============================================================================

(defn- export-memory-to-edn
  "Export all Chroma memory entries to EDN format for backup.
   Returns {:entries [...] :counts {:note N :snippet N ...} :total N}"
  []
  (let [;; Get all entries including expired for complete backup
        entries (chroma/query-entries :limit 50000 :include-expired? true)
        ;; Remove :document field (regenerated on import from content)
        clean-entries (mapv #(dissoc % :document) entries)
        ;; Group by type for counts
        by-type (group-by :type entries)]
    {:entries clean-entries
     :counts (into {} (map (fn [[k v]] [(keyword k) (count v)]) by-type))
     :total (count entries)}))

;; =============================================================================
;; Backup Command
;; =============================================================================

(defn cmd-backup
  "Create timestamped backup with project scope metadata.

   Parameters:
     :scope      - :kg, :memory, :full (default: :kg)
     :dir        - Backup directory (default: data/backups)
     :adapter    - Export adapter :edn, :json (default: :edn)
     :directory  - Working directory for project-id derivation (Go ctx pattern)
     :project-id - Explicit project-id override (default: derived from directory)

   Returns:
     {:success true, :path \"...\", :size N, :counts {...}, :project-id \"...\"}"
  [{:keys [scope dir adapter directory project-id]
    :or {scope :kg
         dir "data/backups"
         adapter :edn}}]
  (let [pid (or project-id (scope/get-current-project-id directory))]
    (log/info "Creating backup" {:scope scope :dir dir :adapter adapter :project-id pid})
    (core/ensure-backup-dir! dir)

    (case scope
      :kg
      (let [;; Export KG data
            export-data (kg-mig/export-to-edn)
            counts (:counts export-data)
            backend (kg-mig/detect-current-backend)
            ;; Generate unique filename
            filename (core/generate-backup-name :kg backend)
            path (str dir "/" filename)
            ;; Apply adapter transform if not :edn
            transformed (if (= adapter :edn)
                          export-data
                          (adapter/transform-export adapter export-data))
            ;; Create metadata with project scope
            metadata (core/backup-metadata :kg backend counts pid)]
        (core/write-backup! path metadata {:data transformed}))

      :memory
      (let [;; Export Memory/Chroma data
            export-data (export-memory-to-edn)
            counts (:counts export-data)
            ;; Generate unique filename
            filename (core/generate-backup-name :memory :chroma)
            path (str dir "/" filename)
            ;; Apply adapter transform if not :edn
            transformed (if (= adapter :edn)
                          export-data
                          (adapter/transform-export adapter export-data))
            ;; Create metadata with project scope
            metadata (core/backup-metadata :memory :chroma counts pid)]
        (core/write-backup! path metadata {:data transformed}))

      :full
      (let [;; Export both KG and Memory
            kg-data (kg-mig/export-to-edn)
            memory-data (export-memory-to-edn)
            kg-backend (kg-mig/detect-current-backend)
            ;; Combined counts
            counts {:kg (:counts kg-data)
                    :memory (:counts memory-data)}
            ;; Generate unique filename
            filename (core/generate-backup-name :full kg-backend)
            path (str dir "/" filename)
            ;; Combined data structure
            combined {:kg kg-data
                      :memory memory-data}
            ;; Apply adapter transform if not :edn
            transformed (if (= adapter :edn)
                          combined
                          (adapter/transform-export adapter combined))
            ;; Create metadata with project scope
            metadata (core/backup-metadata :full kg-backend counts pid)]
        (core/write-backup! path metadata {:data transformed}))

      (throw (ex-info "Unknown scope" {:scope scope :valid [:kg :memory :full]})))))

;; =============================================================================
;; Restore Command
;; =============================================================================

(defn cmd-restore
  "Restore from backup file with project scope guard.

   Parameters:
     :path                - Full path to backup file (required if no :latest)
     :latest              - Restore latest backup for scope (e.g., :kg)
     :dry-run             - Preview without applying (default: false)
     :adapter             - Import adapter (default: :edn)
     :directory           - Working directory for project-id derivation (Go ctx pattern)
     :project-id          - Explicit target project-id (default: derived from directory)
     :force-cross-project - Allow restoring backup from different project (default: false)

   Returns:
     {:success true, :imported {...}, :validation {...}, :project-id \"...\"}

   Cross-project guard:
     By default, restoring a backup from a different project is blocked.
     Set :force-cross-project true to override. Global backups (project-id='global')
     are always allowed. V1 backups without project-id are treated as global."
  [{:keys [path latest dry-run adapter directory project-id force-cross-project]
    :or {dry-run false
         adapter :edn
         force-cross-project false}}]
  (let [backup-path (or path
                        (when latest
                          (:path (core/latest-backup latest))))
        target-pid (or project-id (scope/get-current-project-id directory))]
    (when-not backup-path
      (throw (ex-info "No backup path specified and no latest found"
                      {:latest latest})))

    (log/info "Restoring backup" {:path backup-path :dry-run dry-run :target-project-id target-pid})

    ;; Validate first
    (let [validation (core/validate-backup backup-path)]
      (when-not (:valid? validation)
        (throw (ex-info "Backup validation failed" validation)))

      (if dry-run
        ;; Dry-run: show scope compatibility info
        (let [backup-data (core/read-backup backup-path)
              compat (core/check-scope-compatibility backup-data target-pid)]
          {:dry-run true
           :path backup-path
           :metadata (:metadata validation)
           :would-import (:metadata validation)
           :scope-check compat})

        ;; Actually restore - check scope first
        (let [backup-data (core/read-backup backup-path)
              compat (core/check-scope-compatibility backup-data target-pid)]

          ;; Scope guard: block cross-project restore unless forced
          (when (and (not (:compatible? compat))
                     (not force-cross-project))
            (throw (ex-info "Cross-project restore blocked"
                            {:reason (:reason compat)
                             :backup-project-id (:backup-project-id compat)
                             :target-project-id (:target-project-id compat)
                             :hint "Use :force-cross-project true to override"})))

          (let [data (if (= adapter :edn)
                       (:data backup-data)
                       (adapter/transform-import adapter (:data backup-data)))
                result (kg-mig/import-from-edn data)]
            {:success (empty? (:errors result))
             :path backup-path
             :imported (:imported result)
             :errors (:errors result)
             :project-id target-pid
             :scope-check compat}))))))

;; =============================================================================
;; List Command
;; =============================================================================

(defn cmd-list
  "List available backups with optional project scope filtering.

   Parameters:
     :scope      - Filter by scope (:kg, :memory, :full)
     :backend    - Filter by backend
     :limit      - Max results (default: 20)
     :dir        - Backup directory
     :directory  - Working directory for project-id derivation (Go ctx pattern)
     :project-id - Filter by project scope (reads backup metadata)

   Returns:
     {:backups [...], :count N, :project-id-filter \"...\" (when filtered)}"
  [{:keys [scope backend limit dir directory project-id]
    :or {limit 20
         dir "data/backups"}}]
  (let [pid-filter (or project-id
                       (when directory
                         (scope/get-current-project-id directory)))
        backups (core/list-backups {:scope scope
                                    :backend backend
                                    :project-id pid-filter
                                    :limit limit
                                    :dir dir})]
    (cond-> {:backups backups
             :count (count backups)}
      pid-filter (assoc :project-id-filter pid-filter))))

;; =============================================================================
;; Switch Command
;; =============================================================================

(defn cmd-switch
  "Switch KG backend with automatic migration.

   Parameters:
     :target   - Target backend :datascript, :datalevin, :datahike (required)
     :db-path  - Path for persistent backends
     :dry-run  - Preview without switching (default: false)
     :backup   - Create backup before switch (default: true)

   Returns:
     {:success true, :from :datalevin, :to :datahike, :migrated {...}}"
  [{:keys [target db-path dry-run backup]
    :or {dry-run false
         backup true}}]
  (when-not target
    (throw (ex-info "Target backend required" {:valid [:datascript :datalevin :datahike]})))

  (let [current (kg-mig/detect-current-backend)]
    (log/info "Switching backend" {:from current :to target :dry-run dry-run})

    (when (= current target)
      (throw (ex-info "Already using target backend" {:current current :target target})))

    ;; Create backup first if requested
    (when (and backup (not dry-run))
      (log/info "Creating pre-switch backup")
      (cmd-backup {:scope :kg}))

    (if dry-run
      {:dry-run true
       :from current
       :to target
       :would-migrate (kg-mig/export-to-edn)}

      ;; Perform migration
      (let [target-opts (when db-path {:db-path db-path})
            result (case target
                     :datahike (kg-mig/migrate-to-datahike!
                                (merge target-opts {:dry-run false}))
                     :datalevin (kg-mig/migrate-to-datalevin!
                                 (merge target-opts {:dry-run false}))
                     :datascript (kg-mig/migrate-store! current :datascript
                                                        {:dry-run false}))]
        {:success (:valid? (:validation result) true)
         :from current
         :to target
         :migrated (:imported result)
         :validation (:validation result)}))))

;; =============================================================================
;; Sync Command
;; =============================================================================

(defn cmd-sync
  "Sync KG data to secondary backend without switching.

   Parameters:
     :target      - Target backend (required)
     :target-opts - Backend-specific options
     :backup      - Create backup of target before sync (default: true)

   Returns:
     {:success true, :synced {...}, :validation {...}}"
  [{:keys [target target-opts]
    :or {target-opts {}}}]
  (when-not target
    (throw (ex-info "Target backend required" {:valid [:datascript :datalevin :datahike]})))

  (log/info "Syncing to secondary backend" {:target target})
  (let [result (kg-mig/sync-to-backend! target {:target-opts target-opts})]
    {:success (:valid? (:validation result))
     :from (:source-backend result)
     :to target
     :synced (:imported result)
     :validation (:validation result)}))

;; =============================================================================
;; Validate Command
;; =============================================================================

(defn cmd-validate
  "Validate backup file integrity.

   Parameters:
     :path - Path to backup file (required)

   Returns:
     {:valid? boolean, :errors [...], :metadata {...}}"
  [{:keys [path]}]
  (when-not path
    (throw (ex-info "Backup path required" {})))
  (core/validate-backup path))

;; =============================================================================
;; Adapters Command
;; =============================================================================

(defn cmd-adapters
  "List available migration adapters.

   Returns:
     {:adapters [{:id :edn, :name \"...\", :description \"...\"}...]}"
  [_params]
  {:adapters (adapter/list-adapters)})

;; =============================================================================
;; Export Command
;; =============================================================================

(defn cmd-export
  "Export data with optional adapter transformation.

   Parameters:
     :scope   - :kg, :memory (default: :kg)
     :path    - Output file path (required)
     :adapter - Transform adapter :edn, :json (default: :edn)
     :format  - Output format :edn, :json (default: matches adapter)

   Returns:
     {:success true, :path \"...\", :counts {...}}"
  [{:keys [scope path adapter]
    :or {scope :kg
         adapter :edn}}]
  (when-not path
    (throw (ex-info "Output path required" {})))

  (log/info "Exporting data" {:scope scope :path path :adapter adapter})

  (case scope
    :kg
    (let [data (kg-mig/export-to-edn)
          transformed (adapter/transform-export adapter data)
          parent-dir (.getParentFile (io/file path))]
      (when (and parent-dir (not (.exists parent-dir)))
        (.mkdirs parent-dir))
      (spit path (pr-str transformed))
      {:success true
       :path path
       :counts (:counts data)
       :adapter adapter})

    (throw (ex-info "Scope not yet implemented" {:scope scope}))))

;; =============================================================================
;; Import Command
;; =============================================================================

(defn cmd-import
  "Import data with optional adapter transformation.

   Parameters:
     :path         - Input file path (required)
     :adapter      - Transform adapter :edn, :json (default: :edn)
     :dry-run      - Preview without importing (default: false)
     :merge-strategy - :replace, :append, :merge (default: :append)

   Returns:
     {:success true, :imported {...}}"
  [{:keys [path adapter dry-run]
    :or {adapter :edn
         dry-run false}}]
  (when-not path
    (throw (ex-info "Input path required" {})))

  (log/info "Importing data" {:path path :adapter adapter :dry-run dry-run})

  (let [raw-data (-> path slurp edn/read-string)
        data (adapter/transform-import adapter raw-data)]

    (if dry-run
      {:dry-run true
       :path path
       :would-import {:edges (count (:edges data))
                      :disc (count (:disc data))
                      :synthetic (count (:synthetic data))}}

      (let [result (kg-mig/import-from-edn data)]
        {:success (empty? (:errors result))
         :path path
         :imported (:imported result)
         :errors (:errors result)}))))

;; =============================================================================
;; Help Command
;; =============================================================================

(defn cmd-help
  "Show migration tool help.

   Returns command documentation."
  [_params]
  {:commands
   [{:command "status"
     :description "Current backend info and data counts"
     :params []}
    {:command "backup"
     :description "Create timestamped backup (scope-aware, includes project-id)"
     :params [{:name "scope" :type "keyword" :default ":kg"}
              {:name "dir" :type "string" :default "data/backups"}
              {:name "adapter" :type "keyword" :default ":edn"}
              {:name "directory" :type "string" :description "Working dir for project-id derivation"}
              {:name "project-id" :type "string" :description "Explicit project-id override"}]}
    {:command "restore"
     :description "Restore from backup file (with cross-project guard)"
     :params [{:name "path" :type "string" :required false}
              {:name "latest" :type "keyword" :required false}
              {:name "dry-run" :type "boolean" :default false}
              {:name "directory" :type "string" :description "Working dir for project-id derivation"}
              {:name "project-id" :type "string" :description "Explicit target project-id"}
              {:name "force-cross-project" :type "boolean" :default false
               :description "Allow restoring backup from different project"}]}
    {:command "list"
     :description "List available backups (filterable by project scope)"
     :params [{:name "scope" :type "keyword"}
              {:name "limit" :type "integer" :default 20}
              {:name "directory" :type "string" :description "Working dir for project-id filtering"}
              {:name "project-id" :type "string" :description "Filter by project scope"}]}
    {:command "switch"
     :description "Switch backend with auto-migration"
     :params [{:name "target" :type "keyword" :required true}
              {:name "db-path" :type "string"}
              {:name "dry-run" :type "boolean" :default false}
              {:name "backup" :type "boolean" :default true}]}
    {:command "sync"
     :description "Mirror to secondary backend without switching"
     :params [{:name "target" :type "keyword" :required true}
              {:name "target-opts" :type "map"}]}
    {:command "export"
     :description "Export with optional adapter transform"
     :params [{:name "scope" :type "keyword" :default ":kg"}
              {:name "path" :type "string" :required true}
              {:name "adapter" :type "keyword" :default ":edn"}]}
    {:command "import"
     :description "Import with optional adapter transform"
     :params [{:name "path" :type "string" :required true}
              {:name "adapter" :type "keyword" :default ":edn"}
              {:name "dry-run" :type "boolean" :default false}]}
    {:command "validate"
     :description "Validate backup file integrity"
     :params [{:name "path" :type "string" :required true}]}
    {:command "adapters"
     :description "List available migration adapters"
     :params []}]})

;; =============================================================================
;; Tool Dispatcher
;; =============================================================================

(defn handle-migration
  "Main dispatcher for migration tool.

   Parameters:
     :command - One of: status, backup, restore, list, switch, sync,
                        export, import, validate, adapters, help

   Plus command-specific parameters."
  [{:keys [command] :as params}]
  (log/debug "Migration tool" {:command command :params (dissoc params :command)})
  (let [cmd-params (dissoc params :command)]
    (case (keyword command)
      :status   (cmd-status cmd-params)
      :backup   (cmd-backup cmd-params)
      :restore  (cmd-restore cmd-params)
      :list     (cmd-list cmd-params)
      :switch   (cmd-switch cmd-params)
      :sync     (cmd-sync cmd-params)
      :export   (cmd-export cmd-params)
      :import   (cmd-import cmd-params)
      :validate (cmd-validate cmd-params)
      :adapters (cmd-adapters cmd-params)
      :help     (cmd-help cmd-params)
      (cmd-help cmd-params))))
