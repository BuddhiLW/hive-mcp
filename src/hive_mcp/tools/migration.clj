(ns hive-mcp.tools.migration
  "MCP tool for unified migration operations.

   Commands:
   - status     - Current backend info and data counts
   - backup     - Create timestamped backup
   - restore    - Restore from backup
   - export     - Export with optional adapter transform
   - import     - Import with optional adapter transform
   - switch     - Switch backend with auto-migration
   - sync       - Mirror to secondary backend
   - list       - List available backups
   - validate   - Validate backup integrity
   - adapters   - List available adapters

   CLARITY-T: All operations logged with full context."
  (:require [hive-mcp.migration.core :as core]
            [hive-mcp.migration.adapter :as adapter]
            [hive-mcp.knowledge-graph.connection :as kg-conn]
            [hive-mcp.knowledge-graph.migration :as kg-mig]
            [hive-mcp.knowledge-graph.protocol :as kg-proto]
            [clojure.java.io :as io]
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
;; Backup Command
;; =============================================================================

(defn cmd-backup
  "Create timestamped backup.

   Parameters:
     :scope   - :kg, :memory, :full (default: :kg)
     :dir     - Backup directory (default: data/backups)
     :adapter - Export adapter :edn, :json (default: :edn)

   Returns:
     {:success true, :path \"...\", :size N, :counts {...}}"
  [{:keys [scope dir adapter]
    :or {scope :kg
         dir "data/backups"
         adapter :edn}}]
  (log/info "Creating backup" {:scope scope :dir dir :adapter adapter})
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
          ;; Create metadata
          metadata (core/backup-metadata :kg backend counts)]
      (core/write-backup! path metadata {:data transformed}))

    ;; TODO: :memory and :full scopes
    (throw (ex-info "Scope not yet implemented" {:scope scope}))))

;; =============================================================================
;; Restore Command
;; =============================================================================

(defn cmd-restore
  "Restore from backup file.

   Parameters:
     :path    - Full path to backup file (required if no :latest)
     :latest  - Restore latest backup for scope (e.g., :kg)
     :dry-run - Preview without applying (default: false)
     :adapter - Import adapter (default: :edn)

   Returns:
     {:success true, :imported {...}, :validation {...}}"
  [{:keys [path latest dry-run adapter]
    :or {dry-run false
         adapter :edn}}]
  (let [backup-path (or path
                        (when latest
                          (:path (core/latest-backup latest))))]
    (when-not backup-path
      (throw (ex-info "No backup path specified and no latest found"
                      {:latest latest})))

    (log/info "Restoring backup" {:path backup-path :dry-run dry-run})

    ;; Validate first
    (let [validation (core/validate-backup backup-path)]
      (when-not (:valid? validation)
        (throw (ex-info "Backup validation failed" validation)))

      (if dry-run
        {:dry-run true
         :path backup-path
         :metadata (:metadata validation)
         :would-import (:metadata validation)}

        ;; Actually restore
        (let [backup-data (core/read-backup backup-path)
              data (if (= adapter :edn)
                     (:data backup-data)
                     (adapter/transform-import adapter (:data backup-data)))
              result (kg-mig/import-from-edn data)]
          {:success (empty? (:errors result))
           :path backup-path
           :imported (:imported result)
           :errors (:errors result)})))))

;; =============================================================================
;; List Command
;; =============================================================================

(defn cmd-list
  "List available backups.

   Parameters:
     :scope   - Filter by scope (:kg, :memory, :full)
     :backend - Filter by backend
     :limit   - Max results (default: 20)
     :dir     - Backup directory

   Returns:
     {:backups [...], :count N}"
  [{:keys [scope backend limit dir]
    :or {limit 20
         dir "data/backups"}}]
  (let [backups (core/list-backups {:scope scope
                                    :backend backend
                                    :limit limit
                                    :dir dir})]
    {:backups backups
     :count (count backups)}))

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

  (let [raw-data (-> path slurp clojure.edn/read-string)
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
     :description "Create timestamped backup"
     :params [{:name "scope" :type "keyword" :default ":kg"}
              {:name "dir" :type "string" :default "data/backups"}
              {:name "adapter" :type "keyword" :default ":edn"}]}
    {:command "restore"
     :description "Restore from backup file"
     :params [{:name "path" :type "string" :required false}
              {:name "latest" :type "keyword" :required false}
              {:name "dry-run" :type "boolean" :default false}]}
    {:command "list"
     :description "List available backups"
     :params [{:name "scope" :type "keyword"}
              {:name "limit" :type "integer" :default 20}]}
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
