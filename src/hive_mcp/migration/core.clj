(ns hive-mcp.migration.core
  "Core migration logic for KG, Memory, and future adapters.

   Backup naming: {scope}-{backend}-{timestamp}.edn
   Example: kg-datalevin-20260203T164530.edn

   CLARITY-Y: All operations are non-destructive by default."
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Backup Directory & Naming
;; =============================================================================

(def ^:private default-backup-dir "data/backups")

(defn ensure-backup-dir!
  "Ensure backup directory exists."
  [dir]
  (let [f (io/file dir)]
    (when-not (.exists f)
      (log/info "Creating backup directory" {:path dir})
      (.mkdirs f))
    dir))

(defn generate-backup-name
  "Generate unique backup filename with timestamp.

   Format: {scope}-{backend}-{yyyyMMddTHHmmss}.edn

   Arguments:
     scope   - :kg, :memory, :kanban, :full
     backend - :datascript, :datalevin, :datahike, :chroma

   Returns filename string (not full path)."
  [scope backend]
  (let [now (LocalDateTime/now)
        formatter (DateTimeFormatter/ofPattern "yyyyMMdd'T'HHmmss")
        timestamp (.format now formatter)]
    (format "%s-%s-%s.edn" (name scope) (name backend) timestamp)))

(defn parse-backup-name
  "Parse backup filename to extract metadata.

   Returns {:scope :kg, :backend :datalevin, :timestamp \"20260203T164530\"}
   or nil if filename doesn't match pattern."
  [filename]
  (when-let [[_ scope backend timestamp]
             (re-matches #"(\w+)-(\w+)-(\d{8}T\d{6})\.edn" filename)]
    {:scope (keyword scope)
     :backend (keyword backend)
     :timestamp timestamp
     :filename filename}))

;; =============================================================================
;; Backup Listing
;; =============================================================================

(defn list-backups
  "List all backups in the backup directory.

   Arguments:
     opts - Optional map:
       :dir   - Backup directory (default: data/backups)
       :scope - Filter by scope (:kg, :memory, :kanban, :full)
       :backend - Filter by backend
       :limit - Max results (default: 20)

   Returns vector of backup metadata maps, newest first."
  [& [{:keys [dir scope backend limit]
       :or {dir default-backup-dir
            limit 20}}]]
  (let [backup-dir (io/file dir)]
    (if (.exists backup-dir)
      (->> (.listFiles backup-dir)
           (filter #(.isFile %))
           (filter #(str/ends-with? (.getName %) ".edn"))
           (map (fn [f]
                  (when-let [meta (parse-backup-name (.getName f))]
                    (assoc meta
                           :path (.getAbsolutePath f)
                           :size (.length f)
                           :modified (.lastModified f)))))
           (filter identity)
           (filter #(or (nil? scope) (= scope (:scope %))))
           (filter #(or (nil? backend) (= backend (:backend %))))
           (sort-by :timestamp #(compare %2 %1))  ;; newest first
           (take limit)
           vec)
      [])))

(defn latest-backup
  "Get the most recent backup matching criteria.

   Arguments:
     scope   - :kg, :memory, :kanban, :full (required)
     opts    - Optional {:backend :datalevin, :dir \"path\"}

   Returns backup metadata map or nil."
  [scope & [opts]]
  (first (list-backups (assoc opts :scope scope :limit 1))))

;; =============================================================================
;; Backup Metadata
;; =============================================================================

(defn backup-metadata
  "Create metadata header for backup file.

   Arguments:
     scope     - :kg, :memory, :kanban, :full
     backend   - Source backend
     counts    - Map of entity counts

   Returns metadata map to include in backup."
  [scope backend counts]
  {:backup/version 1
   :backup/scope scope
   :backup/backend backend
   :backup/created-at (java.util.Date.)
   :backup/created-by (System/getProperty "user.name")
   :backup/hostname (try (.getHostName (java.net.InetAddress/getLocalHost))
                         (catch Exception _ "unknown"))
   :backup/counts counts})

;; =============================================================================
;; File I/O
;; =============================================================================

(defn write-backup!
  "Write backup data to file with metadata.

   Arguments:
     path     - Full file path
     metadata - Backup metadata map
     data     - Data to backup (will be merged with metadata)

   Returns {:success true, :path path, :size bytes}."
  [path metadata data]
  (let [parent-dir (.getParentFile (io/file path))]
    (when (and parent-dir (not (.exists parent-dir)))
      (.mkdirs parent-dir))
    (let [content (merge metadata data)]
      (spit path (pr-str content))
      (let [size (.length (io/file path))]
        (log/info "Backup written" {:path path :size size})
        {:success true
         :path path
         :size size
         :metadata metadata}))))

(defn read-backup
  "Read backup file and parse EDN.

   Arguments:
     path - Full file path

   Returns parsed EDN data or throws on error."
  [path]
  (log/info "Reading backup" {:path path})
  (let [content (slurp path)
        data (edn/read-string content)]
    (log/info "Backup loaded" {:path path
                               :scope (:backup/scope data)
                               :counts (:backup/counts data)})
    data))

;; =============================================================================
;; Validation
;; =============================================================================

(defn validate-backup
  "Validate backup file structure and integrity.

   Arguments:
     path - Path to backup file

   Returns {:valid? boolean, :errors [...], :metadata {...}}."
  [path]
  (try
    (let [data (read-backup path)
          errors (cond-> []
                   (nil? (:backup/version data))
                   (conj "Missing :backup/version")

                   (nil? (:backup/scope data))
                   (conj "Missing :backup/scope")

                   (nil? (:backup/counts data))
                   (conj "Missing :backup/counts"))]
      {:valid? (empty? errors)
       :errors errors
       :metadata (select-keys data [:backup/version :backup/scope
                                    :backup/backend :backup/created-at
                                    :backup/counts])})
    (catch Exception e
      {:valid? false
       :errors [(str "Failed to read backup: " (.getMessage e))]
       :metadata nil})))
