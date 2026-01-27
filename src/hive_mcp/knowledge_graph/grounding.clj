(ns hive-mcp.knowledge-graph.grounding
  "Re-grounding workflow for knowledge entries.
   Verifies entries against source files and updates grounding timestamps.
   
   CLARITY-Y: Graceful failure with status codes instead of exceptions."
  (:require [hive-mcp.chroma :as chroma]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.time Instant Duration]))

(defn needs-regrounding?
  "Returns true if entry needs re-grounding.
   
   An entry needs re-grounding if:
   - :grounded-at is nil (never grounded)
   - :grounded-at is older than max-age-days
   
   Arguments:
     entry        - Knowledge entry map with :grounded-at key
     max-age-days - Maximum age in days before regrounding is needed
   
   Returns:
     true if entry needs re-grounding, false otherwise"
  [entry max-age-days]
  (when entry
    (let [grounded-at (:grounded-at entry)]
      (or (nil? grounded-at)
          (let [grounded-instant (Instant/ofEpochMilli (.getTime grounded-at))
                now-instant (Instant/now)
                max-age-duration (Duration/ofDays max-age-days)]
            (.isBefore (.plus grounded-instant max-age-duration) now-instant))))))

(defn- read-source-hash
  "Read file and compute hash.
   
   Arguments:
     source-path - Path to source file
   
   Returns:
     {:hash \"SHA256-HASH\" :exists? boolean} if file exists
     nil if file does not exist
   
   Note: Uses SHA-256 for content fingerprinting."
  [source-path]
  (try
    (let [file (io/file source-path)]
      (if (.exists file)
        (let [content (slurp file)
              hash-bytes (.digest (java.security.MessageDigest/getInstance "SHA-256")
                                  (.getBytes content "UTF-8"))
              hash-hex (apply str (map #(format "%02x" (bit-and % 0xff)) hash-bytes))]
          {:hash hash-hex :exists? true})
        {:exists? false}))
    (catch Exception e
      (log/warn "Failed to read source file for hashing" {:path source-path :error e})
      nil)))

(defn reground-entry!
  "Re-ground a single knowledge entry by verifying against source file.
   
   Steps:
   1. Fetch entry by ID from Chroma
   2. Check if entry has :source-file metadata
   3. Compute current source hash
   4. Compare with stored :source-hash (if any)
   5. Update entry with new grounding timestamp
   
   Arguments:
     entry-id - Knowledge entry ID
   
   Returns:
     {:status :regrounded|:needs-review|:source-missing
      :drift? boolean
      :entry-id id
      :source-file path}
   
   Status meanings:
     :regrounded     - Entry successfully verified and updated
     :needs-review   - Source exists but hash differs (potential drift)
     :source-missing - Source file not found"
  [entry-id]
  (try
    (log/info "Regrounding entry" {:entry-id entry-id})

    ;; 1. Get entry from Chroma
    (let [entry (chroma/get-entry-by-id entry-id)]
      (if (nil? entry)
        {:status :not-found :entry-id entry-id}

        ;; 2. Check if entry has source-file metadata
        (let [metadata (:metadata entry)
              source-file (:source-file metadata)]
          (if (or (nil? source-file) (str/blank? source-file))
            {:status :no-source-metadata :entry-id entry-id}

            ;; 3. Read current source hash
            (let [source-info (read-source-hash source-file)]
              (if (nil? source-info)
                {:status :hash-failed :entry-id entry-id :source-file source-file}

                ;; 4. Determine status based on source existence and hash
                (let [{:keys [hash exists?]} source-info
                      stored-hash (:source-hash metadata)
                      hash-differs? (and stored-hash hash (not= stored-hash hash))

                      ;; Status logic
                      status (cond
                               (not exists?) :source-missing
                               hash-differs? :needs-review
                               :else :regrounded)

                      ;; Update if source exists (even if hash differs)
                      update-data (when exists?
                                    {:grounded-at (java.util.Date.)
                                     :source-hash hash})]

                  ;; 5. Update entry in Chroma if needed
                  (when (and update-data (not= :source-missing status))
                    (chroma/update-entry! entry-id update-data))

                  {:status status
                   :drift? hash-differs?
                   :entry-id entry-id
                   :source-file source-file
                   :source-exists? exists?
                   :stored-hash stored-hash
                   :current-hash hash
                   :updated? (some? update-data)})))))))

    (catch Exception e
      (log/error "Failed to reground entry" {:entry-id entry-id :error e})
      {:status :error :error (.getMessage e) :entry-id entry-id})))

(defn reground-batch!
  "Re-ground multiple knowledge entries.

   Arguments:
     entry-ids - Collection of knowledge entry IDs

   Returns:
     {:total int
      :by-status {status count}
      :results [{:status ... :entry-id ...}]
      :drifted-entries [entry-id]}"
  [entry-ids]
  (log/info "Regrounding batch" {:count (count entry-ids)})

  (let [results (doall (map reground-entry! entry-ids))
        by-status (frequencies (map :status results))
        drifted-entries (->> results
                             (filter :drift?)
                             (map :entry-id)
                             vec)]
    {:total (count results)
     :by-status by-status
     :results results
     :drifted-entries drifted-entries}))

(defn backfill-grounding!
  "Discover and ground all Chroma entries that have source-file metadata.

   Queries Chroma for all entries, filters to those with :source-file in
   metadata, then computes source-hash and sets grounded-at for each.

   Arguments:
     opts - Optional map with:
       :project-id    - Filter to specific project (default: all)
       :limit         - Max entries to process (default: 500)
       :force?        - Re-ground even if already grounded (default: false)
       :max-age-days  - Only re-ground if older than N days (default: 7)

   Returns:
     {:total-scanned int
      :with-source int
      :processed int
      :by-status {status count}
      :drifted-entries [entry-id]}"
  [& [{:keys [project-id limit force? max-age-days]
       :or {limit 500 force? false max-age-days 7}}]]
  (log/info "backfill-grounding! starting"
            {:project-id project-id :limit limit :force? force? :max-age-days max-age-days})
  (try
    (let [;; 1. Query all entries from Chroma
          entries (chroma/query-entries :project-id project-id
                                        :limit limit
                                        :include-expired? true)
          total-scanned (count entries)

          ;; 2. Filter to entries with source-file metadata
          with-source (->> entries
                           (filter (fn [entry]
                                     (let [sf (or (get-in entry [:metadata :source-file])
                                                  (:source-file entry))]
                                       (and sf (not (str/blank? (str sf)))))))
                           vec)

          ;; 3. Filter to entries needing regrounding (unless forced)
          needs-work (if force?
                       with-source
                       (filter #(needs-regrounding? % max-age-days) with-source))
          entry-ids (mapv :id needs-work)

          ;; 4. Run batch regrounding
          result (when (seq entry-ids)
                   (reground-batch! entry-ids))]

      (log/info "backfill-grounding! complete"
                {:total-scanned total-scanned
                 :with-source (count with-source)
                 :processed (count entry-ids)
                 :by-status (:by-status result)})
      {:total-scanned total-scanned
       :with-source (count with-source)
       :processed (count entry-ids)
       :by-status (or (:by-status result) {})
       :drifted-entries (or (:drifted-entries result) [])})

    (catch Exception e
      (log/error "backfill-grounding! failed" {:error e})
      {:error (.getMessage e)
       :total-scanned 0
       :processed 0})))