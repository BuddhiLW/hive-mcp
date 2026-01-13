(ns hive-mcp.tools.swarm.collect
  "Swarm collect handler - retrieve task results.

   PHASE 2 OPTIMIZATION: Check event journal first for sub-100ms detection.
   If event not in journal, falls back to elisp polling with exponential backoff.

   SOLID: SRP - Single responsibility for result collection.
   CLARITY: A - Architectural performance (push-first strategy)."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.channel :as channel]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.validation :as v]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

;; ============================================================
;; Response Builders
;; ============================================================

(defn- build-journal-response
  "Build response from event journal entry.

   CLARITY: R - Clear response structure"
  [task_id journal-event start-time via]
  (let [{:keys [status result error slave-id]} journal-event]
    (core/mcp-success
     {:task_id task_id
      :status status
      :result result
      :error error
      :slave_id slave-id
      :via via
      :elapsed_ms (- (System/currentTimeMillis) start-time)})))

(defn- build-timeout-response
  "Build timeout response for collection.

   CLARITY: R - Clear timeout response"
  [task_id elapsed status]
  {:type "text"
   :text (json/write-str {:task_id task_id
                          :status "timeout"
                          :error (format "Collection timed out after %dms (status was: %s)" elapsed status)
                          :elapsed_ms elapsed})})

(defn- build-elisp-timeout-response
  "Build response for elisp evaluation timeout.

   CLARITY: R - Clear error response"
  [task_id elapsed]
  {:type "text"
   :text (json/write-str {:task_id task_id
                          :status "error"
                          :error "Elisp evaluation timed out"
                          :elapsed_ms elapsed})
   :isError true})

(defn- build-parse-error-response
  "Build response for failed JSON parsing.

   CLARITY: R - Clear error response with debug info"
  [task_id elapsed raw-result]
  {:type "text"
   :text (json/write-str {:task_id task_id
                          :status "error"
                          :error "Failed to parse elisp response"
                          :raw_result raw-result
                          :elapsed_ms elapsed})
   :isError true})

;; ============================================================
;; Polling Logic
;; ============================================================

(defn- parse-collect-result
  "Parse elisp collect result.
   Returns parsed map or nil on failure.

   CLARITY: Y - Yield safe failure (returns nil on parse error)"
  [result]
  (try
    (json/read-str result :key-fn keyword)
    (catch Exception e
      (log/warn "Failed to parse collect result:" result "Error:" (.getMessage e))
      nil)))

(defn- poll-once
  "Execute single poll to elisp.
   Returns {:continue bool, :response response}.

   CLARITY: R - Clear return structure"
  [task_id timeout_ms start-time elisp-timeout]
  (let [elapsed (- (System/currentTimeMillis) start-time)
        elisp (format "(json-encode (hive-mcp-swarm-api-collect \"%s\" %s))"
                      (v/escape-elisp-string task_id)
                      (or timeout_ms "nil"))
        {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp elisp-timeout)]
    (cond
      ;; Elisp call timed out
      timed-out
      {:continue false
       :response (build-elisp-timeout-response task_id elapsed)}

      ;; Elisp call failed
      (not success)
      {:continue false
       :response (core/mcp-error (str "Error: " error))}

      ;; Parse and process result
      :else
      (let [parsed (parse-collect-result result)
            status (:status parsed)]
        (cond
          ;; Parse failed
          (nil? parsed)
          {:continue false
           :response (build-parse-error-response task_id elapsed result)}

          ;; Task complete or failed
          (contains? #{"completed" "timeout" "error"} status)
          {:continue false
           :response (core/mcp-success parsed)}

          ;; Still polling
          (= status "polling")
          {:continue true
           :elapsed elapsed}

          ;; Unknown status - timeout
          :else
          {:continue false
           :response (build-timeout-response task_id elapsed status)})))))

;; ============================================================
;; Collect Handler
;; ============================================================

(defn handle-swarm-collect
  "Collect response from a task with push-first, poll-fallback strategy.

   PHASE 2 OPTIMIZATION: Check event journal first for sub-100ms detection.
   If event not in journal, falls back to elisp polling with exponential backoff.

   Note: emacsclient returns a quoted string which unwrap-emacs-string handles.
   We only need ONE json/read-str to parse the actual JSON from elisp.

   Parameters:
   - task_id: ID of the task to collect results from (required)
   - timeout_ms: How long to wait for completion (default: 300000 = 5min)

   CLARITY: A - Architectural performance (push-first, poll-fallback)"
  [{:keys [task_id timeout_ms]}]
  (core/with-swarm
    (let [timeout (or timeout_ms 300000) ; default 5 minutes
          start-time (System/currentTimeMillis)
          poll-interval-ms (atom 500) ; start at 500ms
          max-poll-interval 5000 ; max 5 seconds between polls
          elisp-timeout 10000] ; 10s timeout per elisp call

      ;; PHASE 2: Check event journal first (push-based, sub-100ms)
      (if-let [journal-event (channel/check-event-journal task_id)]
        ;; Event found in journal - return immediately!
        (do
          (log/info "Task" task_id "found in event journal (push-based)")
          (build-journal-response task_id journal-event start-time "channel-push"))

        ;; Not in journal - fall back to polling
        (loop []
          (let [elapsed (- (System/currentTimeMillis) start-time)
                ;; Check journal again (event might have arrived during poll wait)
                journal-check (channel/check-event-journal task_id)]
            (if journal-check
              ;; Found in journal during poll loop
              (build-journal-response task_id journal-check start-time "channel-push-delayed")

              ;; Still not in journal - poll elisp
              (let [{:keys [continue response elapsed]} (poll-once task_id timeout_ms start-time elisp-timeout)]
                (cond
                  ;; Got final response
                  (not continue)
                  response

                  ;; Still polling and within timeout - wait and retry
                  (< elapsed timeout)
                  (do
                    (Thread/sleep @poll-interval-ms)
                    ;; Exponential backoff
                    (swap! poll-interval-ms #(min max-poll-interval (* % 2)))
                    (recur))

                  ;; Exceeded timeout
                  :else
                  (build-timeout-response task_id elapsed "polling"))))))))))
