(ns hive-mcp.tools.swarm.collect
  "Swarm collect handler - retrieve task results with push-first, poll-fallback strategy."
  (:require [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.channel :as channel]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.dns.validation :as v]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]
            [hive-mcp.dns.result :refer [rescue]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- build-journal-response
  "Build response from event journal entry."
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
  "Build timeout response for collection."
  [task_id elapsed status]
  {:type "text"
   :text (json/write-str {:task_id task_id
                          :status "timeout"
                          :error (format "Collection timed out after %dms (status was: %s)" elapsed status)
                          :elapsed_ms elapsed})})

(defn- build-elisp-timeout-response
  "Build response for elisp evaluation timeout."
  [task_id elapsed]
  {:type "text"
   :text (json/write-str {:task_id task_id
                          :status "error"
                          :error "Elisp evaluation timed out"
                          :elapsed_ms elapsed})
   :isError true})

(defn- build-parse-error-response
  "Build response for failed JSON parsing."
  [task_id elapsed raw-result]
  {:type "text"
   :text (json/write-str {:task_id task_id
                          :status "error"
                          :error "Failed to parse elisp response"
                          :raw_result raw-result
                          :elapsed_ms elapsed})
   :isError true})

(defn- parse-collect-result
  "Parse elisp collect result, returns parsed map or nil on failure."
  [result]
  (rescue nil
          (json/read-str result :key-fn keyword)))

(defn- poll-once
  "Execute single poll to elisp, returns {:continue bool, :response response}."
  [task_id timeout_ms start-time elisp-timeout]
  (let [elapsed (- (System/currentTimeMillis) start-time)
        elisp (format "(json-encode (hive-mcp-swarm-api-collect \"%s\" %s))"
                      (v/escape-elisp-string task_id)
                      (or timeout_ms "nil"))
        {:keys [success result error timed-out]} (ec/eval-elisp-with-timeout elisp elisp-timeout)]
    (cond
      timed-out
      {:continue false
       :response (build-elisp-timeout-response task_id elapsed)}

      (not success)
      {:continue false
       :response (core/mcp-error (str "Error: " error))}

      :else
      (let [parsed (parse-collect-result result)
            status (:status parsed)]
        (cond
          (nil? parsed)
          {:continue false
           :response (build-parse-error-response task_id elapsed result)}

          (contains? #{"completed" "timeout" "error"} status)
          {:continue false
           :response (core/mcp-success parsed)}

          (= status "polling")
          {:continue true
           :elapsed elapsed}

          :else
          {:continue false
           :response (build-timeout-response task_id elapsed status)})))))

(defn handle-swarm-collect
  "Collect response from a task with push-first, poll-fallback strategy."
  [{:keys [task_id timeout_ms]}]
  (core/with-swarm
    (let [timeout (or timeout_ms 300000)
          start-time (System/currentTimeMillis)
          poll-interval-ms (atom 500)
          max-poll-interval 5000
          elisp-timeout 10000]

      (if-let [journal-event (channel/check-event-journal task_id)]
        (do
          (log/info "Task" task_id "found in event journal (push-based)")
          (build-journal-response task_id journal-event start-time "channel-push"))

        (loop []
          (let [_elapsed (- (System/currentTimeMillis) start-time)
                journal-check (channel/check-event-journal task_id)]
            (if journal-check
              (build-journal-response task_id journal-check start-time "channel-push-delayed")

              (let [{:keys [continue response elapsed]} (poll-once task_id timeout_ms start-time elisp-timeout)]
                (cond
                  (not continue)
                  response

                  (< elapsed timeout)
                  (do
                    (Thread/sleep @poll-interval-ms)
                    (swap! poll-interval-ms #(min max-poll-interval (* % 2)))
                    (recur))

                  :else
                  (build-timeout-response task_id elapsed "polling"))))))))))
