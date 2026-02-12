(ns hive-mcp.agent.drone.backend.sdk-drone
  "SDKDroneBackend -- IDroneExecutionBackend via Claude Agent SDK ephemeral sessions."
  (:require [hive-mcp.agent.drone.backend :as backend]
            [hive-mcp.agent.sdk.availability :as avail]
            [hive-mcp.agent.sdk.lifecycle :as lifecycle]
            [clojure.core.async :as async]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- generate-session-id
  "Generate a unique session ID for an ephemeral drone SDK session."
  [drone-id]
  (str "sdk-drone-" (or drone-id "anon") "-" (System/currentTimeMillis)))

(defn- ensure-sdk-available!
  "Check SDK availability, throw if not available."
  []
  (let [status (avail/sdk-status)]
    (when-not (= :available status)
      (throw (ex-info "Claude Agent SDK not available for drone execution"
                      {:sdk-status status
                       :backend :sdk-drone})))))

(defn- drain-output-channel
  "Drain all messages from a core.async channel into a vector with timeout."
  [ch timeout-ms]
  (let [collected (atom [])
        drain-future (future
                       (loop []
                         (let [msg (async/<!! ch)]
                           (if (nil? msg)
                             {:messages @collected :timed-out? false}
                             (do (swap! collected conj msg)
                                 (recur))))))
        result (deref drain-future timeout-ms ::timeout)]
    (if (= ::timeout result)
      {:messages @collected :timed-out? true}
      result)))

(defn- extract-result-text
  "Extract result text from collected SDK messages."
  [messages]
  (->> messages
       (filter #(= :message (:type %)))
       (map :data)
       (remove nil?)
       (str/join "\n")))

(defn- messages->result-map
  "Convert collected messages and timeout status into standardized result map."
  [{:keys [messages timed-out?]} model]
  (let [errors (filter #(= :error (:type %)) messages)
        result-text (extract-result-text messages)
        default-tokens {:input-tokens 0 :output-tokens 0}]
    (cond
      timed-out?
      {:status :timeout
       :result (str "SDK session timed out. Partial output:\n" result-text)
       :tokens default-tokens
       :model model
       :steps (count messages)}

      (seq errors)
      {:status :failed
       :result (str "SDK execution failed: " (:error (first errors)))
       :tokens default-tokens
       :model model
       :steps (count messages)}

      :else
      {:status :completed
       :result result-text
       :tokens default-tokens
       :model model
       :steps (count messages)})))

(defn- spawn-sdk-session!
  "Spawn an ephemeral SDK session for drone task execution."
  [session-id cwd]
  (lifecycle/spawn-headless-sdk!
   session-id
   {:cwd cwd
    :system-prompt "You are a drone worker. Execute the given task precisely and concisely."}))

(defn- dispatch-and-collect!
  "Dispatch task to SDK session and collect all output."
  [session-id task timeout-ms]
  (let [out-ch (lifecycle/dispatch-headless-sdk! session-id task {:raw? true})]
    (drain-output-channel out-ch timeout-ms)))

(defn- kill-session-safe!
  "Kill SDK session, logging but not throwing on failure."
  [session-id]
  (try
    (lifecycle/kill-headless-sdk! session-id)
    (catch Exception e
      (log/warn "[sdk-drone] Failed to kill session"
                {:session-id session-id :error (ex-message e)}))))

(defrecord SDKDroneBackend [default-model timeout-ms]
  backend/IDroneExecutionBackend

  (execute-drone [_this task-context]
    (ensure-sdk-available!)
    (let [{:keys [task model drone-id cwd]} task-context
          effective-model (or model default-model)
          effective-cwd (or cwd ".")
          effective-timeout (or timeout-ms 300000)
          session-id (generate-session-id drone-id)]
      (log/info "[sdk-drone] Starting execution"
                {:session-id session-id :drone-id drone-id :model effective-model})
      (try
        (spawn-sdk-session! session-id effective-cwd)
        (let [output (dispatch-and-collect! session-id task effective-timeout)
              result (messages->result-map output effective-model)]
          (log/info "[sdk-drone] Execution complete"
                    {:session-id session-id :status (:status result) :steps (:steps result)})
          result)
        (catch Exception e
          (log/error "[sdk-drone] Execution failed"
                     {:session-id session-id :error (ex-message e)})
          {:status :failed
           :result (str "SDK drone execution failed: " (ex-message e))
           :tokens {:input-tokens 0 :output-tokens 0}
           :model effective-model
           :steps 0})
        (finally
          (kill-session-safe! session-id)))))

  (supports-validation? [_this]
    true)

  (backend-type [_this]
    :sdk-drone))

(defn ->sdk-drone-backend
  "Create an SDKDroneBackend instance."
  ([] (->sdk-drone-backend {}))
  ([{:keys [default-model timeout-ms]}]
   (->SDKDroneBackend
    (or default-model "claude-sonnet-4-20250514")
    (or timeout-ms 300000))))

(defmethod backend/resolve-backend :sdk-drone [context]
  (->sdk-drone-backend {:default-model (:model context)
                        :timeout-ms (:timeout-ms context)}))
