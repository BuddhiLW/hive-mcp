(ns hive-mcp.agent.openrouter
  "OpenRouter backend for agent delegation.

   Provides access to 100+ models via OpenRouter API.
   Uses OpenAI-compatible format with tool calling support.

   CLARITY-T: Full telemetry with timing, metrics, and error tracking.
   CLARITY-Y: Validates responses to yield safe failures on empty content."
  (:require [hive-mcp.agent.protocol :as proto]
            [hive-mcp.config :as global-config]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private api-url "https://openrouter.ai/api/v1/chat/completions")

(def ^:private timeout-ms
  "HTTP timeout in milliseconds (5 minutes, matching Ollama's 300s)."
  300000)

;;; =============================================================================
;;; Metrics (CLARITY-T: Telemetry First)
;;; =============================================================================

;; Runtime metrics for OpenRouter requests.
;; Thread-safe via atom. Reset with reset-metrics!
(defonce metrics
  (atom {:request-count 0
         :success-count 0
         :error-count 0
         :timeout-count 0
         :total-latency-ms 0}))

(defn reset-metrics!
  "Reset all metrics to zero."
  []
  (reset! metrics {:request-count 0
                   :success-count 0
                   :error-count 0
                   :timeout-count 0
                   :total-latency-ms 0}))

(defn get-metrics
  "Get current metrics snapshot with computed averages."
  []
  (let [m @metrics
        req-count (:request-count m)]
    (assoc m
           :avg-latency-ms (if (pos? req-count)
                             (/ (:total-latency-ms m) req-count)
                             0)
           :error-rate (if (pos? req-count)
                         (double (/ (:error-count m) req-count))
                         0.0))))

(defn- record-request! []
  (swap! metrics update :request-count inc))

(defn- record-success! [latency-ms]
  (swap! metrics #(-> %
                      (update :success-count inc)
                      (update :total-latency-ms + latency-ms))))

(defn- record-error! [latency-ms]
  (swap! metrics #(-> %
                      (update :error-count inc)
                      (update :total-latency-ms + latency-ms))))

(defn- record-timeout! [latency-ms]
  (swap! metrics #(-> %
                      (update :timeout-count inc)
                      (update :error-count inc)
                      (update :total-latency-ms + latency-ms))))

(defn- format-tools
  "Convert MCP tool format to OpenAI function format."
  [tools]
  (when (seq tools)
    (mapv (fn [{:keys [name description inputSchema]}]
            {:type "function"
             :function {:name name
                        :description description
                        :parameters inputSchema}})
          tools)))

(defn- parse-tool-calls
  "Parse OpenAI-format tool calls to internal format."
  [tool-calls]
  (mapv (fn [tc]
          {:id (:id tc)
           :name (get-in tc [:function :name])
           :arguments (json/read-str (get-in tc [:function :arguments]) :key-fn keyword)})
        tool-calls))

(defn parse-response
  "Parse OpenRouter/OpenAI response message into internal format.

   CLARITY-Y: Validates response content to yield safe failures.
   Returns:
     {:type :tool_calls :calls [...]} - if tool calls present
     {:type :text :content \"...\"} - if valid text content
     {:type :error :error \"...\"}  - if empty/nil/whitespace content

   This prevents silent 0-byte file writes when models return empty responses."
  [choice]
  (if (nil? choice)
    {:type :error :error "OpenRouter returned nil message"}
    (let [tool-calls (:tool_calls choice)
          content (:content choice)]
      (cond
        ;; Tool calls present - process normally
        (seq tool-calls)
        {:type :tool_calls
         :calls (parse-tool-calls tool-calls)}

        ;; Validate text content is non-empty
        (str/blank? content)
        {:type :error
         :error (str "OpenRouter returned empty response"
                     (when content " (whitespace-only)"))}

        ;; Valid text response
        :else
        {:type :text
         :content content}))))

(defn- chat-request
  "Make chat completion request to OpenRouter.

   CLARITY-T: Full telemetry with timing and error tracking.
   Includes timeout and error handling to prevent silent failures
   in async contexts (core.async go blocks swallow exceptions)."
  [api-key model messages tools]
  (let [start-ms (System/currentTimeMillis)
        msg-count (count messages)
        tool-count (count tools)]

    ;; Telemetry: Log request start
    (log/debug "OpenRouter request starting"
               {:model model :messages msg-count :tools tool-count})
    (record-request!)

    (try
      (let [body (cond-> {:model model
                          :messages messages}
                   (seq tools) (assoc :tools (format-tools tools)))
            response (http/post api-url
                                {:headers {"Authorization" (str "Bearer " api-key)
                                           "Content-Type" "application/json"
                                           "HTTP-Referer" "https://github.com/BuddhiLW/hive-mcp"}
                                 :body (json/write-str body)
                                 :as :json
                                 :socket-timeout timeout-ms
                                 :connection-timeout timeout-ms
                                 :throw-exceptions false})
            elapsed-ms (- (System/currentTimeMillis) start-ms)
            status (:status response)]

        (cond
          ;; No response (network failure, timeout)
          (nil? status)
          (do
            (record-timeout! elapsed-ms)
            (log/error "OpenRouter request failed: no response"
                       {:model model :elapsed-ms elapsed-ms})
            (throw (ex-info "OpenRouter request failed: no response"
                            {:model model :elapsed-ms elapsed-ms})))

          ;; HTTP error
          (not (<= 200 status 299))
          (let [error-body (try (json/read-str (or (:body response) "{}") :key-fn keyword)
                                (catch Exception _ {}))]
            (record-error! elapsed-ms)
            (log/error "OpenRouter API error"
                       {:status status :error error-body :model model :elapsed-ms elapsed-ms})
            (throw (ex-info (str "OpenRouter API error: " status " - "
                                 (or (:message (:error error-body)) "unknown error"))
                            {:status status :error error-body :model model :elapsed-ms elapsed-ms})))

          ;; Success
          :else
          (do
            (record-success! elapsed-ms)
            (log/info "OpenRouter request completed"
                      {:model model :status status :elapsed-ms elapsed-ms})
            (:body response))))

      (catch java.net.SocketTimeoutException e
        (let [elapsed-ms (- (System/currentTimeMillis) start-ms)]
          (record-timeout! elapsed-ms)
          (log/error "OpenRouter request timed out"
                     {:model model :elapsed-ms elapsed-ms :timeout-ms timeout-ms})
          (throw (ex-info "OpenRouter request timed out"
                          {:model model :elapsed-ms elapsed-ms :timeout-ms timeout-ms}
                          e))))

      (catch Exception e
        (let [elapsed-ms (- (System/currentTimeMillis) start-ms)]
          (record-error! elapsed-ms)
          (log/error e "OpenRouter request exception"
                     {:model model :elapsed-ms elapsed-ms})
          (throw e))))))

(defrecord OpenRouterBackend [api-key model]
  proto/LLMBackend

  (chat [_ messages tools]
    ;; Telemetry is handled by chat-request
    (let [response (chat-request api-key model messages tools)
          choice (get-in response [:choices 0 :message])
          usage (:usage response)  ;; OpenRouter includes usage in response
          result (parse-response choice)]
      (log/debug "OpenRouter response parsed" {:model model :type (:type result)})
      (when (= :error (:type result))
        (log/warn "OpenRouter empty response detected" {:model model :error (:error result)}))
      ;; Include usage info in result for metrics tracking
      (cond-> result
        usage (assoc :usage {:input (:prompt_tokens usage)
                             :output (:completion_tokens usage)
                             :total (:total_tokens usage)}))))

  (model-name [_] model))

(defn openrouter-backend
  "Create an OpenRouterBackend.
   
   Options:
     :api-key - OpenRouter API key (required, or set OPENROUTER_API_KEY env)
     :model   - Model to use (default: anthropic/claude-3-haiku)
   
   Popular models:
     anthropic/claude-3-opus, anthropic/claude-3-sonnet, anthropic/claude-3-haiku
     openai/gpt-4-turbo, openai/gpt-3.5-turbo
     mistralai/mistral-large, mistralai/mixtral-8x7b
     meta-llama/llama-3-70b-instruct"
  [{:keys [api-key model] :or {model "anthropic/claude-3-haiku"}}]
  (let [key (or api-key (global-config/get-secret :openrouter-api-key))]
    (when-not key
      (throw (ex-info "OpenRouter API key required" {:env "OPENROUTER_API_KEY"})))
    (->OpenRouterBackend key model)))
