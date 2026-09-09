(ns hive-mcp.agent.openrouter
  "OpenAI-compatible LLM backend with multi-provider support.

   Supports any provider using the OpenAI /v1/chat/completions shape:
   OpenRouter, Venice AI, Groq, Together, Fireworks, OpenAI, local Ollama.
   Auto-discovers available providers by checking configured API keys."
  (:require [hive-mcp.agent.protocol :as proto]
            [hive-mcp.config.core :as global-config]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.agent.provider :as provider]
            [hive-mcp.agent.provider.model :as model]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Provider surface (facade over hive-mcp.agent.provider.*)
;;;
;;; The provider CONCEPT is stratified next door: `provider.model` (values and
;;; seeds), `provider.policy` (pure decisions), `provider.collect` (config
;;; reads), `provider` (the pipeline). This namespace is the BOUNDARY: it
;;; speaks HTTP. What follows is a thin facade so existing callers keep one
;;; import, and so the vars they resolve still live here.
;;; ---------------------------------------------------------------------------

(def provider-registry
  "SEED registry of known providers. The DEFINITION is
   `effective-provider-registry`: config :llm-providers extends, overrides and
   removes entries. See `hive-mcp.agent.provider.model/seed-registry`."
  model/seed-registry)

(def provider-priority
  "SEED discovery order, not the order that runs. See
   `effective-provider-priority` and `hive-mcp.agent.provider.model/seed-priority`."
  model/seed-priority)

(def ChatCompletionsUrl
  "Malli schema for an OpenAI-compatible chat-completions endpoint URL."
  model/ChatCompletionsUrl)

(def ProviderEntry
  "Malli schema for one provider registry entry."
  model/ProviderEntry)

(def ProviderRegistry
  "Malli schema for the provider registry: provider keyword to entry."
  model/ProviderRegistry)

(defn valid-provider-entry?
  "True when `entry` conforms to `ProviderEntry`."
  [entry]
  (model/valid-provider-entry? entry))

(defn effective-provider-registry
  "The seed registry overlaid with config :llm-providers (add, override, remove)."
  []
  (provider/effective-registry))

(defn effective-provider-priority
  "The discovery order that actually runs, over the effective registry."
  []
  (provider/effective-priority))

(defn validate-provider
  "nil when the provider exists in the effective registry, else an error map."
  [prov]
  (provider/validate-provider prov))

(defn validate-model
  "nil when the model is allowed for the provider, else an error map."
  [prov model]
  (provider/validate-model prov model))

(defn resolve-provider-model
  "Resolve {:provider :model} for an agent spawn or wave.
   Throws on an unknown provider; an unknown model only warns."
  [params]
  (provider/resolve-provider-model params))

;;; ---------------------------------------------------------------------------
;;; Metrics
;;; ---------------------------------------------------------------------------

(def ^:private timeout-ms
  "HTTP timeout in milliseconds (5 minutes)."
  300000)

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

;;; ---------------------------------------------------------------------------
;;; Request/Response (OpenAI-compatible shape)
;;; ---------------------------------------------------------------------------

(defn- format-tools
  "Convert a tool schema vector to OpenAI function-calling format.

   Accepts two input shapes (shape-tolerant):
     1. hive-mcp internal:  {:name _ :description _ :inputSchema _}
     2. hive-agent OpenAI:  {:type \"function\" :function {:name _ :description _ :parameters _}}

   Already-OpenAI entries pass through unchanged. This prevents nil-field 400s
   from strict providers (venice) when hive-agent definitions bypass the
   internal registry shape."
  [tools]
  (when (seq tools)
    (mapv (fn [t]
            (cond
              ;; Already OpenAI function-calling shape — return as-is.
              (and (= "function" (:type t)) (map? (:function t)))
              t

              ;; hive-mcp internal shape.
              (and (:name t) (:inputSchema t))
              {:type "function"
               :function {:name        (:name t)
                          :description (:description t)
                          :parameters  (:inputSchema t)}}

              :else
              (throw (ex-info "format-tools: unrecognized tool schema shape"
                              {:tool t}))))
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
  "Parse OpenAI-compatible response message into internal format."
  [choice]
  (if (nil? choice)
    {:type :error :error "Provider returned nil message"}
    (let [tool-calls (:tool_calls choice)
          content (:content choice)]
      (cond
        (seq tool-calls)
        {:type :tool_calls
         :calls (parse-tool-calls tool-calls)}

        (str/blank? content)
        {:type :error
         :error (str "Provider returned empty response"
                     (when content " (whitespace-only)"))}

        :else
        {:type :text
         :content content}))))

(defn- chat-request
  "Make chat completion request to an OpenAI-compatible endpoint."
  [endpoint-url api-key model messages tools provider-name]
  (let [start-ms (System/currentTimeMillis)
        msg-count (count messages)
        tool-count (count tools)]

    (log/debug (str provider-name " request starting")
               {:model model :messages msg-count :tools tool-count})
    (record-request!)

    (try
      (let [body (cond-> {:model model
                          :messages messages}
                   (seq tools) (assoc :tools (format-tools tools)))
            response (http/post endpoint-url
                                {:headers (cond-> {"Authorization" (str "Bearer " api-key)
                                                   "Content-Type" "application/json"}
                                            (= provider-name "openrouter")
                                            (assoc "HTTP-Referer" "https://github.com/BuddhiLW/hive-mcp"))
                                 :body (json/write-str body)
                                 :as :json
                                 :socket-timeout timeout-ms
                                 :connection-timeout timeout-ms
                                 :throw-exceptions false})
            elapsed-ms (- (System/currentTimeMillis) start-ms)
            status (:status response)]

        (cond
          (nil? status)
          (do
            (record-timeout! elapsed-ms)
            (log/error (str provider-name " request failed: no response")
                       {:model model :elapsed-ms elapsed-ms})
            (throw (ex-info (str provider-name " request failed: no response")
                            {:model model :elapsed-ms elapsed-ms :provider provider-name})))

          (not (<= 200 status 299))
          (let [error-body (try (json/read-str (or (:body response) "{}") :key-fn keyword)
                                (catch Exception _ {}))]
            (record-error! elapsed-ms)
            (log/error (str provider-name " API error")
                       {:status status :error error-body :model model :elapsed-ms elapsed-ms})
            (throw (ex-info (str provider-name " API error: " status " - "
                                 (or (:message (:error error-body)) "unknown error"))
                            {:status status :error error-body :model model
                             :elapsed-ms elapsed-ms :provider provider-name})))

          :else
          (do
            (record-success! elapsed-ms)
            (log/info (str provider-name " request completed")
                      {:model model :status status :elapsed-ms elapsed-ms})
            (:body response))))

      (catch java.net.SocketTimeoutException e
        (let [elapsed-ms (- (System/currentTimeMillis) start-ms)]
          (record-timeout! elapsed-ms)
          (log/error (str provider-name " request timed out")
                     {:model model :elapsed-ms elapsed-ms :timeout-ms timeout-ms})
          (throw (ex-info (str provider-name " request timed out")
                          {:model model :elapsed-ms elapsed-ms :timeout-ms timeout-ms
                           :provider provider-name}
                          e))))

      (catch Exception e
        (when-not (ex-data e) ;; don't re-wrap our own ex-infos
          (let [elapsed-ms (- (System/currentTimeMillis) start-ms)]
            (record-error! elapsed-ms)
            (log/error e (str provider-name " request exception")
                       {:model model :elapsed-ms elapsed-ms})))
        (throw e)))))

;;; ---------------------------------------------------------------------------
;;; OpenAICompatBackend Record
;;; ---------------------------------------------------------------------------

(defrecord OpenAICompatBackend [api-url api-key model provider-name]
  proto/LLMBackend

  (chat [_ messages tools]
    (let [response (chat-request api-url api-key model messages tools provider-name)
          choice (get-in response [:choices 0 :message])
          usage (:usage response)
          result (parse-response choice)]
      (log/debug (str provider-name " response parsed") {:model model :type (:type result)})
      (when (= :error (:type result))
        (log/warn (str provider-name " empty response detected") {:model model :error (:error result)}))
      (cond-> result
        usage (assoc :usage {:input (:prompt_tokens usage)
                             :output (:completion_tokens usage)
                             :total (:total_tokens usage)}))))

  (model-name [_] model))

;;; ---------------------------------------------------------------------------
;;; Provider Discovery
;;; ---------------------------------------------------------------------------

(defn available-providers
  "Providers that can be called right now: in the effective discovery order,
   with either no secret required or a configured one."
  []
  (provider/available-providers))

(defn best-available-provider
  "The highest-priority callable provider, or nil."
  []
  (provider/best-available-provider))

;;; ---------------------------------------------------------------------------
;;; Factory Functions
;;; ---------------------------------------------------------------------------

(defn openai-compat-backend
  "Create an OpenAI-compatible LLM backend.
   Options:
     :provider   - keyword from the EFFECTIVE provider registry: the static
                   entries merged with config :llm-providers, so a provider
                   that exists only in config (or a config override of a
                   static entry's :default-model) is honoured here too
     :api-url    - explicit URL (overrides provider registry)
     :api-key    - explicit API key (overrides secret resolution)
     :model      - model string
     :secret-key - config secret key for API key resolution

   Throws when the named provider is dispatch-routed (`:dispatch` in its
   registry entry, e.g. :anthropic) and no :api-url override is supplied —
   such a provider has no chat-completions endpoint."
  [{:keys [provider api-url api-key model secret-key]}]
  (let [reg-entry      (get (effective-provider-registry) provider)
        dispatch       (:dispatch reg-entry)
        effective-url  (or api-url (:api-url reg-entry))
        effective-sk   (or secret-key (:secret-key reg-entry))
        effective-key  (or api-key
                           (when effective-sk (global-config/get-secret effective-sk)))
        effective-model (or model (:default-model reg-entry) "anthropic/claude-3-haiku")
        prov-name      (or (some-> provider name) "custom")]
    (when (and dispatch (not api-url))
      (throw (ex-info (str prov-name " is not an OpenAI-compat provider (dispatch: "
                          (name dispatch) ")")
                      {:provider provider :dispatch dispatch
                       :fix "Route through the dispatch-specific client, or pass an explicit :api-url"})))
    (when-not effective-url
      (throw (ex-info "API URL required for custom provider"
                      {:provider provider})))
    (when (and (not effective-key) (not= provider :ollama-compat))
      (throw (ex-info (str prov-name " API key required")
                      {:provider provider :secret-key effective-sk
                       :env (when effective-sk
                              (-> (name effective-sk) (str/replace "-" "_") str/upper-case))})))
    (->OpenAICompatBackend effective-url (or effective-key "") effective-model prov-name)))

(defn auto-backend
  "Create a backend using the best available provider.
   Falls back through `effective-provider-priority` until one has a valid key."
  [opts]
  (if-let [prov (provider/best-available-provider)]
    (do
      (log/info "Auto-selected provider" {:provider prov})
      (openai-compat-backend (assoc opts :provider prov)))
    (throw (ex-info "No OpenAI-compatible provider configured. Set at least one API key."
                    {:checked (provider/provider-diagnostic)}))))

(defn openrouter-backend
  "Create an OpenRouter backend. Backward-compatible factory."
  [{:keys [api-key model] :or {model "anthropic/claude-3-haiku"}}]
  (openai-compat-backend {:provider :openrouter :api-key api-key :model model}))
