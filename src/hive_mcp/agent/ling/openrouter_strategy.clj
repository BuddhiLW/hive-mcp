(ns hive-mcp.agent.ling.openrouter-strategy
  "OpenRouter spawn strategy using HTTP API-based ling lifecycle."
  (:require [hive-mcp.agent.ling.strategy :refer [ILingStrategy]]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.config.core :as global-config]
            [hive-mcp.dns.result :refer [rescue]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]
           [java.io BufferedReader InputStreamReader]
           [java.util.concurrent ConcurrentHashMap]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private api-url "https://openrouter.ai/api/v1/chat/completions")

(def ^:const default-buffer-capacity 5000)

(def ^:const default-max-tokens 4096)

(def ^:const http-timeout-secs 300)

(defonce ^:private http-client
  (delay
    (-> (HttpClient/newBuilder)
        (.connectTimeout (Duration/ofSeconds 30))
        (.build))))

(defonce ^:private session-registry (ConcurrentHashMap.))

(defn resolve-api-key
  "Resolve OpenRouter API key from opts or environment."
  [opts]
  (or (:api-key opts)
      (global-config/get-secret :openrouter-api-key)
      (throw (ex-info "OpenRouter API key required. Set OPENROUTER_API_KEY env var or config.edn :secrets."
                      {:env "OPENROUTER_API_KEY"}))))

(defn build-system-prompt
  "Build system prompt for an OpenRouter ling."
  [{:keys [id cwd project-id presets]}]
  (str "You are a ling agent (ID: " id ") in the hive swarm system.\n"
       "Working directory: " (or cwd "unknown") "\n"
       "Project: " (or project-id "unknown") "\n"
       (when (seq presets)
         (str "Presets: " (str/join ", " presets) "\n"))
       "\n"
       "You are an autonomous agent. Complete tasks thoroughly and report results.\n"
       "Be concise but comprehensive in your responses."))

(defn parse-sse-line
  "Parse a Server-Sent Events data line into a JSON map."
  [line]
  (when (and (string? line)
             (str/starts-with? line "data: "))
    (let [data (subs line 6)]
      (when-not (= data "[DONE]")
        (rescue nil (json/read-str data :key-fn keyword))))))

(defn extract-delta-content
  "Extract content delta from a streaming chunk."
  [chunk]
  (get-in chunk [:choices 0 :delta :content]))

(defn- stream-completion!
  "POST streaming completion to OpenRouter and write SSE chunks to ring buffer."
  [{:keys [api-key model stdout-buffer alive?]} messages]
  (let [body (json/write-str {:model model
                              :messages messages
                              :stream true
                              :max_tokens default-max-tokens})
        request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create api-url))
                    (.header "Content-Type" "application/json")
                    (.header "Authorization" (str "Bearer " api-key))
                    (.header "HTTP-Referer" "https://github.com/BuddhiLW/hive-mcp")
                    (.POST (HttpRequest$BodyPublishers/ofString body))
                    (.timeout (Duration/ofSeconds http-timeout-secs))
                    (.build))
        response (.send @http-client request (HttpResponse$BodyHandlers/ofInputStream))
        status (.statusCode response)]
    (if (not (<= 200 status 299))
      (let [error-body (rescue "" (slurp (.body response)))]
        (log/error "OpenRouter streaming request failed"
                   {:status status :model model :body (subs error-body 0 (min 500 (count error-body)))})
        (headless/ring-buffer-append! stdout-buffer
                                      (str "[ERROR] OpenRouter API returned HTTP " status))
        {:error (str "HTTP " status ": " (subs error-body 0 (min 200 (count error-body))))})
      (let [reader (BufferedReader. (InputStreamReader. (.body response)))
            content-acc (StringBuilder.)]
        (try
          (loop []
            (when @alive?
              (when-let [line (.readLine reader)]
                (when-let [chunk (parse-sse-line line)]
                  (when-let [delta (extract-delta-content chunk)]
                    (.append content-acc delta)
                    (headless/ring-buffer-append! stdout-buffer delta)))
                (recur))))
          (catch java.io.IOException e
            (log/debug "Stream reader IO exception" {:error (ex-message e)}))
          (catch Exception e
            (log/warn "Stream reader exception" {:error (ex-message e)})
            (headless/ring-buffer-append! stdout-buffer
                                          (str "[ERROR] Stream error: " (ex-message e))))
          (finally
            (try (.close reader) (catch Exception _))))
        (let [full-content (.toString content-acc)]
          (when (pos? (.length content-acc))
            (headless/ring-buffer-append! stdout-buffer "\n---END-COMPLETION---"))
          {:content full-content})))))

(defn dispatch-async!
  "Send a completion request in a background thread and update conversation history."
  [ling-id user-message]
  (when-let [session (.get session-registry ling-id)]
    (let [{:keys [messages _alive? request-count total-tokens active-thread]} session
          new-messages (conj @messages {:role "user" :content user-message})
          _ (reset! messages new-messages)
          thread (Thread.
                  (fn []
                    (try
                      (swap! request-count inc)
                      (headless/ring-buffer-append! (:stdout-buffer session)
                                                    (str "\n[USER] " (subs user-message 0 (min 100 (count user-message))) "..."))
                      (let [result (stream-completion! session @messages)]
                        (if (:error result)
                          (do
                            (log/error "OpenRouter completion failed"
                                       {:ling-id ling-id :error (:error result)})
                            (headless/ring-buffer-append! (:stdout-buffer session)
                                                          (str "[ERROR] " (:error result))))
                          (when (seq (:content result))
                            (swap! messages conj {:role "assistant"
                                                  :content (:content result)})
                            (when-let [usage (:usage result)]
                              (swap! total-tokens + (or (:total_tokens usage) 0))))))
                      (catch Exception e
                        (log/error "Dispatch async exception"
                                   {:ling-id ling-id :error (ex-message e)})
                        (headless/ring-buffer-append! (:stdout-buffer session)
                                                      (str "[ERROR] " (ex-message e))))
                      (finally
                        (reset! active-thread nil))))
                  (str "hive-openrouter-ling-" ling-id))]
      (.setDaemon thread true)
      (reset! active-thread thread)
      (.start thread)
      thread)))

(defrecord OpenRouterStrategy []
  ILingStrategy

  (strategy-spawn! [_ ling-ctx opts]
    (let [{:keys [id cwd presets project-id model]} ling-ctx
          {:keys [task buffer-capacity]} opts
          api-key (resolve-api-key opts)
          system-prompt (build-system-prompt ling-ctx)
          stdout-buf (headless/create-ring-buffer (or buffer-capacity default-buffer-capacity))
          session {:model model
                   :api-key api-key
                   :messages (atom [{:role "system" :content system-prompt}])
                   :stdout-buffer stdout-buf
                   :system-prompt system-prompt
                   :alive? (atom true)
                   :started-at (System/currentTimeMillis)
                   :cwd cwd
                   :project-id project-id
                   :presets presets
                   :request-count (atom 0)
                   :total-tokens (atom 0)
                   :active-thread (atom nil)}]

      (when (.containsKey session-registry id)
        (throw (ex-info "OpenRouter ling session already exists with this ID"
                        {:ling-id id})))

      (.put session-registry id session)

      (log/info "OpenRouter ling spawned" {:id id :model model :cwd cwd})
      (headless/ring-buffer-append! stdout-buf
                                    (str "[SYSTEM] OpenRouter ling spawned. Model: " model))

      (when task
        (dispatch-async! id task))

      id))

  (strategy-dispatch! [_ ling-ctx task-opts]
    (let [{:keys [id]} ling-ctx
          {:keys [task]} task-opts]
      (if-let [session (.get session-registry id)]
        (if @(:alive? session)
          (do
            (dispatch-async! id task)
            (log/info "Task dispatched to OpenRouter ling" {:ling-id id})
            true)
          (throw (ex-info "OpenRouter ling session is not alive"
                          {:ling-id id})))
        (throw (ex-info "OpenRouter ling session not found"
                        {:ling-id id})))))

  (strategy-status [_ ling-ctx ds-status]
    (let [{:keys [id]} ling-ctx
          session (.get session-registry id)]
      (if session
        (let [buf-stats (headless/ring-buffer-stats (:stdout-buffer session))
              alive? @(:alive? session)
              active? (some? @(:active-thread session))]
          (cond-> (or ds-status {})
            true (assoc :slave/id id
                        :ling/spawn-mode :openrouter
                        :openrouter-alive? alive?
                        :openrouter-active? active?
                        :openrouter-model (:model session)
                        :openrouter-request-count @(:request-count session)
                        :openrouter-total-tokens @(:total-tokens session)
                        :openrouter-message-count (count @(:messages session))
                        :openrouter-started-at (:started-at session)
                        :openrouter-uptime-ms (- (System/currentTimeMillis)
                                                 (:started-at session))
                        :openrouter-stdout buf-stats)
            (nil? ds-status) (assoc :slave/status (if alive? :idle :dead))))
        (when ds-status
          (assoc ds-status :openrouter-alive? false)))))

  (strategy-kill! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      (if-let [session (.get session-registry id)]
        (do
          (reset! (:alive? session) false)

          (when-let [^Thread thread @(:active-thread session)]
            (when (.isAlive thread)
              (.interrupt thread)))

          (.remove session-registry id)

          (log/info "OpenRouter ling killed" {:id id :model (:model session)})
          {:killed? true :id id :model (:model session)})
        (do
          (log/warn "OpenRouter ling not found in registry" {:id id})
          {:killed? true :id id :reason :session-not-found}))))

  (strategy-interrupt! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      {:success? false
       :ling-id id
       :errors ["Interrupt not supported for openrouter spawn mode"]})))

(defn ->openrouter-strategy
  "Create an OpenRouterStrategy instance."
  []
  (->OpenRouterStrategy))

(defn get-session
  "Get session data for an OpenRouter ling."
  [ling-id]
  (when-let [session (.get session-registry ling-id)]
    {:ling-id ling-id
     :model (:model session)
     :alive? @(:alive? session)
     :started-at (:started-at session)
     :cwd (:cwd session)
     :message-count (count @(:messages session))
     :request-count @(:request-count session)
     :total-tokens @(:total-tokens session)
     :stdout-stats (headless/ring-buffer-stats (:stdout-buffer session))}))

(defn get-stdout
  "Get stdout ring buffer contents for an OpenRouter ling."
  ([ling-id] (get-stdout ling-id {}))
  ([ling-id opts]
   (when-let [session (.get session-registry ling-id)]
     (headless/ring-buffer-contents (:stdout-buffer session) opts))))

(defn get-stdout-since
  "Get stdout lines appended after a given timestamp."
  [ling-id since]
  (when-let [session (.get session-registry ling-id)]
    (headless/ring-buffer-contents-since (:stdout-buffer session) since)))

(defn get-conversation
  "Get the full conversation history for an OpenRouter ling."
  [ling-id]
  (when-let [session (.get session-registry ling-id)]
    @(:messages session)))

(defn openrouter-session?
  "Check if a ling-id corresponds to an OpenRouter session."
  [ling-id]
  (.containsKey session-registry ling-id))

(defn list-openrouter-sessions
  "List all active OpenRouter sessions."
  []
  (->> session-registry
       (.keySet)
       (map get-session)
       (remove nil?)
       vec))

(defn kill-all-openrouter!
  "Kill all OpenRouter sessions."
  []
  (let [ids (vec (.keySet session-registry))
        results (for [id ids]
                  (try
                    (let [session (.get session-registry id)]
                      (reset! (:alive? session) false)
                      (when-let [^Thread thread @(:active-thread session)]
                        (when (.isAlive thread) (.interrupt thread)))
                      (.remove session-registry id)
                      {:success true :id id})
                    (catch Exception e
                      {:success false :id id :error (ex-message e)})))]
    {:killed (count (filter :success results))
     :errors (count (remove :success results))}))
