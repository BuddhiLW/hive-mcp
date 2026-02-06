(ns hive-mcp.agent.ling.openrouter-strategy
  "OpenRouter spawn strategy — HTTP API-based ling lifecycle.

   Instead of spawning a subprocess (like headless-strategy), this strategy
   calls the OpenRouter API directly via HTTP, streaming responses to a
   ring buffer. This enables multi-model lings using any OpenRouter model
   (DeepSeek, Llama, Mistral, etc.) without needing `claude` CLI.

   Architecture:
   - Session registry (atom) tracks active conversations
   - Conversation history maintained for multi-turn support
   - Streaming SSE responses written to ring buffer (reuses headless.clj buffers)
   - Dispatch appends user messages and triggers new completions

   SOLID: Single Responsibility — only OpenRouter HTTP interaction.
   CLARITY: L — Pure adapter between ILingStrategy and OpenRouter API."
  (:require [hive-mcp.agent.ling.strategy :refer [ILingStrategy]]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.config :as global-config]
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

;;; =============================================================================
;;; Constants
;;; =============================================================================

(def ^:private api-url "https://openrouter.ai/api/v1/chat/completions")

(def ^:const default-buffer-capacity
  "Default max lines in ring buffer for OpenRouter lings."
  5000)

(def ^:const default-max-tokens
  "Default max tokens per completion."
  4096)

(def ^:const http-timeout-secs
  "HTTP connection timeout in seconds."
  300)

;;; =============================================================================
;;; HTTP Client (Shared, Lazy)
;;; =============================================================================

(defonce ^:private http-client
  (delay
    (-> (HttpClient/newBuilder)
        (.connectTimeout (Duration/ofSeconds 30))
        (.build))))

;;; =============================================================================
;;; Session Registry
;;; =============================================================================

(defonce ^{:private true
           :doc "Registry of active OpenRouter ling sessions.
  Key: ling-id (String)
  Value: {:model String
          :api-key String
          :messages vector (conversation history)
          :stdout-buffer ring-buffer-atom
          :system-prompt String
          :alive? atom(boolean)
          :started-at long
          :cwd String
          :request-count atom(int)
          :total-tokens atom(int)
          :active-thread atom(Thread or nil)}"}
  session-registry
  (ConcurrentHashMap.))

;;; =============================================================================
;;; API Key Resolution
;;; =============================================================================

(defn resolve-api-key
  "Resolve OpenRouter API key from opts or environment.
   Returns the key string or throws."
  [opts]
  (or (:api-key opts)
      (global-config/get-secret :openrouter-api-key)
      (throw (ex-info "OpenRouter API key required. Set OPENROUTER_API_KEY env var or config.edn :secrets."
                      {:env "OPENROUTER_API_KEY"}))))

;;; =============================================================================
;;; System Prompt Builder
;;; =============================================================================

(defn build-system-prompt
  "Build system prompt for an OpenRouter ling.
   Includes basic ling context + project info."
  [{:keys [id cwd project-id presets]}]
  (str "You are a ling agent (ID: " id ") in the hive swarm system.\n"
       "Working directory: " (or cwd "unknown") "\n"
       "Project: " (or project-id "unknown") "\n"
       (when (seq presets)
         (str "Presets: " (str/join ", " presets) "\n"))
       "\n"
       "You are an autonomous agent. Complete tasks thoroughly and report results.\n"
       "Be concise but comprehensive in your responses."))

;;; =============================================================================
;;; Streaming HTTP Request
;;; =============================================================================

(defn parse-sse-line
  "Parse a Server-Sent Events data line.
   Returns the parsed JSON map or nil for non-data/done lines."
  [line]
  (when (and (string? line)
             (str/starts-with? line "data: "))
    (let [data (subs line 6)]
      (when-not (= data "[DONE]")
        (try
          (json/read-str data :key-fn keyword)
          (catch Exception e
            (log/debug "Failed to parse SSE line" {:line data :error (ex-message e)})
            nil))))))

(defn extract-delta-content
  "Extract content delta from a streaming chunk."
  [chunk]
  (get-in chunk [:choices 0 :delta :content]))

(defn- stream-completion!
  "POST to OpenRouter with stream=true, read SSE lines into ring buffer.

   Runs synchronously (intended to be called from a background thread).
   Accumulates the full response text and appends to conversation history.

   Arguments:
     session - Session map from registry
     messages - Full conversation history to send

   Returns:
     {:content String :usage map-or-nil :error String-or-nil}"
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
      ;; HTTP error
      (let [error-body (try (slurp (.body response)) (catch Exception _ ""))]
        (log/error "OpenRouter streaming request failed"
                   {:status status :model model :body (subs error-body 0 (min 500 (count error-body)))})
        (headless/ring-buffer-append! stdout-buffer
                                      (str "[ERROR] OpenRouter API returned HTTP " status))
        {:error (str "HTTP " status ": " (subs error-body 0 (min 200 (count error-body))))})
      ;; Success - stream SSE
      (let [reader (BufferedReader. (InputStreamReader. (.body response)))
            content-acc (StringBuilder.)]
        (try
          (loop []
            (when @alive?
              (when-let [line (.readLine reader)]
                (when-let [chunk (parse-sse-line line)]
                  (when-let [delta (extract-delta-content chunk)]
                    (.append content-acc delta)
                    ;; Write each content chunk to ring buffer as it arrives
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
          ;; Write a newline separator after each completion
          (when (pos? (.length content-acc))
            (headless/ring-buffer-append! stdout-buffer "\n---END-COMPLETION---"))
          {:content full-content})))))

;;; =============================================================================
;;; Background Dispatch
;;; =============================================================================

(defn dispatch-async!
  "Send a completion request in a background thread.
   Updates conversation history with the response.

   Arguments:
     ling-id - Ling identifier
     user-message - String message from user/dispatch"
  [ling-id user-message]
  (when-let [session (.get session-registry ling-id)]
    (let [{:keys [messages alive? request-count total-tokens active-thread]} session
          ;; Append user message to history
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
                          ;; Append assistant response to history
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

;;; =============================================================================
;;; OpenRouter Strategy Implementation
;;; =============================================================================

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

      ;; Check for duplicate
      (when (.containsKey session-registry id)
        (throw (ex-info "OpenRouter ling session already exists with this ID"
                        {:ling-id id})))

      ;; Register session
      (.put session-registry id session)

      (log/info "OpenRouter ling spawned" {:id id :model model :cwd cwd})
      (headless/ring-buffer-append! stdout-buf
                                    (str "[SYSTEM] OpenRouter ling spawned. Model: " model))

      ;; If initial task provided, dispatch it immediately
      (when task
        (dispatch-async! id task))

      ;; Return ling-id (like headless strategy)
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
        ;; No session found
        (when ds-status
          (assoc ds-status :openrouter-alive? false)))))

  (strategy-kill! [_ ling-ctx]
    (let [{:keys [id]} ling-ctx]
      (if-let [session (.get session-registry id)]
        (do
          ;; Signal shutdown
          (reset! (:alive? session) false)

          ;; Interrupt active thread if running
          (when-let [^Thread thread @(:active-thread session)]
            (when (.isAlive thread)
              (.interrupt thread)))

          ;; Remove from registry
          (.remove session-registry id)

          (log/info "OpenRouter ling killed" {:id id :model (:model session)})
          {:killed? true :id id :model (:model session)})
        ;; Session not found - may already be dead
        (do
          (log/warn "OpenRouter ling not found in registry" {:id id})
          {:killed? true :id id :reason :session-not-found})))))

;;; =============================================================================
;;; Factory
;;; =============================================================================

(defn ->openrouter-strategy
  "Create an OpenRouterStrategy instance."
  []
  (->OpenRouterStrategy))

;;; =============================================================================
;;; Session Query Functions (for external access)
;;; =============================================================================

(defn get-session
  "Get session data for an OpenRouter ling. Returns nil if not found."
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
  "Get stdout ring buffer contents for an OpenRouter ling.

   Arguments:
     ling-id - ID of the OpenRouter ling
     opts    - Optional map: {:last-n N}

   Returns:
     Vector of strings, or nil if not found"
  ([ling-id] (get-stdout ling-id {}))
  ([ling-id opts]
   (when-let [session (.get session-registry ling-id)]
     (headless/ring-buffer-contents (:stdout-buffer session) opts))))

(defn get-stdout-since
  "Get stdout lines appended after a given timestamp.

   Arguments:
     ling-id - ID of the OpenRouter ling
     since   - Epoch milliseconds

   Returns:
     Vector of {:text :ts} maps, or nil if not found"
  [ling-id since]
  (when-let [session (.get session-registry ling-id)]
    (headless/ring-buffer-contents-since (:stdout-buffer session) since)))

(defn get-conversation
  "Get the full conversation history for an OpenRouter ling.
   Returns vector of {:role :content} maps, or nil."
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
  "Kill all OpenRouter sessions. For cleanup/testing."
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

(comment
  ;; Usage examples

  ;; Spawn an OpenRouter ling
  ;; (strategy-spawn! (->openrouter-strategy)
  ;;   {:id "or-ling-1" :cwd "/tmp" :model "deepseek/deepseek-chat"
  ;;    :project-id "test"}
  ;;   {:task "Hello, what model are you?"})

  ;; Check session
  ;; (get-session "or-ling-1")

  ;; Get output
  ;; (get-stdout "or-ling-1" {:last-n 20})

  ;; Kill
  ;; (kill-all-openrouter!)
  )
