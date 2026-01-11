(ns hive-mcp.agent.ollama
  "Ollama backend for agent delegation.
   
   Provides local LLM access via Ollama HTTP API.
   Uses OpenAI-compatible tool calling format."
  (:require [hive-mcp.agent.protocol :as proto]
            [clojure.data.json :as json]
            [taoensso.timbre :as log])
  (:import [java.net URI]
           [java.net.http HttpClient HttpRequest HttpRequest$BodyPublishers HttpResponse$BodyHandlers]
           [java.time Duration]))

;;; ============================================================
;;; HTTP Client
;;; ============================================================

(defonce ^:private http-client
  (delay
    (-> (HttpClient/newBuilder)
        (.connectTimeout (Duration/ofSeconds 30))
        (.build))))

(defn- http-post
  "Make HTTP POST request, return parsed JSON response."
  [url body & {:keys [timeout-secs] :or {timeout-secs 300}}]
  (let [request (-> (HttpRequest/newBuilder)
                    (.uri (URI/create url))
                    (.header "Content-Type" "application/json")
                    (.POST (HttpRequest$BodyPublishers/ofString (json/write-str body)))
                    (.timeout (Duration/ofSeconds timeout-secs))
                    (.build))
        response (.send @http-client request (HttpResponse$BodyHandlers/ofString))
        status (.statusCode response)
        body-str (.body response)]
    (if (= status 200)
      (json/read-str body-str :key-fn keyword)
      (throw (ex-info "HTTP request failed"
                      {:status status :body body-str :url url})))))

;;; ============================================================
;;; Ollama Backend
;;; ============================================================

(defn- tools->openai-format
  "Convert hive-mcp tool schemas to OpenAI function format for Ollama."
  [tools]
  (mapv (fn [{:keys [name description inputSchema]}]
          {:type "function"
           :function {:name name
                      :description description
                      :parameters (or inputSchema {:type "object" :properties {}})}})
        tools))

(defn- parse-response
  "Parse Ollama chat response into normalized format."
  [response]
  (let [message (get-in response [:message])
        content (:content message)
        tool-calls (:tool_calls message)]
    (cond
      ;; Tool calls present
      (seq tool-calls)
      {:type :tool_calls
       :calls (mapv (fn [tc]
                      {:id (str (java.util.UUID/randomUUID))
                       :name (get-in tc [:function :name])
                       :arguments (let [args (get-in tc [:function :arguments])]
                                    (if (string? args)
                                      (json/read-str args :key-fn keyword)
                                      args))})
                    tool-calls)}

      ;; Text response
      content
      {:type :text :content content}

      ;; Empty response
      :else
      {:type :text :content ""})))

(defrecord OllamaBackend [host model]
  proto/LLMBackend
  (chat [_ messages tools]
    (log/debug "Ollama chat request" {:model model :msg-count (count messages)})
    (let [ollama-tools (when (seq tools)
                         (tools->openai-format tools))
          body (cond-> {:model model
                        :messages messages
                        :stream false}
                 (seq ollama-tools) (assoc :tools ollama-tools))
          response (http-post (str host "/api/chat") body :timeout-secs 300)]
      (parse-response response)))

  (model-name [_] model))

(defn ollama-backend
  "Create an Ollama backend for agent delegation.
   
   Options:
     :host - Ollama server URL (default: http://localhost:11434)
     :model - Model name (default: devstral-small:24b)"
  ([] (ollama-backend {}))
  ([{:keys [host model]
     :or {host "http://localhost:11434"
          model "devstral-small:24b"}}]
   (->OllamaBackend host model)))
