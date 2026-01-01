(ns emacs-mcp.server
  "MCP server for Emacs interaction via emacsclient."
  (:require [io.modelcontext.clojure-sdk.stdio-server :as io-server]
            [emacs-mcp.tools :as tools]
            [emacs-mcp.docs :as docs]
            [emacs-mcp.chroma :as chroma]
            [emacs-mcp.embeddings.ollama :as ollama]
            [taoensso.timbre :as log])
  (:gen-class))

;; Configure Timbre to write to stderr instead of stdout
;; This is CRITICAL for MCP servers - stdout is the JSON-RPC channel
(log/merge-config!
 {:appenders
  {:println {:enabled? true
             :async? false
             :fn (fn [data]
                   (let [{:keys [output_]} data]
                     (binding [*out* *err*]
                       (println (force output_)))))}}})

;; Convert our tool definitions to the SDK format
(defn make-tool
  "Convert a tool definition with :handler to SDK format."
  [{:keys [name description inputSchema handler]}]
  {:name name
   :description description
   :inputSchema inputSchema
   :handler handler})

(def emacs-server-spec
  {:name "emacs-mcp"
   :version "0.1.0"
   :tools (mapv make-tool (concat tools/tools docs/docs-tools))})

(defn init-embedding-provider!
  "Initialize the embedding provider for semantic memory search.
  Attempts to configure Ollama embeddings for Chroma.
  Fails gracefully if Ollama or Chroma are not available.
  
  Configuration via environment variables:
    CHROMA_HOST - Chroma server host (default: localhost)
    CHROMA_PORT - Chroma server port (default: 8000)
    OLLAMA_HOST - Ollama server URL (default: http://localhost:11434)"
  []
  (try
    ;; Configure Chroma connection - read from env or use defaults
    (let [chroma-host (or (System/getenv "CHROMA_HOST") "localhost")
          chroma-port (or (some-> (System/getenv "CHROMA_PORT") Integer/parseInt) 8000)]
      (chroma/configure! {:host chroma-host :port chroma-port})
      (log/info "Chroma configured:" chroma-host ":" chroma-port))
    ;; Configure Ollama embedding provider
    (let [ollama-host (or (System/getenv "OLLAMA_HOST") "http://localhost:11434")
          provider (ollama/->provider {:host ollama-host})]
      (chroma/set-embedding-provider! provider)
      (log/info "Embedding provider initialized: Ollama at" ollama-host)
      true)
    (catch Exception e
      (log/warn "Could not initialize embedding provider:"
                (.getMessage e)
                "- Semantic search will be unavailable")
      false)))

(defn start!
  "Start the MCP server."
  [& _args]
  (let [server-id (random-uuid)]
    (log/info "Starting emacs-mcp server:" server-id)
    ;; Initialize embedding provider for semantic search (fails gracefully)
    (init-embedding-provider!)
    @(io-server/run! (assoc emacs-server-spec :server-id server-id))))

(defn -main
  "Entry point for the MCP server."
  [& args]
  (apply start! args))

(comment
  ;; For REPL development
  (start!))
