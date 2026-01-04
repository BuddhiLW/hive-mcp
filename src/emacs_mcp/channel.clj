(ns emacs-mcp.channel
  "Bidirectional communication channel between Clojure and Emacs.

   Provides persistent socket connection for push-based events,
   replacing polling-based communication patterns.

   Architecture:
   - IChannel protocol abstracts transport (Unix socket, TCP)
   - EventBus (core.async) for pub/sub within Clojure
   - Bencode message format (compatible with nREPL)

   Uses JDK 16+ native Unix domain sockets - no external dependencies.

   Usage:
     (def ch (unix-channel \"/tmp/emacs-mcp.sock\"))
     (connect! ch)
     (send! ch {:op \"event\" :type \"task-completed\" :task-id \"123\"})
     (subscribe! :task-completed (fn [event] (println event)))
   "
  (:require [clojure.core.async :as async :refer [go go-loop <! >! chan pub sub unsub close!]]
            [nrepl.bencode :as bencode]
            [taoensso.timbre :as log])
  (:import [java.net UnixDomainSocketAddress StandardProtocolFamily InetSocketAddress Socket ServerSocket]
           [java.nio.channels SocketChannel ServerSocketChannel Channels]
           [java.nio.file Files]
           [java.io IOException BufferedInputStream BufferedOutputStream
            PushbackInputStream DataOutputStream ByteArrayOutputStream]))

;; =============================================================================
;; Protocol Definition
;; =============================================================================

(defprotocol IChannel
  "Bidirectional communication channel abstraction."
  (connect! [this] "Establish connection. Returns true on success.")
  (disconnect! [this] "Close connection gracefully.")
  (send! [this msg] "Send bencode message. Returns true on success.")
  (recv! [this] "Receive next bencode message. Blocks until available.")
  (connected? [this] "Check if channel is connected."))

;; =============================================================================
;; Event Bus (core.async pub/sub)
;; =============================================================================

(defonce ^:private event-chan (chan 1024))
(defonce ^:private event-pub (pub event-chan :type))

(defn publish!
  "Publish event to the event bus.
   Event must have :type key for routing."
  [event]
  (when-not (:type event)
    (throw (ex-info "Event must have :type" {:event event})))
  (async/put! event-chan event))

(defn subscribe!
  "Subscribe to events of given type.
   Returns a channel that receives matching events.

   Example:
     (let [ch (subscribe! :task-completed)]
       (go-loop []
         (when-let [event (<! ch)]
           (println \"Task completed:\" event)
           (recur))))"
  [event-type]
  (let [ch (chan 256)]
    (sub event-pub event-type ch)
    ch))

(defn unsubscribe!
  "Unsubscribe channel from event type."
  [event-type ch]
  (unsub event-pub event-type ch)
  (close! ch))

;; =============================================================================
;; Bencode Helpers
;; =============================================================================

(defn- encode-msg
  "Encode Clojure map to bencode bytes."
  [msg]
  (let [baos (ByteArrayOutputStream.)]
    (bencode/write-bencode baos msg)
    (.toByteArray baos)))

(defn- bytes->str
  "Convert byte array to string if needed."
  [v]
  (cond
    (bytes? v) (String. ^bytes v "UTF-8")
    (map? v) (into {} (map (fn [[k v]] [(bytes->str k) (bytes->str v)]) v))
    (sequential? v) (mapv bytes->str v)
    :else v))

(defn- decode-msg
  "Decode bencode from input stream to Clojure map.
   Converts byte arrays to strings for easier handling."
  [^PushbackInputStream in]
  (try
    (-> (bencode/read-bencode in)
        bytes->str)
    (catch Exception e
      (log/debug "Bencode decode error:" (.getMessage e))
      nil)))

;; =============================================================================
;; TCP Channel Implementation (using atoms for mutable state)
;; =============================================================================

(defrecord TcpChannel [host port state]
  ;; state is an atom containing {:socket nil :in nil :out nil}
  IChannel
  (connect! [this]
    (try
      (let [sock (Socket.)]
        (.connect sock (InetSocketAddress. ^String host ^int port) 5000)
        (reset! state
                {:socket sock
                 :in (PushbackInputStream. (BufferedInputStream. (.getInputStream sock)))
                 :out (DataOutputStream. (BufferedOutputStream. (.getOutputStream sock)))})
        (log/info "TCP channel connected to" (str host ":" port))
        true)
      (catch IOException e
        (log/error "TCP connect failed:" (.getMessage e))
        false)))

  (disconnect! [this]
    (when-let [{:keys [socket]} @state]
      (try
        (.close ^Socket socket)
        (log/info "TCP channel disconnected")
        (catch IOException e
          (log/warn "TCP disconnect error:" (.getMessage e))))
      (reset! state {:socket nil :in nil :out nil})))

  (send! [this msg]
    (let [{:keys [socket out]} @state]
      (when (and socket out)
        (try
          (let [bytes (encode-msg msg)]
            (.write ^DataOutputStream out bytes)
            (.flush ^DataOutputStream out)
            true)
          (catch IOException e
            (log/error "TCP send failed:" (.getMessage e))
            false)))))

  (recv! [this]
    (let [{:keys [socket in]} @state]
      (when (and socket in)
        (decode-msg in))))

  (connected? [this]
    (when-let [{:keys [socket]} @state]
      (and socket (not (.isClosed ^Socket socket))))))

(defn tcp-channel
  "Create a TCP channel to host:port."
  [host port]
  (->TcpChannel host port (atom {:socket nil :in nil :out nil})))

;; =============================================================================
;; Unix Socket Channel Implementation (JDK 16+ Native)
;; =============================================================================

(defrecord UnixChannel [path state]
  ;; state is an atom containing {:channel nil :in nil :out nil}
  IChannel
  (connect! [this]
    (try
      (let [addr (UnixDomainSocketAddress/of ^String path)
            ch (SocketChannel/open StandardProtocolFamily/UNIX)]
        (.connect ch addr)
        (reset! state
                {:channel ch
                 :in (PushbackInputStream.
                      (BufferedInputStream.
                       (Channels/newInputStream ch)))
                 :out (DataOutputStream.
                       (BufferedOutputStream.
                        (Channels/newOutputStream ch)))})
        (log/info "Unix channel connected to" path)
        true)
      (catch IOException e
        (log/error "Unix connect failed:" (.getMessage e))
        false)))

  (disconnect! [this]
    (when-let [{:keys [channel]} @state]
      (try
        (.close ^SocketChannel channel)
        (log/info "Unix channel disconnected")
        (catch IOException e
          (log/warn "Unix disconnect error:" (.getMessage e))))
      (reset! state {:channel nil :in nil :out nil})))

  (send! [this msg]
    (let [{:keys [channel out]} @state]
      (when (and channel out)
        (try
          (let [bytes (encode-msg msg)]
            (.write ^DataOutputStream out bytes)
            (.flush ^DataOutputStream out)
            true)
          (catch IOException e
            (log/error "Unix send failed:" (.getMessage e))
            false)))))

  (recv! [this]
    (let [{:keys [channel in]} @state]
      (when (and channel in)
        (decode-msg in))))

  (connected? [this]
    (when-let [{:keys [channel]} @state]
      (and channel (.isConnected ^SocketChannel channel)))))

(defn unix-channel
  "Create a Unix domain socket channel to path."
  [path]
  (->UnixChannel path (atom {:channel nil :in nil :out nil})))

;; =============================================================================
;; Channel Server (Emacs connects to this)
;; =============================================================================

(defonce ^:private server-state (atom nil))

(defn- handle-client
  "Handle incoming client connection."
  [channel client-id]
  (go-loop []
    (when (connected? channel)
      (when-let [msg (recv! channel)]
        (log/debug "Received from" client-id ":" msg)
        ;; Route incoming messages to event bus
        ;; Note: bencode returns string keys, so check for "type" not :type
        (when-let [type-str (get msg "type")]
          (let [event-type (keyword type-str)]
            (publish! (assoc msg :type event-type :client-id client-id))))
        (recur)))))

(defn start-server!
  "Start channel server on Unix socket or TCP port.
   Emacs will connect to this server.

   Options:
     :type - :unix (default) or :tcp
     :path - Unix socket path (for :unix)
     :port - TCP port (for :tcp)

   Returns server state map."
  [{:keys [type path port] :or {type :unix path "/tmp/emacs-mcp-channel.sock"}}]
  (if @server-state
    (do
      (log/warn "Server already running")
      @server-state)
    (let [accept-fn
          (case type
            :unix
            (let [socket-path (java.nio.file.Path/of path (into-array String []))
                  _ (when (Files/exists socket-path (into-array java.nio.file.LinkOption []))
                      (Files/delete socket-path))
                  addr (UnixDomainSocketAddress/of ^String path)
                  server (ServerSocketChannel/open StandardProtocolFamily/UNIX)]
              (.bind server addr)
              (log/info "Unix server listening on" path)
              (fn []
                (let [client (.accept server)
                      ch-state (atom {:channel client
                                      :in (PushbackInputStream.
                                           (BufferedInputStream.
                                            (Channels/newInputStream client)))
                                      :out (DataOutputStream.
                                            (BufferedOutputStream.
                                             (Channels/newOutputStream client)))})]
                  (->UnixChannel path ch-state))))

            :tcp
            (let [server (ServerSocket. ^int port)]
              (log/info "TCP server listening on port" port)
              (fn []
                (let [client (.accept server)
                      ch-state (atom {:socket client
                                      :in (PushbackInputStream.
                                           (BufferedInputStream. (.getInputStream client)))
                                      :out (DataOutputStream.
                                            (BufferedOutputStream. (.getOutputStream client)))})]
                  (->TcpChannel "localhost" port ch-state)))))

          clients (atom {})
          running (atom true)

          accept-loop
          (future
            (while @running
              (try
                (let [client-ch (accept-fn)
                      client-id (str (gensym "client-"))]
                  (swap! clients assoc client-id client-ch)
                  (log/info "Client connected:" client-id)
                  (handle-client client-ch client-id))
                (catch IOException e
                  (when @running
                    (log/error "Accept error:" (.getMessage e)))))))]

      (reset! server-state
              {:type type
               :path path
               :port port
               :clients clients
               :running running
               :accept-loop accept-loop})
      @server-state)))

(defn stop-server!
  "Stop the channel server."
  []
  (when-let [{:keys [running clients path type]} @server-state]
    (reset! running false)
    ;; Close all client connections
    (doseq [[id ch] @clients]
      (disconnect! ch))
    ;; Clean up Unix socket file
    (when (= type :unix)
      (let [socket-path (java.nio.file.Path/of path (into-array String []))]
        (when (Files/exists socket-path (into-array java.nio.file.LinkOption []))
          (Files/delete socket-path))))
    (reset! server-state nil)
    (log/info "Server stopped")))

(defn broadcast!
  "Send message to all connected clients."
  [msg]
  (when-let [{:keys [clients]} @server-state]
    (doseq [[id ch] @clients]
      (when (connected? ch)
        (send! ch msg)))))

;; =============================================================================
;; Convenience Functions
;; =============================================================================

(defn emit-event!
  "Emit an event to all connected clients and local subscribers.
   Uses string keys for consistency with bencode format.

   Example:
     (emit-event! :task-completed {:task-id \"123\" :result \"done\"})"
  [event-type data]
  ;; Convert keyword keys to strings for consistency with bencode
  (let [string-data (into {} (map (fn [[k v]] [(name k) v]) data))
        event (assoc string-data
                     "type" (name event-type)
                     "timestamp" (System/currentTimeMillis)
                     :type event-type)] ; Keep :type for pub/sub routing
    ;; Local pub/sub
    (publish! event)
    ;; Broadcast to connected Emacs clients
    (broadcast! event)))

(comment
  ;; Development REPL examples

  ;; Start Unix socket server (JDK 16+ native)
  (start-server! {:type :unix :path "/tmp/emacs-mcp-channel.sock"})

  ;; Start TCP server
  (start-server! {:type :tcp :port 9999})

  ;; Subscribe to events
  (let [ch (subscribe! :task-completed)]
    (go-loop []
      (when-let [event (<! ch)]
        (println "Received:" event)
        (recur))))

  ;; Emit test event
  (emit-event! :task-completed {:task-id "test-123" :result "success"})

  ;; Stop server
  (stop-server!))
