(ns hive-mcp.transport
  "Transport abstraction layer for hive-mcp channel communication.
   
   Provides a unified protocol for TCP and Unix Domain Socket transports,
   allowing seamless switching between transport types.
   
   Architecture:
   - Transport protocol defines connect!/disconnect!/send!/recv!/connected?
   - UnixTransport uses Java 16+ native UnixDomainSocketAddress
   - TCPTransport uses standard Java NIO SocketChannel
   - Server implementations accept connections and yield client transports
   
   Usage:
     ;; Create transports
     (def unix-t (unix-transport \"/tmp/my.sock\"))
     (def tcp-t (tcp-transport \"localhost\" 9998))
     
     ;; Client operations
     (connect! unix-t)
     (send! unix-t {:type \"ping\"})
     (recv! unix-t) ; => {:type \"pong\"}
     (disconnect! unix-t)
     
     ;; Server operations
     (def server (start-unix-server! \"/tmp/my.sock\" handler-fn))
     (stop-server! server)"
  (:require [nrepl.bencode :as bencode]
            [taoensso.timbre :as log])
  (:import [java.net UnixDomainSocketAddress StandardProtocolFamily]
           [java.nio ByteBuffer]
           [java.nio.channels ServerSocketChannel SocketChannel]
           [java.nio.file Files]
           [java.io ByteArrayOutputStream ByteArrayInputStream PushbackInputStream Closeable]
           [java.util.concurrent Executors]))

;; =============================================================================
;; Bencode Helpers (shared with channel.clj)
;; =============================================================================

(defn encode-msg
  "Encode Clojure map to bencode bytes."
  [msg]
  (let [baos (ByteArrayOutputStream.)]
    (bencode/write-bencode baos msg)
    (.toByteArray baos)))

(defn- bytes->str
  "Convert byte arrays to strings recursively."
  [v]
  (cond
    (bytes? v) (String. ^bytes v "UTF-8")
    (map? v) (into {} (map (fn [[k v]] [(bytes->str k) (bytes->str v)]) v))
    (sequential? v) (mapv bytes->str v)
    :else v))

(defn decode-msg
  "Decode bencode bytes to Clojure map."
  [^bytes data]
  (try
    (let [in (PushbackInputStream. (ByteArrayInputStream. data))]
      (-> (bencode/read-bencode in)
          bytes->str))
    (catch Exception e
      (log/debug "Bencode decode error:" (.getMessage e))
      nil)))

(defn decode-msg-from-stream
  "Decode bencode message from a PushbackInputStream.
   Returns:
   - decoded message map on success
   - :eof on end of stream (clean disconnect)
   - nil on recoverable error (continue reading)"
  [^PushbackInputStream in]
  (try
    ;; Check for EOF before blocking read
    (let [first-byte (.read in)]
      (if (= first-byte -1)
        :eof
        (do
          (.unread in first-byte)
          (-> (bencode/read-bencode in)
              bytes->str))))
    (catch java.io.EOFException _
      (log/debug "Stream EOF reached")
      :eof)
    (catch java.net.SocketException e
      (log/debug "Socket closed:" (.getMessage e))
      :eof)
    (catch Exception e
      (log/debug "Bencode stream decode error:" (.getMessage e))
      nil)))

;; =============================================================================
;; Transport Protocol
;; =============================================================================

(defprotocol ITransport
  "Protocol for bidirectional message transport."
  (connect! [this] "Connect to the remote endpoint. Returns true on success.")
  (disconnect! [this] "Disconnect from the remote endpoint.")
  (connected? [this] "Check if transport is connected.")
  (send! [this msg] "Send a message (map). Returns true on success.")
  (recv! [this] "Receive a message (blocking). Returns map or nil on disconnect.")
  (get-stream [this] "Get underlying Manifold stream if available."))

(defprotocol IServer
  "Protocol for transport servers."
  (stop-server! [this] "Stop the server and cleanup resources.")
  (server-running? [this] "Check if server is running.")
  (get-clients [this] "Get map of connected client IDs to their streams/channels."))

;; =============================================================================
;; Unix Domain Socket Transport (Client)
;; =============================================================================

(deftype UnixTransport [path ^:volatile-mutable channel ^:volatile-mutable input-stream]
  ITransport
  (connect! [this]
    (try
      (let [addr (UnixDomainSocketAddress/of ^String path)
            ch (SocketChannel/open StandardProtocolFamily/UNIX)]
        (.connect ch addr)
        (.configureBlocking ch true)
        (set! channel ch)
        (set! input-stream (PushbackInputStream.
                            (java.nio.channels.Channels/newInputStream ch)))
        (log/debug "UnixTransport connected to" path)
        true)
      (catch Exception e
        (log/error "UnixTransport connect failed:" (.getMessage e))
        false)))

  (disconnect! [this]
    (when channel
      (try
        (.close ^SocketChannel channel)
        (catch Exception e
          (log/debug "UnixTransport disconnect error:" (.getMessage e))))
      (set! channel nil)
      (set! input-stream nil)
      (log/debug "UnixTransport disconnected from" path)))

  (connected? [this]
    (and channel (.isConnected ^SocketChannel channel)))

  (send! [this msg]
    (when (connected? this)
      (try
        (let [data (encode-msg msg)
              buf (ByteBuffer/wrap data)]
          (while (.hasRemaining buf)
            (.write ^SocketChannel channel buf))
          true)
        (catch Exception e
          (log/error "UnixTransport send failed:" (.getMessage e))
          false))))

  (recv! [this]
    (when (and (connected? this) input-stream)
      (decode-msg-from-stream input-stream)))

  (get-stream [this]
    nil)) ; Unix transport doesn't use Manifold streams directly

(defn unix-transport
  "Create a Unix Domain Socket transport for the given path."
  [path]
  (->UnixTransport path nil nil))

;; =============================================================================
;; TCP Transport (Client)
;; =============================================================================

(deftype TCPTransport [host port ^:volatile-mutable channel ^:volatile-mutable input-stream]
  ITransport
  (connect! [this]
    (try
      (let [ch (SocketChannel/open)
            addr (java.net.InetSocketAddress. ^String host ^int port)]
        (.connect ch addr)
        (.configureBlocking ch true)
        (set! channel ch)
        (set! input-stream (PushbackInputStream.
                            (java.nio.channels.Channels/newInputStream ch)))
        (log/debug "TCPTransport connected to" host ":" port)
        true)
      (catch Exception e
        (log/error "TCPTransport connect failed:" (.getMessage e))
        false)))

  (disconnect! [this]
    (when channel
      (try
        (.close ^SocketChannel channel)
        (catch Exception e
          (log/debug "TCPTransport disconnect error:" (.getMessage e))))
      (set! channel nil)
      (set! input-stream nil)
      (log/debug "TCPTransport disconnected from" host ":" port)))

  (connected? [this]
    (and channel (.isConnected ^SocketChannel channel)))

  (send! [this msg]
    (when (connected? this)
      (try
        (let [data (encode-msg msg)
              buf (ByteBuffer/wrap data)]
          (while (.hasRemaining buf)
            (.write ^SocketChannel channel buf))
          true)
        (catch Exception e
          (log/error "TCPTransport send failed:" (.getMessage e))
          false))))

  (recv! [this]
    (when (and (connected? this) input-stream)
      (decode-msg-from-stream input-stream)))

  (get-stream [this]
    nil))

(defn tcp-transport
  "Create a TCP transport for the given host and port."
  [host port]
  (->TCPTransport host port nil nil))

;; =============================================================================
;; Unix Domain Socket Server
;; =============================================================================

(defn- handle-unix-client
  "Handle a connected Unix client in a separate thread.
   Properly handles EOF to avoid spinning on closed connections."
  [^SocketChannel client-channel client-id clients on-message running?]
  (log/info "Unix client connected:" client-id)
  (swap! clients assoc client-id client-channel)
  (try
    (let [in (PushbackInputStream.
              (java.nio.channels.Channels/newInputStream client-channel))]
      (loop []
        (when (and @running? (.isConnected client-channel))
          (let [msg (decode-msg-from-stream in)]
            (cond
              ;; EOF - clean disconnect, exit loop
              (= msg :eof)
              (log/debug "Unix client" client-id "sent EOF")

              ;; Valid message - process and continue
              (some? msg)
              (do
                (log/debug "Received from" client-id ":" msg)
                (when on-message
                  (on-message (assoc msg :client-id client-id)))
                (recur))

              ;; nil - recoverable error, continue reading
              :else
              (recur))))))
    (catch Exception e
      (when @running?
        (log/debug "Unix client" client-id "disconnected:" (.getMessage e)))))
  (swap! clients dissoc client-id)
  (when (.isOpen client-channel)
    (.close client-channel))
  (log/info "Unix client disconnected:" client-id))

(defrecord UnixServer [path server-channel clients running? executor]
  IServer
  (stop-server! [this]
    (reset! running? false)
    ;; Close all client channels
    (doseq [[id ch] @clients]
      (try (.close ^SocketChannel ch) (catch Exception _)))
    (reset! clients {})
    ;; Close server channel
    (when server-channel
      (try (.close ^ServerSocketChannel server-channel) (catch Exception _)))
    ;; Shutdown executor
    (when executor
      (.shutdownNow ^java.util.concurrent.ExecutorService executor))
    ;; Cleanup socket file
    (try
      (Files/deleteIfExists (.toPath (java.io.File. ^String path)))
      (catch Exception _))
    (log/info "Unix server stopped:" path))

  (server-running? [this]
    @running?)

  (get-clients [this]
    @clients)

  Closeable
  (close [this]
    (stop-server! this)))

(defn start-unix-server!
  "Start a Unix Domain Socket server.
   
   Options:
     :path       - Socket file path (required)
     :on-message - Callback for received messages (fn [msg])
     :on-connect - Callback when client connects (fn [client-id])
   
   Returns UnixServer record."
  [{:keys [path on-message on-connect]
    :or {path "/tmp/hive-mcp-channel.sock"}}]
  ;; Cleanup existing socket file
  (try
    (Files/deleteIfExists (.toPath (java.io.File. ^String path)))
    (catch Exception _))

  (let [addr (UnixDomainSocketAddress/of ^String path)
        server-ch (doto (ServerSocketChannel/open StandardProtocolFamily/UNIX)
                    (.bind addr))
        clients (atom {})
        running? (atom true)
        executor (Executors/newCachedThreadPool)
        server (->UnixServer path server-ch clients running? executor)]

    ;; Accept loop in background thread
    (.submit executor
             ^Runnable
             (fn []
               (log/info "Unix server listening on" path)
               (while @running?
                 (try
                   (when-let [client-ch (.accept server-ch)]
                     (let [client-id (str (gensym "unix-client-"))]
                       (when on-connect (on-connect client-id))
                       (.submit executor
                                ^Runnable
                                #(handle-unix-client client-ch client-id
                                                     clients on-message running?))))
                   (catch java.nio.channels.ClosedChannelException _
                     (reset! running? false))
                   (catch Exception e
                     (when @running?
                       (log/error "Unix server accept error:" (.getMessage e))))))))
    server))

(defn broadcast-unix!
  "Broadcast a message to all connected Unix clients."
  [^UnixServer server msg]
  (let [data (encode-msg msg)
        buf-template (ByteBuffer/wrap data)]
    (doseq [[id ^SocketChannel ch] @(:clients server)]
      (when (.isConnected ch)
        (try
          (let [buf (.duplicate buf-template)]
            (.rewind buf)
            (while (.hasRemaining buf)
              (.write ch buf)))
          (catch Exception e
            (log/warn "Broadcast to" id "failed:" (.getMessage e))))))))

;; =============================================================================
;; TCP Server (using Java NIO for consistency)
;; =============================================================================

(defn- handle-tcp-client
  "Handle a connected TCP client in a separate thread.
   Properly handles EOF to avoid spinning on closed connections."
  [^SocketChannel client-channel client-id clients on-message running?]
  (log/info "TCP client connected:" client-id)
  (swap! clients assoc client-id client-channel)
  (try
    (let [in (PushbackInputStream.
              (java.nio.channels.Channels/newInputStream client-channel))]
      (loop []
        (when (and @running? (.isConnected client-channel))
          (let [msg (decode-msg-from-stream in)]
            (cond
              ;; EOF - clean disconnect, exit loop
              (= msg :eof)
              (log/debug "TCP client" client-id "sent EOF")

              ;; Valid message - process and continue
              (some? msg)
              (do
                (log/debug "Received from" client-id ":" msg)
                (when on-message
                  (on-message (assoc msg :client-id client-id)))
                (recur))

              ;; nil - recoverable error, continue reading
              :else
              (recur))))))
    (catch Exception e
      (when @running?
        (log/debug "TCP client" client-id "disconnected:" (.getMessage e)))))
  (swap! clients dissoc client-id)
  (when (.isOpen client-channel)
    (.close client-channel))
  (log/info "TCP client disconnected:" client-id))

(defrecord TCPServer [port server-channel clients running? executor]
  IServer
  (stop-server! [this]
    (reset! running? false)
    ;; Close all client channels
    (doseq [[id ch] @clients]
      (try (.close ^SocketChannel ch) (catch Exception _)))
    (reset! clients {})
    ;; Close server channel
    (when server-channel
      (try (.close ^ServerSocketChannel server-channel) (catch Exception _)))
    ;; Shutdown executor
    (when executor
      (.shutdownNow ^java.util.concurrent.ExecutorService executor))
    (log/info "TCP server stopped on port" port))

  (server-running? [this]
    @running?)

  (get-clients [this]
    @clients)

  Closeable
  (close [this]
    (stop-server! this)))

(defn start-tcp-server!
  "Start a TCP server.
   
   Options:
     :port       - TCP port (required)
     :on-message - Callback for received messages (fn [msg])
     :on-connect - Callback when client connects (fn [client-id])
   
   Returns TCPServer record."
  [{:keys [port on-message on-connect]
    :or {port 9998}}]
  (let [addr (java.net.InetSocketAddress. ^int port)
        server-ch (doto (ServerSocketChannel/open)
                    (.bind addr))
        clients (atom {})
        running? (atom true)
        executor (Executors/newCachedThreadPool)
        server (->TCPServer port server-ch clients running? executor)]

    ;; Accept loop in background thread
    (.submit executor
             ^Runnable
             (fn []
               (log/info "TCP server listening on port" port)
               (while @running?
                 (try
                   (when-let [client-ch (.accept server-ch)]
                     (let [client-id (str (gensym "tcp-client-"))]
                       (when on-connect (on-connect client-id))
                       (.submit executor
                                ^Runnable
                                #(handle-tcp-client client-ch client-id
                                                    clients on-message running?))))
                   (catch java.nio.channels.ClosedChannelException _
                     (reset! running? false))
                   (catch Exception e
                     (when @running?
                       (log/error "TCP server accept error:" (.getMessage e))))))))
    server))

(defn broadcast-tcp!
  "Broadcast a message to all connected TCP clients."
  [^TCPServer server msg]
  (let [data (encode-msg msg)
        buf-template (ByteBuffer/wrap data)]
    (doseq [[id ^SocketChannel ch] @(:clients server)]
      (when (.isConnected ch)
        (try
          (let [buf (.duplicate buf-template)]
            (.rewind buf)
            (while (.hasRemaining buf)
              (.write ch buf)))
          (catch Exception e
            (log/warn "Broadcast to" id "failed:" (.getMessage e))))))))

;; =============================================================================
;; Factory Functions
;; =============================================================================

(defn start-server!
  "Start a server of the specified type.
   
   Options:
     :type       - :unix or :tcp (default: :tcp)
     :path       - Socket path for :unix type
     :port       - Port for :tcp type (default: 9998)
     :on-message - Callback for received messages
     :on-connect - Callback when client connects
   
   Returns server record implementing IServer."
  [{:keys [type] :as opts}]
  (case type
    :unix (start-unix-server! opts)
    :tcp (start-tcp-server! opts)
    (start-tcp-server! opts)))

(defn broadcast!
  "Broadcast message to all clients of a server."
  [server msg]
  (condp instance? server
    UnixServer (broadcast-unix! server msg)
    TCPServer (broadcast-tcp! server msg)
    (throw (ex-info "Unknown server type" {:server (type server)}))))

(defn client-count
  "Get number of connected clients."
  [server]
  (count @(:clients server)))

(comment
  ;; Development REPL examples

  ;; Unix server
  (def unix-srv (start-unix-server! {:path "/tmp/test.sock"
                                     :on-message #(println "Got:" %)}))
  (server-running? unix-srv)
  (stop-server! unix-srv)

  ;; Unix client
  (def unix-cl (unix-transport "/tmp/test.sock"))
  (connect! unix-cl)
  (send! unix-cl {:type "ping" :data "hello"})
  (disconnect! unix-cl)

  ;; TCP server
  (def tcp-srv (start-tcp-server! {:port 9999
                                   :on-message #(println "Got:" %)}))
  (stop-server! tcp-srv)

  ;; TCP client
  (def tcp-cl (tcp-transport "localhost" 9999))
  (connect! tcp-cl)
  (send! tcp-cl {:type "ping" :data "hello"})
  (disconnect! tcp-cl))
