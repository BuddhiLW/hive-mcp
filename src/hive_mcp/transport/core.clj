(ns hive-mcp.transport.core
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
  (:require [bencode.core :as bencode]
            [taoensso.timbre :as log]
            [hive-mcp.dns.result :as result])
  (:import [java.net UnixDomainSocketAddress StandardProtocolFamily]
           [java.nio ByteBuffer]
           [java.nio.channels ServerSocketChannel SocketChannel]
           [java.nio.file Files]
           [java.io ByteArrayOutputStream ByteArrayInputStream PushbackInputStream Closeable]
           [java.util.concurrent Executors]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

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
  (result/rescue nil
                 (let [in (PushbackInputStream. (ByteArrayInputStream. data))]
                   (-> (bencode/read-bencode in)
                       bytes->str))))

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
;; Shared Transport Helpers (Result DSL)
;; =============================================================================

(defn- try-connect!
  "Open, connect and configure a SocketChannel.
   open-fn: zero-arg thunk returning a new SocketChannel
   addr-fn: zero-arg thunk returning the address to connect to
   Returns Result<{:channel :input-stream}>."
  [open-fn addr-fn label]
  (result/try-effect* :transport/connect-failed
                      (let [addr (addr-fn)
                            ch   (open-fn)]
                        (.connect ^SocketChannel ch addr)
                        (.configureBlocking ^SocketChannel ch true)
                        (log/debug label "connected")
                        {:channel ch
                         :input-stream (PushbackInputStream.
                                        (java.nio.channels.Channels/newInputStream ch))})))

(defn- disconnect-channel!
  "Close a SocketChannel if non-nil. Swallows close exceptions."
  [^SocketChannel ch label]
  (when ch
    (result/rescue nil (.close ch))
    (log/debug label "disconnected")))

(defn- try-send!
  "Encode msg and write to channel. Returns Result<true>."
  [^SocketChannel channel msg]
  (result/try-effect* :transport/send-failed
                      (let [data (encode-msg msg)
                            buf  (ByteBuffer/wrap data)]
                        (while (.hasRemaining buf)
                          (.write channel buf))
                        true)))

;; =============================================================================
;; Unix Domain Socket Transport (Client)
;; =============================================================================

(deftype UnixTransport [path ^:volatile-mutable channel ^:volatile-mutable input-stream]
  ITransport
  (connect! [_this]
    (let [r (try-connect!
             #(SocketChannel/open StandardProtocolFamily/UNIX)
             #(UnixDomainSocketAddress/of ^String path)
             (str "Unix:" path))]
      (if (result/ok? r)
        (let [conn (:ok r)]
          (set! channel (:channel conn))
          (set! input-stream (:input-stream conn))
          true)
        (do (log/error "UnixTransport connect failed:" (:message r))
            false))))

  (disconnect! [_]
    (disconnect-channel! channel (str "Unix:" path))
    (set! channel nil)
    (set! input-stream nil))

  (connected? [_]
    (and channel (.isConnected ^SocketChannel channel)))

  (send! [_this msg]
    (when (connected? _this)
      (result/ok? (try-send! channel msg))))

  (recv! [this]
    (when (and (connected? this) input-stream)
      (decode-msg-from-stream input-stream)))

  (get-stream [_]
    nil))

(defn unix-transport
  "Create a Unix Domain Socket transport for the given path."
  [path]
  (->UnixTransport path nil nil))

;; =============================================================================
;; TCP Transport (Client)
;; =============================================================================

(deftype TCPTransport [host port ^:volatile-mutable channel ^:volatile-mutable input-stream]
  ITransport
  (connect! [_this]
    (let [r (try-connect!
             #(SocketChannel/open)
             #(java.net.InetSocketAddress. ^String host ^int port)
             (str "TCP:" host ":" port))]
      (if (result/ok? r)
        (let [conn (:ok r)]
          (set! channel (:channel conn))
          (set! input-stream (:input-stream conn))
          true)
        (do (log/error "TCPTransport connect failed:" (:message r))
            false))))

  (disconnect! [_]
    (disconnect-channel! channel (str "TCP:" host ":" port))
    (set! channel nil)
    (set! input-stream nil))

  (connected? [_]
    (and channel (.isConnected ^SocketChannel channel)))

  (send! [_this msg]
    (when (connected? _this)
      (result/ok? (try-send! channel msg))))

  (recv! [this]
    (when (and (connected? this) input-stream)
      (decode-msg-from-stream input-stream)))

  (get-stream [_]
    nil))

(defn tcp-transport
  "Create a TCP transport for the given host and port."
  [host port]
  (->TCPTransport host port nil nil))

;; =============================================================================
;; Server Helpers
;; =============================================================================

(defn- handle-client
  "Handle a connected client in a separate thread.
   Properly handles EOF to avoid spinning on closed connections.
   `label` is a string like \"Unix\" or \"TCP\" for logging."
  [^SocketChannel client-channel client-id clients on-message running? label]
  (log/info (str label " client connected:") client-id)
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
              (log/debug (str label " client") client-id "sent EOF")

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
        (log/debug (str label " client") client-id "disconnected:" (.getMessage e)))))
  (swap! clients dissoc client-id)
  (when (.isOpen client-channel)
    (.close client-channel))
  (log/info (str label " client disconnected:") client-id))

(defn- shutdown-server-common!
  "Shared shutdown logic for servers. Optional cleanup-fn for extra steps."
  [running? clients server-channel executor cleanup-fn]
  (reset! running? false)
  ;; Close all client channels
  (doseq [[_id ch] @clients]
    (result/rescue nil (.close ^SocketChannel ch)))
  (reset! clients {})
  ;; Close server channel
  (when server-channel
    (result/rescue nil (.close ^ServerSocketChannel server-channel)))
  ;; Shutdown executor
  (when executor
    (.shutdownNow ^java.util.concurrent.ExecutorService executor))
  ;; Optional cleanup (e.g., delete socket file)
  (when cleanup-fn (cleanup-fn)))

(defn- accept-loop!
  "Run server accept loop, spawning client handlers on executor.
   label: e.g. \"Unix\" or \"TCP\" — used for logging and client-id prefix."
  [^ServerSocketChannel server-ch clients running?
   ^java.util.concurrent.ExecutorService executor
   on-connect on-message label log-msg]
  (log/info log-msg)
  (let [prefix (str (.toLowerCase ^String label) "-client-")]
    (while @running?
      (try
        (when-let [client-ch (.accept server-ch)]
          (let [client-id (str (gensym prefix))]
            (when on-connect (on-connect client-id))
            (.submit executor
                     ^Runnable
                     #(handle-client client-ch client-id
                                     clients on-message running? label))))
        (catch java.nio.channels.ClosedChannelException _
          (reset! running? false))
        (catch Exception e
          (when @running?
            (log/error label "server accept error:" (.getMessage e))))))))

;; =============================================================================
;; Unix Domain Socket Server
;; =============================================================================

(defrecord UnixServer [path server-channel clients running? executor]
  IServer
  (stop-server! [_]
    (shutdown-server-common!
     running? clients server-channel executor
     #(result/rescue nil (Files/deleteIfExists (.toPath (java.io.File. ^String path)))))
    (log/info "Unix server stopped:" path))

  (server-running? [_]
    @running?)

  (get-clients [_]
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
  (result/rescue nil (Files/deleteIfExists (.toPath (java.io.File. ^String path))))

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
             #(accept-loop! server-ch clients running? executor
                            on-connect on-message
                            "Unix" (str "Unix server listening on " path)))
    server))

(defn- broadcast-to-clients!
  "Broadcast a message to all connected clients in the clients atom."
  [clients-atom msg]
  (let [data (encode-msg msg)
        buf-template (ByteBuffer/wrap data)]
    (doseq [[_id ^SocketChannel ch] @clients-atom]
      (when (.isConnected ch)
        (result/rescue nil
                       (let [buf (.duplicate buf-template)]
                         (.rewind buf)
                         (while (.hasRemaining buf)
                           (.write ch buf))))))))

;; =============================================================================
;; TCP Server (using Java NIO for consistency)
;; =============================================================================

(defrecord TCPServer [port server-channel clients running? executor]
  IServer
  (stop-server! [_]
    (shutdown-server-common! running? clients server-channel executor nil)
    (log/info "TCP server stopped on port" port))

  (server-running? [_]
    @running?)

  (get-clients [_]
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
             #(accept-loop! server-ch clients running? executor
                            on-connect on-message
                            "TCP" (str "TCP server listening on port " port)))
    server))

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
  (broadcast-to-clients! (:clients server) msg))

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
