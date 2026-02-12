(ns modex-bb.mcp.protocols)

(defprotocol AResource)

(defprotocol APrompt)

;; Main MCP Server protocol
(defprotocol AServer
  (protocol-version [this])
  (server-name [this])
  (version [this])

  (on-receive [this msg] "For testing receive.")
  (on-send [this msg] "For testing sent messages.")
  (enqueue-notification [_this msg] "For testing notifications. Will collapse into an async bus lands.")

  (send-notification [this notification] "Called after a has been sent.")

  (capabilities [this])
  (initialize [this _init-params])

  (list-tools [this])
  (call-tool [this tool-name arg-map])

  (list-resources [this])
  (list-prompts [this]))
