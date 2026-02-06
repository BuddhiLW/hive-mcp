(ns olympus-web.config
  "Configuration for Olympus Web UI")

;; WebSocket configuration
;; Port 7911 = Olympus-dedicated WS server (snapshot on connect, typed events)
;; Port 9999 = General hive-mcp channel (Emacs integration, legacy)
(def ws-url
  "Default WebSocket URL for Olympus WS server.
   Connects to dedicated port 7911 which sends full snapshot on connect."
  "ws://localhost:7911")

(def ws-reconnect-delay-ms
  "Delay between reconnection attempts"
  3000)

(def ws-max-reconnect-attempts
  "Maximum reconnection attempts before giving up"
  10)

;; Graph layout configuration
(def dagre-config
  "Default dagre layout options"
  {:rankdir "TB"      ; TB = top-bottom, LR = left-right
   :nodesep 50        ; Horizontal separation between nodes
   :ranksep 80        ; Vertical separation between ranks
   :marginx 20
   :marginy 20})

;; Node dimensions for layout calculation
(def node-dimensions
  {:agent {:width 180 :height 80}
   :wave {:width 140 :height 60}
   :task {:width 120 :height 40}
   :kg {:width 160 :height 70}})

;; Color scheme (matches CSS variables)
(def colors
  {:agent {:coordinator "#9333ea"
           :ling "#2563eb"
           :drone "#16a34a"}
   :wave {:container "#f97316"
          :pending "#6b7280"
          :running "#eab308"
          :completed "#16a34a"
          :failed "#dc2626"}
   :kg {:note "#06b6d4"
        :snippet "#ec4899"
        :convention "#eab308"
        :decision "#9333ea"
        :axiom "#dc2626"}})

;; Message buffer size
(def max-hivemind-messages 50)

;; Terminal configuration
(def terminal-theme
  "xterm.js theme matching Olympus dark UI."
  {:background "#0f172a"
   :foreground "#f8fafc"
   :cursor "#2563eb"
   :cursorAccent "#0f172a"
   :selectionBackground "rgba(37, 99, 235, 0.3)"
   :black "#1e293b"
   :red "#ef4444"
   :green "#22c55e"
   :yellow "#eab308"
   :blue "#3b82f6"
   :magenta "#a855f7"
   :cyan "#06b6d4"
   :white "#f8fafc"
   :brightBlack "#475569"
   :brightRed "#f87171"
   :brightGreen "#4ade80"
   :brightYellow "#facc15"
   :brightBlue "#60a5fa"
   :brightMagenta "#c084fc"
   :brightCyan "#22d3ee"
   :brightWhite "#ffffff"})

(def terminal-options
  "Default xterm.js terminal options."
  {:cursorBlink true
   :fontSize 13
   :fontFamily "'SF Mono', 'Fira Code', 'Cascadia Code', 'JetBrains Mono', monospace"
   :lineHeight 1.2
   :scrollback 5000
   :convertEol true})

;; Max terminal output lines to buffer before trimming
(def max-terminal-buffer 10000)
