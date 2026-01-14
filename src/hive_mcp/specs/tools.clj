(ns hive-mcp.specs.tools
  "Clojure specs for MCP tool handlers.

   Provides validation specs for:
   - Tool result structures (MCP protocol compliant)
   - Common handler input parameters
   - Handler-specific output schemas
   - Function specs (fdef) for key handlers

   Reference: io.modelcontext.clojure-sdk.specs for MCP protocol patterns

   CLARITY: 'Inputs are guarded' - specs provide declarative validation"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

;; =============================================================================
;; Tool Result Specs (MCP Protocol Compliant)
;; =============================================================================

;; Text content block (most common)
(s/def :content/type #{"text" "image"})
(s/def :content.text/type #{"text"})
(s/def :content.text/text string?)
(s/def ::text-content
  (s/keys :req-un [:content.text/type :content.text/text]))

;; Image content block
(s/def :content.image/type #{"image"})
(s/def :content.image/data string?)  ; base64 encoded
(s/def :content.image/mimeType string?)
(s/def ::image-content
  (s/keys :req-un [:content.image/type :content.image/data :content.image/mimeType]))

;; Content can be text or image
(s/def ::content-item
  (s/or :text ::text-content
        :image ::image-content))

;; Content vector (MCP requires this as array)
(s/def ::content-vector (s/coll-of ::content-item :kind vector?))

;; Tool result with content vector and optional isError
;; Per MCP spec: errors from tool should be in result, not protocol-level
(s/def :tool-result/content ::content-vector)
(s/def :tool-result/isError boolean?)
(s/def ::tool-result
  (s/keys :req-un [:tool-result/content]
          :opt-un [:tool-result/isError]))

;; Simplified tool result (what handlers actually return)
;; Handlers return single content item, transport wraps in vector
(s/def :handler-result/type #{"text"})
(s/def :handler-result/text string?)
(s/def :handler-result/isError boolean?)
(s/def ::handler-result
  (s/keys :req-un [:handler-result/type :handler-result/text]
          :opt-un [:handler-result/isError]))

;; MCP error response (protocol-level, for tool-not-found etc)
(s/def :mcp-error/error string?)
(s/def ::mcp-error
  (s/keys :req-un [:mcp-error/error]))

;; MCP success with arbitrary result
(s/def :mcp-success/result any?)
(s/def ::mcp-success
  (s/keys :req-un [:mcp-success/result]))

;; =============================================================================
;; Common Handler Input Specs
;; =============================================================================

;; Project identifier - either a SHA hash or named identifier
(s/def ::project-id
  (s/and string? #(not (str/blank? %))))

;; Buffer name - Emacs buffer name (e.g., "*scratch*", "file.clj")
(s/def ::buffer-name
  (s/and string? #(not (str/blank? %))))

;; File path - absolute path to a file
(s/def ::file-path
  (s/and string?
         #(not (str/blank? %))
         #(str/starts-with? % "/")))

;; Clojure code string
(s/def ::code
  (s/and string? #(not (str/blank? %))))

;; Line number (1-indexed, positive)
(s/def ::line-number
  (s/and int? pos?))

;; Port number (1-65535)
(s/def ::port
  (s/and int? #(<= 1 % 65535)))

;; Timeout in milliseconds (positive)
(s/def ::timeout-ms
  (s/and int? pos?))

;; Duration categories for memory TTL
(s/def ::duration
  #{"ephemeral" "short" "medium" "long" "permanent"})

;; Memory entry types
(s/def ::memory-type
  #{"note" "snippet" "convention" "decision" "conversation"})

;; Tags (optional array of strings)
(s/def ::tag (s/and string? #(not (str/blank? %))))
(s/def ::tags (s/coll-of ::tag))

;; Limit for query results
(s/def ::limit (s/and int? pos?))

;; Scope filter for memory queries
(s/def ::scope
  (s/or :special #{"all" "global"}
        :specific string?))

;; Session name for CIDER sessions
(s/def ::session-name
  (s/and string? #(not (str/blank? %))))

;; Symbol name for CIDER lookups
(s/def ::symbol-name
  (s/and string? #(not (str/blank? %))))

;; =============================================================================
;; Memory Handler Input/Output Specs
;; =============================================================================

;; Memory entry (what gets stored/returned)
(s/def :memory-entry/id string?)
(s/def :memory-entry/type ::memory-type)
(s/def :memory-entry/content string?)
(s/def :memory-entry/tags ::tags)
(s/def :memory-entry/duration ::duration)
(s/def :memory-entry/created string?)
(s/def :memory-entry/expires (s/nilable string?))
(s/def :memory-entry/project-id ::project-id)
(s/def :memory-entry/access-count (s/and int? #(>= % 0)))
(s/def :memory-entry/helpful-count (s/and int? #(>= % 0)))
(s/def :memory-entry/unhelpful-count (s/and int? #(>= % 0)))

(s/def ::memory-entry
  (s/keys :req-un [:memory-entry/id :memory-entry/type :memory-entry/content]
          :opt-un [:memory-entry/tags :memory-entry/duration :memory-entry/created
                   :memory-entry/expires :memory-entry/project-id
                   :memory-entry/access-count :memory-entry/helpful-count
                   :memory-entry/unhelpful-count]))

;; Memory query result (array of entries)
(s/def ::memory-entries (s/coll-of ::memory-entry))

;; Memory add input
(s/def ::memory-add-input
  (s/keys :req-un [:memory-entry/type :memory-entry/content]
          :opt-un [::tags ::duration]))

;; Memory query input
(s/def ::memory-query-input
  (s/keys :req-un [:memory-entry/type]
          :opt-un [::tags ::limit ::duration ::scope]))

;; Memory get-full input
(s/def ::memory-get-full-input
  (s/keys :req-un [:memory-entry/id]))

;; Semantic search input
(s/def :semantic-search/query string?)
(s/def ::semantic-search-input
  (s/keys :req-un [:semantic-search/query]
          :opt-un [::limit :memory-entry/type]))

;; Semantic search result item
(s/def :search-result/id string?)
(s/def :search-result/type ::memory-type)
(s/def :search-result/distance number?)
(s/def :search-result/preview string?)
(s/def ::search-result-item
  (s/keys :req-un [:search-result/id :search-result/distance]
          :opt-un [:search-result/type :search-result/preview ::tags]))

;; Duration set input
(s/def ::duration-set-input
  (s/keys :req-un [:memory-entry/id ::duration]))

;; Feedback input
(s/def :feedback/feedback #{"helpful" "unhelpful"})
(s/def ::feedback-input
  (s/keys :req-un [:memory-entry/id :feedback/feedback]))

;; Helpfulness ratio output
(s/def :helpfulness/ratio (s/nilable number?))
(s/def :helpfulness/helpful (s/and int? #(>= % 0)))
(s/def :helpfulness/unhelpful (s/and int? #(>= % 0)))
(s/def ::helpfulness-result
  (s/keys :req-un [:helpfulness/ratio :helpfulness/helpful :helpfulness/unhelpful]))

;; =============================================================================
;; Buffer Handler Input/Output Specs
;; =============================================================================

;; Get buffer content input
(s/def ::buffer-content-input
  (s/keys :req-un [::buffer-name]))

;; Buffer info output
(s/def :buffer-info/name string?)
(s/def :buffer-info/size int?)
(s/def :buffer-info/lines int?)
(s/def :buffer-info/mode string?)
(s/def :buffer-info/modified boolean?)
(s/def :buffer-info/file (s/nilable string?))
(s/def :buffer-info/point int?)
(s/def :buffer-info/point-max int?)
(s/def ::buffer-info
  (s/keys :req-un [:buffer-info/name :buffer-info/size :buffer-info/lines
                   :buffer-info/mode :buffer-info/modified]
          :opt-un [:buffer-info/file :buffer-info/point :buffer-info/point-max]))

;; Watch buffer input
(s/def :watch-buffer/lines (s/and int? pos?))
(s/def ::watch-buffer-input
  (s/keys :req-un [::buffer-name]
          :opt-un [:watch-buffer/lines]))

;; Goto line input
(s/def ::goto-line-input
  (s/keys :req-un [::line-number]))

;; Find file input
(s/def ::find-file-input
  (s/keys :req-un [::file-path]))

;; Insert text input
(s/def :insert-text/text string?)
(s/def ::insert-text-input
  (s/keys :req-un [:insert-text/text]))

;; Notify input
(s/def :notify/message string?)
(s/def :notify/type #{"info" "warning" "error"})
(s/def ::notify-input
  (s/keys :req-un [:notify/message]
          :opt-un [:notify/type]))

;; Eval elisp input
(s/def ::eval-elisp-input
  (s/keys :req-un [::code]))

;; Workflow input
(s/def :workflow/name string?)
(s/def :workflow/args map?)
(s/def ::workflow-input
  (s/keys :req-un [:workflow/name]
          :opt-un [:workflow/args]))

;; =============================================================================
;; CIDER Handler Input/Output Specs
;; =============================================================================

;; CIDER status output
(s/def :cider-status/connected boolean?)
(s/def :cider-status/repl-buffer (s/nilable string?))
(s/def :cider-status/namespace (s/nilable string?))
(s/def :cider-status/repl-type (s/nilable string?))
(s/def ::cider-status
  (s/keys :req-un [:cider-status/connected]
          :opt-un [:cider-status/repl-buffer :cider-status/namespace
                   :cider-status/repl-type]))

;; CIDER eval input
(s/def ::cider-eval-input
  (s/keys :req-un [::code]))

;; CIDER session spawn input
(s/def :cider-session/name string?)
(s/def :cider-session/project-dir (s/nilable string?))
(s/def :cider-session/agent-id (s/nilable string?))
(s/def ::cider-spawn-session-input
  (s/keys :req-un [:cider-session/name]
          :opt-un [:cider-session/project-dir :cider-session/agent-id]))

;; CIDER session eval input
(s/def ::cider-session-eval-input
  (s/keys :req-un [::session-name ::code]))

;; CIDER session kill input
(s/def ::cider-session-kill-input
  (s/keys :req-un [::session-name]))

;; CIDER doc input
(s/def ::cider-doc-input
  (s/keys :req-un [::symbol-name]))

;; CIDER apropos input
(s/def :cider-apropos/pattern string?)
(s/def :cider-apropos/search-docs boolean?)
(s/def ::cider-apropos-input
  (s/keys :req-un [:cider-apropos/pattern]
          :opt-un [:cider-apropos/search-docs]))

;; CIDER info output
(s/def :cider-info/ns (s/nilable string?))
(s/def :cider-info/name string?)
(s/def :cider-info/arglists (s/nilable (s/coll-of string?)))
(s/def :cider-info/doc (s/nilable string?))
(s/def :cider-info/file (s/nilable string?))
(s/def :cider-info/line (s/nilable int?))
(s/def :cider-info/spec (s/nilable any?))
(s/def ::cider-info
  (s/keys :req-un [:cider-info/name]
          :opt-un [:cider-info/ns :cider-info/arglists :cider-info/doc
                   :cider-info/file :cider-info/line :cider-info/spec]))

;; CIDER complete input
(s/def :cider-complete/prefix string?)
(s/def ::cider-complete-input
  (s/keys :req-un [:cider-complete/prefix]))

;; =============================================================================
;; Function Specs (fdef)
;; =============================================================================

;; Note: Using qualified keywords for handler functions to avoid collisions
;; Handlers are in namespaces hive-mcp.tools.{memory,buffer,cider}

;; --- Memory Handlers ---

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-add
  :args (s/cat :params ::memory-add-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-query
  :args (s/cat :params ::memory-query-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-query-metadata
  :args (s/cat :params ::memory-query-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-get-full
  :args (s/cat :params ::memory-get-full-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-search-semantic
  :args (s/cat :params ::semantic-search-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-set-duration
  :args (s/cat :params ::duration-set-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-promote
  :args (s/cat :params (s/keys :req-un [:memory-entry/id]))
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-demote
  :args (s/cat :params (s/keys :req-un [:memory-entry/id]))
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-cleanup-expired
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-expiring-soon
  :args (s/cat :params (s/keys :opt-un [::days]))
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-log-access
  :args (s/cat :params (s/keys :req-un [:memory-entry/id]))
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-feedback
  :args (s/cat :params ::feedback-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.memory/handle-mcp-memory-helpfulness-ratio
  :args (s/cat :params (s/keys :req-un [:memory-entry/id]))
  :ret ::handler-result)

;; --- Buffer Handlers ---

(s/fdef hive-mcp.tools.buffer/handle-eval-elisp
  :args (s/cat :params ::eval-elisp-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-list-buffers
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-get-buffer-content
  :args (s/cat :params ::buffer-content-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-current-buffer
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-switch-to-buffer
  :args (s/cat :params (s/keys :req-un [::buffer-name]))
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-find-file
  :args (s/cat :params ::find-file-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-save-buffer
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-goto-line
  :args (s/cat :params ::goto-line-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-insert-text
  :args (s/cat :params ::insert-text-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-project-root
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-recent-files
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-emacs-status
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-mcp-get-context
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-mcp-capabilities
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-mcp-notify
  :args (s/cat :params ::notify-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-mcp-list-workflows
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-mcp-run-workflow
  :args (s/cat :params ::workflow-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-mcp-watch-buffer
  :args (s/cat :params ::watch-buffer-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-mcp-list-special-buffers
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.buffer/handle-mcp-buffer-info
  :args (s/cat :params (s/keys :req-un [::buffer-name]))
  :ret ::handler-result)

;; --- CIDER Handlers ---

(s/fdef hive-mcp.tools.cider/handle-cider-status
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.cider/handle-cider-eval-silent
  :args (s/cat :params ::cider-eval-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.cider/handle-cider-eval-explicit
  :args (s/cat :params ::cider-eval-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.cider/handle-cider-spawn-session
  :args (s/cat :params ::cider-spawn-session-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.cider/handle-cider-list-sessions
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.cider/handle-cider-eval-session
  :args (s/cat :params ::cider-session-eval-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.cider/handle-cider-kill-session
  :args (s/cat :params ::cider-session-kill-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.cider/handle-cider-kill-all-sessions
  :args (s/cat :params any?)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.cider/handle-cider-doc
  :args (s/cat :params ::cider-doc-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.cider/handle-cider-apropos
  :args (s/cat :params ::cider-apropos-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.cider/handle-cider-info
  :args (s/cat :params ::cider-doc-input)
  :ret ::handler-result)

(s/fdef hive-mcp.tools.cider/handle-cider-complete
  :args (s/cat :params ::cider-complete-input)
  :ret ::handler-result)

;; =============================================================================
;; Validation Helpers
;; =============================================================================

(defn valid-handler-result?
  "Check if a map is a valid handler result."
  [m]
  (s/valid? ::handler-result m))

(defn explain-handler-result
  "Explain why a map is not a valid handler result."
  [m]
  (s/explain-data ::handler-result m))

(defn valid-memory-entry?
  "Check if a map is a valid memory entry."
  [m]
  (s/valid? ::memory-entry m))

(defn explain-memory-entry
  "Explain why a map is not a valid memory entry."
  [m]
  (s/explain-data ::memory-entry m))

(defn valid-buffer-info?
  "Check if a map is a valid buffer info."
  [m]
  (s/valid? ::buffer-info m))

(defn explain-buffer-info
  "Explain why a map is not a valid buffer info."
  [m]
  (s/explain-data ::buffer-info m))

(defn valid-cider-status?
  "Check if a map is a valid CIDER status."
  [m]
  (s/valid? ::cider-status m))

(defn explain-cider-status
  "Explain why a map is not a valid CIDER status."
  [m]
  (s/explain-data ::cider-status m))
