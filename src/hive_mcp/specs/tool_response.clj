(ns hive-mcp.specs.tool-response
  "Clojure specs for MCP tool response structures.

   Defines domain specs for:
   - ::content-item - Individual content block (text/image)
   - ::tool-response - Full tool response with content array
   - ::piggyback-message - Hivemind piggyback message format
   - ::hivemind-section - Complete hivemind section with delimiters

   Also provides s/fdef for key functions:
   - hive-mcp.server.routes/make-tool
   - hive-mcp.tools.core/get-hivemind-piggyback

   CLARITY: 'Inputs are guarded' - specs provide declarative validation
   DDD: Domain-specific validation at module boundaries"
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Content Item Specs (MCP Protocol)
;; =============================================================================

;; Content type enum
(s/def ::content-type #{"text" "image"})

;; Text content item
(s/def :content-item.text/type #{"text"})
(s/def :content-item.text/text string?)
(s/def ::text-content-item
  (s/keys :req-un [:content-item.text/type :content-item.text/text]))

;; Image content item
(s/def :content-item.image/type #{"image"})
(s/def :content-item.image/data string?)  ; base64 encoded
(s/def :content-item.image/mimeType string?)
(s/def ::image-content-item
  (s/keys :req-un [:content-item.image/type :content-item.image/data :content-item.image/mimeType]))

;; Content item (union of text/image)
(s/def ::content-item
  (s/or :text ::text-content-item
        :image ::image-content-item))

;; Content array (MCP requires vector)
(s/def ::content-array
  (s/coll-of ::content-item :kind vector?))

;; =============================================================================
;; Tool Response Specs
;; =============================================================================

;; Tool response - the final SDK-compatible response format
(s/def :tool-response/content ::content-array)
(s/def :tool-response/isError boolean?)

(s/def ::tool-response
  (s/keys :req-un [:tool-response/content]
          :opt-un [:tool-response/isError]))

;; =============================================================================
;; Piggyback Message Specs
;; =============================================================================

;; Abbreviated keys for token-efficient transport
(s/def ::a (s/and string? #(not (str/blank? %))))  ; agent-id
(s/def ::e (s/and string? #(not (str/blank? %))))  ; event-type
(s/def ::m string?)                                 ; message (can be empty)

;; Single piggyback message
(s/def ::piggyback-message
  (s/keys :req-un [::a ::e ::m]))

;; Collection of piggyback messages
(s/def ::piggyback-messages
  (s/nilable (s/coll-of ::piggyback-message :kind vector?)))

;; =============================================================================
;; Hivemind Section Specs
;; =============================================================================

;; The hivemind section is the full delimited block embedded in tool responses
;; Format:
;; ---HIVEMIND---
;; [{:a "agent-id" :e "event-type" :m "message"} ...]
;; ---/HIVEMIND---

(defn hivemind-section?
  "Check if string is a valid hivemind section with delimiters."
  [s]
  (and (string? s)
       (str/includes? s "---HIVEMIND---")
       (str/includes? s "---/HIVEMIND---")))

(s/def ::hivemind-section
  (s/and string? hivemind-section?))

;; Text content that may contain hivemind section
(s/def ::text-with-optional-hivemind
  (s/and string?
         (s/or :with-hivemind hivemind-section?
               :without-hivemind (complement hivemind-section?))))

;; =============================================================================
;; Tool Definition Specs (for make-tool input)
;; =============================================================================

(s/def :tool-def/name (s/and string? #(not (str/blank? %))))
(s/def :tool-def/description string?)
(s/def :tool-def/inputSchema map?)
(s/def :tool-def/handler fn?)

(s/def ::tool-def
  (s/keys :req-un [:tool-def/name :tool-def/description :tool-def/inputSchema :tool-def/handler]))

;; SDK tool (output of make-tool) - handler is wrapped
(s/def ::sdk-tool
  (s/keys :req-un [:tool-def/name :tool-def/description :tool-def/inputSchema :tool-def/handler]))

;; =============================================================================
;; Function Specs (fdef)
;; =============================================================================

;; Spec for make-tool
;; Input: tool definition with handler
;; Output: SDK-compatible tool with wrapped handler
(s/fdef hive-mcp.server.routes/make-tool
  :args (s/cat :tool-def ::tool-def)
  :ret ::sdk-tool)

;; Agent ID spec for piggyback
(s/def ::agent-id (s/and string? #(not (str/blank? %))))

;; Spec for get-hivemind-piggyback
;; Input: agent-id string
;; Output: nilable vector of piggyback messages
(s/fdef hive-mcp.tools.core/get-hivemind-piggyback
  :args (s/cat :agent-id ::agent-id)
  :ret ::piggyback-messages)

;; =============================================================================
;; Validation Helpers
;; =============================================================================

(defn valid-content-item?
  "Check if a map is a valid content item."
  [m]
  (s/valid? ::content-item m))

(defn explain-content-item
  "Explain why a map is not a valid content item."
  [m]
  (s/explain-data ::content-item m))

(defn valid-tool-response?
  "Check if a map is a valid tool response."
  [m]
  (s/valid? ::tool-response m))

(defn explain-tool-response
  "Explain why a map is not a valid tool response."
  [m]
  (s/explain-data ::tool-response m))

(defn valid-piggyback-message?
  "Check if a map is a valid piggyback message."
  [m]
  (s/valid? ::piggyback-message m))

(defn explain-piggyback-message
  "Explain why a map is not a valid piggyback message."
  [m]
  (s/explain-data ::piggyback-message m))

(defn valid-hivemind-section?
  "Check if a string is a valid hivemind section."
  [s]
  (s/valid? ::hivemind-section s))

(defn valid-tool-def?
  "Check if a map is a valid tool definition."
  [m]
  (s/valid? ::tool-def m))

(defn explain-tool-def
  "Explain why a map is not a valid tool definition."
  [m]
  (s/explain-data ::tool-def m))

;; =============================================================================
;; Generators for Testing
;; =============================================================================

(defn text-content-gen
  "Generator for text content items."
  []
  (s/gen ::text-content-item))

(defn piggyback-message-gen
  "Generator for piggyback messages."
  []
  (s/gen ::piggyback-message))
