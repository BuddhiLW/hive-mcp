(ns hive-mcp.server.routes
  "MCP server route definitions and tool dispatch.

   CLARITY-L: Layers stay pure - routing logic separated from server lifecycle.
   SRP: Single responsibility for tool route construction and dispatch.

   This module handles:
   - Tool definition conversion to SDK format
   - Piggyback message embedding for hivemind communication
   - Server spec building with capability-based filtering
   - Hot-reload support for tools"
  (:require [hive-mcp.tools :as tools]
            [hive-mcp.docs :as docs]
            [taoensso.timbre :as log]
            [clojure.spec.alpha :as s]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later


;; =============================================================================
;; Specs for Tool Definitions
;; =============================================================================

(s/def ::tool-def
  (s/keys :req-un [::name ::description ::inputSchema ::handler]))

(s/def ::name string?)
(s/def ::description string?)
(s/def ::inputSchema map?)
(s/def ::handler fn?)

(s/def ::tool-response
  (s/keys :req-un [::content]))

(s/def ::content (s/coll-of map?))

;; =============================================================================
;; SRP Helpers for Content Normalization
;; =============================================================================

(defn normalize-content
  "Normalize handler result to content array.
   SRP: Single responsibility for content normalization.
   Handles: sequential (passthrough), map (wrap), other (text wrap)."
  [result]
  (cond
    (sequential? result) (vec result)
    (map? result) [result]
    :else [{:type "text" :text (str result)}]))

(defn find-last-text-idx
  "Find index of last text-type item in content (searching from end).
   SRP: Single responsibility for text item location.
   Returns nil if no text item found."
  [content]
  (some (fn [[idx item]]
          (when (= "text" (:type item)) idx))
        (map-indexed vector (reverse content))))

(defn wrap-piggyback
  "Append piggyback messages to content with HIVEMIND delimiters.
   SRP: Single responsibility for piggyback embedding.
   Appends to last text item if exists, otherwise adds new text item.

   Format:
   ---HIVEMIND---
   [{:a \"agent-id\" :e \"event-type\" :m \"message\"}]
   ---/HIVEMIND---"
  [content piggyback]
  (if (and piggyback (seq piggyback))
    (let [piggyback-text (str "\n\n---HIVEMIND---\n"
                              (pr-str piggyback)
                              "\n---/HIVEMIND---")]
      (if-let [last-text-idx (find-last-text-idx content)]
        (let [actual-idx (- (count content) 1 last-text-idx)
              last-item (nth content actual-idx)]
          (assoc content actual-idx
                 (update last-item :text str piggyback-text)))
        (conj content {:type "text" :text piggyback-text})))
    content))

;; =============================================================================
;; Agent ID Extraction
;; =============================================================================

(defn extract-agent-id
  "Extract agent-id from args map, handling both snake_case and kebab-case keys.

   DRY: Consolidates 4-way fallback pattern for JSON key variation handling.
   MCP tools may receive agent_id or agent-id in either keyword or string form.

   Returns default if no agent-id found in args."
  [args default]
  (or (:agent_id args)
      (:agent-id args)
      (get args "agent_id")
      (get args "agent-id")
      default))

;; =============================================================================
;; Composable Handler Wrappers (SRP: Each wrapper single responsibility)
;; =============================================================================

(defn wrap-handler-normalize
  "Wrap handler to normalize its result to content array.
   SRP: Single responsibility - content normalization only.

   (wrap-handler-normalize handler) returns handler that:
   - Calls original handler
   - Normalizes result via normalize-content"
  [handler]
  (fn [args]
    (normalize-content (handler args))))

(defn- get-piggyback-messages
  "Get hivemind piggyback messages for agent.
   SRP: Single responsibility - piggyback retrieval.
   Encapsulates dynamic require/resolve pattern."
  [agent-id]
  (require 'hive-mcp.tools.core)
  ((resolve 'hive-mcp.tools.core/get-hivemind-piggyback) agent-id))

(defn wrap-handler-piggyback
  "Wrap handler to attach hivemind piggyback messages.
   SRP: Single responsibility - piggyback embedding only.

   Expects handler to return normalized content (vector of items).
   Extracts agent-id from args, retrieves piggyback, embeds in content."
  [handler]
  (fn [args]
    (let [content (handler args)
          agent-id (extract-agent-id args "coordinator")
          piggyback (get-piggyback-messages agent-id)]
      (wrap-piggyback content piggyback))))

(defn wrap-handler-response
  "Wrap handler to build SDK response format.
   SRP: Single responsibility - response building only.

   Wraps handler result in {:content ...} map."
  [handler]
  (fn [args]
    {:content (handler args)}))

;; =============================================================================
;; Tool Definition Conversion
;; =============================================================================

(s/fdef make-tool
  :args (s/cat :tool-def ::tool-def)
  :ret ::tool-response)

(defn make-tool
  "Convert a tool definition with :handler to SDK format.
   Wraps handler to attach pending hivemind messages via content embedding.

   Uses composable handler wrappers (SRP: each wrapper single responsibility):
   - wrap-handler-normalize: converts result to content array
   - wrap-handler-piggyback: embeds hivemind messages with agent-id extraction
   - wrap-handler-response: builds {:content ...} response

   Composition via -> threading enables clear data flow:
   handler -> normalize -> piggyback -> response"
  [{:keys [name description inputSchema handler]}]
  {:name name
   :description description
   :inputSchema inputSchema
   :handler (-> handler
                wrap-handler-normalize
                wrap-handler-piggyback
                wrap-handler-response)})

;; =============================================================================
;; Server Spec Building
;; =============================================================================

(defn build-server-spec
  "Build MCP server spec with capability-based tool filtering.

   MUST be called AFTER init-embedding-provider! to get accurate Chroma status.

   Uses tools/get-filtered-tools for dynamic kanban tool switching:
   - Chroma available -> mcp_mem_kanban_* tools
   - Chroma unavailable -> org_kanban_native_* tools (fallback)"
  []
  (let [filtered-tools (tools/get-filtered-tools)]
    (log/info "Building server spec with" (count filtered-tools) "tools (capability-filtered)")
    {:name "hive-mcp"
     :version "0.1.0"
     :tools (mapv make-tool (concat filtered-tools docs/docs-tools))}))

;; DEPRECATED: Static spec kept for backward compatibility with tests
;; Prefer build-server-spec for capability-aware tool list
(def emacs-server-spec
  {:name "hive-mcp"
   :version "0.1.0"
   ;; hivemind/tools already included in tools/tools aggregation
   :tools (mapv make-tool (concat tools/tools docs/docs-tools))})

;; =============================================================================
;; Hot-Reload Support
;; =============================================================================

(defn refresh-tools!
  "Hot-reload all tools in the running server.
   CLARITY: Open for extension - allows runtime tool updates without restart.

   Uses capability-based filtering - re-checks Chroma availability
   to dynamically switch between mem-kanban and org-kanban-native tools.

   Parameters:
     server-context-atom - atom containing the server context with :tools key

   Returns:
     count of tools refreshed, or nil if no context"
  [server-context-atom]
  (when-let [context @server-context-atom]
    (let [tools-atom (:tools context)
          filtered-tools (tools/get-filtered-tools)
          new-tools (mapv make-tool (concat filtered-tools docs/docs-tools))]
      ;; Clear and re-register all tools
      (reset! tools-atom {})
      (doseq [tool new-tools]
        (swap! tools-atom assoc (:name tool) {:tool (dissoc tool :handler)
                                              :handler (:handler tool)}))
      (log/info "Hot-reloaded" (count new-tools) "tools (capability-filtered)")
      (count new-tools))))

(defn debug-tool-handler
  "Get info about a registered tool handler (for debugging).

   Parameters:
     server-context-atom - atom containing the server context
     tool-name - string name of the tool to inspect

   Returns:
     map with :name, :handler-class, :tool-keys or nil if not found"
  [server-context-atom tool-name]
  (when-let [context @server-context-atom]
    (let [tools-atom (:tools context)
          tool-entry (get @tools-atom tool-name)]
      (when tool-entry
        {:name tool-name
         :handler-class (str (type (:handler tool-entry)))
         :tool-keys (keys (:tool tool-entry))}))))

;; =============================================================================
;; Tool Registration for Agent Delegation
;; =============================================================================

(defn register-tools-for-delegation!
  "Register filtered tools for agent delegation.

   Delegates to hive-mcp.agent/register-tools! with capability-filtered tools.

   Returns:
     count of tools registered"
  []
  (require 'hive-mcp.agent)
  (let [register-tools! (resolve 'hive-mcp.agent/register-tools!)
        filtered-tools (tools/get-filtered-tools)]
    (register-tools! filtered-tools)
    (log/info "Registered" (count filtered-tools) "tools for agent delegation (capability-filtered)")
    (count filtered-tools)))
