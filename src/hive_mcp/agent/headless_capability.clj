(ns hive-mcp.agent.headless-capability
  "HeadlessCapability ADT — closed algebraic type for headless backend capabilities.

   Built on hive-dsl.adt/defadt. Provides type-safe capability enumeration
   with compile-time exhaustiveness checking via adt-case.

   Variants:
     :cap/hooks          — Pre/post tool-use hook injection
     :cap/interrupts     — Graceful query interruption
     :cap/subagents      — Native subagent definitions
     :cap/checkpointing  — Session checkpoint/rewind
     :cap/mcp-tools      — MCP tool server integration
     :cap/streaming      — Incremental streaming output
     :cap/multi-turn     — Persistent multi-turn sessions
     :cap/budget-guard   — Per-session cost budgeting
     :cap/saa            — SAA lifecycle phases

   Usage:
     (require '[hive-mcp.agent.headless-capability :as hc])

     ;; Construct
     (hc/headless-capability :cap/hooks)
     ;; => {:adt/type :HeadlessCapability, :adt/variant :cap/hooks}

     ;; Coerce from keyword (nil if invalid)
     (hc/->headless-capability :cap/streaming)

     ;; Predicate
     (hc/headless-capability? x)

     ;; Exhaustive dispatch
     (adt-case HeadlessCapability cap
       :cap/hooks         :hookable
       :cap/interrupts    :interruptable
       :cap/subagents     :hosts-subagents
       :cap/checkpointing :checkpointable
       :cap/mcp-tools     :has-mcp
       :cap/streaming     :streams
       :cap/multi-turn    :multi-turn
       :cap/budget-guard  :budgeted
       :cap/saa           :saa-enabled)"
  (:require [hive-dsl.adt :refer [defadt adt-variant]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; ADT Definition
;; =============================================================================

(defadt HeadlessCapability
  "Capabilities a headless backend may declare.
   :cap/hooks          — Pre/post tool-use hook injection
   :cap/interrupts     — Graceful query interruption
   :cap/subagents      — Native subagent definitions
   :cap/checkpointing  — Session checkpoint/rewind
   :cap/mcp-tools      — MCP tool server integration
   :cap/streaming      — Incremental streaming output
   :cap/multi-turn     — Persistent multi-turn sessions
   :cap/budget-guard   — Per-session cost budgeting
   :cap/saa            — SAA lifecycle phases"
  :cap/hooks
  :cap/interrupts
  :cap/subagents
  :cap/checkpointing
  :cap/mcp-tools
  :cap/streaming
  :cap/multi-turn
  :cap/budget-guard
  :cap/saa)

;; =============================================================================
;; Keyword Coercion
;; =============================================================================

(defn from-keyword
  "Coerce a keyword or string to a HeadlessCapability ADT value.
   Returns nil if the input is not a valid capability.

   (from-keyword :cap/hooks) => {:adt/type :HeadlessCapability, :adt/variant :cap/hooks}
   (from-keyword :bogus)     => nil"
  [k]
  (let [kw (cond
             (keyword? k) k
             (string? k) (keyword k)
             :else nil)]
    (when kw (->headless-capability kw))))

(defn to-keyword
  "Extract the variant keyword from a HeadlessCapability ADT value.
   This is the inverse of from-keyword for round-trip serialization.

   (to-keyword (headless-capability :cap/hooks)) => :cap/hooks"
  [hc]
  (adt-variant hc))

;; =============================================================================
;; Variant Sets
;; =============================================================================

(def all-capabilities
  "Set of all HeadlessCapability variant keywords."
  (:variants HeadlessCapability))

(def sdk-capabilities
  "Set of capabilities typically provided by a full SDK backend (Claude Agent SDK)."
  #{:cap/hooks :cap/interrupts :cap/subagents :cap/checkpointing
    :cap/mcp-tools :cap/streaming :cap/multi-turn :cap/budget-guard :cap/saa})

(def basic-capabilities
  "Set of basic capabilities available to all headless backends."
  #{:cap/streaming :cap/multi-turn})

;; =============================================================================
;; Capability Protocol Mapping
;; =============================================================================

(def capability->protocol
  "Map from capability keywords to the ISP protocol that provides them.
   Used for `satisfies?` checks at runtime."
  {:cap/hooks         'hive-mcp.addons.headless-caps/IHookable
   :cap/checkpointing 'hive-mcp.addons.headless-caps/ICheckpointable
   :cap/subagents     'hive-mcp.addons.headless-caps/ISubagentHost
   :cap/budget-guard  'hive-mcp.addons.headless-caps/IBudgetGuardable})
