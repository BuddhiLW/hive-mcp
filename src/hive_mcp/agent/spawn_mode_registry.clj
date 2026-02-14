(ns hive-mcp.agent.spawn-mode-registry
  "Single source of truth for ling spawn modes and their properties.

   All consumers derive from this registry — no scattered enums.
   Leaf namespace: zero hive-mcp dependencies (safe to require anywhere).

   Design principle: Knowledge-Layer-First / SST (Single Source of Truth).
   Adding a new spawn mode = adding one entry here. All downstream
   validation, MCP schemas, strategy dispatch, and slot limits derive automatically.

   Sum type: vterm | headless | agent-sdk | openrouter
   MCP surface: vterm | headless (headless is alias for agent-sdk since 0.12.0)")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Registry
;; =============================================================================

(def registry
  "Spawn mode registry. array-map preserves insertion order.

   Each mode has:
   - :description     Human-readable description
   - :requires-emacs? Whether this mode needs an Emacs daemon
   - :io-model        How I/O works (:buffer, :stdin-stdout, :api)
   - :slot-limit      Max concurrent instances (nil = unlimited)
   - :mcp?            Visible in MCP tool enums (default true)
   - :alias-of        If this mode is an alias for another (e.g. :headless -> :agent-sdk)
   - :capabilities    Set of capability keywords this mode supports"
  (array-map
   ;; === Emacs-bound modes ===
   :vterm       {:description   "Emacs vterm buffer — interactive, Emacs-bound"
                 :requires-emacs? true
                 :io-model      :buffer
                 :slot-limit    6
                 :mcp?          true
                 :alias-of      nil
                 :capabilities  #{:interactive :emacs-visible :dispatch :kill}}

   ;; === Subprocess modes ===
   :headless    {:description   "Alias for :agent-sdk (legacy name, auto-mapped since 0.12.0)"
                 :requires-emacs? false
                 :io-model      :stdin-stdout
                 :slot-limit    nil
                 :mcp?          true
                 :alias-of      :agent-sdk
                 :capabilities  #{:dispatch :kill :stdin :stdout-ring}}

   :agent-sdk   {:description   "Claude Agent SDK via subprocess (default for headless)"
                 :requires-emacs? false
                 :io-model      :stdin-stdout
                 :slot-limit    nil
                 :mcp?          false
                 :alias-of      nil
                 :capabilities  #{:dispatch :kill :stdin :stdout-ring :subagents}}

   ;; === API modes ===
   :openrouter  {:description   "Direct OpenRouter API calls (multi-model, no CLI)"
                 :requires-emacs? false
                 :io-model      :api
                 :slot-limit    nil
                 :mcp?          false
                 :alias-of      nil
                 :capabilities  #{:dispatch :kill :multi-model}}))

;; =============================================================================
;; Derived views (computed once at load time)
;; =============================================================================

(def all-modes
  "Set of all valid spawn mode keywords."
  (set (keys registry)))

(def all-mode-strings
  "Set of all valid spawn mode strings."
  (set (map name all-modes)))

(def mcp-modes
  "Ordered vector of mode strings visible in MCP tool enums."
  (->> registry
       (filter (fn [[_k v]] (:mcp? v)))
       (mapv (comp name key))))

(def internal-modes
  "Set of mode keywords that are internal (not MCP-visible)."
  (->> registry
       (remove (fn [[_k v]] (:mcp? v)))
       (map key)
       set))

(def mode->slot-limit
  "Map of mode keyword -> slot limit (nil = unlimited)."
  (into {} (map (fn [[k v]] [k (:slot-limit v)])) registry))

(def emacs-modes
  "Set of modes that require an Emacs daemon."
  (->> registry
       (filter (fn [[_k v]] (:requires-emacs? v)))
       (map key)
       set))

(def headless-modes
  "Set of modes that do NOT require Emacs (subprocess or API)."
  (->> registry
       (remove (fn [[_k v]] (:requires-emacs? v)))
       (map key)
       set))

(def alias-map
  "Map of alias keyword -> canonical keyword. Only entries with :alias-of."
  (->> registry
       (filter (fn [[_k v]] (:alias-of v)))
       (into {} (map (fn [[k v]] [k (:alias-of v)])))))

;; =============================================================================
;; Functions
;; =============================================================================

(defn valid-mode?
  "Check if mode (keyword or string) is a valid spawn mode."
  [m]
  (let [kw (if (keyword? m) m (keyword m))]
    (contains? all-modes kw)))

(defn resolve-alias
  "Resolve a mode to its canonical form. :headless -> :agent-sdk, others unchanged."
  [mode]
  (get alias-map mode mode))

(defn requires-emacs?
  "Does this spawn mode require an Emacs daemon?"
  [mode]
  (get-in registry [mode :requires-emacs?] false))

(defn slot-limit
  "Get the slot limit for a mode (nil = unlimited)."
  [mode]
  (get mode->slot-limit mode))

(defn io-model
  "Get the I/O model for a mode (:buffer, :stdin-stdout, :api)."
  [mode]
  (get-in registry [mode :io-model]))

(defn capabilities
  "Get the capability set for a mode."
  [mode]
  (get-in registry [mode :capabilities] #{}))

(defn has-capability?
  "Check if a mode has a specific capability."
  [mode capability]
  (contains? (capabilities mode) capability))

(defn mcp-enum
  "Generate MCP JSON schema enum for spawn_mode tool definitions."
  []
  mcp-modes)
