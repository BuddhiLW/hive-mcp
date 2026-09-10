(ns hive-mcp.extensions.registry
  "Opaque extension registry for optional capabilities.

   Provides a thread-safe registry where external projects can register
   implementations at startup. Consumers look up extensions by opaque
   keyword keys without knowing which project provides them.

   Usage:
     ;; Registration (at startup, by extension project)
     (register! :gs/struct-cmp my-cmp-fn)

     ;; Consumption (anywhere in hive-mcp)
     (if-let [f (get-extension :gs/struct-cmp)]
       (f node-a node-b)
       default-value)

   Thread safety: All operations are atomic via atom + swap!.
   Idempotent: Re-registering the same key replaces silently."
  (:require [hive-mcp.protocols.registry :as reg]
            [malli.core :as m]
            [hive-addon.registry.commands :as addon-cmds]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Registry State
;; =============================================================================

(defonce ^:private ext-slot (reg/multi-slot {}))

(defonce ^:private tool-slot (reg/multi-slot {}))

(defonce ^:private schema-registry
  (atom {}))

;; Composite tool command contributions.
;; Shape: {"analysis" {"lint" {:handler fn :params {...} :description "..." :addon :kondo} ...}}
;; The store itself lives in hive-addon.registry.commands, NOT here.
;; An addon depends on hive-di / hive-contracts / hive-addon, never on hive-mcp
;; core, so hive-addon owns the seam and this namespace is only a thin facade
;; over it. A second atom on this side made contribute! and get-commands two
;; implementations of one concept: an addon migrated to the correct seam would
;; mount, report :status :ok, and silently vanish from the tool surface,
;; because nothing here ever read hive-addon's store.
(defn- all-contributions
  "The whole contribution tree, in the shape this facade has always returned:
   {tool-name {command-name spec}}. Rebuilt from hive-addon on each call rather
   than cached, so there is exactly one source of truth."
  []
  (into {}
        (map (juxt identity addon-cmds/get-commands))
        (addon-cmds/contributed-tool-names)))

;; Listeners notified after a command contribution or retraction, so the
;; advertised surface can follow a contribution made AFTER boot.
;; Shape: {listener-id (fn [{:type :contribute|:retract :tool-name .. :addon-id ..}])}
(defonce ^:private contribution-listeners (atom {}))

;; =============================================================================
;; Public API
;; =============================================================================

(defn register!
  "Register an extension function under an opaque keyword key.
   Thread-safe, idempotent. Re-registration replaces the previous value."
  [k f]
  {:pre [(keyword? k) (ifn? f)]}
  (reg/reg-put! ext-slot k f)
  k)

(defn register-many!
  "Register multiple extensions at once from a map of {keyword fn}.
   Thread-safe, atomic."
  [m]
  {:pre [(map? m)]}
  (reg/reg-merge! ext-slot m))

(defn get-extension
  "Look up a registered extension by keyword key.
   Returns the function if registered, or default (nil if not provided)."
  ([k]
   (get (reg/reg-snapshot ext-slot) k))
  ([k default]
   (get (reg/reg-snapshot ext-slot) k default)))

(defn extension-available?
  "Check if an extension is registered under the given key."
  [k]
  (contains? (reg/reg-snapshot ext-slot) k))

(defn registered-keys
  "Return the set of all registered extension keys."
  []
  (set (keys (reg/reg-snapshot ext-slot))))

(defn deregister!
  "Remove an extension registration. Returns the key."
  [k]
  (reg/reg-remove! ext-slot k)
  k)

(defn clear-all!
  "Remove all registrations (fn + schema + tool + contributions). Intended for testing only."
  []
  (reg/reg-clear! ext-slot)
  (reset! schema-registry {})
  (reg/reg-clear! tool-slot)
  (addon-cmds/clear!)
  nil)

;; =============================================================================
;; Schema Extension Registry
;; =============================================================================

(defn register-schema!
  "Register schema properties for a tool. Merges with existing.
   Properties is a map of {\"param_name\" {:type ... :description ...}}.
   Thread-safe, idempotent."
  [tool-name properties]
  {:pre [(string? tool-name) (map? properties)]}
  (swap! schema-registry update tool-name merge properties)
  tool-name)

(defn get-schema-extensions
  "Get merged schema property extensions for a tool. Returns map or nil."
  [tool-name]
  (get @schema-registry tool-name))

(defn clear-all-schemas!
  "Remove all schema registrations. Intended for testing only."
  []
  (reset! schema-registry {})
  nil)

;; =============================================================================
;; Tool Registry (dynamic MCP tool definitions)
;; =============================================================================

(defn register-tool!
  "Register a full MCP tool definition for dynamic discovery.
   Tool-def must have :name (string) and :handler (ifn?).
   Thread-safe, idempotent. Last-write-wins by tool name."
  [tool-def]
  {:pre [(string? (:name tool-def)) (ifn? (:handler tool-def))]}
  (reg/reg-put! tool-slot (:name tool-def) tool-def)
  (:name tool-def))

(defn get-registered-tools
  "Return seq of all dynamically registered tool definitions."
  []
  (vals (reg/reg-snapshot tool-slot)))

(defn deregister-tool!
  "Remove a dynamically registered tool by name. Returns the name."
  [tool-name]
  (reg/reg-remove! tool-slot tool-name)
  tool-name)

(defn clear-all-tools!
  "Remove all tool registrations. Intended for testing only."
  []
  (reg/reg-clear! tool-slot)
  nil)

;; =============================================================================
;; Composite Tool Command Contributions
;; =============================================================================

(defn add-contribution-listener!
  "Register `f` to be called with {:type :contribute|:retract :tool-name s
   :addon-id a :commands [..]} after every contribute-commands! /
   retract-commands! / retract-all-by-addon!. Idempotent by id. A listener that
   throws is ignored — it must never break a contribution."
  [listener-id f]
  {:pre [(keyword? listener-id) (ifn? f)]}
  (swap! contribution-listeners assoc listener-id f)
  listener-id)

(defn remove-contribution-listener!
  "Drop a contribution listener. Returns the id."
  [listener-id]
  (swap! contribution-listeners dissoc listener-id)
  listener-id)

(defn- notify-contribution!
  [event]
  (doseq [[_ f] @contribution-listeners]
    (try (f event) (catch Throwable _ nil))))

;; Have we registered our notifier with hive-addon's own listener seam?
;;
;; A `delay`, not a load-time side effect: the probe runs at the first
;; contribution, so a hive-addon that arrives later is still picked up and
;; merely LOADING this namespace mutates nothing.
;;
;; Soft-resolved because hive-mcp pins hive-addon at a version that may predate
;; the seam. When it is absent this is false and the facade notifies directly,
;; exactly as it always did. When it is present, hive-addon notifies instead,
;; and it does so for contributions made by ANY caller, which is the point: an
;; addon that stops going through this facade and calls the registry directly
;; still reaches the advertised tool surface.
;;
;; notify-contribution! already guards every listener with its own try/catch, so
;; it satisfies hive-addon's contract that a listener must not throw (that
;; registry is in the portable three-host stratum and cannot catch for us).
(defonce ^:private seam-registered?
  (delay
    (boolean
     (when-let [add! (try (requiring-resolve 'hive-addon.registry.commands/add-listener!)
                          (catch Throwable _ nil))]
       (try (add! ::host-surface notify-contribution!) true
            (catch Throwable _ false))))))

(defn- notify-once!
  "Notify the host's contribution listeners exactly once.

   Two notifiers now exist for one event, and firing both would deliver every
   contribution twice: hive-addon's seam (which we register with) and this
   facade's direct call. Whichever is in play, a listener sees the event once."
  [event]
  (when-not @seam-registered?
    (notify-contribution! event)))

(defn contribute-commands!
  "Register commands that compose into a named composite tool.
   tool-name: \"analysis\", addon-id: :kondo
   commands: {\"lint\" {:handler fn :params {\"path\" {...}} :description \"...\"}}
   Notifies the contribution listeners afterwards.

   The contribution itself lands in hive-addon.registry.commands. This name
   stays public so the addons that soft-resolve it keep working while they
   migrate to the hive-addon seam one at a time; the listener notification is
   what this host adds on top, and hive-addon does not own it."
  [tool-name addon-id commands]
  (addon-cmds/contribute! tool-name addon-id commands)
  (notify-once! {:type :contribute :tool-name tool-name :addon-id addon-id
                         :commands (mapv name (keys commands))})
  (all-contributions))

(defn retract-commands!
  "Remove all commands contributed by an addon from a tool. Notifies the
   contribution listeners afterwards."
  [tool-name addon-id]
  (addon-cmds/retract! tool-name addon-id)
  (notify-once! {:type :retract :tool-name tool-name :addon-id addon-id})
  (all-contributions))

(defn retract-all-by-addon!
  "Remove all contributions from an addon across all tools (for shutdown).
   Notifies the contribution listeners once per tool the addon had touched."
  [addon-id]
  ;; the touched set must be read BEFORE the retraction, or there is nothing
  ;; left to attribute the notifications to
  (let [touched (into [] (keep (fn [[tn cmds]]
                                 (when (some #(= addon-id (:addon (val %))) cmds) tn)))
                      (all-contributions))]
    (addon-cmds/retract-all! addon-id)
    (doseq [tn touched]
      (notify-once! {:type :retract :tool-name tn :addon-id addon-id}))
    (all-contributions)))

(defn get-contributed-commands
  "Get contributed commands for a composite tool name."
  [tool-name]
  (addon-cmds/get-commands tool-name))

(defn contributed-tool-names
  "Return vector of tool names that have command contributions."
  []
  (addon-cmds/contributed-tool-names))

(def ExtensionFn
  "Schema for a registered extension value: any invokable."
  [:fn ifn?])

(m/=> get-extension
      [:function
       [:=> [:cat :keyword] [:maybe ExtensionFn]]
       [:=> [:cat :keyword :any] :any]])