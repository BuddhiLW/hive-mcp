(ns hive-mcp.workflows.registry
  "EDN workflow registry — load FSM specs from resources/fsm/*.edn at boot.

   The registry manages the lifecycle of data-driven FSM workflows:
   1. Scan:     Read .edn spec files from resources/fsm/
   2. Register: Associate handler-maps with workflow names
   3. Compile:  Compile specs with handlers via (fsm/compile spec handler-map)
   4. Lookup:   Retrieve compiled FSMs by name for execution

   ## Design

   EDN specs use keyword handlers (e.g., :start, :smite) that are resolved
   to actual functions at compile time via the handler-map. This separates
   the workflow topology (data) from the implementation (code).

   ## Usage

   ```clojure
   (require '[hive-mcp.workflows.registry :as wf-reg])

   ;; At boot
   (wf-reg/init!)

   ;; At runtime
   (when-let [wf (wf-reg/get-workflow :forge-belt)]
     (fsm/run wf resources initial-state))

   ;; Dev hot-reload
   (wf-reg/reload!)
   ```

   SOLID-S: Registry only — no execution logic.
   SOLID-O: Open to new workflows via EDN files + handler registration.
   CLARITY-L: Pure data layer (EDN specs) + code layer (handler maps)."
  (:require [clojure.java.io :as io]
            [hive.events.fsm :as fsm]
            [sci.core :as sci]
            [taoensso.timbre :as log])
  (:import [java.io PushbackReader]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Registry Atom
;; =============================================================================

(defonce ^:private registry
  (atom {}))

;; =============================================================================
;; EDN Scanning
;; =============================================================================

(defn- edn-file->workflow-name
  "Derive workflow name keyword from EDN filename.
   e.g., \"forge-belt.edn\" -> :forge-belt"
  [^java.io.File f]
  (let [name (.getName f)]
    (keyword (subs name 0 (- (count name) 4)))))

(defn- read-edn-spec
  "Read an EDN spec file using Clojure reader (not edn/read-string).
   Clojure reader is needed because specs contain (fn [...] ...) forms
   in dispatch predicates. These are read as lists and later compiled
   by SCI in hive.events.fsm/compile."
  [^java.io.File f]
  (with-open [rdr (PushbackReader. (io/reader f))]
    (read rdr)))

(defn scan-fsm-specs
  "Scan resources/fsm/ for .edn spec files. Returns map of
   {workflow-name {:spec parsed-edn}}.

   Uses classpath resource lookup so it works both in dev and uberjar."
  []
  (let [fsm-dir (io/resource "fsm")]
    (if fsm-dir
      (let [dir (io/file fsm-dir)]
        (if (.isDirectory dir)
          (let [edn-files (->> (.listFiles dir)
                               (filter #(.endsWith (.getName ^java.io.File %) ".edn"))
                               (sort-by #(.getName ^java.io.File %)))]
            (reduce
             (fn [acc f]
               (try
                 (let [wf-name (edn-file->workflow-name f)
                       spec (read-edn-spec f)]
                   (log/info "Scanned FSM spec" {:workflow wf-name :file (.getName ^java.io.File f)})
                   (assoc acc wf-name {:spec spec}))
                 (catch Exception e
                   (log/error e "Failed to read FSM spec" {:file (.getName ^java.io.File f)})
                   acc)))
             {}
             edn-files))
          (do
            (log/warn "FSM resource path is not a directory" {:path (str fsm-dir)})
            {})))
      (do
        (log/warn "No fsm/ directory found on classpath")
        {}))))

;; =============================================================================
;; Handler Registration
;; =============================================================================

(defn register-handlers!
  "Associate a handler-map with a workflow name.

   The handler-map maps keyword handler IDs to actual functions:
     {:start   handle-start
      :smite   handle-smite
      :survey  handle-survey
      :spark   handle-spark
      :end     handle-end
      :halt    handle-halt
      :error   handle-error}

   Must be called after scan-fsm-specs has populated the registry.
   If the workflow name doesn't exist in the registry, logs a warning."
  [workflow-name handler-map]
  (if (get @registry workflow-name)
    (do
      (swap! registry assoc-in [workflow-name :handler-map] handler-map)
      (log/info "Registered handlers" {:workflow workflow-name
                                       :handlers (keys handler-map)}))
    (log/warn "Cannot register handlers — workflow not found in registry"
              {:workflow workflow-name
               :available (keys @registry)})))

;; =============================================================================
;; Compilation
;; =============================================================================

(defn- sci-eval-if-form
  "If v is a list (unevaluated fn form from EDN), compile it via SCI.
   Otherwise return v unchanged (already a fn or other value)."
  [sci-ctx v]
  (if (list? v)
    (sci/eval-form sci-ctx v)
    v))

(defn- compile-opts-fns
  "Compile (fn ...) forms in :opts that were read as list data from EDN.

   fsm/compile handles dispatch predicates via SCI, but NOT opts like
   :subscriptions handlers, :pre, and :post hooks. This function resolves
   those before passing to fsm/compile.

   Handles:
   - :opts :subscriptions {path {:handler (fn ...)}} → eval handler fns
   - :opts :pre (fn ...) → eval
   - :opts :post (fn ...) → eval"
  [spec]
  (let [sci-ctx (sci/init {})]
    (cond-> spec
      ;; Compile subscription handler fns
      (get-in spec [:opts :subscriptions])
      (update-in [:opts :subscriptions]
                 (fn [subs]
                   (reduce-kv
                    (fn [acc path sub]
                      (assoc acc path
                             (update sub :handler #(sci-eval-if-form sci-ctx %))))
                    {}
                    subs)))
      ;; Compile :pre hook
      (list? (get-in spec [:opts :pre]))
      (update-in [:opts :pre] #(sci-eval-if-form sci-ctx %))
      ;; Compile :post hook
      (list? (get-in spec [:opts :post]))
      (update-in [:opts :post] #(sci-eval-if-form sci-ctx %)))))

(defn- compile-workflow
  "Compile a single workflow entry that has both :spec and :handler-map.
   First compiles any (fn ...) forms in :opts via SCI, then delegates
   to fsm/compile for handler resolution and dispatch predicate compilation.
   Returns the entry with :compiled added, or unchanged if missing either."
  [{:keys [spec handler-map] :as entry}]
  (if (and spec handler-map)
    (try
      (let [resolved-spec (compile-opts-fns spec)
            compiled (fsm/compile resolved-spec handler-map)]
        (assoc entry :compiled compiled))
      (catch Exception e
        (log/error e "Failed to compile workflow" {:spec-keys (keys spec)})
        (assoc entry :compile-error (ex-message e))))
    entry))

(defn compile-registry!
  "Compile all workflows that have both a spec and handler-map.
   Workflows without handlers are left uncompiled (waiting for registration)."
  []
  (swap! registry
         (fn [reg]
           (reduce-kv
            (fn [acc wf-name entry]
              (if (and (:spec entry) (:handler-map entry))
                (let [compiled-entry (compile-workflow entry)]
                  (log/info "Compiled workflow" {:workflow wf-name
                                                 :success? (boolean (:compiled compiled-entry))})
                  (assoc acc wf-name compiled-entry))
                (do
                  (log/debug "Skipping compilation — missing handler-map" {:workflow wf-name})
                  (assoc acc wf-name entry))))
            {}
            reg)))
  :ok)

;; =============================================================================
;; Lookup
;; =============================================================================

(defn get-workflow
  "Look up a compiled FSM by workflow name. Returns the compiled FSM
   ready for (fsm/run compiled resources initial-state), or nil."
  [workflow-name]
  (get-in @registry [workflow-name :compiled]))

(defn get-spec
  "Look up the raw EDN spec for a workflow. Useful for inspection/debugging."
  [workflow-name]
  (get-in @registry [workflow-name :spec]))

(defn list-workflows
  "List all registered workflows with their status.
   Returns map of {name {:has-spec? bool :has-handlers? bool :compiled? bool}}."
  []
  (reduce-kv
   (fn [acc wf-name {:keys [spec handler-map compiled compile-error]}]
     (assoc acc wf-name {:has-spec?     (boolean spec)
                         :has-handlers? (boolean handler-map)
                         :compiled?     (boolean compiled)
                         :error         compile-error}))
   {}
   @registry))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(defn reload!
  "Re-scan specs from disk and re-compile all workflows with handlers.
   Preserves existing handler-map registrations.
   Dev hot-reload friendly."
  []
  (let [old-handlers (reduce-kv
                      (fn [acc wf-name {:keys [handler-map]}]
                        (if handler-map
                          (assoc acc wf-name handler-map)
                          acc))
                      {}
                      @registry)]
    ;; Reset with fresh scan
    (reset! registry (scan-fsm-specs))
    ;; Re-associate preserved handlers
    (doseq [[wf-name hmap] old-handlers]
      (when (get @registry wf-name)
        (swap! registry assoc-in [wf-name :handler-map] hmap)))
    ;; Recompile
    (compile-registry!)
    (log/info "Registry reloaded" {:workflows (keys @registry)
                                   :preserved-handlers (keys old-handlers)})))

(defn reset-registry!
  "Clear the entire registry. Mainly for testing."
  []
  (reset! registry {})
  :ok)

;; =============================================================================
;; Forge Belt Registration (First Entry)
;; =============================================================================

(defn register-forge-belt!
  "Register the forge-belt workflow handlers.
   Requires hive-mcp.workflows.forge-belt namespace to be loaded.

   Maps EDN keyword handlers to forge-belt implementation fns:
     :start  → handle-start
     :smite  → handle-smite
     :survey → handle-survey
     :spark  → handle-spark
     :end    → handle-end
     :halt   → handle-halt
     :error  → handle-error"
  []
  (require 'hive-mcp.workflows.forge-belt)
  (let [belt-ns (find-ns 'hive-mcp.workflows.forge-belt)]
    (register-handlers!
     :forge-belt
     {:start  (ns-resolve belt-ns 'handle-start)
      :smite  (ns-resolve belt-ns 'handle-smite)
      :survey (ns-resolve belt-ns 'handle-survey)
      :spark  (ns-resolve belt-ns 'handle-spark)
      :end    (ns-resolve belt-ns 'handle-end)
      :halt   (ns-resolve belt-ns 'handle-halt)
      :error  (ns-resolve belt-ns 'handle-error)})))

;; =============================================================================
;; Wrap Session Registration
;; =============================================================================

(defn register-wrap-session!
  "Register the wrap-session workflow handlers.
   Requires hive-mcp.workflows.wrap-session namespace to be loaded.

   Maps EDN keyword handlers to wrap-session implementation fns:
     :start       → handle-start
     :gather      → handle-gather
     :crystallize → handle-crystallize
     :kg-edges    → handle-kg-edges
     :notify      → handle-notify
     :evict       → handle-evict
     :end         → handle-end
     :error       → handle-error"
  []
  (require 'hive-mcp.workflows.wrap-session)
  (let [ns' (find-ns 'hive-mcp.workflows.wrap-session)]
    (register-handlers!
     :wrap-session
     {:start       (ns-resolve ns' 'handle-start)
      :gather      (ns-resolve ns' 'handle-gather)
      :crystallize (ns-resolve ns' 'handle-crystallize)
      :kg-edges    (ns-resolve ns' 'handle-kg-edges)
      :notify      (ns-resolve ns' 'handle-notify)
      :evict       (ns-resolve ns' 'handle-evict)
      :end         (ns-resolve ns' 'handle-end)
      :error       (ns-resolve ns' 'handle-error)})))

;; =============================================================================
;; Complete Session Registration
;; =============================================================================

(defn register-complete-session!
  "Register the complete-session workflow handlers.
   Requires hive-mcp.workflows.complete-session namespace to be loaded.

   Maps EDN keyword handlers to complete-session implementation fns:
     :start       → handle-start
     :commit      → handle-commit
     :kanban      → handle-kanban
     :crystallize → handle-crystallize
     :shout       → handle-shout
     :plan-check  → handle-plan-check
     :evict       → handle-evict
     :end         → handle-end
     :error       → handle-error"
  []
  (require 'hive-mcp.workflows.complete-session)
  (let [ns' (find-ns 'hive-mcp.workflows.complete-session)]
    (register-handlers!
     :complete-session
     {:start       (ns-resolve ns' 'handle-start)
      :commit      (ns-resolve ns' 'handle-commit)
      :kanban      (ns-resolve ns' 'handle-kanban)
      :crystallize (ns-resolve ns' 'handle-crystallize)
      :shout       (ns-resolve ns' 'handle-shout)
      :plan-check  (ns-resolve ns' 'handle-plan-check)
      :evict       (ns-resolve ns' 'handle-evict)
      :end         (ns-resolve ns' 'handle-end)
      :error       (ns-resolve ns' 'handle-error)})))

;; =============================================================================
;; Catchup Session Registration
;; =============================================================================

(defn register-catchup!
  "Register the catchup-session workflow handlers.
   Requires hive-mcp.workflows.catchup-session namespace to be loaded.

   Maps EDN keyword handlers to catchup-session implementation fns:
     :start          -> handle-start
     :scope-resolve  -> handle-scope-resolve
     :query-memory   -> handle-query-memory
     :gather-context -> handle-gather-context
     :enrich-kg      -> handle-enrich-kg
     :maintenance    -> handle-maintenance
     :deliver        -> handle-deliver
     :end            -> handle-end
     :error          -> handle-error"
  []
  (require 'hive-mcp.workflows.catchup-session)
  (let [ns' (find-ns 'hive-mcp.workflows.catchup-session)]
    (register-handlers!
     :catchup
     {:start          (ns-resolve ns' 'handle-start)
      :scope-resolve  (ns-resolve ns' 'handle-scope-resolve)
      :query-memory   (ns-resolve ns' 'handle-query-memory)
      :gather-context (ns-resolve ns' 'handle-gather-context)
      :enrich-kg      (ns-resolve ns' 'handle-enrich-kg)
      :maintenance    (ns-resolve ns' 'handle-maintenance)
      :deliver        (ns-resolve ns' 'handle-deliver)
      :end            (ns-resolve ns' 'handle-end)
      :error          (ns-resolve ns' 'handle-error)})))

;; =============================================================================
;; Init (Boot Entry Point)
;; =============================================================================

(defn init!
  "Initialize the workflow registry at server boot.

   1. Scans resources/fsm/ for .edn spec files
   2. Registers all built-in workflow handlers
   3. Compiles all workflows with registered handlers

   Call this once during server startup."
  []
  (log/info "Initializing workflow registry...")
  (reset! registry (scan-fsm-specs))
  (register-forge-belt!)
  (register-wrap-session!)
  (register-complete-session!)
  (register-catchup!)
  (compile-registry!)
  (let [workflows (list-workflows)]
    (log/info "Workflow registry initialized" {:workflows workflows})
    workflows))
