(ns hive-mcp.extensions.loader
  "Extension loader — resolves and registers optional capabilities at startup.

   This is the single point where external namespace symbols are resolved.
   All other code uses the opaque registry keys from extensions.registry.

   Called once at system startup (init.clj).
   Graceful degradation: if resolution fails, no extensions are registered
   and all consumers fall back to their defaults."
  (:require [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Resolution Helpers
;; =============================================================================

(defn- try-resolve
  "Attempt to resolve a fully-qualified symbol.
   Returns the var if available, nil otherwise."
  [sym]
  (try
    (requiring-resolve sym)
    (catch Exception _
      nil)))

(defn- resolve-extension-group
  "Resolve a group of extensions from a single namespace.
   ns-sym: namespace symbol (e.g. 'some.ns)
   mappings: seq of [fn-name-str registry-key] pairs

   Returns map of {registry-key resolved-fn} for successfully resolved fns."
  [ns-sym mappings]
  (reduce
   (fn [acc [fn-name registry-key]]
     (if-let [f (try-resolve (symbol (str ns-sym) fn-name))]
       (assoc acc registry-key f)
       acc))
   {}
   mappings))

;; =============================================================================
;; Extension Manifests
;; =============================================================================
;;
;; Each manifest maps: [fn-name-in-source-ns  registry-key]
;; Registry keys are opaque — they describe the CAPABILITY, not the source.

;; Registry keys use SHORT OPAQUE prefixes. Do NOT use descriptive names
;; that reveal the capability or algorithm. Key format: :XX/short-verb
;;
;; Prefix legend (INTERNAL — do not document outside this file):
;;   :gs  = graph-structure     :ge  = graph-emergence
;;   :gx  = graph-xfer          :es  = enhance-score
;;   :ep  = enhance-plan        :ec  = enhance-context
;;   :cr  = context-recon       :al  = agent-loop
;;   :sk  = session-kernel      :dl  = data-lifecycle
;;   :dp  = dispatch            :ag  = agent-gateway
;;   :pm  = prompt-material     :uc  = unified-context
;;   :tc  = task-classifier     :dm  = dsl-macros
;;   :dv  = dsl-verbs           :dr  = dsl-response
;;   :dd  = decompose           :cb  = context-budget
;;   :bx  = multi-batch         :fb  = forge-belt
;;   :ch  = crystal-hooks       :cl  = crystal-recall
;;   :ck  = crystal-tools       :da  = done-archive

(def ^:private ext-group-a
  {:ns 'hive-knowledge.similarity
   :fns [["relation-signature"        :gs/sig]
         ["neighbor-overlap"          :gs/overlap]
         ["edge-type-similarity"      :gs/edge-cmp]
         ["structural-similarity"     :gs/struct-cmp]
         ["build-signature-index"     :gs/build-idx]
         ["find-structurally-similar" :gs/find-similar]
         ["pairwise-similarity"       :gs/pairwise]
         ["find-structural-roles"     :gs/find-roles]
         ["similarity-report"         :gs/report]
         ["classify-abstraction-level" :gs/classify]
         ["extract-knowledge-gaps"    :gs/detect-gaps]]})

(def ^:private ext-group-b
  {:ns 'hive-knowledge.emergence
   :fns [["detect-emergent-clusters" :ge/detect]
         ["create-synthetic-node!"   :ge/create-node!]
         ["detect-and-create!"       :ge/detect-create!]
         ["emergence-report"         :ge/report]]})

(def ^:private ext-group-c
  {:ns 'hive-knowledge.cross-pollination
   :fns [["cross-pollination-score"           :gx/score]
         ["cross-pollination-candidate?"      :gx/eligible?]
         ["cross-pollination-promotion-tiers" :gx/tiers]]})

(def ^:private ext-group-d
  {:ns 'hive-knowledge.scoring
   :fns [["score-observations" :es/score]]})

(def ^:private ext-group-e
  {:ns 'hive-knowledge.planning
   :fns [["generate-plan" :ep/generate]]})

(def ^:private ext-group-f
  {:ns 'hive-knowledge.context
   :fns [["enrich-task-context" :ec/enrich]]})

(def ^:private ext-group-g
  {:ns 'hive-knowledge.context-reconstruction
   :fns [["create-session-kg!"        :cr/create!]
         ["compress-turn!"            :cr/compress!]
         ["reconstruct-context"       :cr/reconstruct]
         ["build-compressed-messages" :cr/messages]
         ["promote-to-global!"        :cr/promote!]
         ["promotable-nodes"          :cr/promotable]
         ["session-stats"             :cr/stats]
         ["close-session!"            :cr/close!]]})

(def ^:private ext-group-h
  {:ns 'hive-knowledge.agentic-loop
   :fns [["completion-language?"    :al/completion?]
         ["select-relevant-tools"   :al/select-tools]
         ["compress-context"        :al/compress]
         ["goal-satisfaction?"      :al/goal?]
         ["should-terminate?"       :al/terminate?]
         ["evaluate-result"         :al/evaluate]
         ["select-next-tool"        :al/next-tool]
         ["run-agentic-loop"        :al/run]]})

(def ^:private ext-group-i
  {:ns 'hive-knowledge.session-kg
   :fns [["extract-key-facts"       :sk/facts]
         ["score-node-importance"    :sk/importance]
         ["compress-for-prompt"      :sk/compress]
         ["detect-superseded"        :sk/superseded]
         ["record-observation!"      :sk/record-obs!]
         ["record-reasoning!"        :sk/record-rsn!]
         ["reconstruct-context"      :sk/reconstruct]
         ["seed-from-global!"        :sk/seed!]]})

(def ^:private ext-group-j
  {:ns 'hive-knowledge.drone-loop
   :fns [["merge-session-to-global!" :dl/merge!]]})

(def ^:private ext-group-k
  {:ns 'hive-knowledge.dispatch
   :fns [["->graph-context" :dp/graph-ctx]]})

(def ^:private ext-group-l
  {:ns 'hive-agent.loop.core
   :fns [["run-agent" :ag/run]]})

(def ^:private ext-group-m
  {:ns 'hive-agent.context.core
   :fns [["build-context" :ag/context]]})

(def ^:private ext-group-n
  {:ns 'hive-agent.tools.definitions
   :fns [["tool-definitions" :ag/tools]]})

(def ^:private ext-group-o
  {:ns 'hive-agent.kg.priming
   :fns [["prime-context"   :pm/prime]
         ["resolve-seeds"   :pm/seeds]]})

;; --- Group P: Unified Context Enrichment ---
(def ^:private ext-group-p
  {:ns 'hive-knowledge.unified-context
   :fns [["resolve-seeds"    :uc/resolve-seeds]
         ["gather-context"   :uc/gather]
         ["available?"       :uc/available]
         ["enrich-context"   :uc/enrich]]})

;; =========================================================================
;; IP Migration Extension Groups (fallback manifests for groups M-X)
;; =========================================================================
;;
;; These mirror the IP migration groups in hive-knowledge/init.clj.
;; The init! path is preferred; these exist so the manifest fallback works.

;; --- Group Q: Task Classifier ---
(def ^:private ext-group-q
  {:ns 'hive-knowledge.task-classifier
   :fns [["score-file-count"  :tc/s1]
         ["score-complexity"  :tc/s2]
         ["score-task-type"   :tc/s3]
         ["score-keywords"    :tc/s4]
         ["classify-task"     :tc/run]]})

;; --- Group R: DSL Macros ---
(def ^:private ext-group-r
  {:ns 'hive-knowledge.dsl-macros
   :fns [["expand-a" :dm/a]
         ["expand-b" :dm/b]
         ["expand-c" :dm/c]
         ["expand-d" :dm/d]]})

;; --- Group S: DSL Verbs ---
(def ^:private ext-group-s
  {:ns 'hive-knowledge.dsl-verbs
   :fns [["compile-verbs" :dv/compile]]})

;; --- Group T: DSL Response ---
(def ^:private ext-group-t
  {:ns 'hive-knowledge.dsl-response
   :fns [["omit-fields"         :dr/omit]
         ["strip-fields"        :dr/strip]
         ["abbreviate"          :dr/abbrev]
         ["format-entry"        :dr/entry]
         ["compact-output"      :dr/compact]
         ["minimize-output"     :dr/minimal]
         ["compress-output"     :dr/compress]
         ["format-text"         :dr/text]
         ["format-content"      :dr/content]
         ["transform-batch-op"  :dr/batch-op]
         ["transform-batch-env" :dr/batch-env]]})

;; --- Group U: Decompose ---
(def ^:private ext-group-u
  {:ns 'hive-knowledge.decompose
   :fns [["estimate-complexity"  :dd/est]
         ["get-step-budget"      :dd/budget]
         ["split-by-functions"   :dd/split-fn]
         ["split-by-sections"    :dd/split-sec]
         ["split-by-operations"  :dd/split-op]
         ["auto-decompose"       :dd/decomp]]})

;; --- Group V: Context Budget ---
(def ^:private ext-group-v
  {:ns 'hive-knowledge.budget
   :fns [["estimate-tokens"          :cb/a]
         ["truncate-to-budget"       :cb/b]
         ["allocate-ling-context"    :cb/c]
         ["allocate-drone-context"   :cb/d]
         ["allocate-unified-entries" :cb/e]]})

;; --- Group W: Context Reconstruction Extensions ---
(def ^:private ext-group-w
  {:ns 'hive-knowledge.reconstruction
   :fns [["fetch-ref-data"            :cr/e]
         ["gather-kg-context"         :cr/f]
         ["compress-kg-subgraph"      :cr/g]
         ["render-compressed-context" :cr/h]
         ["reconstruct-full"          :cr/i]
         ["catchup-to-ref-context"    :cr/j]]})

;; --- Group X: Multi Batch ---
(def ^:private ext-group-x
  {:ns 'hive-knowledge.multi-batch
   :fns [["parse-ref"             :bx/a]
         ["extract-result-data"   :bx/b]
         ["resolve-ref"           :bx/c]
         ["resolve-refs-in-value" :bx/d]
         ["resolve-op-refs"       :bx/e]
         ["collect-ref-op-ids"    :bx/f]
         ["validate-ref-deps"     :bx/g]
         ["detect-cycles"         :bx/h]
         ["assign-waves"          :bx/i]
         ["execute-wave"          :bx/j]]})

;; --- Group Y: Forge Belt ---
(def ^:private ext-group-y
  {:ns 'hive-knowledge.forge-belt
   :fns [;; Predicates
         ["quenched?"           :fb/q1]
         ["has-tasks?"          :fb/q2]
         ["no-tasks?"           :fb/q3]
         ["continuous?"         :fb/q4]
         ["single-shot?"        :fb/q5]
         ;; Handlers
         ["handle-start"        :fb/h1]
         ["handle-smite"        :fb/h2]
         ["handle-survey"       :fb/h3]
         ["handle-spark"        :fb/h4]
         ["handle-end"          :fb/h5]
         ["handle-halt"         :fb/h6]
         ["handle-error"        :fb/h7]
         ;; Subscription handlers
         ["on-smite-count-change" :fb/s1]
         ["on-spark-count-change" :fb/s2]
         ;; Compilation & execution
         ["compile-belt"        :fb/compile]
         ["run-belt"            :fb/run]
         ["run-single-strike"   :fb/strike]
         ["run-continuous-belt" :fb/cont]]})

;; --- Group Z: Crystal Hooks ---
(def ^:private ext-group-z
  {:ns 'hive-knowledge.crystal-hooks
   :fns [["process-promotions"        :ch/a]
         ["process-decay"             :ch/b]
         ["process-cross-pollination" :ch/c]
         ["process-memory-decay"      :ch/d]]})

;; --- Group AA: Crystal Recall ---
(def ^:private ext-group-aa
  {:ns 'hive-knowledge.crystal-recall
   :fns [["detect-recall-context"  :cl/a]
         ["classify-query-intent"  :cl/b]
         ["aggregate-recalls"      :cl/c]
         ["merge-recall-histories" :cl/d]]})

;; --- Group AB: Crystal Tools ---
(def ^:private ext-group-ab
  {:ns 'hive-knowledge.crystal-tools
   :fns [["build-crystal-edges" :ck/a]]})

;; --- Group AC: Done Archive ---
(def ^:private ext-group-ac
  {:ns 'hive-knowledge.done-archive
   :fns [["archive-done-task!"   :da/archive!]
         ["query-done-tasks"     :da/query]
         ["recent-completions"   :da/recent]
         ["milestone-progress"   :da/milestone]]})

;; =============================================================================
;; All Extension Manifests
;; =============================================================================

(def hive-knowledge-manifests
  "Manifests for hive-knowledge extension groups (A-K, P-AC).
   Public so HiveKnowledgeAddon can resolve without direct registration."
  [ext-group-a ext-group-b ext-group-c ext-group-d ext-group-e
   ext-group-f ext-group-g ext-group-h ext-group-i ext-group-j
   ext-group-k ext-group-p
   ;; IP migration groups (Q-AC)
   ext-group-q ext-group-r ext-group-s ext-group-t ext-group-u
   ext-group-v ext-group-w ext-group-x ext-group-y ext-group-z
   ext-group-aa ext-group-ab ext-group-ac])

(def hive-agent-manifests
  "Manifests for hive-agent extension groups (L-O).
   Public so HiveAgentAddon can resolve without direct registration."
  [ext-group-l ext-group-m ext-group-n ext-group-o])

(def ^:private all-manifests
  (into hive-knowledge-manifests hive-agent-manifests))

;; =============================================================================
;; Extension Self-Registration
;; =============================================================================
;;
;; Extension projects (hive-knowledge, hive-agent) can provide their own
;; init! functions that self-register into the registry. This is the
;; preferred path — extensions know what they provide.

(def ^:private extension-initializers
  "Namespace symbols for extension self-registration init! functions.
   Each must have a zero-arg init! that returns {:registered [...] :total N}."
  ['hive-knowledge.init/init!
   'hive-agent.init/init!])

(defn- try-call-initializer
  "Attempt to call an extension initializer. Returns result or nil."
  [sym]
  (try
    (when-let [init-fn (try-resolve sym)]
      (let [result (init-fn)]
        (log/info "Extension initializer" sym "registered" (:total result 0) "capabilities")
        result))
    (catch Exception e
      (log/debug "Extension initializer" sym "not available:" (.getMessage e))
      nil)))

;; =============================================================================
;; Public API
;; =============================================================================

(defn resolve-manifests
  "Resolve a seq of extension manifests without registering in the registry.
   Returns map of {registry-key resolved-fn} for all successfully resolved fns.

   Use with hive-knowledge-manifests or hive-agent-manifests for addon-based loading:
     (resolve-manifests hive-knowledge-manifests)
   The caller (e.g. addon bridge) is responsible for registration."
  [manifests]
  (reduce
   (fn [acc {:keys [ns fns]}]
     (merge acc (resolve-extension-group ns fns)))
   {}
   manifests))

(defn load-extensions!
  "Resolve and register all available extensions.
   Called once at startup. Thread-safe, idempotent.

   Dual strategy:
   1. Try extension self-registration (init! functions) — preferred path
   2. Fall back to manifest-based resolution for any keys not yet registered

   Returns map of {:registered [keys...] :total count :sources {...}}."
  []
  (let [;; Strategy 1: Let extensions self-register
        init-results (keep try-call-initializer extension-initializers)
        init-total (reduce + 0 (map :total init-results))

        ;; Strategy 2: Manifest-based resolution (fills gaps)
        manifest-resolved (reduce
                           (fn [acc {:keys [ns fns]}]
                             (let [resolved (resolve-extension-group ns fns)
                                   ;; Only register keys not already registered by init!
                                   new-keys (remove ext/extension-available? (keys resolved))]
                               (merge acc (select-keys resolved new-keys))))
                           {}
                           all-manifests)

        ;; Register any manifest-resolved keys not covered by init!
        _ (when (seq manifest-resolved)
            (ext/register-many! manifest-resolved)
            (log/info "Manifest loader filled" (count manifest-resolved) "additional capabilities"))

        total-registered (count (ext/registered-keys))]

    (if (pos? total-registered)
      (log/info "Extensions loaded:" total-registered "total capabilities"
                "(init!:" init-total ", manifest:" (count manifest-resolved) ")")
      (log/debug "No extensions found on classpath — all capabilities will use defaults"))

    {:registered (vec (ext/registered-keys))
     :total total-registered
     :sources {:initializers init-total
               :manifest (count manifest-resolved)}}))
