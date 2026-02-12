(ns hive-mcp.plan.parser.edn
  "EDN plan parser — extracts and parses EDN plan structures from content.

   Supports:
   1. Raw EDN content with :steps or :plan/steps
   2. ```edn code blocks containing plan structures
   3. Phase-based plans ({:phase N :tasks [...]}) across multiple blocks
   4. Mixed markdown/text with embedded EDN

   Architecture: Parser combinator pattern — strategies composed via `some`,
   normalizers as composable transform pipeline, state machine for brace matching.

   CC-free constructs used throughout:
   - if-let, when-let, when-not (FREE) instead of if, when, cond
   - case (FREE) instead of cond for character dispatch
   - cond-> (FREE) for conditional map building"

  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [hive-mcp.plan.schema :as schema]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; EDN Block Extraction
;; =============================================================================

(def ^:private edn-block-pattern
  "Regex pattern to match ```edn ... ``` code blocks"
  #"(?s)```edn\s*\n(.*?)\n```")

(defn- extract-edn-blocks
  "Extract all EDN code blocks from content.
   Returns: vector of EDN strings found in ```edn ... ``` blocks"
  [content]
  (->> (re-seq edn-block-pattern content)
       (mapv second)))

(def ^:private edn-plan-pattern
  "Regex to detect {:steps [...] or {:plan/steps [...] in content."
  #"\{[^}]*:(?:plan/)?steps\s*\[")

(def ^:private edn-phase-pattern
  "Regex to detect {:phase N :tasks [...] blocks."
  #"\{[^}]*:(?:phase/)?(?:phase|id)\s+\d+[^}]*:(?:phase/)?tasks\s*\[")

;; =============================================================================
;; Detection Predicates
;; =============================================================================

(defn contains-edn-block?
  "Check if content contains any ```edn ... ``` blocks."
  [content]
  (boolean (re-find edn-block-pattern content)))

(defn contains-edn-plan?
  "Check if content contains EDN plan (raw or in blocks).

   Detects ```edn blocks, raw :steps structures, and phase-based blocks."
  [content]
  (boolean
   (when-let [s (and (string? content) content)]
     (some #(re-find % s) [edn-block-pattern edn-plan-pattern edn-phase-pattern]))))

;; =============================================================================
;; Safe EDN Parsing
;; =============================================================================

(defn- try-parse-edn
  "Safely attempt to parse EDN string.
   Returns: {:success true :data ...} or {:success false :error ...}"
  [edn-str]
  (try
    {:success true :data (edn/read-string edn-str)}
    (catch Exception e
      {:success false :error (.getMessage e)})))

;; =============================================================================
;; Balanced Brace Extraction (State Machine)
;; =============================================================================

(defn- advance-brace-state
  "Pure state transition for balanced-brace parser.

   Uses case (CC-free) for character dispatch, if-let (CC-free) for guards.
   Returns updated state map with :action (:continue or :done)."
  [{:keys [depth in-string escape-next]} c]
  (if-let [_ escape-next]
    ;; After escape char: consume and clear flag
    {:depth depth :in-string in-string :escape-next false :action :continue}
    (if-let [_ in-string]
      ;; Inside string literal: only escape and close-quote matter
      (case c
        \\ {:depth depth :in-string true :escape-next true :action :continue}
        \" {:depth depth :in-string false :escape-next false :action :continue}
        {:depth depth :in-string true :escape-next false :action :continue})
      ;; Outside string: handle braces and open-quote
      (case c
        \" {:depth depth :in-string true :escape-next false :action :continue}
        \{ {:depth (inc depth) :in-string false :escape-next false :action :continue}
        \} (let [new-depth (dec depth)]
             {:depth new-depth :in-string false :escape-next false
              :action (if-let [_ (when-not (pos? new-depth) :balanced)] :done :continue)})
        {:depth depth :in-string false :escape-next false :action :continue}))))

(defn- find-balanced-edn
  "Find the first balanced {} substring starting from start-idx.
   Returns: EDN substring or nil if unbalanced."
  [content start-idx]
  (let [len (count content)]
    (loop [idx start-idx
           state {:depth 0 :in-string false :escape-next false}]
      (when-let [c (when-not (>= idx len) (nth content idx))]
        (let [new-state (advance-brace-state state c)]
          (case (:action new-state)
            :done (subs content start-idx (inc idx))
            :continue (recur (inc idx) new-state)))))))

(defn- extract-edn-from-content
  "Extract EDN map from mixed markdown/text content.
   Finds the first balanced {} that contains :steps or :plan/steps."
  [content]
  (when-let [start-idx (and (string? content) (str/index-of content "{"))]
    (when-let [edn-str (find-balanced-edn content start-idx)]
      (when-let [_ (re-find #":(?:plan/)?steps\s*\[" edn-str)]
        edn-str))))

;; =============================================================================
;; Structural Predicates
;; =============================================================================

(defn- get-steps-key
  "Get steps from EDN data, checking both namespaced and non-namespaced keys."
  [data]
  (if-let [steps (:steps data)] steps (:plan/steps data)))

(defn- is-plan-edn?
  "Check if parsed EDN looks like a plan (has :steps or :plan/steps key)."
  [data]
  (when-let [_ (map? data)]
    (when-let [steps (get-steps-key data)]
      (vector? steps))))

(defn- is-phase-edn?
  "Check if parsed EDN looks like a phase block ({:phase N :tasks [...]})."
  [data]
  (when-let [_ (map? data)]
    (when-let [_ (if-let [t (:tasks data)] t (:phase/tasks data))]
      (if-let [p (:phase data)] p (:phase/id data)))))

;; =============================================================================
;; Step Normalization Pipeline (Composable Transforms)
;; =============================================================================

(defn- strip-namespace
  "Remove namespace from a keyword if present."
  [k]
  (if-let [_ (keyword? k)]
    (keyword (name k))
    k))

(defn- keyword->string
  "Convert keyword to string using name, pass strings through unchanged."
  [v]
  (if-let [_ (keyword? v)] (name v) v))

(defn- strip-all-namespaces
  "Remove namespaces from all keys in a map. First stage of normalization."
  [step]
  (reduce-kv (fn [m k v] (assoc m (strip-namespace k) v)) {} step))

(defn- coerce-id
  "Coerce :id from keyword to string (schema requires string)."
  [step]
  (if-let [id (:id step)]
    (assoc step :id (keyword->string id))
    step))

(defn- alias-dependencies
  "Normalize :dependencies alias -> :depends-on (SAA plans use :dependencies)."
  [step]
  (if-let [deps (when-not (contains? step :depends-on) (:dependencies step))]
    (-> step (assoc :depends-on deps) (dissoc :dependencies))
    step))

(defn- coerce-depends-on
  "Coerce :depends-on items from keywords to strings."
  [step]
  (if-let [deps (:depends-on step)]
    (assoc step :depends-on (mapv keyword->string deps))
    step))

(defn- alias-file
  "Normalize :file (singular string) -> :files (vector)."
  [step]
  (if-let [f (when-not (contains? step :files) (:file step))]
    (-> step
        (assoc :files (if-let [_ (string? f)] [f] (vec f)))
        (dissoc :file))
    step))

(defn- normalize-edn-step
  "Normalize an EDN step map via composable transform pipeline.

   Pipeline: strip-namespaces -> coerce-id -> alias-deps -> coerce-deps -> alias-file"
  [step]
  (-> step
      strip-all-namespaces
      coerce-id
      alias-dependencies
      coerce-depends-on
      alias-file))

;; =============================================================================
;; Plan Normalization
;; =============================================================================

(defn- normalize-edn-plan
  "Normalize an EDN plan map, converting namespaced keys to non-namespaced.
   Also normalizes nested step maps and coerces plan-level :id to string."
  [data]
  (let [base-map (-> data strip-all-namespaces coerce-id)
        steps (get-steps-key data)]
    (cond-> base-map
      steps (assoc :steps (mapv normalize-edn-step steps)))))

;; =============================================================================
;; Phase-Based Plan Parsing (Multi-Block EDN)
;; =============================================================================

(defn- ns-key
  "Retrieve value trying plain key, then namespaced variant. CC-free via if-let."
  [m plain-k ns-k]
  (if-let [v (plain-k m)] v (ns-k m)))

(defn- ns-key-or
  "Retrieve value trying plain key, namespaced, then default. CC-free via if-let."
  [m plain-k ns-k default]
  (if-let [v (plain-k m)] v (if-let [v2 (ns-k m)] v2 default)))

(defn- normalize-phase-task
  "Normalize a single task within a phase context.

   Resolves namespaced keys, injects cross-phase dependencies for first task,
   and normalizes desc/estimate fields."
  [task cross-deps is-first-task?]
  (let [task-id   (keyword->string (ns-key task :id :task/id))
        task-deps (mapv keyword->string (ns-key-or task :depends-on :task/depends-on []))
        all-deps  (if-let [_ is-first-task?] (into cross-deps task-deps) task-deps)]
    (cond-> (normalize-edn-step task)
      true (assoc :id task-id)
      true (assoc :depends-on all-deps)

      (ns-key task :desc :task/desc)
      (assoc :description (ns-key task :desc :task/desc))

      (ns-key task :estimate :task/estimate)
      (assoc :estimate (keyword->string (ns-key task :estimate :task/estimate))))))

(defn- phase-tasks->steps
  "Convert a phase's :tasks to plan :steps with cross-phase dependency injection.

   First task of phase inherits cross-phase deps (lightweight ordering)."
  [phase prior-phase-last-tasks]
  (let [tasks      (ns-key phase :tasks :phase/tasks)
        phase-deps (ns-key-or phase :depends-on :phase/depends-on [])
        cross-deps (vec (keep #(get prior-phase-last-tasks %) phase-deps))]
    (vec (map-indexed
          (fn [idx task]
            (normalize-phase-task task cross-deps (zero? idx)))
          tasks))))

(defn- phases->plan
  "Flatten multiple phase blocks into a single plan with :steps.

   Phases sorted by :phase number. Cross-phase deps create ordering edges."
  [phase-blocks & {:keys [title]}]
  (let [sorted-phases (sort-by #(ns-key-or % :phase :phase/id 0) phase-blocks)
        {:keys [steps]}
        (reduce (fn [{:keys [steps last-tasks]} phase]
                  (let [phase-num   (ns-key phase :phase :phase/id)
                        phase-steps (phase-tasks->steps phase last-tasks)
                        last-id     (:id (last phase-steps))]
                    {:steps      (into steps phase-steps)
                     :last-tasks (assoc last-tasks phase-num last-id)}))
                {:steps [] :last-tasks {}}
                sorted-phases)]
    {:steps steps
     :title (if-let [t title] t
                    (if-let [n (:name (first sorted-phases))] n
                            "Untitled Plan"))}))

;; =============================================================================
;; Plan Finalization
;; =============================================================================

(defn- finalize-edn-plan
  "Normalize and validate parsed EDN plan data.

   Applies defaults, normalization, and schema validation.
   Returns {:success true :plan ...} or {:success false :error ...}"
  [plan-data]
  (let [normalized-edn (normalize-edn-plan plan-data)
        plan-with-defaults
        (cond-> normalized-edn
          (not (:id normalized-edn))
          (assoc :id (str "plan-" (System/currentTimeMillis)))

          (not (:title normalized-edn))
          (assoc :title "Untitled Plan"))
        normalized (schema/normalize-plan
                    (assoc plan-with-defaults :source-format :edn))]
    (if-let [_ (schema/valid-plan? normalized)]
      {:success true :plan normalized}
      {:success false
       :error "Plan failed schema validation"
       :details (schema/explain-plan normalized)})))

;; =============================================================================
;; Parse Strategy Functions (Combinator Pattern)
;; =============================================================================

(defn- try-direct-parse
  "Strategy 1: Parse content directly as EDN plan."
  [content _opts]
  (let [{:keys [success data]} (try-parse-edn content)]
    (when-let [plan-data (and success (is-plan-edn? data) data)]
      (finalize-edn-plan plan-data))))

(defn- try-embedded-extraction
  "Strategy 2: Extract balanced {} containing :steps from mixed content."
  [content _opts]
  (when-let [extracted-edn (extract-edn-from-content content)]
    (let [{:keys [success data]} (try-parse-edn extracted-edn)]
      (when-let [plan-data (and success (is-plan-edn? data) data)]
        (finalize-edn-plan plan-data)))))

(defn- try-single-edn-block
  "Strategy 3: Find single ```edn block with :steps."
  [content _opts]
  (let [blocks (extract-edn-blocks content)]
    (when-let [plan-data (first (keep (fn [block]
                                        (let [{:keys [success data]} (try-parse-edn block)]
                                          (when-let [_ (and success (is-plan-edn? data))]
                                            data)))
                                      blocks))]
      (finalize-edn-plan plan-data))))

(defn- try-phase-block-parse
  "Strategy 4: Multiple phase blocks ({:phase N :tasks [...]}).

   Detects pattern where each block is {:phase N :tasks [...]} and
   flattens them into a single plan with :steps."
  [content {:keys [title]}]
  (let [blocks (extract-edn-blocks content)
        parsed-blocks (keep (fn [block]
                              (when-let [{:keys [success data]} (try-parse-edn block)]
                                (when-let [_ success] data)))
                            blocks)
        phase-blocks (filter is-phase-edn? parsed-blocks)]
    (when-let [_ (when-not (< (count phase-blocks) 2) :enough)]
      (finalize-edn-plan (phases->plan phase-blocks :title title)))))

;; =============================================================================
;; EDN Plan Parsing (Public API)
;; =============================================================================

(def ^:private parse-strategies
  "Ordered vector of parse strategy functions.
   Each takes [content opts] and returns {:success true :plan ...} or nil.
   First non-nil result wins (combinator pattern via `some`)."
  [try-direct-parse
   try-embedded-extraction
   try-single-edn-block
   try-phase-block-parse])

(defn parse-edn-plan
  "Parse plan from EDN content or EDN blocks.

   Tries four strategies via combinator pattern (first success wins):
   1. Parse content directly as EDN (for raw EDN plans)
   2. Extract balanced {} containing :steps from mixed content
   3. Find single ```edn block with :steps
   4. Collect multiple ```edn phase blocks and flatten to unified :steps

   Supports both namespaced (:plan/steps, :step/id) and plain keys.

   Args:
   - content: String containing EDN (raw or in code blocks)
   - opts: Optional map with :title for plan title extraction

   Returns:
   - {:success true :plan ...} with normalized plan
   - {:success false :error ...} if no valid plan found"
  ([content] (parse-edn-plan content {}))
  ([content opts]
   (if-let [result (some #(% content opts) parse-strategies)]
     result
     {:success false
      :error "No EDN plan found (tried direct parse, embedded extraction, ```edn blocks, and phase blocks)"})))
