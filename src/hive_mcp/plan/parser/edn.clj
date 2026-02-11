(ns hive-mcp.plan.parser.edn
  "EDN plan parser — extracts and parses EDN plan structures from content.

   Supports:
   1. Raw EDN content with :steps or :plan/steps
   2. ```edn code blocks containing plan structures
   3. Phase-based plans ({:phase N :tasks [...]}) across multiple blocks
   4. Mixed markdown/text with embedded EDN

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
  "Regex pattern to detect EDN plan structure anywhere in content.
   Matches {:steps [...] or {:plan/steps [...] with optional whitespace."
  #"\{[^}]*:(?:plan/)?steps\s*\[")

(def ^:private edn-phase-pattern
  "Regex pattern to detect phase-based plan structure.
   Matches {:phase N ... :tasks [...] blocks."
  #"\{[^}]*:(?:phase/)?(?:phase|id)\s+\d+[^}]*:(?:phase/)?tasks\s*\[")

(defn- looks-like-edn-plan?
  "Check if content looks like raw EDN with plan structure.

   Detects patterns like {:plan/steps [...] or {:steps [...] anywhere in content.
   Handles leading whitespace, markdown headers, or other prefixes."
  [content]
  (boolean
   (and (string? content)
        (re-find edn-plan-pattern content))))

(defn contains-edn-block?
  "Check if content contains any ```edn ... ``` blocks."
  [content]
  (boolean (re-find edn-block-pattern content)))

(defn contains-edn-plan?
  "Check if content contains EDN plan (raw or in blocks).

   Detects:
   - ```edn ... ``` code blocks
   - Raw EDN content with :plan/steps or :steps
   - Phase-based blocks with :phase N :tasks [...]"
  [content]
  (or (contains-edn-block? content)
      (looks-like-edn-plan? content)
      (boolean (and (string? content) (re-find edn-phase-pattern content)))))

(defn- try-parse-edn
  "Safely attempt to parse EDN string.

   Returns: {:success true :data ...} or {:success false :error ...}"
  [edn-str]
  (try
    {:success true :data (edn/read-string edn-str)}
    (catch Exception e
      {:success false :error (.getMessage e)})))

(defn- extract-edn-from-content
  "Extract EDN map from mixed markdown/text content.

   Finds the first balanced {} that contains :steps or :plan/steps.
   Handles nested braces correctly, respects string literals.

   Args:
   - content: String that may contain EDN embedded in other text

   Returns:
   - EDN substring if found, nil otherwise"
  [content]
  (when (string? content)
    (let [start-idx (str/index-of content "{")]
      (when start-idx
        (loop [idx start-idx
               depth 0
               in-string false
               escape-next false]
          (if (>= idx (count content))
            nil  ;; Unbalanced - no closing brace found
            (let [c (nth content idx)]
              (cond
                ;; Handle escape sequences in strings
                escape-next
                (recur (inc idx) depth in-string false)

                ;; Backslash - next char is escaped
                (and in-string (= c \\))
                (recur (inc idx) depth in-string true)

                ;; String delimiter
                (= c \")
                (recur (inc idx) depth (not in-string) false)

                ;; Inside string - skip brace matching
                in-string
                (recur (inc idx) depth in-string false)

                ;; Opening brace
                (= c \{)
                (recur (inc idx) (inc depth) in-string false)

                ;; Closing brace
                (= c \})
                (let [new-depth (dec depth)]
                  (if (zero? new-depth)
                    ;; Found matching close brace - extract substring
                    (let [edn-str (subs content start-idx (inc idx))]
                      ;; Verify it contains :steps
                      (when (re-find #":(?:plan/)?steps\s*\[" edn-str)
                        edn-str))
                    (recur (inc idx) new-depth in-string false)))

                ;; Any other character
                :else
                (recur (inc idx) depth in-string false)))))))))

;; =============================================================================
;; EDN Normalization
;; =============================================================================

(defn- get-steps-key
  "Get the steps from EDN data, checking both namespaced and non-namespaced keys."
  [data]
  (or (:steps data)
      (:plan/steps data)))

(defn- is-plan-edn?
  "Check if parsed EDN looks like a plan (has :steps or :plan/steps key)."
  [data]
  (and (map? data)
       (let [steps (get-steps-key data)]
         (and steps (vector? steps)))))

(defn- is-phase-edn?
  "Check if parsed EDN looks like a phase block ({:phase N :tasks [...]})."
  [data]
  (and (map? data)
       (or (:tasks data) (:phase/tasks data))
       (or (:phase data) (:phase/id data))))

(defn- strip-namespace
  "Remove namespace from a keyword if present."
  [k]
  (if (keyword? k)
    (keyword (name k))
    k))

(defn- keyword->string
  "Convert keyword to string using name, pass strings through unchanged."
  [v]
  (if (keyword? v) (name v) v))

(defn- normalize-edn-step
  "Normalize an EDN step map, converting namespaced keys to non-namespaced.

   :step/id -> :id, :step/title -> :title, etc.
   Also coerces :id and :depends-on items from keywords to strings.

   Aliases normalized:
   - :dependencies -> :depends-on (SAA plans use :dependencies)
   - :file (string) -> :files (vector) (SAA plans use singular :file)"
  [step]
  (let [base (reduce-kv (fn [m k v]
                          (assoc m (strip-namespace k) v))
                        {}
                        step)
        ;; Coerce :id from keyword to string (schema requires :string)
        base (if-let [id (:id base)]
               (assoc base :id (keyword->string id))
               base)
        ;; Normalize :dependencies alias -> :depends-on (SAA plans use :dependencies)
        base (if (and (contains? base :dependencies)
                      (not (contains? base :depends-on)))
               (-> base
                   (assoc :depends-on (:dependencies base))
                   (dissoc :dependencies))
               base)
        ;; Coerce :depends-on items from keywords to strings
        base (if-let [deps (:depends-on base)]
               (assoc base :depends-on (mapv keyword->string deps))
               base)
        ;; Normalize :file (singular string) -> :files (vector)
        base (if (and (contains? base :file)
                      (not (contains? base :files)))
               (-> base
                   (assoc :files (if (string? (:file base))
                                   [(:file base)]
                                   (vec (:file base))))
                   (dissoc :file))
               base)]
    base))

(defn- normalize-edn-plan
  "Normalize an EDN plan map, converting namespaced keys to non-namespaced.

   :plan/title -> :title, :plan/steps -> :steps, etc.
   Also normalizes nested step maps and coerces plan-level :id to string."
  [data]
  (let [base-map (reduce-kv (fn [m k v]
                              (assoc m (strip-namespace k) v))
                            {}
                            data)
        ;; Coerce plan-level :id from keyword to string
        base-map (if-let [id (:id base-map)]
                   (assoc base-map :id (keyword->string id))
                   base-map)
        steps (get-steps-key data)]
    (if steps
      (assoc base-map :steps (mapv normalize-edn-step steps))
      base-map)))

;; =============================================================================
;; Phase-Based Plan Parsing (Multi-Block EDN)
;; =============================================================================

(defn- phase-tasks->steps
  "Convert a phase's :tasks to plan :steps, prefixing IDs for uniqueness
   and injecting phase-level :depends-on as cross-phase task dependencies.

   A phase like:
     {:phase 2 :name \"Hooks\" :depends-on [1]
      :tasks [{:id :p2-t1 :title \"Design hook\" :desc \"...\"}]}

   With prior phase 1 having tasks [:p1-t1 :p1-t2], produces steps where
   each task in phase 2 depends on the LAST task of each dependency phase
   (lightweight cross-phase ordering)."
  [phase prior-phase-last-tasks]
  (let [tasks (or (:tasks phase) (:phase/tasks phase))
        phase-deps (or (:depends-on phase) (:phase/depends-on phase) [])
        ;; Cross-phase deps: depend on last task of each prior phase
        cross-deps (vec (keep #(get prior-phase-last-tasks %) phase-deps))]
    (mapv (fn [task]
            (let [task-id (keyword->string (or (:id task) (:task/id task)))
                  task-deps (mapv keyword->string (or (:depends-on task) (:task/depends-on task) []))
                  ;; First task of phase inherits cross-phase deps
                  all-deps (if (= task (first tasks))
                             (into cross-deps task-deps)
                             task-deps)]
              (-> (normalize-edn-step task)
                  (assoc :id task-id)
                  (assoc :depends-on all-deps)
                  (cond->
                   (or (:desc task) (:task/desc task))
                    (assoc :description (or (:desc task) (:task/desc task)))

                    (or (:estimate task) (:task/estimate task))
                    (assoc :estimate (keyword->string (or (:estimate task) (:task/estimate task))))))))
          tasks)))

(defn- phases->plan
  "Flatten multiple phase blocks into a single plan with :steps.

   Phases are sorted by :phase number. Each phase's tasks become steps.
   Phase-level :depends-on creates cross-phase ordering edges."
  [phase-blocks & {:keys [title]}]
  (let [sorted-phases (sort-by #(or (:phase %) (:phase/id %) 0) phase-blocks)
        ;; Build map of phase-number -> last task ID (for cross-phase deps)
        {:keys [steps last-tasks]}
        (reduce (fn [{:keys [steps last-tasks]} phase]
                  (let [phase-num (or (:phase phase) (:phase/id phase))
                        phase-steps (phase-tasks->steps phase last-tasks)
                        last-id (:id (last phase-steps))]
                    {:steps (into steps phase-steps)
                     :last-tasks (assoc last-tasks phase-num last-id)}))
                {:steps [] :last-tasks {}}
                sorted-phases)]
    {:steps steps
     :title (or title
                (:name (first sorted-phases))
                "Untitled Plan")}))

;; =============================================================================
;; EDN Plan Parsing (Public API)
;; =============================================================================

(defn- finalize-edn-plan
  "Normalize and validate parsed EDN plan data.

   Applies defaults, normalization, and schema validation.
   Returns {:success true :plan ...} or {:success false :error ...}"
  [plan-data]
  (let [;; Normalize namespaced keys to non-namespaced
        normalized-edn (normalize-edn-plan plan-data)
        ;; Generate ID and title if not provided
        plan-with-defaults (cond-> normalized-edn
                             (not (:id normalized-edn))
                             (assoc :id (str "plan-" (System/currentTimeMillis)))

                             (not (:title normalized-edn))
                             (assoc :title "Untitled Plan"))
        normalized (schema/normalize-plan
                    (assoc plan-with-defaults :source-format :edn))]
    (if (schema/valid-plan? normalized)
      {:success true :plan normalized}
      {:success false
       :error "Plan failed schema validation"
       :details (schema/explain-plan normalized)})))

(defn- try-parse-phase-blocks
  "Strategy 4: Parse multiple ```edn blocks as phase definitions.

   Detects pattern where each block is {:phase N :tasks [...]} and
   flattens them into a single plan with :steps.

   Returns {:success true :plan ...} or nil if blocks aren't phases."
  [content plan-title]
  (let [blocks (extract-edn-blocks content)
        parsed-blocks (keep (fn [block]
                              (let [result (try-parse-edn block)]
                                (when (:success result)
                                  (:data result))))
                            blocks)
        phase-blocks (filter is-phase-edn? parsed-blocks)]
    (when (>= (count phase-blocks) 2)
      (let [plan-data (phases->plan phase-blocks :title plan-title)]
        (finalize-edn-plan plan-data)))))

(defn parse-edn-plan
  "Parse plan from EDN content or EDN blocks.

   Tries four strategies in order:
   1. Parse content directly as EDN (for raw EDN plans)
   2. Extract balanced {} containing :steps from mixed content
   3. Find single ```edn block with :steps
   4. Collect multiple ```edn phase blocks ({:phase N :tasks [...]})
      and flatten to unified :steps

   Supports both namespaced (:plan/steps, :step/id) and plain keys.

   Args:
   - content: String containing EDN (raw or in code blocks)
   - opts: Optional map with :title for plan title extraction

   Returns:
   - {:success true :plan ...} with normalized plan
   - {:success false :error ...} if no valid plan found"
  ([content] (parse-edn-plan content {}))
  ([content {:keys [title]}]
   ;; Strategy 1: Try parsing content directly as EDN
   (let [direct-parse (try-parse-edn content)]
     (if (and (:success direct-parse) (is-plan-edn? (:data direct-parse)))
       (finalize-edn-plan (:data direct-parse))
       ;; Strategy 2: Extract EDN from mixed markdown/text content
       (let [extracted-edn (extract-edn-from-content content)
             extracted-result (when extracted-edn
                                (let [parsed (try-parse-edn extracted-edn)]
                                  (when (and (:success parsed) (is-plan-edn? (:data parsed)))
                                    (finalize-edn-plan (:data parsed)))))]
         (or extracted-result
             ;; Strategy 3: Find single ```edn block with :steps
             (let [blocks (extract-edn-blocks content)
                   plan-data (first (for [block blocks
                                          :let [parsed (try-parse-edn block)]
                                          :when (:success parsed)
                                          :when (is-plan-edn? (:data parsed))]
                                      (:data parsed)))]
               (if plan-data
                 (finalize-edn-plan plan-data)
                 ;; Strategy 4: Multiple phase blocks
                 (or (try-parse-phase-blocks content title)
                     {:success false
                      :error "No EDN plan found (tried direct parse, embedded extraction, ```edn blocks, and phase blocks)"})))))))))
