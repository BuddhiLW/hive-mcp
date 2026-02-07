(ns hive-mcp.context.reconstruction
  "KG-compressed context reconstruction for agent communication.

   Transforms context-store references + KG node IDs + scope into
   bounded compressed context markdown. Bridges:
     context-store (refs) -> KG subgraph (structural edges) -> rendered prompt

   Core pipeline:
     1. fetch-ref-data      - resolve context-store entries by category->ctx-id
     2. gather-kg-context   - traverse KG from node IDs (bounded depth, structural only)
     3. compress-kg-subgraph - render KG subgraph as compact edge-list (~200 tokens)
     4. render-compressed-context - combine ref summaries + KG subgraph (3000 char budget)
     5. reconstruct-context - top-level fn matching RefContext reconstruct-fn signature

   Design principles:
   - STRUCTURAL edges only: implements, supersedes, depends-on, refines, contradicts, derived-from
   - Co-access edges are noise for reconstruction
   - Bounded: max depth 2, max 10 KG nodes, 3000 char output
   - Graceful degradation: missing refs, empty KG, expired entries all produce partial output

   SOLID-S: Single responsibility - context reconstruction only.
   SOLID-O: Open for extension - new renderers can be added.
   CLARITY-Y: Yield safe failure - never throws, degrades gracefully."
  (:require [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.knowledge-graph.queries :as kg-queries]
            [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const max-output-chars
  "Maximum output size for reconstructed context. ~750 tokens."
  3000)

(def ^:const max-kg-nodes
  "Maximum KG nodes to include in traversal results."
  10)

(def ^:const max-kg-depth
  "Maximum KG traversal depth from seed nodes."
  2)

(def structural-relations
  "Structural KG relations that carry semantic meaning for context reconstruction.
   Co-access edges (:co-access) are noise - they indicate file proximity, not conceptual links."
  #{:implements :supersedes :depends-on :refines :contradicts :derived-from :applies-to})

;; =============================================================================
;; Step 1: Fetch Reference Data
;; =============================================================================

(defn fetch-ref-data
  "Resolve context-store entries by category->ctx-id map.

   Arguments:
     ctx-refs - Map of category (keyword) to context-store ID (string)
                e.g. {:axioms \"ctx-123\" :conventions \"ctx-456\"}

   Returns:
     Map of category -> data (the :data field from context-store entries).
     Missing/expired entries are omitted silently (graceful degradation).

   Example:
     (fetch-ref-data {:axioms \"ctx-123\" :decisions \"ctx-456\"})
     => {:axioms [...] :decisions [...]}"
  [ctx-refs]
  (when (seq ctx-refs)
    (reduce-kv
     (fn [acc category ctx-id]
       (if-let [entry (try
                        (context-store/context-get ctx-id)
                        (catch Exception e
                          (log/debug "fetch-ref-data: failed to get" ctx-id
                                     (.getMessage e))
                          nil))]
         (assoc acc category (:data entry))
         (do
           (log/debug "fetch-ref-data: missing/expired ref" category ctx-id)
           acc)))
     {}
     ctx-refs)))

;; =============================================================================
;; Step 2: Gather KG Context
;; =============================================================================

(defn gather-kg-context
  "Traverse KG from seed node IDs, collecting structural edges.

   Arguments:
     kg-node-ids - Vector of KG node IDs to start traversal from
     scope       - Project scope string (e.g. \"hive-mcp\") for filtering

   Returns:
     {:nodes #{<unique node IDs>}
      :edges [{:from <id> :to <id> :relation <kw> :confidence <float>} ...]}

   Bounded: max-kg-depth traversal, max-kg-nodes total.
   Only follows structural-relations (not co-access noise)."
  [kg-node-ids scope]
  (when (seq kg-node-ids)
    (let [all-edges (atom [])
          all-nodes (atom (set kg-node-ids))
          ;; Traverse from each seed node
          _ (doseq [node-id kg-node-ids
                    :when (< (count @all-nodes) max-kg-nodes)]
              (try
                (let [results (kg-queries/traverse
                               node-id
                               {:direction  :both
                                :relations  structural-relations
                                :max-depth  max-kg-depth
                                :scope      scope})]
                  (doseq [{:keys [node-id edge]} results
                          :when (< (count @all-nodes) max-kg-nodes)]
                    (swap! all-nodes conj node-id)
                    (swap! all-edges conj
                           {:from       (:kg-edge/from edge)
                            :to         (:kg-edge/to edge)
                            :relation   (:kg-edge/relation edge)
                            :confidence (:kg-edge/confidence edge)})))
                (catch Exception e
                  (log/debug "gather-kg-context: traverse failed for" node-id
                             (.getMessage e)))))
          ;; Deduplicate edges by [from, to, relation]
          unique-edges (->> @all-edges
                           (group-by (juxt :from :to :relation))
                           vals
                           (map first))]
      {:nodes @all-nodes
       :edges (vec unique-edges)})))

;; =============================================================================
;; Step 3: Compress KG Subgraph
;; =============================================================================

(defn- relation->arrow
  "Convert a KG relation keyword to a compact arrow representation."
  [relation]
  (case relation
    :implements   "-impl->"
    :supersedes   "-super->"
    :depends-on   "-dep->"
    :refines      "-ref->"
    :contradicts  "-contra->"
    :derived-from "-from->"
    :applies-to   "-apply->"
    (str "-" (name relation) "->")))

(defn- truncate-id
  "Truncate a memory ID to first 8 chars for compact display.
   e.g. '20260207023915-34ca6b05' -> '20260207'"
  [id]
  (if (and (string? id) (> (count id) 8))
    (subs id 0 8)
    (str id)))

(defn compress-kg-subgraph
  "Render KG subgraph as compact edge-list representation.

   Arguments:
     kg-context - Output from gather-kg-context:
                  {:nodes #{...} :edges [{:from :to :relation :confidence}]}

   Returns:
     String with compact edge-list format (~200 tokens for 10 edges).
     Returns nil when no edges.

   Example output:
     KG Subgraph (5 nodes, 4 edges):
       20260207 -impl-> 20260206 [0.9]
       20260206 -dep-> 20260205 [1.0]"
  [kg-context]
  (when (and kg-context (seq (:edges kg-context)))
    (let [{:keys [nodes edges]} kg-context
          header (str "KG Subgraph (" (count nodes) " nodes, "
                      (count edges) " edges):")
          edge-lines (mapv (fn [{:keys [from to relation confidence]}]
                             (str "  " (truncate-id from)
                                  " " (relation->arrow relation) " "
                                  (truncate-id to)
                                  (when confidence
                                    (str " [" (format "%.1f" (double confidence)) "]"))))
                           edges)]
      (str/join "\n" (cons header edge-lines)))))

;; =============================================================================
;; Step 4: Render Compressed Context
;; =============================================================================

(defn- summarize-ref-category
  "Produce a compact summary line for a ref category's data.
   Handles vectors (lists of entries) and maps."
  [category data]
  (cond
    ;; Vector of entries - count + type
    (sequential? data)
    (str (name category) ": " (count data) " entries")

    ;; Map - show keys
    (map? data)
    (str (name category) ": {" (str/join ", " (map name (keys data))) "}")

    ;; Scalar - just stringify
    :else
    (str (name category) ": " (pr-str data))))

(defn- extract-key-content
  "Extract key content items from ref data for the compressed output.
   Prioritizes axioms and decisions as they are most valuable for agent context."
  [ref-data char-budget]
  (let [sections (atom [])
        remaining (atom char-budget)]

    ;; Axioms are highest priority - include content previews
    (when-let [axioms (:axioms ref-data)]
      (when (and (sequential? axioms) (pos? @remaining))
        (let [axiom-lines (for [a (take 10 axioms)
                                :let [content (cond
                                                (string? a) a
                                                (map? a) (or (:content a) (pr-str a))
                                                :else (pr-str a))
                                      line (str "- " (subs content 0 (min (count content) 120)))]
                                :when (pos? @remaining)]
                            (do (swap! remaining - (count line))
                                line))]
          (when (seq axiom-lines)
            (swap! sections conj (str "### Axioms (" (count axioms) " total)"))
            (swap! sections into axiom-lines)))))

    ;; Decisions - just count + IDs if available
    (when-let [decisions (:decisions ref-data)]
      (when (and (sequential? decisions) (pos? @remaining))
        (let [count-line (str "### Decisions: " (count decisions) " active")]
          (swap! remaining - (count count-line))
          (swap! sections conj count-line))))

    ;; Priority conventions - count
    (when-let [convs (:priority-conventions ref-data)]
      (when (and (sequential? convs) (pos? @remaining))
        (let [count-line (str "### Priority Conventions: " (count convs) " loaded")]
          (swap! remaining - (count count-line))
          (swap! sections conj count-line))))

    ;; Other categories - just counts
    (doseq [[cat data] (dissoc ref-data :axioms :decisions :priority-conventions)]
      (when (pos? @remaining)
        (let [summary (summarize-ref-category cat data)]
          (swap! remaining - (count summary))
          (swap! sections conj (str "### " summary)))))

    @sections))

(defn render-compressed-context
  "Combine ref data summaries + KG subgraph into bounded markdown.

   Arguments:
     ref-data   - Output from fetch-ref-data (category -> data)
     kg-context - Output from gather-kg-context ({:nodes :edges})

   Returns:
     String, bounded to max-output-chars.

   Structure:
     ## Reconstructed Context (KG-Compressed)
     [ref summaries with key content]
     [KG subgraph edge-list]"
  [ref-data kg-context]
  (let [parts (atom ["## Reconstructed Context (KG-Compressed)" ""])
        kg-str (compress-kg-subgraph kg-context)
        ;; Reserve space for KG subgraph
        kg-budget (if kg-str (+ (count kg-str) 10) 0)
        ref-budget (- max-output-chars kg-budget 100) ;; 100 for header/footer

        ;; Extract key content from refs
        ref-sections (when (seq ref-data)
                       (extract-key-content ref-data ref-budget))]

    ;; Add ref sections
    (when (seq ref-sections)
      (swap! parts into ref-sections)
      (swap! parts conj ""))

    ;; Add KG subgraph
    (when kg-str
      (swap! parts conj kg-str)
      (swap! parts conj ""))

    ;; Summary footer
    (let [ref-count (count ref-data)
          node-count (count (:nodes kg-context))
          edge-count (count (:edges kg-context))]
      (swap! parts conj
             (str "---\n*Context: " ref-count " ref categories"
                  (when (pos? node-count)
                    (str ", " node-count " KG nodes, " edge-count " edges"))
                  "*")))

    ;; Enforce budget
    (let [result (str/join "\n" @parts)]
      (if (> (count result) max-output-chars)
        (str (subs result 0 (- max-output-chars 20)) "\n...[truncated]")
        result))))

;; =============================================================================
;; Step 5: Top-Level Reconstruct Function
;; =============================================================================

(defn reconstruct-context
  "Top-level context reconstruction function.

   Matches RefContext reconstruct-fn signature:
     (fn [ctx-refs kg-node-ids scope] -> string)

   Pipeline:
     1. Fetch ref data from context-store
     2. Gather KG context via bounded traversal
     3. Render compressed context markdown

   Arguments:
     ctx-refs    - Map of category->ctx-id (from catchup dual-write)
     kg-node-ids - Vector of KG node IDs for graph traversal
     scope       - Project scope string

   Returns:
     Compressed context markdown string (max 3000 chars).
     Never throws - returns partial/empty context on failure.

   Usage:
     (->ref-context prompt {:reconstruct-fn reconstruct-context
                            :ctx-refs {...} :kg-node-ids [...] :scope \"hive-mcp\"})"
  [ctx-refs kg-node-ids scope]
  (try
    (let [ref-data  (fetch-ref-data ctx-refs)
          kg-ctx    (gather-kg-context kg-node-ids scope)
          rendered  (render-compressed-context ref-data kg-ctx)]
      (log/debug "reconstruct-context: refs=" (count ref-data)
                 "kg-nodes=" (count (:nodes kg-ctx))
                 "output-chars=" (count rendered))
      rendered)
    (catch Exception e
      (log/warn "reconstruct-context failed (graceful degradation):" (.getMessage e))
      ;; Return minimal context on failure
      (str "## Context (reconstruction failed)\n"
           "Refs available: " (str/join ", " (map name (keys ctx-refs))) "\n"
           "KG nodes: " (count kg-node-ids) "\n"
           "Error: " (.getMessage e)))))

;; =============================================================================
;; Bridge: Catchup Refs -> RefContext Options
;; =============================================================================

(defn catchup-refs->ref-context-opts
  "Convert catchup workflow output to RefContext factory options.

   Takes the context-refs map from catchup response and converts to
   opts suitable for ->ref-context. Also extracts KG node IDs from
   decisions/conventions in the context-store if available.

   Arguments:
     catchup-response - Map with :context-refs from catchup workflow
                        e.g. {:context-refs {:axioms \"ctx-1\" :decisions \"ctx-2\"}}
     scope            - Project scope string

   Returns:
     Map suitable as opts to ->ref-context:
     {:ctx-refs {...} :kg-node-ids [...] :scope \"...\" :reconstruct-fn <fn>}"
  [catchup-response scope]
  (let [ctx-refs (or (:context-refs catchup-response) {})
        ;; Try to extract KG node IDs from decision entries
        ;; Decisions often have IDs that are KG nodes
        kg-node-ids (when-let [decisions-ref (:decisions ctx-refs)]
                      (try
                        (when-let [entry (context-store/context-get decisions-ref)]
                          (let [data (:data entry)]
                            (when (sequential? data)
                              (->> data
                                   (keep (fn [d]
                                           (cond
                                             (string? d) nil  ;; Raw strings are not IDs
                                             (map? d) (:id d) ;; Entry maps have :id
                                             :else nil)))
                                   (take max-kg-nodes)
                                   vec))))
                        (catch Exception e
                          (log/debug "kg-node-ids extraction failed:" (.getMessage e))
                          nil)))]
    {:ctx-refs       ctx-refs
     :kg-node-ids    (or kg-node-ids [])
     :scope          scope
     :reconstruct-fn reconstruct-context}))
