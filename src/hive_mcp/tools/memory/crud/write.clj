(ns hive-mcp.tools.memory.crud.write
  "Write operations for memory: add entry with KG edge creation.

   SOLID: SRP - Handles only memory creation and KG edge wiring.
   CLARITY: L - Layers stay pure with clear domain separation.

   Handlers:
   - handle-add: Create new memory entry (with optional KG edge creation)

   Knowledge Graph Integration:
   - add supports kg_implements, kg_supersedes, kg_depends_on, kg_refines
   - Edges are stored in DataScript with full provenance

   Intelligence delegated to focused submodules:
   - classify.clj: Abstraction level auto-classification (P1.5)
   - gaps.clj: Knowledge gap auto-detection (P2.8)"
  (:require [hive-mcp.tools.memory.core :refer [with-chroma]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.memory.format :as fmt]
            [hive-mcp.tools.memory.duration :as dur]
            [hive-mcp.tools.memory.classify :as classify]
            [hive-mcp.tools.memory.gaps :as gaps]
            [hive-mcp.tools.core :refer [mcp-json mcp-error coerce-vec!]]
            [hive-mcp.chroma :as chroma]
            [hive-mcp.plans :as plans]
            [hive-mcp.plan.gate :as plan-gate]
            [hive-mcp.knowledge-graph.edges :as kg-edges]
            [hive-mcp.knowledge-graph.schema :as kg-schema]
            [hive-mcp.agent.context :as ctx]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Knowledge Graph Edge Creation
;; ============================================================

(defn- update-target-incoming!
  "Update target entry's kg-incoming field with the new edge ID.
   Appends to existing incoming edges if present."
  [target-id edge-id]
  (when-let [target-entry (chroma/get-entry-by-id target-id)]
    (let [existing-incoming (or (:kg-incoming target-entry) [])
          updated-incoming (conj existing-incoming edge-id)]
      (chroma/update-entry! target-id {:kg-incoming updated-incoming}))))

(defn- create-kg-edges!
  "Create KG edges for the given relationships.

   Arguments:
     entry-id    - The newly created memory entry ID
     kg-params   - Map with :kg_implements, :kg_supersedes, :kg_depends_on, :kg_refines
     project-id  - Project scope for edge attribution
     agent-id    - Agent creating the edges (for attribution)

   Returns:
     Vector of created edge IDs

   Side effects:
     - Creates edges in DataScript KG store
     - Updates target entries' kg-incoming field in Chroma (bidirectional lookup)"
  [entry-id {:keys [kg_implements kg_supersedes kg_depends_on kg_refines]} project-id agent-id]
  (let [created-by (when agent-id (str "agent:" agent-id))
        create-edges (fn [targets relation]
                       (when (seq targets)
                         (mapv (fn [target-id]
                                 (let [edge-id (kg-edges/add-edge!
                                                {:from entry-id
                                                 :to target-id
                                                 :relation relation
                                                 :scope project-id
                                                 :confidence 1.0
                                                 :created-by created-by})]
                                   ;; Update target's kg-incoming for bidirectional lookup
                                   (update-target-incoming! target-id edge-id)
                                   edge-id))
                               targets)))]
    (vec (concat
          (create-edges kg_implements :implements)
          (create-edges kg_supersedes :supersedes)
          (create-edges kg_depends_on :depends-on)
          (create-edges kg_refines :refines)))))

;; ============================================================
;; Tag Building
;; ============================================================

(defn- build-entry-tags
  "Build complete tags vector: base → agent → KG markers → scope."
  [tags-vec agent-id kg-vecs project-id]
  (let [agent-tag (when agent-id (str "agent:" agent-id))
        tags-with-agent (if agent-tag (conj tags-vec agent-tag) tags-vec)
        {:keys [kg-implements-vec kg-supersedes-vec kg-depends-on-vec kg-refines-vec]} kg-vecs
        kg-tags (cond-> []
                  (seq kg-implements-vec) (conj "kg:has-implements")
                  (seq kg-supersedes-vec) (conj "kg:has-supersedes")
                  (seq kg-depends-on-vec) (conj "kg:has-depends-on")
                  (seq kg-refines-vec) (conj "kg:has-refines"))
        tags-with-kg (into tags-with-agent kg-tags)]
    (scope/inject-project-scope tags-with-kg project-id)))

;; ============================================================
;; Plan Gate Validation
;; ============================================================

(defn- validate-plan-gate!
  "FSM Gate: validate plan content before storage.
   Ensures plan-to-kanban compatibility at write time.
   Throws ex-info on gate rejection."
  [content]
  (when (plan-gate/plan-content? content)
    (let [gate-result (plan-gate/validate-for-storage content)]
      (when-not (:valid? gate-result)
        (throw (ex-info (plan-gate/format-gate-error gate-result)
                        {:type :plan-gate-rejected
                         :phase (:phase gate-result)
                         :errors (:errors gate-result)}))))))

;; ============================================================
;; Entry Indexing
;; ============================================================

(defn- index-entry!
  "Index entry in appropriate collection (plans or memory).
   Plans route to OpenRouter 4096-dim collection, memory to Ollama 768-dim."
  [plan? {:keys [type content tags-with-scope content-hash duration-str
                 expires project-id abstraction-level knowledge-gaps agent-id]}]
  (if plan?
    (plans/index-plan!
     {:type type :content content :tags tags-with-scope
      :content-hash content-hash :duration duration-str
      :expires (or expires "") :project-id project-id
      :abstraction-level abstraction-level
      :knowledge-gaps knowledge-gaps :agent-id agent-id})
    (chroma/index-memory-entry!
     {:type type :content content :tags tags-with-scope
      :content-hash content-hash :duration duration-str
      :expires (or expires "") :project-id project-id
      :abstraction-level abstraction-level
      :knowledge-gaps knowledge-gaps})))

;; ============================================================
;; Post-Index Finalization
;; ============================================================

(defn- finalize-entry!
  "Wire KG edges, fetch created entry, notify Olympus, format response."
  [entry-id plan? kg-params project-id agent-id
   {:keys [tags-with-scope type knowledge-gaps]}]
  (let [edge-ids (create-kg-edges! entry-id kg-params project-id agent-id)
        _ (when (and (seq edge-ids) (not plan?))
            (chroma/update-entry! entry-id {:kg-outgoing edge-ids}))
        created (if plan? (plans/get-plan entry-id) (chroma/get-entry-by-id entry-id))]
    (log/info "Created memory entry:" entry-id
              (when (seq edge-ids) (str " with " (count edge-ids) " KG edges"))
              (when (seq knowledge-gaps) (str " gaps:" (count knowledge-gaps))))
    (try
      (when-let [publish-fn (requiring-resolve 'hive-mcp.channel/publish!)]
        (publish-fn {:type :memory-added :id entry-id :memory-type type
                     :tags tags-with-scope :project-id project-id}))
      (catch Exception _ nil))
    (mcp-json (cond-> (fmt/entry->json-alist created)
                (seq edge-ids) (assoc :kg_edges_created edge-ids)))))

;; ============================================================
;; Add Handler
;; ============================================================

(defn handle-add
  "Add an entry to project memory (Chroma-only storage).
   Stores full entry in Chroma with content, metadata, and embedding.

   When directory is provided, uses that path to determine project scope
   instead of relying on Emacs's current buffer (fixes /wrap scoping issue).

   When agent_id is provided (or CLAUDE_SWARM_SLAVE_ID env var is set),
   adds an 'agent:{id}' tag for tracking which ling created the entry.

   Knowledge Graph Integration:
   When kg_* parameters are provided, creates corresponding KG edges:
     - kg_implements: List of entry IDs this implements
     - kg_supersedes: List of entry IDs this supersedes
     - kg_depends_on: List of entry IDs this depends on
     - kg_refines: List of entry IDs this refines

   Edges are stored in DataScript with full provenance (scope, agent, timestamp).

   ELM Principle: Array parameters are coerced with helpful error messages."
  [{:keys [type content tags duration directory agent_id
           kg_implements kg_supersedes kg_depends_on kg_refines abstraction_level]}]
  (try
    (if (and abstraction_level (not (kg-schema/valid-abstraction-level? abstraction_level)))
      (mcp-error (str "Invalid abstraction_level: " abstraction_level))
      (let [tags-vec (coerce-vec! tags :tags [])
            kg-vecs {:kg-implements-vec (coerce-vec! kg_implements :kg_implements [])
                     :kg-supersedes-vec (coerce-vec! kg_supersedes :kg_supersedes [])
                     :kg-depends-on-vec (coerce-vec! kg_depends_on :kg_depends_on [])
                     :kg-refines-vec    (coerce-vec! kg_refines :kg_refines [])}
            directory (or directory (ctx/current-directory))
            abstraction-level (or abstraction_level
                                  (classify/classify-abstraction-level type content tags-vec))
            knowledge-gaps (gaps/extract-knowledge-gaps content)]
        (log/info "mcp-memory-add:" type "directory:" directory "agent_id:" agent_id)
        (with-chroma
          (let [project-id (scope/get-current-project-id directory)
                agent-id (or agent_id (ctx/current-agent-id)
                             (System/getenv "CLAUDE_SWARM_SLAVE_ID"))
                tags-with-scope (build-entry-tags tags-vec agent-id kg-vecs project-id)
                content-hash (chroma/content-hash content)
                duration-str (or duration "long")
                expires (dur/calculate-expires duration-str)
                existing (chroma/find-duplicate type content-hash :project-id project-id)]
            (if existing
              (let [merged-tags (distinct (concat (:tags existing) tags-with-scope))
                    updated (chroma/update-entry! (:id existing) {:tags merged-tags})]
                (log/info "Duplicate found, merged tags:" (:id existing))
                (mcp-json (fmt/entry->json-alist updated)))
              (let [plan? (= type "plan")
                    _ (when plan? (validate-plan-gate! content))
                    entry-ctx {:type type :content content :tags-with-scope tags-with-scope
                               :content-hash content-hash :duration-str duration-str
                               :expires expires :project-id project-id
                               :abstraction-level abstraction-level
                               :knowledge-gaps knowledge-gaps :agent-id agent-id}
                    entry-id (index-entry! plan? entry-ctx)
                    kg-params {:kg_implements (:kg-implements-vec kg-vecs)
                               :kg_supersedes (:kg-supersedes-vec kg-vecs)
                               :kg_depends_on (:kg-depends-on-vec kg-vecs)
                               :kg_refines    (:kg-refines-vec kg-vecs)}]
                (finalize-entry! entry-id plan? kg-params project-id agent-id entry-ctx)))))))
    (catch clojure.lang.ExceptionInfo e
      (if (#{:coercion-error :embedding-too-long :plan-gate-rejected} (:type (ex-data e)))
        (mcp-error (.getMessage e))
        (throw e)))))
