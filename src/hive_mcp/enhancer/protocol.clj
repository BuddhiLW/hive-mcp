(ns hive-mcp.enhancer.protocol
  "IKnowledgeEnhancer - Bridge to proprietary knowledge enhancement.

   CLARITY-L: Layers stay pure - this is the port (interface).
   Implementation lives in proprietary hive-knowledge repo.

   This is the primary integration point between open hive-mcp
   and closed hive-knowledge. The protocol defines AI-powered
   knowledge enhancement capabilities that require ML/GNN models.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol IKnowledgeEnhancer
  "Protocol for AI-powered knowledge enhancement.

   Provides semantic query enhancement, trust computation,
   emergence detection, and cross-pollination suggestions.

   Implementations:
   - BasicEnhancer: No-op fallback (open source, in this repo)
   - ProprietaryEnhancer: Full ML/GNN implementation (hive-knowledge repo)

   Usage:
   1. Call (load-enhancer) at system startup
   2. Use (get-enhancer) to access the active implementation
   3. If hive-knowledge is on classpath, ProprietaryEnhancer loads
   4. Otherwise, BasicEnhancer provides safe no-op defaults"

  (enhance-query [this query opts]
    "Enhance a query with semantic context.

     Uses embedding models to expand query with related concepts,
     synonyms, and domain-specific terms.

     Parameters:
       query - The original query string or structured query map
       opts  - Enhancement options:
               :scope      - Project scope for context (optional)
               :max-terms  - Max expansion terms (default 10)
               :threshold  - Similarity threshold 0.0-1.0 (default 0.7)

     Returns:
       Enriched query map with:
       :original    - The input query
       :expanded    - Query with semantic expansions
       :terms       - Vector of expansion terms with scores
       :embeddings  - Vector embeddings if available (nil in BasicEnhancer)")

  (compute-trust [this entry]
    "Compute trust score for a memory entry using ML models.

     Analyzes entry content, source, access patterns, and validation
     history to compute a trust score.

     Parameters:
       entry - Memory entry map with at least :id, :content, :type

     Returns:
       Trust score 0.0-1.0 where:
       0.0-0.3 - Low trust (unverified, inconsistent)
       0.3-0.7 - Medium trust (some validation)
       0.7-1.0 - High trust (verified, consistent, grounded)

     BasicEnhancer returns 0.5 (neutral) for all entries.")

  (detect-emergence [this scope]
    "Detect emergent patterns using GNN analysis.

     Analyzes the knowledge graph structure to find:
     - Clusters of related concepts forming new abstractions
     - Cross-project patterns that indicate reusable knowledge
     - Structural similarities suggesting hidden relationships

     Parameters:
       scope - Scope to analyze (project-id, :global, or nil for all)

     Returns:
       Vector of emergence candidates:
       [{:type       :cluster|:cross-project|:structural
         :confidence 0.0-1.0
         :entries    [entry-ids involved]
         :suggested  {:type :convention|:decision|:axiom
                      :content \"Proposed synthesis...\"
                      :rationale \"Why this emerged...\"}}]

     BasicEnhancer returns empty vector [].")

  (suggest-cross-pollination [this entry-id]
    "Suggest cross-project knowledge sharing opportunities.

     Identifies entries in other projects that could benefit from
     or contribute to the given entry's knowledge.

     Parameters:
       entry-id - ID of the source entry

     Returns:
       Vector of suggestions:
       [{:target-project \"project-id\"
         :target-entry   \"entry-id\" or nil if new
         :action         :share|:merge|:link|:promote
         :confidence     0.0-1.0
         :rationale      \"Why this cross-pollination...\"}]

     BasicEnhancer returns empty vector [].")

  (extract-learnings [this exploration-result]
    "Extract structured learnings from exploration results.

     Analyzes code exploration, debugging sessions, or research
     results to extract reusable knowledge.

     Parameters:
       exploration-result - Map with exploration data:
         :files-read    - Files examined
         :patterns      - Code patterns found
         :decisions     - Decisions made
         :problems      - Problems encountered
         :solutions     - Solutions applied

     Returns:
       Vector of extracted learnings:
       [{:type        :convention|:snippet|:decision|:note
         :content     \"The learning...\"
         :confidence  0.0-1.0
         :tags        [suggested tags]
         :duration    :ephemeral|:short|:medium|:long|:permanent
         :scope       suggested scope}]

     BasicEnhancer returns empty vector []."))

;; =============================================================================
;; Active Enhancer Management
;; =============================================================================

(defonce ^:private active-enhancer (atom nil))

(defn set-enhancer!
  "Set the active knowledge enhancer implementation.
   Called during system initialization by loader."
  [enhancer]
  {:pre [(satisfies? IKnowledgeEnhancer enhancer)]}
  (reset! active-enhancer enhancer))

(defn get-enhancer
  "Get the active knowledge enhancer.
   Throws if no enhancer has been loaded."
  []
  (or @active-enhancer
      (throw (ex-info "No knowledge enhancer configured. Call load-enhancer first."
                      {:hint "Use hive-mcp.enhancer.loader/load-enhancer"}))))

(defn enhancer-set?
  "Check if an enhancer has been configured."
  []
  (some? @active-enhancer))
