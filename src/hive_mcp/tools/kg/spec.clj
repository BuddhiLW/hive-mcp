(ns hive-mcp.tools.kg.spec
  "clojure.spec contracts for KG tool handlers.

   Applies type-theoretic best practices via spec:
   - Domain primitive specs (node-id, relation, confidence, scope, direction)
   - Handler input parameter map specs (per-handler validation contracts)
   - fdef contracts for all 22 public handler functions + 2 helpers
   - MCP response sum type (success | error | validation-error)
   - Instrumentation helpers for dev/test

   Follows the pattern established in hive-mcp.dns.result.spec:
   - Smart constructor validation
   - Sum type discrimination
   - instrument!/unstrument! for runtime contract checking

   Four handler groups:
   1. Queries    (7 handlers) — read-only graph operations
   2. Commands   (4 handlers) — write/mutate graph operations
   3. Versioning (7 handlers) — Yggdrasil branch/merge operations
   4. Migration  (4 handlers) — backend migration operations

   References:
   - hive-mcp.dns.result.spec (exemplar pattern)
   - hive-mcp.knowledge-graph.schema (domain types)
   - MCP Protocol Specification (handler response format)"
  (:require [clojure.spec.alpha :as s]
            [hive-mcp.tools.kg :as kg]
            [hive-mcp.tools.kg.queries :as kg-queries]
            [hive-mcp.tools.kg.commands :as kg-commands]
            [hive-mcp.knowledge-graph.schema :as schema]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Primitive Domain Specs
;; =============================================================================

;; Node IDs: non-empty strings (memory entry IDs, UUIDs, or timestamp-based)
(s/def ::node-id (s/and string? seq))

;; Relation type keyword: member of schema/relation-types set
(s/def ::relation-keyword schema/relation-types)

;; Relation as string input from MCP (JSON always sends strings)
(s/def ::relation-string
  (s/and string? seq #(contains? schema/relation-types (keyword %))))

;; Relation input: MCP sends strings, internal code may use keywords
(s/def ::relation-input
  (s/or :string ::relation-string
        :keyword ::relation-keyword))

;; Confidence score: number in closed interval [0.0, 1.0]
(s/def ::confidence (s/and number? #(<= 0.0 % 1.0)))

;; Scope: non-empty string (e.g., "hive-mcp", "scope:project:hive-mcp")
(s/def ::scope-str (s/and string? seq))

;; Direction for graph traversal
(s/def ::direction #{"outgoing" "incoming" "both"})

;; Positive integer (max-depth, limit)
(s/def ::pos-int (s/and integer? pos?))

;; Non-negative integer (expected counts)
(s/def ::nat-int (s/and integer? #(>= % 0)))

;; Agent/creator ID: non-empty string
(s/def ::agent-id (s/and string? seq))

;; File path: non-empty string
(s/def ::file-path (s/and string? seq))

;; Backend type: supported KG storage backends
(s/def ::backend-type #{"datascript" "datalevin" "datahike"})

;; Branch name: non-empty string
(s/def ::branch-name (s/and string? seq))

;; =============================================================================
;; MCP Response Sum Type
;; =============================================================================
;;
;; Handlers return one of three response shapes:
;;
;;   1. MCP success:    {:type "text" :text "<json-string>"}
;;   2. MCP error:      {:type "text" :text "<message>" :isError true}
;;   3. Validation err: {:error "<message>"}  (from validate-node-id pattern)
;;
;; Shape (3) is a legacy pattern from queries.clj where validate-node-id
;; returns {:error msg} directly via the (or (validate ...) (mcp-json ...))
;; pattern. New code should prefer mcp-error for consistency.

(s/def ::mcp-text-response
  (s/and map?
         #(= "text" (:type %))
         #(string? (:text %))))

(s/def ::mcp-success
  (s/and ::mcp-text-response
         #(not (:isError %))))

(s/def ::mcp-error
  (s/and ::mcp-text-response
         #(true? (:isError %))))

(s/def ::validation-error
  (s/and map?
         #(contains? % :error)
         #(string? (:error %))))

(s/def ::handler-response
  (s/or :success    ::mcp-success
        :error      ::mcp-error
        :validation ::validation-error))

;; =============================================================================
;; Unqualified Key Specs (for s/keys :req-un / :opt-un)
;; =============================================================================

;; Query params
(s/def ::start_node  ::node-id)
(s/def ::node_id     ::node-id)
(s/def ::from_node   ::node-id)
(s/def ::to_node     ::node-id)
(s/def ::max_depth   ::pos-int)
(s/def ::limit       ::pos-int)
(s/def ::scope       (s/nilable ::scope-str))

;; Command params
(s/def ::from        (s/and string? seq))  ;; node-id OR branch name (dual use)
(s/def ::to          ::node-id)
(s/def ::relation    ::relation-input)
(s/def ::created_by  ::agent-id)
(s/def ::edge_id     (s/and string? seq))
(s/def ::to_scope    ::scope-str)
(s/def ::entry_id    ::node-id)
(s/def ::force       boolean?)
(s/def ::project_id  (s/and string? seq))
(s/def ::max_age_days ::pos-int)

;; Versioning params
(s/def ::name        ::branch-name)
(s/def ::source      (s/and string? seq))  ;; branch name or snapshot UUID

;; Migration params
(s/def ::path             ::file-path)
(s/def ::source_backend   ::backend-type)
(s/def ::target_backend   ::backend-type)
(s/def ::dry_run          boolean?)
(s/def ::export_path      ::file-path)
(s/def ::target_db_path   ::file-path)
(s/def ::expected_edges     ::nat-int)
(s/def ::expected_disc      ::nat-int)
(s/def ::expected_synthetic ::nat-int)

;; =============================================================================
;; Query Handler Input Parameter Specs
;; =============================================================================

(s/def ::traverse-params
  (s/keys :req-un [::start_node]
          :opt-un [::direction ::max_depth ::scope]))

(s/def ::impact-params
  (s/keys :req-un [::node_id]
          :opt-un [::max_depth ::scope]))

(s/def ::find-path-params
  (s/keys :req-un [::from_node ::to_node]
          :opt-un [::direction ::max_depth ::scope]))

(s/def ::subgraph-params
  (s/keys :req-un [::scope]))

(s/def ::contradictions-params
  (s/keys :opt-un [::scope]))

(s/def ::node-context-params
  (s/keys :req-un [::node_id]))

(s/def ::stats-params map?)

;; =============================================================================
;; Command Handler Input Parameter Specs
;; =============================================================================

(s/def ::add-edge-params
  (s/keys :req-un [::from ::to ::relation]
          :opt-un [::scope ::confidence ::created_by]))

(s/def ::promote-params
  (s/keys :req-un [::edge_id ::to_scope]))

(s/def ::reground-params
  (s/keys :req-un [::entry_id]
          :opt-un [::force]))

(s/def ::backfill-params
  (s/keys :opt-un [::project_id ::limit ::force ::max_age_days]))

;; =============================================================================
;; Versioning Handler Input Parameter Specs
;; =============================================================================

(s/def ::branch-params
  (s/keys :req-un [::name]
          :opt-un [::from]))

(s/def ::checkout-params
  (s/keys :req-un [::name]))

(s/def ::branches-params map?)

(s/def ::snapshot-id-params map?)

(s/def ::history-params
  (s/keys :opt-un [::limit]))

(s/def ::merge-params
  (s/keys :req-un [::source]))

(s/def ::versioning-status-params map?)

;; =============================================================================
;; Migration Handler Input Parameter Specs
;; =============================================================================

(s/def ::migrate-params
  (s/keys :req-un [::source_backend ::target_backend]
          :opt-un [::dry_run ::export_path ::target_db_path]))

(s/def ::export-params
  (s/keys :req-un [::path]))

(s/def ::import-params
  (s/keys :req-un [::path]))

(s/def ::validate-migration-params
  (s/keys :opt-un [::expected_edges ::expected_disc ::expected_synthetic]))

;; =============================================================================
;; Tool Definition Structural Spec
;; =============================================================================

(s/def ::tool-definition
  (s/and map?
         #(string? (:name %))
         #(string? (:description %))
         #(map? (:inputSchema %))
         #(= "object" (get-in % [:inputSchema :type]))
         #(fn? (:handler %))))

;; =============================================================================
;; Helper Function Contracts
;; =============================================================================

;; validate-node-id: (any?, string?) -> (nilable {:error string?})
;; Returns nil on valid input, {:error msg} on invalid input.
(s/fdef kg-queries/validate-node-id
  :args (s/cat :node-id any? :param-name string?)
  :ret  (s/nilable ::validation-error)
  :fn   (fn [{:keys [args ret]}]
          (if (and (string? (:node-id args))
                   (seq (:node-id args)))
            (nil? ret)
            (some? ret))))

;; parse-relations-filter: any? -> (nilable set-of-keywords)
;; Normalizes relation filter input to keyword set (or nil).
(s/fdef kg-queries/parse-relations-filter
  :args (s/cat :relations any?)
  :ret  (s/nilable (s/coll-of keyword? :kind set?)))

;; =============================================================================
;; Query Handler fdef Contracts
;; =============================================================================

(s/fdef kg-queries/handle-kg-traverse
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg-queries/handle-kg-impact-analysis
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg-queries/handle-kg-find-path
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg-queries/handle-kg-subgraph
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg-queries/handle-kg-contradictions
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg-queries/handle-kg-node-context
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg-queries/handle-kg-stats
  :args (s/cat :params map?)
  :ret  ::handler-response)

;; =============================================================================
;; Command Handler fdef Contracts
;; =============================================================================

(s/fdef kg-commands/handle-kg-add-edge
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg-commands/handle-kg-promote
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg-commands/handle-kg-reground
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg-commands/handle-kg-backfill-grounding
  :args (s/cat :params map?)
  :ret  ::handler-response)

;; =============================================================================
;; Versioning Handler fdef Contracts
;; =============================================================================

(s/fdef kg/handle-kg-branch
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg/handle-kg-checkout
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg/handle-kg-branches
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg/handle-kg-snapshot-id
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg/handle-kg-history
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg/handle-kg-merge
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg/handle-kg-versioning-status
  :args (s/cat :params map?)
  :ret  ::handler-response)

;; =============================================================================
;; Migration Handler fdef Contracts
;; =============================================================================

(s/fdef kg/handle-kg-migrate
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg/handle-kg-export
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg/handle-kg-import
  :args (s/cat :params map?)
  :ret  ::handler-response)

(s/fdef kg/handle-kg-validate-migration
  :args (s/cat :params map?)
  :ret  ::handler-response)

;; =============================================================================
;; Instrumentation Helpers
;; =============================================================================

(def ^:private all-instrumentable-syms
  "All public function symbols that can be instrumented."
  [;; Query helpers
   `kg-queries/validate-node-id
   `kg-queries/parse-relations-filter
   ;; Query handlers
   `kg-queries/handle-kg-traverse
   `kg-queries/handle-kg-impact-analysis
   `kg-queries/handle-kg-find-path
   `kg-queries/handle-kg-subgraph
   `kg-queries/handle-kg-contradictions
   `kg-queries/handle-kg-node-context
   `kg-queries/handle-kg-stats
   ;; Command handlers
   `kg-commands/handle-kg-add-edge
   `kg-commands/handle-kg-promote
   `kg-commands/handle-kg-reground
   `kg-commands/handle-kg-backfill-grounding
   ;; Versioning handlers
   `kg/handle-kg-branch
   `kg/handle-kg-checkout
   `kg/handle-kg-branches
   `kg/handle-kg-snapshot-id
   `kg/handle-kg-history
   `kg/handle-kg-merge
   `kg/handle-kg-versioning-status
   ;; Migration handlers
   `kg/handle-kg-migrate
   `kg/handle-kg-export
   `kg/handle-kg-import
   `kg/handle-kg-validate-migration])

(defn instrument!
  "Enable spec checking on all KG tool handler functions.
   Use in dev/test, not production (performance cost).

   Instruments 24 functions:
   - 2 helpers (validate-node-id, parse-relations-filter)
   - 7 query handlers
   - 4 command handlers
   - 7 versioning handlers
   - 4 migration handlers"
  []
  (require '[clojure.spec.test.alpha :as stest])
  ((resolve 'clojure.spec.test.alpha/instrument)
   all-instrumentable-syms))

(defn unstrument!
  "Disable spec checking (restore production performance)."
  []
  (require '[clojure.spec.test.alpha :as stest])
  ((resolve 'clojure.spec.test.alpha/unstrument)
   all-instrumentable-syms))
