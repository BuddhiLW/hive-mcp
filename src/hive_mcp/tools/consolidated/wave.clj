(ns hive-mcp.tools.consolidated.wave
  "Consolidated Wave CLI tool for drone wave operations."
  (:require [hive-mcp.tools.cli :refer [make-cli-handler]]
            [hive-mcp.tools.swarm :as swarm-handlers]
            [hive-mcp.tools.diff :as diff-handlers]
            [taoensso.timbre :as log]))

(defn- handle-dispatch-unified
  "Route to validated or plain dispatch based on validate param."
  [{:keys [validate] :as params}]
  (if validate
    (swarm-handlers/handle-dispatch-validated-wave params)
    (swarm-handlers/handle-dispatch-drone-wave params)))

(defn- handle-dispatch-validated-shim
  "DEPRECATED: Use dispatch with validate:true instead."
  [params]
  (log/warn {:event :deprecation-warning
             :command "dispatch-validated"
             :message "DEPRECATED: Use 'dispatch' with validate:true instead."})
  (handle-dispatch-unified (assoc params :validate true)))

(def handlers
  {:dispatch           handle-dispatch-unified
   :dispatch-validated handle-dispatch-validated-shim
   :status             swarm-handlers/handle-get-wave-status
   :review             diff-handlers/handle-review-wave-diffs
   :approve            diff-handlers/handle-approve-wave-diffs
   :reject             diff-handlers/handle-reject-wave-diffs
   :auto-approve       diff-handlers/handle-auto-approve-wave-diffs})

(def handle-wave
  (make-cli-handler handlers))

(def tool-def
  {:name "wave"
   :consolidated true
   :description "Drone wave operations: dispatch (parallel drones, use validate:true for lint), dispatch-validated (deprecated, use dispatch+validate), status (execution progress), review (see proposed diffs), approve (apply diffs), reject (discard diffs), auto-approve (safe diffs only). Use command='help' to list all."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"
                                         :enum ["dispatch" "dispatch-validated" "status" "review" "approve" "reject" "auto-approve" "help"]
                                         :description "Wave operation to perform"}
                              "tasks" {:type "array"
                                       :items {:type "object"
                                               :properties {"file" {:type "string"}
                                                            "task" {:type "string"}}
                                               :required ["file" "task"]}
                                       :description "Array of {file, task} objects"}
                              "preset" {:type "string"
                                        :description "Drone preset (default: drone-worker)"}
                              "trace" {:type "boolean"
                                       :description "Emit progress events"}
                              "cwd" {:type "string"
                                     :description "Working directory for path resolution"}
                              "validate" {:type "boolean"
                                          :description "Run clj-kondo lint after execution"}
                              "max_retries" {:type "integer"
                                             :description "Max retry iterations"}
                              "lint_level" {:type "string"
                                            :enum ["error" "warning" "info"]
                                            :description "Lint severity threshold"}
                              "model" {:type "string"
                                       :description "Override model for drones (e.g. 'deepseek/deepseek-v3.2'). Bypasses smart routing."}
                              "mode" {:type "string"
                                      :enum ["delegate" "agentic"]
                                      :description "Execution mode: 'delegate' (default, external fn) or 'agentic' (in-process loop with session store)"}
                              "backend" {:type "string"
                                         :enum ["openrouter" "hive-agent" "legacy-loop" "sdk-drone" "fsm-agentic"]
                                         :description "Drone execution backend (default: openrouter for delegate mode, fsm-agentic for agentic mode)"}
                              "seeds" {:type "array"
                                       :items {:type "string"}
                                       :description "Domain topic seeds for context priming (e.g. [\"fp\", \"ddd\"]). Injects relevant domain knowledge into drone prompts."}
                              "ctx_refs" {:type "object"
                                          :description "Map of category->ctx-id for compressed context. When provided, creates RefContext (token-efficient vs text). E.g. {\"axioms\": \"ctx-123\", \"decisions\": \"ctx-456\"}"}
                              "kg_node_ids" {:type "array"
                                             :items {:type "string"}
                                             :description "Node IDs for context resolution seeds. Combined with ctx_refs for structural context reconstruction."}
                              "wave_id" {:type "string"
                                         :description "Wave ID to operate on"}
                              "diff_ids" {:type "array"
                                          :items {:type "string"}
                                          :description "Specific diff IDs to approve"}
                              "reason" {:type "string"
                                        :description "Reason for rejection"}}
                 :required ["command"]}
   :handler handle-wave})

(def tools [tool-def])
