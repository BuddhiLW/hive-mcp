(ns hive-mcp.tools.agora
  "MCP tools for Agora multi-ling dialogue system.

   Exposes Nash Equilibrium dialogue infrastructure as MCP tools:
   - agora_create_dialogue: Create a new dialogue session (ling-based)
   - agora_create_debate: Create auto-orchestrated drone debate
   - agora_dispatch: Send message within dialogue (with signal parsing)
   - agora_check_consensus: Check Nash equilibrium status
   - agora_list_dialogues: List all dialogues
   - agora_join_dialogue: Add participant to dialogue
   - agora_debate_status: Get drone debate status

   Result DSL: Internal logic returns Result maps ({:ok val} or {:error category}).
   Single try-result boundary at each handler level. Zero nested try-catch."

  (:require [hive-mcp.dns.result :as result]
            [hive-mcp.tools.core :refer [mcp-json mcp-error]]
            [hive-mcp.agora.dialogue :as dialogue]
            [hive-mcp.agora.debate :as debate]
            [hive-mcp.agora.consensus :as consensus]
            [hive-mcp.agora.schema :as schema]
            [hive-mcp.agora.stages :as stages]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ============================================================
;; Result DSL Helpers (boundary pattern — mirrors tools/kg.clj)
;; ============================================================

(defn- try-result
  "Execute thunk f returning Result; catch unexpected exceptions as error Result.
   Catches ExceptionInfo (structured) and Exception (generic) separately."
  [category f]
  (try
    (f)
    (catch clojure.lang.ExceptionInfo e
      (log/warn (name category) ":" (ex-message e) (ex-data e))
      (result/err category {:message (ex-message e)}))
    (catch Exception e
      (log/error e (str (name category) " failed"))
      (result/err category {:message (.getMessage e)}))))

(defn- result->mcp
  "Convert Result to MCP response.
   {:ok data} -> (mcp-json data), {:error ...} -> (mcp-error message)."
  [r]
  (if (result/ok? r)
    (mcp-json (:ok r))
    (mcp-error (or (:message r) (str (:error r))))))

;; ============================================================
;; Validation Helpers (return Results)
;; ============================================================

(defn- require-param
  "Validate a parameter is non-nil. Returns Result."
  [value param-name]
  (if (nil? value)
    (result/err :agora/validation-failed
                {:message (str "Missing required parameter: " param-name)})
    (result/ok value)))

(defn- require-min-count
  "Validate a collection has at least n items. Returns Result."
  [coll n param-name]
  (if (or (nil? coll) (< (count coll) n))
    (result/err :agora/validation-failed
                {:message (str param-name " requires at least " n " items")})
    (result/ok coll)))

(defn- require-dialogue
  "Validate dialogue_id exists and fetch dialogue. Returns Result with dialogue."
  [dialogue-id]
  (result/let-ok [_ (require-param dialogue-id "dialogue_id")]
                 (if-let [d (schema/get-dialogue dialogue-id)]
                   (result/ok d)
                   (result/err :agora/not-found
                               {:message (str "Dialogue not found: " dialogue-id)}))))

;; ============================================================
;; Conversion Helpers (pure, zero branching at call site)
;; ============================================================

(defn- safe-name
  "Keyword/string -> string name, nil -> fallback."
  ([x] (safe-name x "unknown"))
  ([x fallback]
   (cond
     (keyword? x) (name x)
     (string? x)  x
     :else         fallback)))

(defn- safe-kw
  "String -> keyword, nil -> nil."
  [x]
  (when x (keyword x)))

(defn- ensure-vec [x]
  (if (vector? x) x (vec x)))

(defn- kw-or-str
  "Get value by keyword key or string key from map."
  [m k]
  (or (get m k) (get m (clojure.core/name k))))

(defn- truncate-preview
  "Truncate a string to max-len chars with ellipsis."
  [s max-len]
  (when s
    (if (> (count s) max-len)
      (str (subs s 0 max-len) "...")
      s)))

(defn- result-from-nullable
  "Wrap nullable value as Result: non-nil -> ok, nil -> err."
  [value err-category err-msg]
  (if value
    (result/ok value)
    (result/err err-category {:message err-msg})))

(defn- build-participant-status
  "Build per-participant equilibrium status vector (pure)."
  [dialogue-id participants]
  (mapv (fn [p]
          (let [t (consensus/last-turn-for dialogue-id p)
                s (:signal t)]
            {:participant      p
             :short-name       (consensus/extract-short-name p)
             :last-signal      (safe-name s nil)
             :in-equilibrium?  (boolean
                                (when s
                                  (contains? consensus/equilibrium-signals s)))}))
        participants))

(defn- format-turn
  "Format a single dialogue turn for output (pure)."
  [t]
  {:turn-num  (:turn-num t)
   :from      (:sender t)
   :to        (:receiver t)
   :signal    (safe-name (:signal t) nil)
   :message   (:message t)
   :timestamp (when-let [ts (:timestamp t)] (.getTime ts))})

(defn- enrich-dialogue-summary
  "Enrich a raw dialogue entry with turn count and participants (pure)."
  [d]
  (let [turns (schema/get-turns (:id d))
        dlg   (schema/get-dialogue (:id d))]
    {:id           (:id d)
     :topic        (:name d)
     :status       (safe-name (:status d))
     :participants (vec (or (:participants dlg) []))
     :turn-count   (count turns)
     :created      (:created d)}))

(defn- coerce-role-map
  "Coerce a role map from JSON (string keys) or EDN (keyword keys).
   key-pairs: [[out-key & lookup-keys] ...] where lookup-keys are tried in order."
  [r key-pairs]
  (reduce (fn [m [out-k & in-ks]]
            (assoc m out-k (some #(kw-or-str r %) in-ks)))
          {} key-pairs))

;; ============================================================
;; Pure Logic (Result-returning functions)
;; ============================================================

(defn- create-dialogue* [{:keys [participants topic]}]
  (let [pvec  (ensure-vec participants)
        topic (or topic "Unspecified dialogue")
        id    (dialogue/create-dialogue {:participants pvec :topic topic})]
    (log/info "Created Agora dialogue:" id "with participants:" pvec)
    (result/ok {:dialogue-id id :participants pvec :topic topic :status "active"})))

(defn- dispatch* [{:keys [dialogue_id to message from timeout_ms files signal]}]
  (let [r  (dialogue/dialogue-dispatch
            {:dialogue-id dialogue_id :from from :to to
             :message message :signal (safe-kw signal)
             :timeout_ms timeout_ms :files files})
        cs (consensus/check-consensus dialogue_id)]
    (log/info "Agora dispatch to" to "in dialogue" dialogue_id
              "signal:" (:signal r) "detection:" (:signal-detection r)
              "consensus:" cs)
    (result/ok {:dialogue-id      dialogue_id
                :turn             (:turn r)
                :signal           (safe-name (:signal r))
                :signal-detection (safe-name (:signal-detection r) nil)
                :consensus-status (safe-name cs)
                :dispatch-result  (:dispatch-result r)})))

(defn- check-consensus* [{:keys [dialogue_id]}]
  (result/let-ok [_dialogue (require-dialogue dialogue_id)]
                 (let [r            (consensus/consensus-result dialogue_id)
                       turns        (schema/get-turns dialogue_id)
                       last-turn    (last (sort-by :turn-number turns))
                       preview      (truncate-preview (:message last-turn) 100)
                       participants (consensus/get-participants dialogue_id)
                       pstatus      (build-participant-status dialogue_id participants)]
                   (log/debug "Consensus check for" dialogue_id ":" r)
                   (result/ok {:dialogue-id       dialogue_id
                               :status            (safe-name (:status r))
                               :nash-equilibrium? (boolean (:nash-equilibrium? r))
                               :approval-ratio    (or (:approval-ratio r) 0.0)
                               :participants      (or (:participants r) 0)
                               :turn-count        (or (:turn-count r) 0)
                               :progress-score    (or (:progress-score r) 0.0)
                               :mediator-needed?  (boolean (:mediator-needed? r))
                               :mediator-reason   (safe-name (:mediator-reason r) nil)
                               :participant-status pstatus
                               :last-turn         (when last-turn
                                                    {:from    (:sender last-turn)
                                                     :signal  (safe-name (:signal last-turn) nil)
                                                     :preview preview})}))))

(defn- list-dialogues* [{:keys [status]}]
  (let [skw      (safe-kw status)
        ds       (if skw (schema/list-dialogues skw) (schema/list-dialogues))
        enriched (mapv enrich-dialogue-summary ds)]
    (log/debug "Listed" (count enriched) "dialogues"
               (when skw (str "with status " skw)))
    (result/ok {:dialogues enriched
                :count     (count enriched)
                :filter    (safe-name skw nil)})))

(defn- join-dialogue* [{:keys [dialogue_id slave_id]}]
  (let [joined? (dialogue/join-dialogue dialogue_id slave_id)]
    (if joined?
      (do (log/info "Participant" slave_id "joined dialogue" dialogue_id)
          (result/ok {:success true :dialogue-id dialogue_id :participant slave_id}))
      (result/err :agora/join-failed
                  {:message (str "Dialogue not found: " dialogue_id)}))))

(defn- get-history* [{:keys [dialogue_id limit]}]
  (result/let-ok [_ (require-dialogue dialogue_id)]
                 (let [dlg       (dialogue/get-dialogue dialogue_id)
                       all-turns (dialogue/get-dialogue-turns dialogue_id)
                       turns     (if (and limit (pos? limit))
                                   (take-last limit all-turns)
                                   all-turns)
                       fmt       (mapv format-turn turns)]
                   (log/debug "Retrieved" (count fmt) "turns for dialogue" dialogue_id)
                   (result/ok {:dialogue-id  dialogue_id
                               :topic        (:topic dlg)
                               :status       (safe-name (:status dlg) nil)
                               :participants (vec (:participants dlg))
                               :turn-count   (count all-turns)
                               :turns        fmt}))))

;; --- Drone Debates ---

(defn- create-debate* [{:keys [topic roles methodology blocking]}]
  (result/let-ok [_ (require-param topic "topic")
                  _ (require-min-count roles 2 "roles")]
                 (let [role-maps (mapv #(coerce-role-map % [[:role :role] [:position :position]]) roles)
                       mkw    (safe-kw methodology)
                       r      (if blocking
                                (debate/start-debate! topic role-maps {:methodology mkw})
                                (debate/start-debate-async! topic role-maps {:methodology mkw}))]
                   (log/info "Created drone debate:" (:dialogue-id r)
                             "blocking:" blocking "methodology:" (or mkw :opinion))
                   (result/ok r))))

(defn- debate-status* [{:keys [dialogue_id]}]
  (result/let-ok [_ (require-param dialogue_id "dialogue_id")]
                 (result-from-nullable
                  (debate/get-debate-status dialogue_id)
                  :agora/not-found
                  (str "Debate not found: " dialogue_id))))

(defn- continue-debate* [{:keys [dialogue_id]}]
  (result/let-ok [_ (require-param dialogue_id "dialogue_id")]
                 (result-from-nullable
                  (debate/continue-debate! dialogue_id)
                  :agora/not-found
                  (str "Debate not active or not found: " dialogue_id))))

(defn- list-debates* [_]
  (let [ds (debate/list-active-debates)]
    (result/ok {:debates ds :count (count ds)})))

;; --- Staged Debates ---

(defn- create-staged-debate* [{:keys [topic research_roles debate_roles methodology]}]
  (result/let-ok [_ (require-param topic "topic")
                  _ (require-min-count research_roles 1 "research_roles")
                  _ (require-min-count debate_roles 2 "debate_roles")]
                 (let [research-maps (mapv #(coerce-role-map % [[:role :role]
                                                                [:position :focus_area :role]])
                                           research_roles)
                       debate-maps   (mapv #(coerce-role-map % [[:role :role] [:position :position]])
                                           debate_roles)
                       r             (stages/create-staged-debate!
                                      topic research-maps debate-maps
                                      {:methodology (safe-kw methodology)})]
                   (log/info "Created staged debate:" (:dialogue-id r)
                             "research:" (count research-maps) "debate:" (count debate-maps))
                   (result/ok r))))

(defn- stage-status* [{:keys [dialogue_id]}]
  (result/let-ok [_ (require-param dialogue_id "dialogue_id")]
                 (result-from-nullable
                  (stages/get-stage-status dialogue_id)
                  :agora/not-found
                  (str "Staged debate not found: " dialogue_id))))

;; ============================================================
;; Tool Handlers (thin boundary layer)
;; ============================================================

(defn handle-agora-create-dialogue
  "Create a new Agora dialogue session."
  [params]
  (result->mcp (try-result :agora/create-dialogue #(create-dialogue* params))))

(defn handle-agora-dispatch
  "Dispatch a message within an Agora dialogue."
  [params]
  (result->mcp (try-result :agora/dispatch #(dispatch* params))))

(defn handle-agora-check-consensus
  "Check Nash equilibrium status for a dialogue."
  [params]
  (result->mcp (try-result :agora/check-consensus #(check-consensus* params))))

(defn handle-agora-list-dialogues
  "List all Agora dialogues, optionally filtered by status."
  [params]
  (result->mcp (try-result :agora/list-dialogues #(list-dialogues* params))))

(defn handle-agora-join-dialogue
  "Add a participant to an existing dialogue."
  [params]
  (result->mcp (try-result :agora/join-dialogue #(join-dialogue* params))))

(defn handle-agora-get-history
  "Retrieve dialogue transcript with all turns."
  [params]
  (result->mcp (try-result :agora/get-history #(get-history* params))))

(defn handle-agora-create-debate
  "Create an auto-orchestrated drone debate."
  [params]
  (result->mcp (try-result :agora/create-debate #(create-debate* params))))

(defn handle-agora-debate-status
  "Get status of a drone debate."
  [params]
  (result->mcp (try-result :agora/debate-status #(debate-status* params))))

(defn handle-agora-continue-debate
  "Continue an async drone debate by executing next turn."
  [params]
  (result->mcp (try-result :agora/continue-debate #(continue-debate* params))))

(defn handle-agora-list-debates
  "List all active drone debates."
  [params]
  (result->mcp (try-result :agora/list-debates #(list-debates* params))))

(defn handle-agora-create-staged-debate
  "Create a two-stage debate with research + debate phases."
  [params]
  (result->mcp (try-result :agora/create-staged-debate #(create-staged-debate* params))))

(defn handle-agora-stage-status
  "Get stage status for a staged debate."
  [params]
  (result->mcp (try-result :agora/stage-status #(stage-status* params))))

;; ============================================================
;; Tool Definitions
;; ============================================================

(def tools
  "REMOVED: Flat agora tools no longer exposed. Use consolidated `agora` tool."
  [])
