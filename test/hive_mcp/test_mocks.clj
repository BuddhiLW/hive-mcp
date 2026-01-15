(ns hive-mcp.test-mocks
  "Reusable mock infrastructure for hive-mcp tests.

   DRY: Consolidates duplicated mock patterns from multiple test files.
   Reference: docs/DRY-AUDIT-REPORT.md - A2 Mock Factory Functions, A7 with-redefs Boilerplate

   Provides:
   - Emacsclient mocks (from tools_duration_test.clj)
   - Evaluator mocks (from resilience_test.clj)
   - Chroma mocks (from tools_duration_test.clj)
   - Swarm mocks (from swarm_test.clj)
   - Hivemind mocks (from permissions_test.clj)
   - Hot reload mocks (from hot_test.clj, hot_reload_e2e_test.clj)

   === ADOPTION CANDIDATES ===

   Test files that can benefit from centralized mocks (document only, refactor separately):

   Emacsclient (with-mock-emacsclient):
   - tools/swarm_handlers_pinning_test.clj (60+ with-redefs)
   - tools/cider_pinning_test.clj (uses local with-elisp-mock macro)
   - tools/magit_pinning_test.clj (uses local with-elisp-mock macro)
   - tools/core_test.clj (7 with-redefs)
   - tools_duration_test.clj (uses local with-mock-emacsclient)

   Chroma (with-mock-chroma):
   - tools/memory_pinning_test.clj (40+ with-redefs)
   - tools_duration_test.clj (uses local with-mock-chroma)

   Swarm (with-default-swarm-mocks):
   - tools/swarm_handlers_pinning_test.clj (swarm-addon-available? checks)
   - tools/swarm_test.clj (8+ swarm-addon-available? checks)

   Channel (make-channel-capture):
   - events/effects_test.clj (channel/emit-event! redefs)
   - prompts/infra_test.clj (channel/emit-event! redefs)

   Shell (with-mock-shell):
   - tools/swarm_jvm_pinning_test.clj (15+ shell/sh redefs)

   Hivemind (make-hivemind-ask-approve, make-hivemind-ask-deny):
   - permissions_test.clj (10+ hivemind/ask! redefs)

   Hot Reload (with-mock-hot-reload):
   - tools/hot_test.clj (5 hot reload redefs)
   - hot_reload_e2e_test.clj (6 hot reload redefs)"
  (:require [clojure.string :as str]
            [hive-mcp.emacsclient :as ec]
            [hive-mcp.evaluator :as evaluator]
            [hive-mcp.chroma :as chroma]))

;; =============================================================================
;; Emacsclient Mocks (A2 - from tools_duration_test.clj)
;; =============================================================================

(defn mock-emacsclient-success
  "Creates a mock eval-elisp that returns success with given result.
   
   Usage:
     (with-mock-emacsclient (mock-emacsclient-success \"42\")
       (ec/eval-elisp \"...\")) => {:success true :result \"42\" ...}"
  [result]
  (fn [_elisp]
    {:success true :result result :duration-ms 10}))

(defn mock-emacsclient-failure
  "Creates a mock eval-elisp that returns failure with given error.
   
   Usage:
     (with-mock-emacsclient (mock-emacsclient-failure \"Connection refused\")
       (ec/eval-elisp \"...\")) => {:success false :error \"Connection refused\" ...}"
  [error]
  (fn [_elisp]
    {:success false :error error :duration-ms 10}))

(defn mock-emacsclient-not-loaded
  "Mock for when hive-mcp.el is not loaded (featurep returns nil).
   
   Returns nil for featurep checks, error for other calls."
  []
  (fn [elisp]
    (if (str/includes? elisp "featurep")
      {:success true :result "nil" :duration-ms 5}
      {:success false :error "Function not available" :duration-ms 10})))

(defn mock-emacsclient-loaded
  "Mock for when hive-mcp.el is loaded.
   
   Returns t for featurep checks, api-result for other calls."
  [api-result]
  (fn [elisp]
    (if (str/includes? elisp "featurep")
      {:success true :result "t" :duration-ms 5}
      {:success true :result api-result :duration-ms 10})))

(defmacro with-mock-emacsclient
  "Execute body with mocked emacsclient/eval-elisp.
   
   Usage:
     (with-mock-emacsclient (mock-emacsclient-success \"result\")
       (test-handler-that-uses-eval-elisp))"
  [mock-fn & body]
  `(with-redefs [ec/eval-elisp ~mock-fn]
     ~@body))

;; =============================================================================
;; Evaluator Mocks (A2 - from resilience_test.clj)
;; =============================================================================

(defn make-failing-evaluator
  "Creates a mock evaluator that always fails with a specific error.
   
   Usage:
     (let [eval (make-failing-evaluator \"REPL not connected\")]
       (evaluator/eval-code eval \"(+ 1 2)\")) => {:success false :error \"...\"}"
  [error-message]
  (reify evaluator/ReplEvaluator
    (eval-code [_ _]
      {:success false :error error-message})
    (connected? [_] false)
    (get-status [_] {:connected false :error error-message})))

(defn make-succeeding-evaluator
  "Creates a mock evaluator that always succeeds with a specific result.
   
   Usage:
     (let [eval (make-succeeding-evaluator \"42\")]
       (evaluator/eval-code eval \"(+ 1 2)\")) => {:success true :result \"42\"}"
  [result]
  (reify evaluator/ReplEvaluator
    (eval-code [_ _]
      {:success true :result result})
    (connected? [_] true)
    (get-status [_] {:connected true})))

(defn make-flaky-evaluator
  "Creates an evaluator that fails N times before succeeding.
   Useful for testing retry logic.
   
   Usage:
     (let [eval (make-flaky-evaluator 2 \"success\")]
       ;; First 2 calls fail, 3rd succeeds
       (evaluator/eval-code eval \"...\") => failure
       (evaluator/eval-code eval \"...\") => failure  
       (evaluator/eval-code eval \"...\") => {:success true :result \"success\"})"
  [failures-before-success result]
  (let [attempts (atom 0)]
    (reify evaluator/ReplEvaluator
      (eval-code [_ _]
        (let [attempt (swap! attempts inc)]
          (if (> attempt failures-before-success)
            {:success true :result result}
            {:success false :error (str "Transient failure " attempt)})))
      (connected? [_] true)
      (get-status [_] {:connected true}))))

;; =============================================================================
;; Chroma Mocks (A2/A7 - from tools_duration_test.clj)
;; =============================================================================

(defn make-test-entry
  "Create a test memory entry with given overrides.
   
   Usage:
     (make-test-entry) => default test entry
     (make-test-entry :id \"custom-id\" :duration \"permanent\") => customized"
  [& {:keys [id type content duration tags expires]
      :or {id "test-id"
           type "note"
           content "Test content"
           duration "long"
           tags []
           expires nil}}]
  {:id id
   :type type
   :content content
   :duration duration
   :tags tags
   :expires expires
   :created (str (java.time.ZonedDateTime/now))})

(defmacro with-mock-chroma
  "Execute body with mocked Chroma functions.
   
   Options:
   - :configured? - whether Chroma is configured (default: true)
   - :entry - entry to return from get-entry-by-id (default: make-test-entry)
   - :entries - list of entries for query functions
   
   Usage:
     (with-mock-chroma {:configured? true :entry (make-test-entry :id \"abc\")}
       (memory/handle-mcp-memory-promote {:id \"abc\"}))"
  [{:keys [configured? entry entries]
    :or {configured? true}} & body]
  `(with-redefs [chroma/embedding-configured? (constantly ~configured?)
                 chroma/get-entry-by-id (fn [id#]
                                          (if ~entry
                                            (assoc ~entry :id id#)
                                            (make-test-entry :id id#)))
                 chroma/update-entry! (fn [id# updates#]
                                        (merge (or ~entry (make-test-entry :id id#)) updates#))
                 chroma/query-entries (fn [& _#] (or ~entries []))
                 chroma/delete-entry! (fn [_#] true)
                 chroma/cleanup-expired! (fn [] {:deleted 0})
                 chroma/entries-expiring-soon (fn [_# & _opts#] (or ~entries []))]
     ~@body))

(defmacro with-chroma-not-configured
  "Execute body with Chroma not configured.
   Shorthand for (with-mock-chroma {:configured? false} ...)
   
   Usage:
     (with-chroma-not-configured
       (is (str/includes? (:text result) \"Chroma not configured\")))"
  [& body]
  `(with-mock-chroma {:configured? false}
     ~@body))

;; =============================================================================
;; Swarm Mocks (A7 - from swarm_test.clj)
;; =============================================================================

(defmacro with-default-swarm-mocks
  "Execute body with common swarm mocks pre-configured.
   
   Options:
   - :addon-available? - whether swarm addon is available (default: true)
   - :elisp-result - result to return from elisp calls (default: \"{}\")
   
   Usage:
     (with-default-swarm-mocks {:elisp-result (json/write-str {:slaves-count 2})}
       (swarm/handle-swarm-status {}))"
  [{:keys [addon-available? elisp-result]
    :or {addon-available? true
         elisp-result "{}"}} & body]
  `(with-redefs [~'hive-mcp.tools.swarm/swarm-addon-available? (constantly ~addon-available?)
                 ec/eval-elisp-with-timeout
                 (fn [_elisp# _timeout#]
                   {:success true :result ~elisp-result :timed-out false})]
     ~@body))

;; =============================================================================
;; Channel Mocks (A7 - from effects_test.clj, prompts/infra_test.clj)
;; =============================================================================

(defn make-channel-capture
  "Create a mock channel emit function that captures events.
   
   Returns [mock-fn captured-events-atom].
   
   Usage:
     (let [[mock-emit! captured] (make-channel-capture)]
       (with-redefs [channel/emit-event! mock-emit!]
         (do-something-that-emits))
       @captured => [{:type :event-type :data {...}} ...])"
  []
  (let [captured (atom [])]
    [(fn [event-type data]
       (swap! captured conj {:type event-type :data data}))
     captured]))

;; =============================================================================
;; Shell/Process Mocks (A7 - from swarm_jvm_pinning_test.clj)
;; =============================================================================

(defmacro with-mock-shell
  "Execute body with mocked clojure.java.shell/sh.
   
   Usage:
     (with-mock-shell (fn [& args] {:exit 0 :out \"output\" :err \"\"})
       (shell/sh \"ps\" \"aux\"))"
  [mock-fn & body]
  `(with-redefs [clojure.java.shell/sh ~mock-fn]
     ~@body))

(defn mock-shell-success
  "Creates a mock shell function that returns success with given output.
   
   Usage:
     (with-mock-shell (mock-shell-success \"process output\")
       ...)"
  [output]
  (fn [& _args]
    {:exit 0 :out output :err ""}))

(defn mock-shell-failure
  "Creates a mock shell function that returns failure with given error.

   Usage:
     (with-mock-shell (mock-shell-failure \"command not found\")
       ...)"
  [error]
  (fn [& _args]
    {:exit 1 :out "" :err error}))

;; =============================================================================
;; Hivemind Mocks (A7 - from permissions_test.clj)
;; =============================================================================

(defn make-hivemind-ask-approve
  "Creates a mock hivemind/ask! that auto-approves (returns first option).
   Optionally captures calls to an atom.

   Usage:
     (with-redefs [hivemind/ask! (make-hivemind-ask-approve)]
       ...)

   With capture:
     (let [captured (atom [])]
       (with-redefs [hivemind/ask! (make-hivemind-ask-approve captured)]
         ...)
       @captured => [{:agent-id \"...\" :question \"...\" :options [...]}])"
  ([]
   (make-hivemind-ask-approve nil))
  ([capture-atom]
   (fn [agent-id question options & _]
     (when capture-atom
       (swap! capture-atom conj {:agent-id agent-id
                                 :question question
                                 :options options}))
     {:decision (first options) :by "test"})))

(defn make-hivemind-ask-deny
  "Creates a mock hivemind/ask! that denies (returns second option).
   Optionally captures calls to an atom.

   Usage:
     (with-redefs [hivemind/ask! (make-hivemind-ask-deny)]
       ...)"
  ([]
   (make-hivemind-ask-deny nil))
  ([capture-atom]
   (fn [agent-id question options & _]
     (when capture-atom
       (swap! capture-atom conj {:agent-id agent-id
                                 :question question
                                 :options options}))
     {:decision (second options) :by "test"})))

(defn make-hivemind-shout-capture
  "Create a mock hivemind/shout! that captures shouts.

   Returns [mock-fn captured-atom].

   Usage:
     (let [[mock-shout! captured] (make-hivemind-shout-capture)]
       (with-redefs [hivemind/shout! mock-shout!]
         (do-something-that-shouts))
       @captured => [{:agent-id \"...\" :event-type :progress :data {...}}])"
  []
  (let [captured (atom [])]
    [(fn [agent-id event-type data]
       (swap! captured conj {:agent-id agent-id
                             :event-type event-type
                             :data data}))
     captured]))

;; =============================================================================
;; Hot Reload Mocks (A7 - from hot_test.clj, hot_reload_e2e_test.clj)
;; =============================================================================

(defn mock-hot-reload-success
  "Creates a mock hot-core-reload! that returns success.

   Usage:
     (with-redefs [hot/hot-core-reload! (mock-hot-reload-success)]
       ...)"
  []
  (fn [] {:success true :reloaded-ns ["test.ns"]}))

(defn mock-hot-reload-failure
  "Creates a mock hot-core-reload! that returns failure.

   Usage:
     (with-redefs [hot/hot-core-reload! (mock-hot-reload-failure \"Syntax error\")]
       ...)"
  [error-msg]
  (fn [] {:success false :error error-msg}))

(defn mock-debounce-flush-success
  "Creates a mock debounce-flush! that returns success.

   Usage:
     (with-redefs [hot/debounce-flush! (mock-debounce-flush-success)]
       ...)"
  []
  (fn [] {:success true :flushed true}))

(defn mock-watcher-status
  "Creates a mock watcher-status that returns given status.

   Usage:
     (with-redefs [hot/watcher-status (mock-watcher-status {:watching? true :paths [\"src\"]})]
       ...)"
  [status]
  (fn [] status))

(defmacro with-mock-hot-reload
  "Execute body with mocked hot reload functions.

   Options:
   - :reload-result - result from hot-core-reload! (default: success)
   - :flush-result - result from debounce-flush! (default: success)
   - :watcher-status - result from watcher-status (default: {:watching? true})

   Usage:
     (with-mock-hot-reload {:reload-result {:success true :reloaded-ns [\"my.ns\"]}}
       (hot/handle-hot-reload {}))"
  [{:keys [reload-result flush-result watcher-status]
    :or {reload-result {:success true :reloaded-ns ["test.ns"]}
         flush-result {:success true :flushed true}
         watcher-status {:watching? true :paths ["src"]}}} & body]
  `(with-redefs [~'hive-mcp.hot/hot-core-reload! (fn [] ~reload-result)
                 ~'hive-mcp.hot/debounce-flush! (fn [] ~flush-result)
                 ~'hive-mcp.hot/watcher-status (fn [] ~watcher-status)]
     ~@body))
