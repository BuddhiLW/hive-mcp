(ns hive-mcp.tools.swarm-jvm-pinning-test
  "Pinning tests for JVM process management and resource guard.

   Tests cover the following functions:
   - parse-jvm-process-line: Parse ps output into process map
   - parse-etime-to-minutes: Convert elapsed time to minutes
   - classify-jvm-process: Classify JVM process by type
   - handle-jvm-cleanup: Find and cleanup orphaned JVMs
   - handle-resource-guard: Check resources before spawning

   Pure functions are tested directly.
   Handler functions use with-redefs to mock shell commands."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [clojure.data.json :as json]
            [clojure.java.shell :as shell]
            [hive-mcp.tools.swarm :as swarm]
            [hive-mcp.tools.swarm.jvm :as jvm]))

;; =============================================================================
;; Test Helpers
;; =============================================================================

(defn mock-shell-sh
  "Creates a mock shell/sh that returns the given output."
  [output & {:keys [exit] :or {exit 0}}]
  (fn [& _args]
    {:exit exit :out output :err ""}))

(defn mock-shell-error
  "Creates a mock shell/sh that simulates a command failure."
  [error-msg]
  (fn [& _args]
    {:exit 1 :out "" :err error-msg}))

(defmacro with-mock-shell
  "Execute body with mocked shell/sh."
  [mock-fn & body]
  `(with-redefs [shell/sh ~mock-fn]
     ~@body))

(defn parse-json-result
  "Parse MCP JSON result text."
  [result]
  (json/read-str (:text result) :key-fn keyword))

;; =============================================================================
;; parse-jvm-process-line Tests (Pure Function)
;; =============================================================================

(deftest parse-jvm-process-line-valid-input-test
  (testing "Parses valid ps output line correctly"
    (let [line "  12345   1.5   2.3   05:32 /usr/bin/java -jar app.jar"
          result (swarm/parse-jvm-process-line line)]
      (is (= "12345" (:pid result))
          "Should extract PID")
      (is (= "1.5" (:cpu result))
          "Should extract CPU percentage")
      (is (= "2.3" (:mem result))
          "Should extract memory percentage")
      (is (= "05:32" (:etime result))
          "Should extract elapsed time")
      (is (str/includes? (:cmd result) "java")
          "Should extract command"))))

(deftest parse-jvm-process-line-multiword-cmd-test
  (testing "Handles command with multiple words and arguments"
    (let [line "99999   0.1   5.2   1-02:30:15 java -Xmx2g -jar server.jar --port 8080"
          result (swarm/parse-jvm-process-line line)]
      (is (= "99999" (:pid result)))
      (is (= "1-02:30:15" (:etime result)))
      (is (str/includes? (:cmd result) "-Xmx2g")
          "Should preserve command arguments"))))

(deftest parse-jvm-process-line-insufficient-parts-test
  (testing "Returns nil for lines with insufficient parts"
    (is (nil? (swarm/parse-jvm-process-line "12345 1.5"))
        "Should return nil when fewer than 5 parts")
    (is (nil? (swarm/parse-jvm-process-line ""))
        "Should return nil for empty string")
    (is (nil? (swarm/parse-jvm-process-line "   "))
        "Should return nil for whitespace only")))

(deftest parse-jvm-process-line-trims-whitespace-test
  (testing "Handles varying whitespace correctly"
    (let [line "    123    0.5    1.0    00:01    java -jar test.jar   "
          result (swarm/parse-jvm-process-line line)]
      (is (= "123" (:pid result))
          "Should trim leading/trailing whitespace from PID")
      (is (= "0.5" (:cpu result))
          "Should handle multiple spaces between fields"))))

;; =============================================================================
;; parse-etime-to-minutes Tests (Pure Function)
;; =============================================================================

(deftest parse-etime-to-minutes-mm-ss-test
  (testing "Parses MM:SS format correctly"
    (is (= 5 (swarm/parse-etime-to-minutes "05:30"))
        "5:30 should be 5 minutes")
    (is (= 0 (swarm/parse-etime-to-minutes "00:45"))
        "0:45 should be 0 minutes")
    (is (= 59 (swarm/parse-etime-to-minutes "59:59"))
        "59:59 should be 59 minutes")))

(deftest parse-etime-to-minutes-hh-mm-ss-test
  (testing "Parses HH:MM:SS format correctly"
    (is (= 60 (swarm/parse-etime-to-minutes "01:00:00"))
        "1 hour should be 60 minutes")
    (is (= 90 (swarm/parse-etime-to-minutes "01:30:00"))
        "1:30 should be 90 minutes")
    (is (= 125 (swarm/parse-etime-to-minutes "02:05:30"))
        "2:05:30 should be 125 minutes")))

(deftest parse-etime-to-minutes-dd-hh-mm-ss-test
  (testing "Parses DD-HH:MM:SS format correctly"
    (is (= 1440 (swarm/parse-etime-to-minutes "1-00:00:00"))
        "1 day should be 1440 minutes")
    (is (= 1500 (swarm/parse-etime-to-minutes "1-01:00:00"))
        "1 day 1 hour should be 1500 minutes")
    (is (= 2880 (swarm/parse-etime-to-minutes "2-00:00:00"))
        "2 days should be 2880 minutes")
    (is (= 2945 (swarm/parse-etime-to-minutes "2-01:05:30"))
        "2d 1h 5m should be 2945 minutes")))

(deftest parse-etime-to-minutes-invalid-input-test
  (testing "Returns 0 for invalid input"
    (is (= 0 (swarm/parse-etime-to-minutes "invalid"))
        "Should return 0 for non-numeric input")
    (is (= 0 (swarm/parse-etime-to-minutes ""))
        "Should return 0 for empty string")
    (is (= 0 (swarm/parse-etime-to-minutes nil))
        "Should handle nil gracefully")))

;; =============================================================================
;; classify-jvm-process Tests (Pure Function with Mock)
;; =============================================================================

(deftest classify-jvm-process-shadow-cljs-test
  (testing "Classifies shadow-cljs processes"
    (with-redefs [jvm/get-process-swarm-info (constantly nil)]
      (let [proc {:pid "123" :cmd "java -jar shadow-cljs.jar watch app"}
            result (jvm/classify-jvm-process proc)]
        (is (= :shadow-cljs (:type result))
            "Should identify shadow-cljs process")
        (is (false? (:swarm-spawned result))
            "Non-swarm process should have swarm-spawned false")))))

(deftest classify-jvm-process-hive-mcp-test
  (testing "Classifies hive-mcp processes"
    (with-redefs [jvm/get-process-swarm-info (constantly nil)]
      (let [proc {:pid "456" :cmd "java -cp hive-mcp.jar clojure.main"}
            result (jvm/classify-jvm-process proc)]
        (is (= :hive-mcp (:type result))
            "Should identify hive-mcp process")))))

(deftest classify-jvm-process-nrepl-test
  (testing "Classifies nREPL processes"
    (with-redefs [jvm/get-process-swarm-info (constantly nil)]
      (let [proc {:pid "789" :cmd "java -jar clojure nrepl.server"}
            result (jvm/classify-jvm-process proc)]
        (is (= :nrepl (:type result))
            "Should identify nREPL process")))))

(deftest classify-jvm-process-leiningen-test
  (testing "Classifies Leiningen processes"
    (with-redefs [jvm/get-process-swarm-info (constantly nil)]
      (let [proc {:pid "101" :cmd "java -jar leiningen-standalone.jar repl"}
            result (jvm/classify-jvm-process proc)]
        (is (= :leiningen (:type result))
            "Should identify Leiningen process")))))

(deftest classify-jvm-process-other-test
  (testing "Classifies unknown JVM processes as :other"
    (with-redefs [jvm/get-process-swarm-info (constantly nil)]
      (let [proc {:pid "999" :cmd "java -jar some-random-app.jar"}
            result (jvm/classify-jvm-process proc)]
        (is (= :other (:type result))
            "Should classify unknown as :other")))))

(deftest classify-jvm-process-swarm-spawned-test
  (testing "Identifies swarm-spawned processes"
    (with-redefs [jvm/get-process-swarm-info
                  (constantly {:swarm-slave-id "slave-1"
                               :swarm-master-id "master-1"
                               :swarm-depth 1})]
      (let [proc {:pid "222" :cmd "java -jar app.jar"}
            result (jvm/classify-jvm-process proc)]
        (is (true? (:swarm-spawned result))
            "Swarm process should have swarm-spawned true")
        (is (= "slave-1" (:swarm-slave-id result))
            "Should include swarm slave ID")
        (is (= "master-1" (:swarm-master-id result))
            "Should include swarm master ID")
        (is (= 1 (:swarm-depth result))
            "Should include swarm depth")))))

;; =============================================================================
;; handle-jvm-cleanup Tests (Handler with Mocks)
;; =============================================================================

(def sample-ps-output
  "  1234   1001   0.5   1.2   05:30 java -jar shadow-cljs.jar
  5678   1002   1.0   2.5   1-02:00:00 java -jar orphan-app.jar
  9012   1    0.1   0.5   2-00:00:00 java -jar truly-orphaned.jar")

(def sample-parent-output
  "  1001   1000   bash
  1002   1    init
  1000   999   claude")

(deftest handle-jvm-cleanup-dry-run-test
  (testing "Dry run returns orphans without killing"
    (with-redefs [shell/sh (fn [& args]
                             (let [cmd (first args)]
                               (cond
                                 (and (= "ps" cmd) (some #(= "pid,ppid,pcpu,pmem,etime,args" %) args))
                                 {:exit 0 :out sample-ps-output :err ""}

                                 (and (= "ps" cmd) (some #(= "pid,ppid,comm" %) args))
                                 {:exit 0 :out sample-parent-output :err ""}

                                 :else {:exit 0 :out "" :err ""})))
                  jvm/get-process-swarm-info (constantly nil)]
      (let [result (jvm/handle-jvm-cleanup {:dry_run true})
            data (parse-json-result result)]
        (is (= "text" (:type result))
            "Should return MCP text response")
        (is (nil? (:isError result))
            "Should not be an error")
        (is (true? (:dry-run data))
            "Should indicate dry run mode")
        (is (empty? (:killed data))
            "Dry run should not kill any processes")))))

(deftest handle-jvm-cleanup-respects-keep-types-test
  (testing "Respects keep_types parameter to protect certain JVM types"
    (with-redefs [shell/sh (fn [& args]
                             (let [cmd (first args)]
                               (cond
                                 (and (= "ps" cmd) (some #(= "pid,ppid,pcpu,pmem,etime,args" %) args))
                                 {:exit 0 :out sample-ps-output :err ""}

                                 (and (= "ps" cmd) (some #(= "pid,ppid,comm" %) args))
                                 {:exit 0 :out sample-parent-output :err ""}

                                 :else {:exit 0 :out "" :err ""})))
                  jvm/get-process-swarm-info (constantly nil)]
      (let [result (jvm/handle-jvm-cleanup {:dry_run true
                                            :keep_types ["shadow-cljs"]})
            data (parse-json-result result)]
        ;; shadow-cljs should be protected
        (is (some #(= "protected-type" (:reason %))
                  (:details data))
            "Shadow-cljs should be marked as protected type")))))

(deftest handle-jvm-cleanup-true-orphans-mode-test
  (testing "True orphans mode only targets truly orphaned processes"
    (with-redefs [shell/sh (fn [& args]
                             (let [cmd (first args)]
                               (cond
                                 (and (= "ps" cmd) (some #(= "pid,ppid,pcpu,pmem,etime,args" %) args))
                                 {:exit 0 :out sample-ps-output :err ""}

                                 (and (= "ps" cmd) (some #(= "pid,ppid,comm" %) args))
                                 {:exit 0 :out sample-parent-output :err ""}

                                 :else {:exit 0 :out "" :err ""})))
                  jvm/get-process-swarm-info (constantly nil)]
      (let [result (jvm/handle-jvm-cleanup {:dry_run true
                                            :true_orphans_only true
                                            :keep_types []})
            data (parse-json-result result)]
        (is (= "true-orphans" (get-in data [:orphan-detection :mode]))
            "Should use true-orphans mode")))))

(deftest handle-jvm-cleanup-error-handling-test
  (testing "Gracefully handles when no JVMs are found (shell errors caught internally)"
    ;; Note: find-jvm-processes catches exceptions and returns [] 
    ;; So handle-jvm-cleanup still returns success with empty results
    (with-redefs [shell/sh (fn [& _] (throw (Exception. "ps command failed")))]
      (let [result (jvm/handle-jvm-cleanup {:dry_run true})
            data (parse-json-result result)]
        (is (= "text" (:type result))
            "Should return MCP response")
        (is (nil? (:isError result))
            "Shell errors are caught internally, returns success with empty data")
        (is (= 0 (:total-jvm-processes data))
            "Should report 0 processes when shell fails")))))

(deftest handle-jvm-cleanup-swarm-only-mode-test
  (testing "Swarm-only mode filters to swarm-spawned processes"
    (with-redefs [shell/sh (fn [& args]
                             (let [cmd (first args)]
                               (cond
                                 (and (= "ps" cmd) (some #(= "pid,ppid,pcpu,pmem,etime,args" %) args))
                                 {:exit 0 :out sample-ps-output :err ""}

                                 (and (= "ps" cmd) (some #(= "pid,ppid,comm" %) args))
                                 {:exit 0 :out sample-parent-output :err ""}

                                 :else {:exit 0 :out "" :err ""})))
                  jvm/get-process-swarm-info (constantly nil)]
      (let [result (jvm/handle-jvm-cleanup {:dry_run true
                                            :swarm_only true})
            data (parse-json-result result)]
        (is (true? (:swarm-only-mode data))
            "Should indicate swarm-only mode")))))

;; =============================================================================
;; handle-resource-guard Tests (Handler with Mocks)
;; =============================================================================

(def sample-meminfo
  "MemTotal:       16000000 kB
MemFree:         2000000 kB
MemAvailable:    8000000 kB
Buffers:          500000 kB
Cached:          4000000 kB")

(def sample-meminfo-high-usage
  "MemTotal:       16000000 kB
MemFree:          500000 kB
MemAvailable:    1000000 kB
Buffers:          100000 kB
Cached:          1000000 kB")

(deftest handle-resource-guard-healthy-test
  (testing "Returns can-spawn true when memory is healthy"
    (with-redefs [shell/sh (fn [& args]
                             (cond
                               (= "cat" (first args))
                               {:exit 0 :out sample-meminfo :err ""}

                               :else {:exit 0 :out "" :err ""}))]
      (let [result (jvm/handle-resource-guard {})
            data (parse-json-result result)]
        (is (= "text" (:type result))
            "Should return MCP text response")
        (is (nil? (:isError result))
            "Should not be an error")
        (is (true? (:can-spawn data))
            "Should allow spawning when memory healthy")
        (is (= "healthy" (name (:status data)))
            "Status should be healthy")))))

(deftest handle-resource-guard-high-memory-test
  (testing "Returns can-spawn false when memory is high"
    (with-redefs [shell/sh (fn [& args]
                             (cond
                               (= "cat" (first args))
                               {:exit 0 :out sample-meminfo-high-usage :err ""}

                               (= "ps" (first args))
                               {:exit 0 :out "" :err ""}

                               :else {:exit 0 :out "" :err ""}))]
      (let [result (jvm/handle-resource-guard {:auto_cleanup false})
            data (parse-json-result result)]
        (is (false? (:can-spawn data))
            "Should deny spawning when memory high")
        (is (= "capacity-reached" (name (:status data)))
            "Status should be capacity-reached")))))

(deftest handle-resource-guard-custom-thresholds-test
  (testing "Respects custom threshold parameters"
    (with-redefs [shell/sh (fn [& args]
                             (cond
                               (= "cat" (first args))
                               {:exit 0 :out sample-meminfo :err ""}

                               :else {:exit 0 :out "" :err ""}))]
      (let [result (jvm/handle-resource-guard {:ram_threshold 90
                                               :min_available_mb 1024})
            data (parse-json-result result)]
        (is (= 90 (get-in data [:memory :threshold-percent]))
            "Should use custom RAM threshold")
        (is (= 1024 (get-in data [:memory :min-available-mb]))
            "Should use custom min available MB")))))

(deftest handle-resource-guard-auto-cleanup-test
  (testing "Auto-cleanup runs when memory is high and enabled"
    (let [cleanup-called (atom false)]
      (with-redefs [shell/sh (fn [& args]
                               (cond
                                 (= "cat" (first args))
                                 {:exit 0 :out sample-meminfo-high-usage :err ""}

                                 (= "ps" (first args))
                                 {:exit 0 :out "" :err ""}

                                 :else {:exit 0 :out "" :err ""}))
                    jvm/handle-jvm-cleanup (fn [_]
                                             (reset! cleanup-called true)
                                             {:type "text"
                                              :text "{\"orphans-found\":0,\"killed\":[]}"})]
        (let [result (jvm/handle-resource-guard {:auto_cleanup true
                                                 :cleanup_dry_run true})
              data (parse-json-result result)]
          (is @cleanup-called
              "Should call jvm cleanup when memory high")
          (is (true? (get-in data [:cleanup :ran]))
              "Should indicate cleanup ran"))))))

(deftest handle-resource-guard-error-handling-test
  (testing "Handles memory read errors gracefully"
    (with-redefs [shell/sh (fn [& _] {:exit 1 :out "" :err "Failed to read"})]
      (let [result (jvm/handle-resource-guard {})]
        (is (= "text" (:type result))
            "Should return MCP response even on error")
        (is (true? (:isError result))
            "Should indicate error")
        (is (str/includes? (:text result) "error")
            "Should contain error message")))))

(deftest handle-resource-guard-recommendation-test
  (testing "Provides appropriate recommendations"
    (with-redefs [shell/sh (fn [& args]
                             (cond
                               (= "cat" (first args))
                               {:exit 0 :out sample-meminfo :err ""}

                               :else {:exit 0 :out "" :err ""}))]
      (let [result (jvm/handle-resource-guard {})
            data (parse-json-result result)]
        (is (string? (:recommendation data))
            "Should include recommendation")
        (is (str/includes? (:recommendation data) "Safe")
            "Healthy state should recommend spawning is safe")))))

;; =============================================================================
;; Response Format Consistency Tests
;; =============================================================================

(deftest handlers-return-consistent-mcp-format-test
  (testing "All JVM handlers return consistent MCP response format"
    (with-redefs [shell/sh (fn [& args]
                             (cond
                               (= "cat" (first args))
                               {:exit 0 :out sample-meminfo :err ""}

                               (= "ps" (first args))
                               {:exit 0 :out "" :err ""}

                               :else {:exit 0 :out "" :err ""}))
                  jvm/get-process-swarm-info (constantly nil)]
      ;; Test jvm-cleanup
      (let [cleanup-result (jvm/handle-jvm-cleanup {:dry_run true})]
        (is (contains? cleanup-result :type))
        (is (contains? cleanup-result :text))
        (is (= "text" (:type cleanup-result))))

      ;; Test resource-guard
      (let [guard-result (jvm/handle-resource-guard {})]
        (is (contains? guard-result :type))
        (is (contains? guard-result :text))
        (is (= "text" (:type guard-result)))))))

(deftest handlers-error-format-consistent-test
  (testing "Resource guard returns consistent error format"
    ;; Note: jvm-cleanup catches shell errors internally via find-jvm-processes
    ;; So we only test resource-guard for error format consistency
    (with-redefs [shell/sh (fn [& _] {:exit 1 :out "" :err "Mock error"})]
      ;; Test resource-guard error (propagates errors properly)
      (let [guard-result (jvm/handle-resource-guard {})]
        (is (= "text" (:type guard-result)))
        (is (true? (:isError guard-result)))
        (is (str/includes? (:text guard-result) "error"))))))
