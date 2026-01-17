(ns hive-mcp.tools.swarm.jvm.parser-test
  "Tests for JVM process output parsing.
   Covers multimethod dispatch and elapsed time parsing."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.swarm.jvm.parser :as parser]))

;; =============================================================================
;; parse-process-line Tests
;; =============================================================================

(deftest test-parse-process-line
  (testing "Valid Linux ps output line"
    (let [line "12345 5.2 3.1 01:30 java -jar app.jar"
          result (parser/parse-process-line line)]
      (is (= "12345" (:pid result)))
      (is (= "5.2" (:cpu result)))
      (is (= "3.1" (:mem result)))
      (is (= "01:30" (:etime result)))
      (is (= "java -jar app.jar" (:cmd result)))))

  (testing "Line with multiple spaces between fields"
    (let [line "  98765   0.0   1.5   00:05   /usr/bin/java -Xmx512m"
          result (parser/parse-process-line line)]
      (is (= "98765" (:pid result)))
      (is (= "0.0" (:cpu result)))
      (is (= "1.5" (:mem result)))
      (is (= "00:05" (:etime result)))
      (is (= "/usr/bin/java -Xmx512m" (:cmd result)))))

  (testing "Command with many spaces preserved"
    (let [line "11111 0.1 0.2 10:00 java -cp /path/to/classes MainClass arg1 arg2"
          result (parser/parse-process-line line)]
      (is (= "java -cp /path/to/classes MainClass arg1 arg2" (:cmd result)))))

  (testing "Insufficient fields returns nil"
    (is (nil? (parser/parse-process-line "12345 5.2 3.1 01:30")))
    (is (nil? (parser/parse-process-line "12345")))
    (is (nil? (parser/parse-process-line "")))))

;; =============================================================================
;; parse-etime-to-minutes Tests
;; =============================================================================

(deftest test-parse-etime-to-minutes
  (testing "MM:SS format (minutes:seconds)"
    (is (= 5 (parser/parse-etime-to-minutes "05:23")))
    (is (= 0 (parser/parse-etime-to-minutes "00:30")))
    (is (= 59 (parser/parse-etime-to-minutes "59:59"))))

  (testing "HH:MM:SS format (hours:minutes:seconds)"
    (is (= 90 (parser/parse-etime-to-minutes "01:30:45")))
    (is (= 60 (parser/parse-etime-to-minutes "01:00:00")))
    (is (= 150 (parser/parse-etime-to-minutes "02:30:00")))
    (is (= 0 (parser/parse-etime-to-minutes "00:00:00"))))

  (testing "DD-HH:MM:SS format (days-hours:minutes:seconds)"
    ;; 2 days + 12 hours + 30 min = 2880 + 720 + 30 = 3630
    (is (= 3630 (parser/parse-etime-to-minutes "2-12:30:45")))
    (is (= 1440 (parser/parse-etime-to-minutes "1-00:00:00")))
    (is (= 2880 (parser/parse-etime-to-minutes "2-00:00:00")))
    (is (= 1530 (parser/parse-etime-to-minutes "1-01:30:00"))))

  (testing "Edge cases"
    (is (= 0 (parser/parse-etime-to-minutes "")))
    (is (= 0 (parser/parse-etime-to-minutes "invalid")))
    (is (= 0 (parser/parse-etime-to-minutes "abc:def")))))

;; =============================================================================
;; parse-process-line-extended Tests
;; =============================================================================

(deftest test-parse-process-line-extended
  (testing "Valid extended ps output line with PPID"
    (let [line "12345 1234 5.2 3.1 01:30 java -jar app.jar"
          result (parser/parse-process-line-extended line)]
      (is (= "12345" (:pid result)))
      (is (= "1234" (:ppid result)))
      (is (= "5.2" (:cpu result)))
      (is (= "3.1" (:mem result)))
      (is (= "01:30" (:etime result)))
      (is (= "java -jar app.jar" (:cmd result)))))

  (testing "Line with init as parent (PPID=1)"
    (let [line "99999 1 0.5 2.0 2-10:30:00 /usr/bin/java -server"
          result (parser/parse-process-line-extended line)]
      (is (= "99999" (:pid result)))
      (is (= "1" (:ppid result)))
      (is (= "2-10:30:00" (:etime result)))))

  (testing "Command with complex arguments"
    (let [line "55555 44444 1.0 1.5 00:30 java -Xms256m -Xmx1024m -jar /opt/app/service.jar --port 8080"
          result (parser/parse-process-line-extended line)]
      (is (= "55555" (:pid result)))
      (is (= "44444" (:ppid result)))
      (is (= "java -Xms256m -Xmx1024m -jar /opt/app/service.jar --port 8080" (:cmd result)))))

  (testing "Insufficient fields returns nil"
    (is (nil? (parser/parse-process-line-extended "12345 1234 5.2 3.1 01:30")))
    (is (nil? (parser/parse-process-line-extended "12345 1234")))
    (is (nil? (parser/parse-process-line-extended "")))))

;; =============================================================================
;; Integration Tests
;; =============================================================================

(deftest test-etime-parsing-integration
  (testing "Parsed etime can be converted to minutes"
    (let [line "12345 5.2 3.1 01:30:45 java"
          parsed (parser/parse-process-line line)
          minutes (parser/parse-etime-to-minutes (:etime parsed))]
      (is (= 90 minutes))))

  (testing "Extended parsed etime can be converted"
    (let [line "12345 1 5.2 3.1 2-12:30:45 java"
          parsed (parser/parse-process-line-extended line)
          minutes (parser/parse-etime-to-minutes (:etime parsed))]
      (is (= 3630 minutes)))))
