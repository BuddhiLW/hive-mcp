(ns hive-mcp.tools.swarm.jvm.parser
  "Process output parsing for JVM process management.

   Pattern: Multimethod dispatch by OS for platform-specific parsing.
   Currently supports Linux; Darwin can be added via defmethod.

   CLARITY: Layers stay pure - parsing separated from I/O."
  (:require [clojure.string :as str]))

;;; ============================================================
;;; OS Detection
;;; ============================================================

(def ^:private os-name
  "Detect current operating system."
  (let [os (str/lower-case (System/getProperty "os.name"))]
    (cond
      (str/includes? os "linux") :linux
      (str/includes? os "mac") :darwin
      (str/includes? os "win") :windows
      :else :unknown)))

;;; ============================================================
;;; Process Line Parsing (Multimethod by OS)
;;; ============================================================

(defmulti parse-process-line
  "Parse a ps output line into a process map.
   Dispatches based on operating system."
  (fn [_line] os-name))

(defmethod parse-process-line :linux
  [line]
  (let [parts (str/split (str/trim line) #"\s+" 5)]
    (when (>= (count parts) 5)
      {:pid (first parts)
       :cpu (second parts)
       :mem (nth parts 2)
       :etime (nth parts 3)
       :cmd (nth parts 4)})))

(defmethod parse-process-line :darwin
  [line]
  ;; Darwin ps has similar format to Linux
  (parse-process-line :linux line))

(defmethod parse-process-line :default
  [_line]
  nil)

;;; ============================================================
;;; Elapsed Time Parsing
;;; ============================================================

(defn parse-etime-to-minutes
  "Parse elapsed time (format: [[DD-]HH:]MM:SS) to minutes.

   Examples:
     \"05:23\" -> 5
     \"01:30:45\" -> 90
     \"2-12:30:45\" -> 3030 (2 days + 12 hours + 30 min)"
  [etime]
  (try
    (let [parts (str/split etime #"[-:]")
          nums (map #(Integer/parseInt %) parts)]
      (case (count nums)
        2 (first nums) ; MM:SS -> minutes
        3 (+ (* 60 (first nums)) (second nums)) ; HH:MM:SS -> minutes
        4 (+ (* 24 60 (first nums)) (* 60 (second nums)) (nth nums 2)) ; DD-HH:MM:SS
        0))
    (catch Exception _ 0)))

;;; ============================================================
;;; Extended Process Line Parsing (with PPID)
;;; ============================================================

(defmulti parse-process-line-extended
  "Parse ps output line with extended info (including PPID).
   Format: PID PPID CPU MEM ETIME ARGS"
  (fn [_line] os-name))

(defmethod parse-process-line-extended :linux
  [line]
  (let [parts (str/split (str/trim line) #"\s+" 6)]
    (when (>= (count parts) 6)
      {:pid (nth parts 0)
       :ppid (nth parts 1)
       :cpu (nth parts 2)
       :mem (nth parts 3)
       :etime (nth parts 4)
       :cmd (nth parts 5)})))

(defmethod parse-process-line-extended :darwin
  [line]
  (parse-process-line-extended :linux line))

(defmethod parse-process-line-extended :default
  [_line]
  nil)
