(ns hive-mcp.tools.swarm.jvm.parser
  "Process output parsing for JVM process management via multimethod dispatch by OS."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private os-name
  "Detect current operating system."
  (let [os (str/lower-case (System/getProperty "os.name"))]
    (cond
      (str/includes? os "linux") :linux
      (str/includes? os "mac") :darwin
      (str/includes? os "win") :windows
      :else :unknown)))

(defmulti parse-process-line
  "Parse a ps output line into a process map."
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
  (parse-process-line :linux line))

(defmethod parse-process-line :default
  [_line]
  nil)

(defn parse-etime-to-minutes
  "Parse elapsed time (format: [[DD-]HH:]MM:SS) to minutes."
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

(defmulti parse-process-line-extended
  "Parse ps output line with extended info including PPID."
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
