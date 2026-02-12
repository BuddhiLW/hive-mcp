(ns hive-mcp.tools.swarm.jvm.classifier
  "JVM process discovery, classification, and swarm environment detection."
  (:require [clojure.java.shell :as shell]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.tools.swarm.jvm.parser :as parser]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn find-jvm-processes
  "Find all JVM processes with their details including parent info."
  []
  (try
    (let [result (shell/sh "ps" "-eo" "pid,ppid,pcpu,pmem,etime,args" "--no-headers")
          lines (str/split-lines (:out result))
          jvm-lines (filter #(re-find #"java" %) lines)]
      (keep parser/parse-process-line-extended jvm-lines))
    (catch Exception e
      (log/error "Error finding JVM processes:" (.getMessage e))
      [])))

(defn get-all-process-parents
  "Get pid-to-parent map for all processes in a single ps call."
  []
  (try
    (let [result (shell/sh "ps" "-eo" "pid,ppid,comm" "--no-headers")
          lines (str/split-lines (:out result))]
      (into {}
            (keep (fn [line]
                    (let [parts (str/split (str/trim line) #"\s+" 3)]
                      (when (= 3 (count parts))
                        [(first parts) {:ppid (second parts) :comm (nth parts 2)}])))
                  lines)))
    (catch Exception _ {})))

(defn get-process-swarm-info
  "Get swarm environment variables from /proc/<pid>/environ."
  [pid]
  (try
    (let [environ-file (str "/proc/" pid "/environ")
          content (slurp environ-file)
          entries (str/split content #"\x00")
          env-map (into {} (keep #(let [parts (str/split % #"=" 2)]
                                    (when (= 2 (count parts))
                                      [(first parts) (second parts)]))
                                 entries))
          slave-id (get env-map "CLAUDE_SWARM_SLAVE_ID")
          master-id (get env-map "CLAUDE_SWARM_MASTER")
          depth (get env-map "CLAUDE_SWARM_DEPTH")]
      (when (or slave-id master-id depth)
        {:swarm-slave-id slave-id
         :swarm-master-id master-id
         :swarm-depth (when depth (try (Integer/parseInt depth) (catch Exception _ nil)))}))
    (catch Exception _
      nil)))

(def ^:private type-patterns
  "Regex patterns for JVM process type classification."
  [[:shadow-cljs #"shadow-cljs|shadow\.cljs"]
   [:hive-mcp #"hive-mcp|hive_mcp"]
   [:clojure-mcp #"clojure-mcp|clj-mcp"]
   [:nrepl #"nrepl"]
   [:leiningen #"leiningen"]])

(defn classify-type
  "Classify a JVM process type based on command line."
  [cmd]
  (or (some (fn [[type pattern]]
              (when (re-find pattern cmd) type))
            type-patterns)
      :other))

(defn classify-jvm-process
  "Classify a JVM process by type and swarm status."
  [{:keys [cmd pid] :as proc}]
  (let [swarm-info (get-process-swarm-info pid)
        proc-type (classify-type cmd)]
    (-> proc
        (assoc :type proc-type)
        (assoc :swarm-spawned (boolean swarm-info))
        (merge swarm-info))))

(defn discover-and-classify
  "Find all JVM processes, classify them, and return grouped results."
  []
  (let [all-procs (find-jvm-processes)
        all-parents (get-all-process-parents)
        classified (map classify-jvm-process all-procs)
        by-type (group-by :type classified)]
    {:processes classified
     :by-type by-type
     :parents all-parents}))
