(ns hive-mcp.dependency-boundary-test
  "Executable architecture guard for the host→addon boundary: hive-mcp must not
   name an addon namespace or carry an addon artifact in :deps."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private forbidden-addon-ns-prefixes
  #{"hive-emacs"})

(def ^:private forbidden-backend-artifacts
  "Concrete storage / query / vector backends. hive-mcp core is backend-neutral:
   each arrives at runtime through the deployer's gitignored local.deps.edn (or a
   launch alias) and is reached only by slot-factory late-bind, never as a
   committed dependency shipped to consumers. Axiom 20260725153948-0f523feb;
   Phase-B decision 20260713171220-639b8fbb; 20260907083253-0b6a2bca."
  #{"io.github.hive-agi/hive-proximum"
    "org.replikativ/proximum"
    "io.github.hive-agi/hive-milvus"
    "io.github.hive-agi/hive-qdrant"
    "clj-qdrant/clj-qdrant"
    "io.github.hive-agi/milvus-clj"
    "io.github.hive-agi/hive-datahike"
    "io.github.hive-agi/hive-datalevin"
    "datalevin/datalevin"
    "io.replikativ/datahike"
    "io.github.replikativ/datahike"
    "io.github.hive-agi/yggdrasil"})

(defn- clojure-sources
  []
  (->> (file-seq (io/file "src"))
       (filter #(.isFile ^java.io.File %))
       (filter #(str/ends-with? (.getName ^java.io.File %) ".clj"))))

(defn- ns-form
  [^java.io.File file]
  (try
    (let [form (read-string {:read-cond :preserve} (slurp file))]
      (when (and (seq? form) (= 'ns (first form))) form))
    (catch Exception _ nil)))

(defn- required-namespaces
  "Every namespace symbol NS-FORM loads at compile time."
  [form]
  (->> form
       (filter #(and (seq? %) (#{:require :use} (first %))))
       (mapcat rest)
       (map #(cond (symbol? %) % (sequential? %) (first %)))
       (filter symbol?)
       (map str)))

(defn- addon-require?
  [ns-name]
  (some #(or (= ns-name %) (str/starts-with? ns-name (str % ".")))
        forbidden-addon-ns-prefixes))

(defn- all-dep-maps
  "Every {coord coord-map} dependency map in a parsed deps.edn: the top-level
   :deps plus every alias's :deps / :extra-deps / :replace-deps / :override-deps.
   The EDN reader normalises the #:local{:root ...} namespaced-map form to
   {:local/root ...}, so a :local/root anywhere is caught by key lookup."
  [deps]
  (->> (cons (:deps deps)
             (for [[_ alias-map] (:aliases deps)
                   k [:deps :extra-deps :replace-deps :override-deps]
                   :let [m (get alias-map k)]]
               m))
       (filter map?)))

(defn- local-root-coords
  "Coordinate symbols across DEPS whose coordinate map carries :local/root."
  [deps]
  (for [dep-map (all-dep-maps deps)
        [coord coord-map] dep-map
        :when (and (map? coord-map) (contains? coord-map :local/root))]
    coord))

(deftest production-code-never-requires-an-addon-namespace
  (doseq [file (clojure-sources)
          :let [form (ns-form file)]
          :when form
          required (required-namespaces form)]
    (is (not (addon-require? required))
        (str "addon namespace " required " required from " (.getPath ^java.io.File file)))))

(deftest dependency-map-has-no-addon-artifact
  (testing "addons arrive through deployment config, never through :deps"
    (let [deps (edn/read-string (slurp "deps.edn"))
          artifact-names (->> deps :deps keys (map str) set)]
      (doseq [prefix forbidden-addon-ns-prefixes]
        (is (not-any? #(str/includes? % prefix) artifact-names)
            (str prefix " must not be a hive-mcp dependency"))))))

(deftest committed-deps-edn-has-no-local-root
  (testing ":local/root belongs in the gitignored local.deps.edn (personal
            builds), never in the committed deps.edn that ships to consumers"
    (let [deps (edn/read-string (slurp "deps.edn"))
          offenders (vec (local-root-coords deps))]
      (is (empty? offenders)
          (str "committed deps.edn must carry no :local/root; found "
               (count offenders) ": " offenders
               ". Move sibling/local overrides to local.deps.edn.")))))

(deftest committed-deps-edn-has-no-concrete-backend
  (testing "concrete storage/query/vector backends arrive via local.deps.edn +
            slot-factory late-bind, never as a committed dependency"
    (let [deps (edn/read-string (slurp "deps.edn"))
          declared (->> (all-dep-maps deps) (mapcat keys) (map str) set)
          offenders (vec (filter declared forbidden-backend-artifacts))]
      (is (empty? offenders)
          (str "committed deps.edn must declare no concrete backend; found: "
               offenders ". These belong in local.deps.edn.")))))
