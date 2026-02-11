(ns hive-mcp.agent.sdk.phase-compress
  "Phase-boundary compression for SAA lifecycle transitions."
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol IPhaseCompressor
  "Protocol for compressing observations at SAA phase boundaries."
  (compress-phase [this phase-name observations opts]))

(defrecord NoOpPhaseCompressor []
  IPhaseCompressor
  (compress-phase [_ phase-name observations _opts]
    (let [obs-strs (map str (take 20 observations))
          compressed (str/join "\n" obs-strs)]
      (log/debug "[phase-compress] NoOp compression"
                 {:phase phase-name
                  :observation-count (count observations)
                  :kept (count obs-strs)})
      {:compressed-context compressed
       :entries-created 0
       :compressor :noop})))

(defn resolve-compressor
  "Resolve the best available phase compressor, falling back to NoOp."
  []
  (or (try
        (let [ctor (requiring-resolve
                    'hive-agent-bridge.compress/->KGPhaseCompressor)]
          (log/info "[phase-compress] Resolved custom compressor from hive-agent-bridge")
          (ctor))
        (catch Exception _
          (log/debug "[phase-compress] hive-agent-bridge not available, using NoOp")
          nil))
      (->NoOpPhaseCompressor)))
