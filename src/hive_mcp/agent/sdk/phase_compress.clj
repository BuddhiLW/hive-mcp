(ns hive-mcp.agent.sdk.phase-compress
  "Phase-boundary compression for SAA lifecycle.

   At each SAA phase transition (silence->abstract, abstract->act),
   observations are compressed into a compact context string that gets
   injected into the next phase's prompt.

   Extension point:
   - NoOpPhaseCompressor (default): Returns observations as-is, relies on
     Claude's native context compaction. Zero external dependencies.
   - Custom compressors can be provided via hive-agent-bridge, resolved
     at runtime via requiring-resolve."
  (:require [clojure.string :as str]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol IPhaseCompressor
  "Protocol for compressing observations at SAA phase boundaries.

   Implementations receive the phase name, collected observations, and
   an options map. They return a map with:
     :compressed-context  string  — compact context for the next phase
     :entries-created     int     — entries created (0 for NoOp)
     :compressor          keyword — :noop or implementation-specific"
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
  "Resolve the best available phase compressor.

   Attempts to load a custom compressor from hive-agent-bridge.
   Falls back to NoOpPhaseCompressor if unavailable.

   Returns an IPhaseCompressor instance."
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
