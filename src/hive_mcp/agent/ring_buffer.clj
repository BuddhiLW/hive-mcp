(ns hive-mcp.agent.ring-buffer
  "Bounded ring buffer for stdout/stderr capture.

   Provider-agnostic infrastructure used by ProcessBuilder and OpenRouter
   backends for streaming output capture. Extracted from agent/headless.clj
   to enable reuse across headless backend implementations.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ===========================================================================
;; Ring Buffer
;; ===========================================================================

(def ^:const default-buffer-capacity
  "Default max lines in ring buffer."
  5000)

(defn create-ring-buffer
  "Create a bounded ring buffer for stdout/stderr capture."
  ([] (create-ring-buffer default-buffer-capacity))
  ([capacity]
   (atom {:lines []
          :timestamps []
          :capacity capacity
          :total-lines-seen 0
          :dropped 0})))

(defn ring-buffer-append!
  "Append a line to the ring buffer, dropping oldest if at capacity."
  [buffer line]
  (let [ts (System/currentTimeMillis)]
    (swap! buffer
           (fn [{:keys [lines timestamps capacity total-lines-seen dropped] :as state}]
             (let [new-lines (conj lines line)
                   new-ts (conj (or timestamps []) ts)
                   over (- (count new-lines) capacity)]
               (if (pos? over)
                 (assoc state
                        :lines (subvec new-lines over)
                        :timestamps (subvec new-ts over)
                        :total-lines-seen (inc total-lines-seen)
                        :dropped (+ dropped over))
                 (assoc state
                        :lines new-lines
                        :timestamps new-ts
                        :total-lines-seen (inc total-lines-seen))))))))

(defn ring-buffer-contents
  "Get current ring buffer contents, optionally last N lines."
  ([buffer] (ring-buffer-contents buffer {}))
  ([buffer {:keys [last-n]}]
   (let [{:keys [lines]} @buffer]
     (if (and last-n (pos? last-n) (> (count lines) last-n))
       (subvec lines (- (count lines) last-n))
       lines))))

(defn- binary-search-after
  "Find index of first element in sorted ts-vec strictly greater than since.
   Returns n (length) if all elements <= since."
  [ts-vec since n]
  (loop [lo 0 hi n]
    (if (>= lo hi)
      lo
      (let [mid (quot (+ lo hi) 2)]
        (if (<= (nth ts-vec mid) since)
          (recur (inc mid) hi)
          (recur lo mid))))))

(defn- make-line-entries
  "Create [{:text line :ts ts} ...] from lines and timestamps vectors."
  [lines ts-vec start end]
  (mapv (fn [i] {:text (nth lines i) :ts (nth ts-vec i)})
        (range start end)))

(defn ring-buffer-contents-since
  "Get ring buffer lines appended after a given timestamp."
  [buffer since]
  (let [{:keys [lines timestamps]} @buffer
        ts-vec (or timestamps [])
        n (count ts-vec)]
    (if (or (zero? n) (nil? since))
      (mapv (fn [line ts] {:text line :ts ts})
            lines
            (if (seq ts-vec) ts-vec (repeat (count lines) 0)))
      (let [idx (binary-search-after ts-vec since n)]
        (if (>= idx n)
          []
          (make-line-entries lines ts-vec idx n))))))

(defn ring-buffer-stats
  "Get ring buffer statistics."
  [buffer]
  (let [{:keys [lines capacity total-lines-seen dropped]} @buffer]
    {:current-lines (count lines)
     :capacity capacity
     :total-lines-seen total-lines-seen
     :dropped dropped}))
