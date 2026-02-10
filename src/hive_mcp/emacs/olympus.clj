(ns hive-mcp.emacs.olympus
  "Pure Clojure core for Olympus grid view layout calculation.

   SOLID-S: Single Responsibility - only layout calculation, no I/O.
   CLARITY-L: Pure domain logic, no side effects.

   Layout algorithm:
   n=1: {:rows 1 :cols 1}           ; full screen
   n=2: {:rows 1 :cols 2}           ; side-by-side
   n=3: {:rows 2 :cols 2 :empty #{[1 1]}}  ; 2+1
   n=4: {:rows 2 :cols 2}           ; 2x2
   n=5+: {:tabs (ceil n/4) :per-tab 4}  ; tabbed"
  (:require [clojure.spec.alpha :as s]))

;;; =============================================================================
;;; Specs (Domain Contracts)
;;; =============================================================================

(s/def ::rows nat-int?)
(s/def ::cols nat-int?)
(s/def ::empty-cells (s/coll-of (s/tuple nat-int? nat-int?) :kind set?))
(s/def ::tabs pos-int?)
(s/def ::per-tab pos-int?)

(s/def ::grid-layout
  (s/keys :req-un [::rows ::cols]
          :opt-un [::empty-cells]))

(s/def ::tabbed-layout
  (s/keys :req-un [::tabs ::per-tab]))

(s/def ::layout
  (s/or :grid ::grid-layout
        :tabbed ::tabbed-layout
        :empty (s/keys :req-un [::rows ::cols])))

(s/def ::position
  (s/keys :req-un [::row ::col]
          :opt-un [::tab]))

(s/def ::ling
  (s/keys :req [:slave/id]
          :opt [:slave/name]))

;;; =============================================================================
;;; Layout Calculation (Pure Functions)
;;; =============================================================================

(defn calculate-layout
  "Calculate optimal grid layout for n lings.

   Returns layout spec:
   - n=0: {:rows 0 :cols 0}
   - n=1: {:rows 1 :cols 1}
   - n=2: {:rows 1 :cols 2}
   - n=3: {:rows 2 :cols 2 :empty-cells #{[1 1]}}
   - n=4: {:rows 2 :cols 2}
   - n=5+: {:tabs (ceil n/4) :per-tab 4}

   Arguments:
   - n: Number of lings to layout

   Returns: Layout specification map"
  [n]
  ;; TODO: Implement layout calculation
  ;; Stub returns structure matching tests
  (cond
    (or (nil? n) (neg? n) (zero? n))
    {:rows 0 :cols 0}

    (= n 1)
    {:rows 1 :cols 1}

    (= n 2)
    {:rows 1 :cols 2}

    (= n 3)
    {:rows 2 :cols 2 :empty-cells #{[1 1]}}

    (= n 4)
    {:rows 2 :cols 2}

    :else ;; n >= 5: tabbed layout
    {:tabs (int (Math/ceil (/ n 4)))
     :per-tab 4}))

(defn assign-positions
  "Assign lings to grid positions based on layout.

   Arguments:
   - lings: Sequence of ling maps with :slave/id
   - layout: Layout specification from calculate-layout

   Returns: Map of {ling-id -> {:row R :col C :tab T}}"
  [lings layout]
  ;; TODO: Implement position assignment
  ;; Stub returns structure matching tests
  (if (empty? lings)
    {}
    (let [tabbed? (contains? layout :tabs)
          empty-cells (or (:empty-cells layout) #{})]
      (if tabbed?
        ;; Tabbed layout: distribute across tabs
        (into {}
              (map-indexed
               (fn [idx ling]
                 (let [tab (quot idx (:per-tab layout))
                       pos-in-tab (mod idx (:per-tab layout))
                       row (quot pos-in-tab 2)
                       col (mod pos-in-tab 2)]
                   [(:slave/id ling) {:row row :col col :tab tab}]))
               lings))
        ;; Grid layout: fill cells, skip empty
        (let [rows (or (:rows layout) 1)
              cols (or (:cols layout) 1)
              all-cells (for [r (range rows)
                              c (range cols)
                              :when (not (contains? empty-cells [r c]))]
                          [r c])]
          (into {}
                (map (fn [ling [row col]]
                       [(:slave/id ling) {:row row :col col :tab nil}])
                     lings
                     all-cells)))))))

;;; =============================================================================
;;; Layout Queries (Pure Functions)
;;; =============================================================================

(defn grid-capacity
  "Calculate total capacity of a layout.

   Arguments:
   - layout: Layout specification

   Returns: Maximum lings that fit in layout"
  [layout]
  (if (contains? layout :tabs)
    (* (:tabs layout) (:per-tab layout))
    (let [total (* (or (:rows layout) 0) (or (:cols layout) 0))
          empty-count (count (or (:empty-cells layout) #{}))]
      (- total empty-count))))

(defn tabbed?
  "Check if layout uses tabs.

   Arguments:
   - layout: Layout specification

   Returns: Boolean"
  [layout]
  (contains? layout :tabs))

(defn position-for-cell
  "Get the ling at a specific grid cell.

   Arguments:
   - positions: Position map from assign-positions
   - row: Row index (0-based)
   - col: Column index (0-based)
   - tab: Optional tab index (nil for non-tabbed)

   Returns: Ling ID or nil"
  [positions row col tab]
  (some (fn [[ling-id pos]]
          (when (and (= (:row pos) row)
                     (= (:col pos) col)
                     (= (:tab pos) tab))
            ling-id))
        positions))
