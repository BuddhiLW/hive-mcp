(ns hive-mcp.tools.olympus
  "MCP tool handlers for Olympus grid control.

   SOLID-S: Tool handlers only - orchestrates olympus core + DataScript.
   CLARITY-L: Adapter layer between MCP and domain.

   Tools:
   - olympus_status: Get current grid layout and ling positions
   - olympus_focus: Focus/maximize specific ling
   - olympus_arrange: Trigger grid arrangement
   - olympus_tab: Navigate between tabs"
  (:require [hive-mcp.olympus :as olympus]
            [hive-mcp.swarm.datascript.core :as ds]
            [hive-mcp.events.core :as ev]))

;;; =============================================================================
;;; DataScript Queries
;;; =============================================================================

(defn- get-active-lings
  "Query all active lings from DataScript.
   Returns sequence of ling maps with :slave/id, :slave/name."
  []
  ;; TODO: Implement DataScript query
  ;; Stub returns empty for now
  [])

(defn- get-olympus-state
  "Query current Olympus state from DataScript.
   Returns {:active-tab :layout-mode :ling-positions}."
  []
  ;; TODO: Implement DataScript query
  {:active-tab 0
   :layout-mode :auto
   :ling-positions {}})

(defn- save-olympus-state!
  "Persist Olympus state to DataScript.

   Arguments:
   - state: Map with :active-tab :layout-mode :ling-positions"
  [state]
  ;; TODO: Implement DataScript transaction
  nil)

;;; =============================================================================
;;; Event Emission
;;; =============================================================================

(defn- emit-layout-changed!
  "Emit event when layout changes (for Emacs to respond)."
  [layout positions]
  (ev/dispatch! {:event/type :olympus/layout-changed
                 :layout layout
                 :positions positions}))

(defn- emit-focus-changed!
  "Emit event when focus changes."
  [ling-id]
  (ev/dispatch! {:event/type :olympus/focus-changed
                 :ling-id ling-id}))

;;; =============================================================================
;;; Tool Handlers
;;; =============================================================================

(defn handle-olympus-status
  "Get current grid layout and ling positions.

   MCP Tool: olympus_status

   Parameters: (none)

   Returns:
   {:success true
    :ling-count N
    :layout {:rows R :cols C} or {:tabs T :per-tab P}
    :positions {ling-id {:row R :col C :tab T}}
    :active-tab N
    :layout-mode :auto|:manual|:stacked}"
  [params]
  (let [lings (get-active-lings)
        state (get-olympus-state)
        layout (olympus/calculate-layout (count lings))
        positions (olympus/assign-positions lings layout)]
    {:success true
     :ling-count (count lings)
     :layout layout
     :positions positions
     :active-tab (:active-tab state)
     :layout-mode (:layout-mode state)}))

(defn handle-olympus-focus
  "Focus/maximize a specific ling.

   MCP Tool: olympus_focus

   Parameters:
   - ling-id: Specific ling ID to focus
   - position: Position number (1-4) to focus
   - restore: If true, restore grid view (unfocus)

   Returns:
   {:success true/false
    :focused-ling ling-id or nil
    :error message if failed}"
  [{:keys [ling-id position restore]}]
  (cond
    restore
    (do
      (emit-focus-changed! nil)
      {:success true
       :focused-ling nil})

    ling-id
    (let [lings (get-active-lings)
          exists? (some #(= (:slave/id %) ling-id) lings)]
      (if exists?
        (do
          (emit-focus-changed! ling-id)
          {:success true
           :focused-ling ling-id})
        {:success false
         :error (str "Ling not found: " ling-id)}))

    position
    (let [state (get-olympus-state)
          positions (:ling-positions state)
          ;; Find ling at position (1-indexed, left-to-right, top-to-bottom)
          row (quot (dec position) 2)
          col (mod (dec position) 2)
          ling-id (olympus/position-for-cell positions row col (:active-tab state))]
      (if ling-id
        (do
          (emit-focus-changed! ling-id)
          {:success true
           :focused-ling ling-id})
        {:success false
         :error (str "No ling at position " position)}))

    :else
    {:success false
     :error "Must specify ling-id, position, or restore"}))

(defn handle-olympus-arrange
  "Trigger grid arrangement.

   MCP Tool: olympus_arrange

   Parameters:
   - mode: :auto (default), :manual, or :stacked

   Returns:
   {:success true
    :layout-mode :auto|:manual|:stacked
    :refreshed true}"
  [{:keys [mode]}]
  (let [layout-mode (or mode :auto)
        lings (get-active-lings)
        layout (olympus/calculate-layout (count lings))
        positions (olympus/assign-positions lings layout)]
    ;; Persist state
    (save-olympus-state! {:active-tab 0
                          :layout-mode layout-mode
                          :ling-positions positions})
    ;; Emit event for Emacs
    (emit-layout-changed! layout positions)
    {:success true
     :layout-mode layout-mode
     :refreshed true}))

(defn handle-olympus-tab
  "Navigate between tabs (for 5+ lings).

   MCP Tool: olympus_tab

   Parameters:
   - direction: :next or :prev
   - tab: Specific tab number to jump to (0-indexed)

   Returns:
   {:success true
    :active-tab N}"
  [{:keys [direction tab]}]
  (let [state (get-olympus-state)
        current-tab (:active-tab state)
        lings (get-active-lings)
        layout (olympus/calculate-layout (count lings))
        max-tabs (or (:tabs layout) 1)
        new-tab (cond
                  tab (min (max 0 tab) (dec max-tabs))
                  (= direction :next) (mod (inc current-tab) max-tabs)
                  (= direction :prev) (mod (dec current-tab) max-tabs)
                  :else current-tab)]
    (save-olympus-state! (assoc state :active-tab new-tab))
    {:success true
     :active-tab new-tab}))

;;; =============================================================================
;;; Tool Registration (for tools.clj)
;;; =============================================================================

(def tool-handlers
  "Map of tool names to handlers for registration."
  {"olympus_status" handle-olympus-status
   "olympus_focus" handle-olympus-focus
   "olympus_arrange" handle-olympus-arrange
   "olympus_tab" handle-olympus-tab})
