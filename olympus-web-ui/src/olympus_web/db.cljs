(ns olympus-web.db
  "Initial app-db state for Olympus Web UI.
   
   State shape follows memory decision 20260204143952-3b6c35a5:
   - :connection - WebSocket status
   - :agents - Agent hierarchy (coordinator > lings > drones)
   - :waves - Wave task progress
   - :kg-entries - Knowledge graph memory nodes
   - :hivemind-messages - Recent coordination messages
   - :project-tree - Hierarchical project structure (HCR Wave 5)
   - :ui - View state and selections")

(def default-db
  {:connection {:ws nil
                :status :disconnected  ; :disconnected | :connecting | :connected
                :url "ws://localhost:9999"
                :reconnect-attempts 0
                :last-error nil}

   ;; Agents indexed by id
   ;; Shape: {id {:id "..." :type :ling|:drone :status :idle|:working|:completed
   ;;             :task "..." :parent-id nil|"parent-id" :project-id "..."}}
   :agents {}

   ;; Waves indexed by id
   ;; Shape: {id {:id "..." :status :pending|:running|:completed|:failed
   ;;             :tasks [{:file "..." :status :pending|:running|:completed|:failed :diff-id nil}]}}
   :waves {}

   ;; KG memory entries indexed by id
   ;; Shape: {id {:id "..." :type :note|:snippet|:convention|:decision|:axiom
   ;;             :content "..." :tags [...] :edges [{:to "id" :relation :implements|...}]}}
   :kg-entries {}

   ;; Recent hivemind messages (last 50)
   ;; Shape: [{:agent-id "..." :event-type :started|:progress|:completed|:error|:blocked
   ;;          :message "..." :timestamp 123456}]
   :hivemind-messages []

   ;; Project tree hierarchy (HCR Wave 5)
   ;; Shape: {:projects {id {:id "..." :path "..." :type "workspace"|"service"|...
   ;;                        :parent-id nil|"..." :tags [...] :ling-count 0 :git-root "..."}}
   ;;         :roots ["project-id" ...]
   ;;         :children {"parent-id" ["child-id" ...]}
   ;;         :expanded #{"project-id" ...}
   ;;         :selected-project nil}
   :project-tree {:projects {}
                  :roots []
                  :children {}
                  :expanded #{}
                  :selected-project nil}

   ;; Terminal state - xterm.js ling output viewers
   ;; Shape: {:tabs {ling-id {:id "..." :subscribed? bool :cursor 0 :buffer []}}
   ;;         :active-tab nil|ling-id
   ;;         :available-lings []}
   :terminal {:tabs {}
              :active-tab nil
              :available-lings []}

   ;; UI state
   :ui {:view :agent-graph  ; :agent-graph | :wave-graph | :kg-graph | :project-tree | :terminal-view
        :selected-id nil     ; Currently selected node
        :selected-type nil   ; :agent | :wave | :task | :kg | :project
        :filters {:agent-types #{:ling :drone}
                  :agent-statuses #{:idle :working :completed}
                  :memory-types #{:note :snippet :convention :decision :axiom}}
        :layout :TB          ; :TB (top-bottom) | :LR (left-right)
        :sidebar-collapsed? false}})
