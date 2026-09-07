(ns hive-mcp.tools.kanban.list.source
  "Port for reading a scoped kanban board (DIP seam of `kanban list`).

   `IBoardSource/scoped-board` takes the scope spec and a FetchPlan and
   answers {:entries [...] :multi-project? bool} with EVERY row of the
   scoped board that carries the plan's required tags, up to the plan's
   window. The facade-backed adapter lives in
   hive-mcp.tools.memory-kanban.query (it is the boundary); this ns holds
   the protocol and the in-memory adapter both are held to.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol IBoardSource
  (scoped-board [this scope-spec fetch-plan]
    "scope-spec: {:project-id s :include-descendants? bool :scope \"all\"|nil}.
     fetch-plan: hive-mcp.tools.kanban.list.schema/FetchPlan.
     Returns {:entries [entry ...] :multi-project? bool}."))

(defn- project-of
  [entry]
  (or (:project-id entry)
      (some (fn [t]
              (when (and (string? t) (.startsWith ^String t "scope:project:"))
                (subs t (count "scope:project:"))))
            (:tags entry))))

(defrecord SeqBoardSource [entries]
  IBoardSource
  (scoped-board [_ {:keys [project-id scope]} {:keys [required-tags window]}]
    (let [all?    (= scope "all")
          visible (->> entries
                       (filter (fn [e]
                                 (and (or all? (nil? project-id)
                                          (= project-id (project-of e)))
                                      (every? (set (:tags e)) required-tags))))
                       (take window)
                       vec)]
      {:entries        visible
       :multi-project? (boolean (or all? (next (distinct (map project-of visible)))))})))

(defn ->seq-source
  "In-memory board source over a seq of entries."
  [entries]
  (->SeqBoardSource (vec entries)))
