(ns hive-mcp.extensions.registry-delegation-test
  "An addon depends on hive-di / hive-contracts / hive-addon, never on hive-mcp
   core, so hive-addon.registry.commands is the seam that owns command
   contributions and this host namespace is only a facade over it.

   While the host kept its own atom the two were separate implementations of one
   concept, and the failure that hid it was silent: an addon that migrated to the
   correct seam still mounted and still reported :status :ok, then vanished from
   the tool surface because nothing on the host side ever read hive-addon's
   store. These tests pin that ONE store backs both names, in both directions."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.registry.commands :as addon-cmds]
            [hive-mcp.extensions.registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tool "analysis")

(defn- reset-world! []
  (addon-cmds/clear!)
  (registry/clear-all!))

(use-fixtures :each (fn [t] (reset-world!) (t) (reset-world!)))

(deftest a-host-contribution-lands-in-the-addon-store
  (registry/contribute-commands! tool :kondo {"lint" {:handler identity :description "d"}})
  (is (= ["lint"] (keys (addon-cmds/get-commands tool)))
      "the host must not keep a second store the addon seam cannot see")
  (is (= :kondo (:addon (get (addon-cmds/get-commands tool) "lint")))
      "the contributing addon is stamped, so shutdown can retract exactly its own"))

(deftest an-addon-contribution-is-visible-through-the-host-facade
  (addon-cmds/contribute! tool :cljs {"build" {:handler identity}})
  (is (= ["build"] (keys (registry/get-contributed-commands tool)))
      "this is the migration that used to mount green and then vanish")
  (is (= [tool] (registry/contributed-tool-names))))

(deftest both-seams-accumulate-into-one-tree
  (registry/contribute-commands! tool :kondo {"lint" {:handler identity}})
  (addon-cmds/contribute! tool :cljs {"build" {:handler identity}})
  (is (= ["build" "lint"] (sort (keys (registry/get-contributed-commands tool))))
      "a half-migrated fleet must see every command, whichever seam placed it"))

(deftest retraction-reaches-across-the-facade
  (registry/contribute-commands! tool :kondo {"lint" {:handler identity}})
  (addon-cmds/contribute! tool :cljs {"build" {:handler identity}})
  (testing "the host retracts only the named addon's commands"
    (registry/retract-commands! tool :kondo)
    (is (= ["build"] (keys (registry/get-contributed-commands tool)))))
  (testing "retract-all-by-addon! clears the rest"
    (registry/retract-all-by-addon! :cljs)
    (is (empty? (registry/get-contributed-commands tool)))))

(deftest listeners-still-fire-around-the-delegated-store
  (let [events (atom [])]
    (registry/add-contribution-listener! ::probe (fn [e] (swap! events conj e)))
    (try
      (registry/contribute-commands! tool :kondo {"lint" {:handler identity}})
      (registry/retract-all-by-addon! :kondo)
      (is (= [:contribute :retract] (mapv :type @events))
          "notification is the host's own contribution on top of the seam;
           hive-addon does not own it and must not silently drop it")
      (is (= [tool tool] (mapv :tool-name @events))
          "retract-all-by-addon! reads the touched set BEFORE retracting, or it
           has nothing left to attribute the notification to")
      (finally (registry/remove-contribution-listener! ::probe)))))

(deftest clear-all-empties-the-single-store
  (addon-cmds/contribute! tool :cljs {"build" {:handler identity}})
  (registry/clear-all!)
  (is (empty? (addon-cmds/get-commands tool))
      "clearing the host must not leave contributions stranded in the seam"))
