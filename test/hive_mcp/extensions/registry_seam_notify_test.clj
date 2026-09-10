(ns hive-mcp.extensions.registry-seam-notify-test
  "Once hive-addon owns the command store AND a listener seam, two notifiers
   exist for one event: hive-addon's, and this facade's own direct call. Firing
   both delivers every contribution twice; firing neither is the silent-vanish
   failure the delegation work exists to end. Exactly one must fire.

   The facade registers with the seam by SOFT resolution, so it does the right
   thing on both sides of the hive-addon release that ships the seam. These
   tests pin the invariant that survives either way: a listener sees each event
   exactly once."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.registry.commands :as addon-cmds]
            [hive-mcp.extensions.registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tool "analysis")

(defn- seam-available? []
  (some? (try (requiring-resolve 'hive-addon.registry.commands/add-listener!)
              (catch Throwable _ nil))))

(defn- reset-world! []
  (registry/remove-contribution-listener! ::probe)
  (registry/clear-all!))

(use-fixtures :each (fn [t] (reset-world!) (t) (reset-world!)))

(deftest a-contribution-through-the-facade-notifies-exactly-once
  (let [seen (atom [])]
    (registry/add-contribution-listener! ::probe (fn [e] (swap! seen conj e)))
    (registry/contribute-commands! tool :kondo {"lint" {:handler identity}})
    (is (= 1 (count @seen))
        "duplicate delivery is what happens when both the seam and the facade
         fire; a surface refreshed twice per contribution is a bug that only
         shows up as churn")
    (is (= :contribute (:type (first @seen))))
    (is (= tool (:tool-name (first @seen))))))

(deftest a-retraction-notifies-exactly-once
  (let [seen (atom [])]
    (registry/contribute-commands! tool :kondo {"lint" {:handler identity}})
    (registry/add-contribution-listener! ::probe (fn [e] (swap! seen conj e)))
    (registry/retract-commands! tool :kondo)
    (is (= 1 (count @seen)))
    (is (= :retract (:type (first @seen))))))

(deftest retract-all-notifies-once-per-touched-tool
  (let [seen (atom [])]
    (registry/contribute-commands! "analysis" :kondo {"lint" {:handler identity}})
    (registry/contribute-commands! "code" :kondo {"fmt" {:handler identity}})
    (registry/contribute-commands! "memory" :other {"add" {:handler identity}})
    (registry/add-contribution-listener! ::probe (fn [e] (swap! seen conj e)))
    (registry/retract-all-by-addon! :kondo)
    (is (= 2 (count @seen)) "once per tool, not once per command and not twice per tool")
    (is (= #{"analysis" "code"} (set (map :tool-name @seen)))
        "a tool the addon never touched must not be announced as changed")))

(deftest a-contribution-made-DIRECTLY-on-the-seam-still-reaches-the-host
  (testing "this is the migration case the whole delegation exists for: an addon
            that stops going through the facade and calls hive-addon's registry
            itself must still refresh the advertised surface"
    (if (seam-available?)
      (let [seen (atom [])]
        (registry/add-contribution-listener! ::probe (fn [e] (swap! seen conj e)))
        ;; force the facade to register with the seam before we bypass it
        (registry/contribute-commands! tool :kondo {"lint" {:handler identity}})
        (reset! seen [])
        (addon-cmds/contribute! tool :cljs {"build" {:handler identity}})
        (is (= 1 (count @seen))
            "without the facade registering as a seam listener, this event is
             lost and the addon mounts green then vanishes from the surface")
        (is (= :cljs (:addon-id (first @seen)))))
      (is true "hive-addon on this classpath predates the seam; nothing to assert"))))

(deftest the-host-listener-satisfies-the-seams-no-throw-contract
  (testing "hive-addon's registry is in the portable three-host stratum and
            cannot catch for its listeners, so whatever we register there must
            not throw. The host notifier guards each listener individually."
    (registry/add-contribution-listener! ::probe (fn [_] (throw (ex-info "boom" {}))))
    (is (some? (registry/contribute-commands! tool :kondo {"lint" {:handler identity}}))
        "a throwing host listener must not break the contribution, whichever
         notifier delivered the event")
    (is (= ["lint"] (keys (registry/get-contributed-commands tool))))))
