(ns hive-mcp.enhancer.loader
  "Dynamic loader for IKnowledgeEnhancer implementations.

   CLARITY-L: Layers stay pure - this bridges open/closed boundary.

   Attempts to load the proprietary hive-knowledge implementation.
   Falls back to BasicEnhancer if not available.

   This pattern allows hive-mcp to be fully functional without
   proprietary components while gaining enhanced capabilities
   when hive-knowledge is on the classpath."
  (:require [hive-mcp.enhancer.protocol :as proto]
            [hive-mcp.enhancer.basic :as basic]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- try-load-proprietary
  "Attempt to load the proprietary hive-knowledge enhancer.

   Uses dynamic require to avoid compile-time dependency.
   Returns the enhancer instance or nil if loading fails."
  []
  (try
    (require 'hive-knowledge.core)
    (let [create-fn (resolve 'hive-knowledge.core/create-enhancer)]
      (when create-fn
        (create-fn)))
    (catch Exception e
      ;; Expected when hive-knowledge not on classpath
      ;; Log at debug level only
      (when (System/getProperty "hive.debug")
        (println "[enhancer] Proprietary enhancer not available:" (.getMessage e)))
      nil)))

(defn load-enhancer
  "Load the best available knowledge enhancer.

   Attempts to load in order:
   1. Proprietary hive-knowledge enhancer (if on classpath)
   2. BasicEnhancer fallback (always available)

   Sets the loaded enhancer as active via set-enhancer!.

   Returns:
     The loaded enhancer instance (also set as active)

   Side effects:
     - Requires hive-knowledge.core if available
     - Sets active enhancer via protocol/set-enhancer!"
  []
  (let [enhancer (or (try-load-proprietary)
                     (basic/create-basic-enhancer))]
    (proto/set-enhancer! enhancer)
    enhancer))

(defn enhancer-type
  "Get the type of the currently active enhancer.

   Returns:
     :proprietary - hive-knowledge enhancer loaded
     :basic       - fallback BasicEnhancer
     :none        - no enhancer loaded yet"
  []
  (if (proto/enhancer-set?)
    (let [enhancer (proto/get-enhancer)]
      (if (instance? hive_mcp.enhancer.basic.BasicEnhancer enhancer)
        :basic
        :proprietary))
    :none))

(defn reload-enhancer!
  "Force reload of the enhancer.

   Useful for development when hive-knowledge has been
   added to the classpath after initial load.

   Returns:
     The newly loaded enhancer instance"
  []
  (load-enhancer))
