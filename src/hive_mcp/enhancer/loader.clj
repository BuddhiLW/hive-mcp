(ns hive-mcp.enhancer.loader
  "Dynamic loader for IKnowledgeEnhancer implementations.

   CLARITY-L: Layers stay pure - this bridges core/extension boundary.

   Attempts to load enhanced implementation via extension point.
   Falls back to BasicEnhancer if not available.

   This pattern allows hive-mcp to be fully functional without
   optional extensions while gaining enhanced capabilities
   when extensions are available on the classpath."
  (:require [hive-mcp.enhancer.protocol :as proto]
            [hive-mcp.enhancer.basic :as basic]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- try-load-enhanced
  "Attempt to load the enhanced extension enhancer.

   Uses extension registry to avoid compile-time dependency.
   Returns the enhancer instance or nil if loading fails."
  []
  (try
    (when-let [create-fn (requiring-resolve 'hive-mcp.extensions.registry/get-extension)]
      (when-let [enhancer-fn (create-fn :enhancer/create)]
        (enhancer-fn)))
    (catch Exception e
      ;; Expected when enhanced extension not on classpath
      ;; Log at debug level only
      (when (System/getProperty "hive.debug")
        (println "[enhancer] Enhanced extension not available:" (.getMessage e)))
      nil)))

(defn load-enhancer
  "Load the best available knowledge enhancer.

   Attempts to load in order:
   1. Enhanced extension (if available)
   2. BasicEnhancer fallback (always available)

   Sets the loaded enhancer as active via set-enhancer!.

   Returns:
     The loaded enhancer instance (also set as active)

   Side effects:
     - Loads enhanced extension if available
     - Sets active enhancer via protocol/set-enhancer!"
  []
  (let [enhancer (or (try-load-enhanced)
                     (basic/create-basic-enhancer))]
    (proto/set-enhancer! enhancer)
    enhancer))

(defn enhancer-type
  "Get the type of the currently active enhancer.

   Returns:
     :enhanced    - extension enhancer loaded
     :basic       - fallback BasicEnhancer
     :none        - no enhancer loaded yet"
  []
  (if (proto/enhancer-set?)
    (let [enhancer (proto/get-enhancer)]
      (if (instance? hive_mcp.enhancer.basic.BasicEnhancer enhancer)
        :basic
        :enhanced))
    :none))

(defn reload-enhancer!
  "Force reload of the enhancer.

   Useful for development when enhanced extensions have been
   added to the classpath after initial load.

   Returns:
     The newly loaded enhancer instance"
  []
  (load-enhancer))
