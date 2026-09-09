;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.channel.blocks
  "Delimited response blocks as an OPEN set: any addon registers one, the host
   renders whatever is registered and learns nothing about any of them.

   ## Why this exists

   The FRONTIER block was first landed the wrong way: a named key on the
   activation provider's answer, a bespoke clause in that provider's
   sanitizer, and a dedicated line in the middleware's `cond->`. Adding a
   SECOND such block meant editing the host again. That is the shape the
   ecosystem exists to avoid, and config.edn already shows the right one, where
   a memory store is `{:milvus {:addon :hive-milvus ...}}` and hive-mcp core
   knows no backend (axiom 20260711221408-2ae3b6ae).

   So: a block is a registered emitter, and WHICH emitters exist follows from
   which addons are configured. Adding one is an addon plus a config entry, not
   a commit here.

   ## The contract

   An addon registers under a `block`-namespaced key:

       (ext/register! :block/frontier (fn [ctx] -> body-or-nil))

   `ctx` is the per-request view `{:tool-name :cues :caller-id}`, which is what
   the pre-existing generic channel (`:cu/piggyback-drain`, keyed by caller-id
   alone) could not offer: it is buffered, so it cannot compute anything from
   the call it is riding. A block that wants to react to THIS call needs the
   ctx, and that is the whole reason this is a second seam rather than a reuse.

   The key's NAME becomes the tag: `:block/frontier` renders `---FRONTIER---`.
   A nil or blank body renders nothing, so an emitter with nothing to say costs
   one function call.

   ## Budget belongs here, not in each emitter's good manners

   A registry makes blocks cheap to add, and cheap-to-add is how a tool
   response turns into noise the agent learns to skip. `max-blocks` and
   `max-body-chars` are enforced in this loop, once, for everyone.

   Total: an emitter that throws costs its own block and nothing else."
  (:require [clojure.string :as str]
            [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))

(def key-namespace
  "Extension keys in this namespace are block emitters."
  "block")

(def max-blocks
  "Blocks one response may carry, after sorting. A response is a working
   surface, not a dashboard."
  6)

(def max-body-chars
  "Per-block body ceiling. An emitter that ignores its own size does not get to
   spend the whole response."
  4000)

(defn emitter-keys
  "Registered block-emitter keys, sorted, so block ORDER in a response is
   deterministic rather than a function of registration order."
  []
  (->> (ext/registered-keys)
       (filter #(= key-namespace (namespace %)))
       sort
       vec))

(defn tag-of
  "Wire tag for an emitter key: `:block/frontier` -> \"FRONTIER\"."
  [k]
  (str/upper-case (name k)))

(defn- emit-one
  "[tag body] for one emitter, or nil.

   Truncation is marked rather than silent: a body cut without a mark reads as
   a complete answer that happens to end oddly."
  [k ctx]
  (try
    (when-let [f (ext/get-extension k)]
      (when-let [body (f ctx)]
        (let [s (if (string? body) body (pr-str body))]
          (when-not (str/blank? s)
            [(tag-of k)
             (if (> (count s) max-body-chars)
               (str (subs s 0 max-body-chars) " ...[truncated]")
               s)]))))
    (catch Throwable t
      (log/debug t "blocks: emitter" k "failed; its block is skipped")
      nil)))

(defn render
  "`[[tag body] ...]` for every registered emitter that produced something,
   at most `max-blocks`. Never throws."
  [ctx]
  (try
    (into [] (comp (keep #(emit-one % ctx)) (take max-blocks)) (emitter-keys))
    (catch Throwable t
      (log/debug t "blocks: render failed; response carries no blocks")
      [])))
