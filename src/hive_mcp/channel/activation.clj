;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.channel.activation
  "Activation seam for the memory piggyback drain.

   `drain-ctx` builds the ctx that `memory-piggyback/drain!` hands to the
   ranker. Without a registered provider it is exactly `{:tokens <cues>}` —
   today's behaviour byte for byte. With one, the provider's `:pins` and
   `:floor-cap` are merged in and its `:tokens` are unioned onto the cues.

   DIP: this ns knows only the extension KEY and the shape of the answer. It
   never learns what a rule is, how eligibility is decided, or that core.logic
   exists — that lives behind `:memory/activation` (see
   `hive-knowledge.activation.addon`).

   Total: a provider that throws, hangs on a bad shape, or returns garbage
   degrades to the plain cue ctx. Activation must never be able to break a tool
   response."
  (:require [clojure.set :as set]
            [hive-mcp.extensions.registry :as ext]
            [taoensso.timbre :as log]))

(def extension-key
  "Registry key an activation provider registers under. The provider is
   `(fn [activation-ctx] -> {:pins #{id} :tokens #{token} :floor-cap n
                             :frontier [{:id :title :T :via}]})`."
  :memory/activation)

(def max-frontier
  "Proposals one response may carry. The frontier is a sidebar; a long one
   stops being read and starts costing what it was built to save."
  4)

(defn- name-str
  "Keyword, symbol or string to a plain string; nil for anything else."
  [x]
  (cond
    (string? x) (not-empty x)
    (or (keyword? x) (symbol? x)) (name x)
    :else nil))

(defn- sane-proposal
  "Coerce one frontier proposal to the wire shape, or nil.

   A proposal invites the agent to spend a fetch, so a row with no string id or
   no title is DROPPED rather than rendered: an id with nothing to judge it by
   spends context to say nothing. `:T` and `:via` are stringified because the
   block is read by an agent, not by a provider that cares about keywords."
  [p]
  (when (map? p)
    (let [id (name-str (:id p))
          title (name-str (:title p))]
      (when (and id title)
        (cond-> {:id id :title title}
          (name-str (:T p)) (assoc :T (name-str (:T p)))
          (name-str (:via p)) (assoc :via (name-str (:via p))))))))

(defn- sane
  "Coerce a provider answer to the subset of keys the ranker and the frontier
   block accept, dropping anything malformed. Returns nil when nothing usable
   survives."
  [answer]
  (when (map? answer)
    (let [pins (:pins answer)
          tokens (:tokens answer)
          cap (:floor-cap answer)
          frontier (when (sequential? (:frontier answer))
                     (into [] (comp (keep sane-proposal) (take max-frontier))
                           (:frontier answer)))]
      (cond-> nil
        (coll? pins) (assoc :pins (into #{} (filter string?) pins))
        (coll? tokens) (assoc :tokens (into #{} (filter string?) tokens))
        (and (integer? cap) (pos? cap)) (assoc :floor-cap cap)
        (seq frontier) (assoc :frontier frontier)))))

(defn provider
  "The registered activation provider, or nil."
  []
  (ext/get-extension extension-key))

(defn drain-ctx
  "Drain ctx for one tool call: `{:tokens cues}` merged with whatever the
   activation provider contributes.

   `activation-ctx` is the read-only view a rule gets — tool name, the cue
   tokens harvested under `task-signal`'s allowlist, and the caller. Rules see
   the ALLOWLISTED cues only; raw tool args never reach them, so
   `task-signal/denied-arg-keys` governs activation input by construction."
  [{:keys [tool-name cues caller-id] :as _activation-ctx}]
  (let [base {:tokens (or cues #{})}]
    (if-let [f (provider)]
      (try
        (if-let [{:keys [pins tokens floor-cap frontier]}
                 (sane (f {:tool-name tool-name :cues (or cues #{}) :caller-id caller-id}))]
          (cond-> base
            (seq tokens) (update :tokens set/union tokens)
            (seq pins) (assoc :pins pins)
            floor-cap (assoc :floor-cap floor-cap)
            (seq frontier) (assoc :frontier frontier))
          base)
        (catch Throwable t
          (log/debug t "activation: provider failed; drain falls back to cues")
          base))
      base)))
