(ns hive-mcp.nrepl.classloader-gc
  "nREPL middleware to prevent DynamicClassLoader proliferation.

   Problem:
   --------
   bb-mcp sends stateless evals ({:op \"eval\" :code ...}) without a :session key.
   nREPL's session middleware creates a NEW session for each such eval, and each
   session creates a new clojure.lang.DynamicClassLoader. These classloaders hold
   references to evaluated code, preventing GC. Under bb-mcp's high-volume MCP
   tool forwarding (every Claude tool call = one nREPL eval), this causes:
   - 20K+ DynamicClassLoader instances in the heap
   - GC death spiral as old-gen fills with unreachable classloader graphs
   - Eventually OOM or sustained full-GC pauses

   Solution:
   ---------
   This middleware intercepts session-less eval ops and assigns them to a single
   shared 'bb-mcp-pooled' session. The shared session reuses one DynamicClassLoader
   for all bb-mcp evals. Since bb-mcp evals are stateless tool calls (they don't
   define new classes or need classloader isolation), this is safe.

   Additionally, the shared session's classloader is periodically rotated to allow
   GC of code compiled in previous cycles. This bounds the maximum classloader
   retention to one rotation window.

   Tradeoff:
   ---------
   - bb-mcp evals share a single classloader -> no per-eval isolation
   - defn redefinition within bb-mcp evals works (same as normal REPL)
   - require/import works (delegated to parent classloader)
   - Normal CIDER sessions are UNAFFECTED (they provide their own :session key)

   Architecture:
   -------------
   This middleware sits BEFORE nREPL's session middleware in the stack. When it sees
   an eval op without a :session, it injects the shared session ID. nREPL's session
   middleware then finds the existing session instead of creating a new one."
  (:require [nrepl.middleware :as middleware]
            [nrepl.transport :as transport]
            [taoensso.timbre :as log])
  (:import [clojure.lang DynamicClassLoader]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Shared Session State
;; =============================================================================

(defonce ^:private shared-session-id
  (atom nil))

(defonce ^:private session-eval-counter
  (atom 0))

;; Rotate the shared session after this many evals to bound classloader retention.
;; Default: 10000 evals. Set to 0 to disable rotation.
(defonce ^:private classloader-rotation-threshold
  (atom 10000))

;; =============================================================================
;; Classloader Counting (for monitoring/testing)
;; =============================================================================

(defn count-dynamic-classloaders
  "Count live DynamicClassLoader instances via the current thread's classloader chain.
   This is an approximation -- actual count requires heap walking.
   Returns a map with :chain-depth and :thread-context-cl info."
  []
  (let [tcl (.getContextClassLoader (Thread/currentThread))]
    (loop [cl tcl
           depth 0
           dcl-count 0]
      (if (nil? cl)
        {:chain-depth depth
         :dynamic-classloader-count dcl-count
         :thread-context-classloader (str tcl)}
        (recur (.getParent cl)
               (inc depth)
               (if (instance? DynamicClassLoader cl)
                 (inc dcl-count)
                 dcl-count))))))

;; =============================================================================
;; Session Rotation
;; =============================================================================

(defn- should-rotate?
  "Check if the shared session should be rotated based on eval count.
   Returns true if threshold > 0 and counter exceeds threshold."
  []
  (let [threshold @classloader-rotation-threshold]
    (and (pos? threshold)
         (>= @session-eval-counter threshold))))

(defn- rotate-session!
  "Force rotation of the shared session by clearing the session ID.
   The next eval will trigger nREPL's session middleware to create a new session,
   which becomes the new shared session. The old session (and its classloader)
   becomes eligible for GC once all in-flight evals complete."
  []
  (let [old-id @shared-session-id]
    (reset! shared-session-id nil)
    (reset! session-eval-counter 0)
    (log/info "Rotated bb-mcp shared nREPL session"
              {:old-session-id old-id
               :classloader-info (count-dynamic-classloaders)})))

;; =============================================================================
;; Middleware Implementation
;; =============================================================================

(defn wrap-shared-classloader
  "nREPL middleware that pins session-less evals to a shared session.

   When an eval op arrives WITHOUT a :session key (the bb-mcp pattern):
   1. If we have a shared session ID, inject it into the request
   2. If not, let nREPL create a new session and capture its ID from the response
   3. Periodically rotate the shared session to bound classloader retention

   Evals WITH a :session key (CIDER, normal REPL) pass through untouched."
  [handler]
  (fn [{:keys [op session] :as msg}]
    (if (and (= op "eval") (nil? session))
      ;; Session-less eval: bb-mcp tool forwarding path
      (let [current-shared @shared-session-id]
        (if current-shared
          ;; Reuse existing shared session
          (do
            (swap! session-eval-counter inc)
            ;; Check rotation after incrementing
            (when (should-rotate?)
              (rotate-session!))
            ;; Inject the shared session ID into the request.
            ;; nREPL's session middleware will find this existing session
            ;; instead of creating a new one (and a new classloader).
            (handler (assoc msg :session current-shared)))

          ;; No shared session yet -- let nREPL create one, capture the ID
          ;; Wrap the transport to intercept the response and capture session ID.
          (let [original-transport (:transport msg)
                capturing-transport (reify transport/Transport
                                      (recv [_] (transport/recv original-transport))
                                      (recv [_ timeout] (transport/recv original-transport timeout))
                                      (send [_ response]
                                        ;; Capture the session ID from the first response
                                        (when-let [new-session (:session response)]
                                          (when (compare-and-set! shared-session-id nil new-session)
                                            (reset! session-eval-counter 1)
                                            (log/info "Pinned bb-mcp shared nREPL session"
                                                      {:session-id new-session})))
                                        (transport/send original-transport response)))]
            (handler (assoc msg :transport capturing-transport)))))

      ;; Non-eval op or has explicit session: pass through unmodified
      (handler msg))))

;; =============================================================================
;; Middleware Descriptor (for nREPL middleware resolution)
;; =============================================================================

(middleware/set-descriptor! #'wrap-shared-classloader
                            {:requires #{}
                             :expects #{"eval" "session"}
                             :handles {}})

;; =============================================================================
;; Configuration API
;; =============================================================================

(defn set-rotation-threshold!
  "Set the eval count threshold for shared session rotation.
   Set to 0 to disable rotation (shared session lives forever).
   Default: 10000."
  [n]
  (reset! classloader-rotation-threshold n)
  (log/info "Set classloader rotation threshold to" n))

(defn status
  "Get current shared session status for monitoring."
  []
  {:shared-session-id @shared-session-id
   :eval-count @session-eval-counter
   :rotation-threshold @classloader-rotation-threshold
   :classloader-info (count-dynamic-classloaders)})

(defn force-rotate!
  "Force immediate rotation of the shared session.
   Useful for manual GC pressure relief."
  []
  (rotate-session!)
  (status))

(comment
  ;; Monitor classloader status
  (status)

  ;; Force rotation when heap pressure is high
  (force-rotate!)

  ;; Disable rotation (single classloader forever)
  (set-rotation-threshold! 0)

  ;; Count classloaders in the chain
  (count-dynamic-classloaders))
