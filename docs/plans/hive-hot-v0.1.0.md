# hive-hot Events & MCP Integration Plan

## Overview

Add hive-events integration to existing hive-hot v0.1.0 and create MCP tools for external triggering.

**Scope:**
1. hive-events integration (adapter pattern)
2. MCP tools (hot_reload, hot_status)

**Non-scope:** File watcher (v0.2.0)

## Architecture

```
┌─────────────┐         ┌──────────────────┐         ┌─────────────┐
│  hive-hot   │ ──────► │  hot_bridge.clj  │ ──────► │ hive-events │
│  (unchanged)│         │  (adapter)       │         │             │
│  listeners  │         │  listener→event  │         │  dispatch   │
└─────────────┘         └──────────────────┘         └─────────────┘
       ▲                         │                          │
       │                         ▼                          ▼
       │                ┌──────────────────┐         ┌─────────────┐
       └────────────────│  :hot-reload fx  │◄────────│  MCP tools  │
                        │  (triggers hot)  │         │  hot_reload │
                        └──────────────────┘         │  hot_status │
                                                     └─────────────┘
```

**Key Design:** hive-hot remains dependency-free. Adapter lives in hive-mcp.

## Implementation Steps

### Phase 1: Event Handlers & Effects (hive-mcp)

#### 1.1 Add :hot-reload effect to `effects.clj`

**File:** `src/hive_mcp/events/effects.clj`

```clojure
(defn- handle-hot-reload
  "Execute :hot-reload effect - trigger namespace reload via hive-hot."
  [{:keys [opts]}]
  (try
    (require '[hive-hot.core :as hot])
    (let [reload! (resolve 'hive-hot.core/reload!)]
      (when reload!
        (reload! (or opts {}))))
    (catch Exception e
      (log/error "[hot-reload] Effect failed:" (.getMessage e)))))

;; Register in register-effects!
(ev/reg-fx :hot-reload handle-hot-reload)
```

#### 1.2 Add event handlers to `handlers.clj`

**File:** `src/hive_mcp/events/handlers.clj`

```clojure
;; Handler: :hot/reload-start
(defn- handle-hot-reload-start [_ [_ data]]
  {:log {:level :info :message "Hot reload starting"}
   :channel-publish {:event :hot-reload-start :data data}})

;; Handler: :hot/reload-success
(defn- handle-hot-reload-success [_ [_ {:keys [unloaded loaded ms]}]]
  {:log {:level :info :message (str "Reloaded " (count loaded) " ns in " ms "ms")}
   :channel-publish {:event :hot-reload-success
                     :data {:unloaded (mapv str unloaded)
                            :loaded (mapv str loaded)
                            :ms ms}}})

;; Handler: :hot/reload-error
(defn- handle-hot-reload-error [_ [_ {:keys [failed error]}]]
  {:log {:level :error :message (str "Reload failed: " failed)}
   :channel-publish {:event :hot-reload-error
                     :data {:failed (str failed)
                            :error (when error (.getMessage error))}}})

;; Register in register-handlers!
(ev/reg-event :hot/reload-start [] handle-hot-reload-start)
(ev/reg-event :hot/reload-success [] handle-hot-reload-success)
(ev/reg-event :hot/reload-error [] handle-hot-reload-error)
```

### Phase 2: Hot Bridge Adapter (NEW FILE)

**File:** `src/hive_mcp/events/hot_bridge.clj`

```clojure
(ns hive-mcp.events.hot-bridge
  "Adapter bridging hive-hot listener API to hive-events dispatch.

   SOLID: Adapter pattern - connects incompatible interfaces
   CLARITY: Composition over modification - doesn't change hive-hot"
  (:require [hive-mcp.events.core :as ev]
            [taoensso.timbre :as log]))

(defonce ^:private *connected (atom false))
(def ^:private listener-id :hive-events-bridge)

(defn- hot-event->hive-event
  "Transform hive-hot event map to hive-events vector."
  [{:keys [type] :as event}]
  (case type
    :reload-start   [:hot/reload-start (dissoc event :type)]
    :reload-success [:hot/reload-success (select-keys event [:unloaded :loaded :ms])]
    :reload-error   [:hot/reload-error (select-keys event [:failed :error])]
    :component-callback [:hot/component-callback (select-keys event [:component :callback])]
    nil))

(defn- on-hot-event [event]
  (when-let [hive-event (hot-event->hive-event event)]
    (log/debug "[hot-bridge] Dispatching:" (first hive-event))
    (ev/dispatch hive-event)))

(defn connect!
  "Connect to hive-hot by registering listener. Graceful if unavailable."
  []
  (if @*connected
    false
    (try
      (require '[hive-hot.core :as hot])
      (let [add-listener! (resolve 'hive-hot.core/add-listener!)]
        (add-listener! listener-id on-hot-event)
        (reset! *connected true)
        (log/info "[hot-bridge] Connected to hive-hot")
        true)
      (catch Exception e
        (log/warn "[hot-bridge] hive-hot not available:" (.getMessage e))
        false))))

(defn disconnect! []
  (when @*connected
    (try
      (let [remove-listener! (resolve 'hive-hot.core/remove-listener!)]
        (remove-listener! listener-id))
      (catch Exception _))
    (reset! *connected false)))

(defn connected? [] @*connected)
(defn init! [] (connect!))
```

### Phase 3: MCP Tools

#### 3.1 hot_reload tool

**File:** `src/hive_mcp/tools/hot.clj` (NEW)

```clojure
(ns hive-mcp.tools.hot
  "MCP tools for hot-reload triggering."
  (:require [hive-mcp.events.core :as ev]
            [taoensso.timbre :as log]))

(def hot-reload-tool
  {:name "hot_reload"
   :description "Trigger namespace hot-reload. Reloads changed namespaces and dependents."
   :inputSchema {:type "object"
                 :properties {:only {:type "string"
                                     :description "Filter: 'all' | 'loaded' | regex pattern"}}
                 :required []}})

(defn handle-hot-reload
  [{:keys [only]}]
  (let [opts (when only
               (case only
                 "all" {:only :all}
                 "loaded" {:only :loaded}
                 {:only (re-pattern only)}))]
    (try
      (require '[hive-hot.core :as hot])
      (let [reload! (resolve 'hive-hot.core/reload!)
            result (reload! (or opts {}))]
        {:success (:success result)
         :loaded (count (:loaded result))
         :unloaded (count (:unloaded result))
         :ms (:ms result)
         :error (when (:failed result) (str (:failed result)))})
      (catch Exception e
        {:success false :error (.getMessage e)}))))
```

#### 3.2 hot_status tool

```clojure
(def hot-status-tool
  {:name "hot_status"
   :description "Get hot-reload system status including registered components."
   :inputSchema {:type "object" :properties {} :required []}})

(defn handle-hot-status
  [_]
  (try
    (require '[hive-hot.core :as hot])
    (let [status (resolve 'hive-hot.core/status)
          s (status)]
      {:initialized (:initialized? s)
       :components (into {} (map (fn [[k v]]
                                   [k {:ns (str (:ns v))
                                       :status (:status v)
                                       :last-reload (str (:last-reload v))}])
                                 (:components s)))
       :listener-count (:listener-count s)})
    (catch Exception e
      {:available false :error (.getMessage e)})))
```

### Phase 4: Wire Up Integration

#### 4.1 Add hive-hot to deps.edn

**File:** `deps.edn`

```clojure
;; Add under aliases or main deps
hive-agi/hive-hot {:local/root "../hive-hot"}
```

#### 4.2 Register tools in tools.clj

**File:** `src/hive_mcp/tools.clj`

```clojure
(require '[hive-mcp.tools.hot :as hot-tools])

;; Add to get-tools or similar
hot-tools/hot-reload-tool
hot-tools/hot-status-tool
```

#### 4.3 Initialize bridge on startup

**File:** `src/hive_mcp/server.clj` (in startup sequence)

```clojure
(require '[hive-mcp.events.hot-bridge :as hot-bridge])
;; After events init
(hot-bridge/init!)
```

## Files to Create/Modify

| File | Action | Lines |
|------|--------|-------|
| `src/hive_mcp/events/hot_bridge.clj` | CREATE | ~60 |
| `src/hive_mcp/tools/hot.clj` | CREATE | ~70 |
| `src/hive_mcp/events/effects.clj` | MODIFY | +15 |
| `src/hive_mcp/events/handlers.clj` | MODIFY | +30 |
| `src/hive_mcp/tools.clj` | MODIFY | +5 |
| `src/hive_mcp/server.clj` | MODIFY | +3 |
| `deps.edn` | MODIFY | +1 |

## Event Flow

```
External Trigger:
  MCP hot_reload → hot/reload! → notify! :reload-success
                                      ↓
                              hot-bridge listener
                                      ↓
                       ev/dispatch [:hot/reload-success]
                                      ↓
                          handler → :channel-publish
                                      ↓
                              Emacs notification

Internal Trigger:
  REPL (hot/reload!) → notify! → hot-bridge → hive-events → Emacs
```

## Verification

### 1. Unit Tests

```clojure
;; test/hive_mcp/events/hot_bridge_test.clj
(deftest hot-event-transform-test
  (is (= [:hot/reload-success {:loaded ['a 'b] :ms 100}]
         (hot-event->hive-event {:type :reload-success :loaded ['a 'b] :ms 100}))))
```

### 2. Integration Test via nREPL

```clojure
;; Load the system
(require '[hive-mcp.events.core :as ev])
(require '[hive-mcp.events.hot-bridge :as hot-bridge])
(require '[hive-hot.core :as hot])

;; Initialize
(ev/init!)
(hot-bridge/init!)
(hot/init! {:dirs ["src"]})

;; Test bridge
(hot-bridge/connected?)  ;; => true

;; Test reload → events flow
(hot/reload!)
;; Should see: [hot-bridge] Dispatching: :hot/reload-success

;; Test effect
(ev/dispatch [:hot/reload {}])
;; Should trigger reload via effect
```

### 3. MCP Tool Test

```clojure
;; Via MCP
(handle-hot-reload {})
;; => {:success true :loaded 0 :unloaded 0 :ms N}

(handle-hot-status {})
;; => {:initialized true :components {} :listener-count 1}
```

## Success Criteria

- [ ] `hot-bridge/connect!` registers listener with hive-hot
- [ ] `hot/reload!` triggers `:hot/reload-success` event
- [ ] Event emits `:channel-publish` for Emacs notification
- [ ] MCP `hot_reload` tool triggers reload
- [ ] MCP `hot_status` tool returns system state
- [ ] Graceful degradation if hive-hot unavailable

## Dependencies

- hive-hot v0.1.0 (local reference)
- hive-events (existing in hive-mcp)
- No changes to hive-hot repo required
