# Plan: result-dsl refactor — addons/pool.clj (11 try-catch)

## Prerequisite
Add `[hive-dsl.result :as result]` to ns `:require`.

## 11 try-catch blocks — classification & action

### RESCUE (9 blocks → `result/rescue`)

| # | Line | Function | Before | After |
|---|------|----------|--------|-------|
| 1 | L90 | `conn-healthy?` | `try bridge-status, catch log/debug + false` | `(result/rescue false (let [status ...] (:connected? status)))` |
| 2 | L129 | `spawn-connection!` outer | `try factory-fn, catch log/error + nil` | `(result/rescue nil (let [bridge (factory-fn)] ...))` |
| 3 | L135 | `spawn-connection!` inner | `try transport-type, catch :unknown` | `(result/rescue :unknown (bridge/transport-type bridge))` |
| 4 | L186 | `borrow` evict-stop | `try stop-bridge!, catch _` | `(result/rescue nil (bridge/stop-bridge! ...))` |
| 5 | L236 | `evict!` stop | `try stop-bridge!, catch log/debug` | `(result/rescue nil (bridge/stop-bridge! ...))` |
| 6 | L291 | `drain!` cancel future | `try .cancel, catch _` | `(result/rescue nil (.cancel hf false))` |
| 7 | L293 | `drain!` shutdown sched | `try .shutdownNow, catch _` | `(result/rescue nil (.shutdownNow sched))` |
| 8 | L300 | `drain!` stop-all loop | `doseq + try + mutable errors atom` | Functional: `mapv` + `rescue nil` + `keep` metadata for errors |
| 9 | L337 | `run-health-check!` stop dead | `try stop-bridge!, catch _` | `(result/rescue nil (bridge/stop-bridge! ...))` |

### ALREADY-MINIMAL (2 blocks — leave as-is, add `; boundary` comment)

| # | Line | Function | Reason |
|---|------|----------|--------|
| 10 | L353 | `start-health-scheduler!` | Runnable boundary — exception escape kills ScheduledExecutor silently |
| 11 | L447 | `with-connection` macro | try/finally resource cleanup — not error handling |

## Execution order (single pass, top-to-bottom)

1. **ns require** — add `result` alias
2. **#1** `conn-healthy?` (L90) — rescue false
3. **#2+#3** `spawn-connection!` (L129) — rescue nil (outer) + rescue :unknown (inner)
4. **#4** `borrow` (L186) — rescue nil (one-liner)
5. **#5** `evict!` (L236) — rescue nil
6. **#6+#7** `drain!` scheduler cleanup (L291-293) — two rescue nil
7. **#8** `drain!` stop-all loop (L300) — refactor to functional pipeline:
   ```clojure
   ;; BEFORE: doseq + mutable errors atom
   ;; AFTER:
   (let [conns   @(:all-conns pool)
         results (mapv (fn [conn]
                         (result/rescue nil (bridge/stop-bridge! (:bridge conn))))
                       conns)
         errors  (into []
                       (keep #(some-> (meta %) ::result/error :message))
                       results)]
     ...)
   ```
8. **#9** `run-health-check!` (L337) — rescue nil
9. **#10** `start-health-scheduler!` (L353) — add `; boundary` comment only
10. **#11** `with-connection` (L447) — add `; boundary` comment only

## Post-refactor verification

- `grep -c '(try' src/hive_mcp/addons/pool.clj` → **2** (boundaries #10, #11)
- No `log/warn` or `log/debug` inside catch blocks
- Public API unchanged: same defn names, same arities
- `(require 'hive-mcp.addons.pool :reload)` — clean compile
