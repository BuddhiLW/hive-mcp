(ns hive-mcp.agent.sdk.execution
  "SDK query execution via persistent ClaudeSDKClient for multi-turn dispatch."
  (:require [clojure.core.async :as async :refer [chan >!! close!]]
            [hive-mcp.agent.sdk.python :as py]
            [hive-mcp.agent.sdk.session :as session]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn execute-phase!
  "Execute a single SAA phase via the persistent ClaudeSDKClient."
  [ling-id prompt phase]
  (let [out-ch (chan 1024)
        sess (session/get-session ling-id)
        safe-id (session/ling-id->safe-id ling-id)
        client-var (or (:client-ref sess)
                       (str "_hive_client_" safe-id))
        loop-var (or (:py-loop-var sess)
                     (str "_hive_loop_" safe-id))
        results-var (str "_hive_results_" safe-id)
        session-var (str "_hive_session_" safe-id)
        turn-count (inc (or (:turn-count sess) 0))]
    (log/info "[sdk.execution] Starting phase" {:ling-id ling-id
                                                :phase phase
                                                :turn turn-count
                                                :prompt (subs prompt 0 (min 100 (count prompt)))})
    (session/update-session! ling-id {:phase phase
                                      :phase-started-at (System/currentTimeMillis)
                                      :turn-count turn-count})
    (async/thread
      (try
        (py/py-run (str "import asyncio\n"
                        "\n"
                        "async def _hive_query_" safe-id "_t" turn-count "():\n"
                        "    client = globals().get('" client-var "')\n"
                        "    if client is None:\n"
                        "        raise RuntimeError('No persistent client for " safe-id "')\n"
                        "    results = []\n"
                        "    session_id = None\n"
                        "    await client.query(" (pr-str prompt) ")\n"
                        "    async for msg in client.receive_response():\n"
                        "        results.append(str(msg))\n"
                        "        if hasattr(msg, 'session_id'):\n"
                        "            session_id = msg.session_id\n"
                        "    return results, session_id\n"
                        "\n"
                        "_hive_query_future_" safe-id " = asyncio.run_coroutine_threadsafe(\n"
                        "    _hive_query_" safe-id "_t" turn-count "(),\n"
                        "    " loop-var "\n"
                        ")\n"
                        results-var ", " session-var " = _hive_query_future_" safe-id ".result()\n"))
        (let [results (py/py->clj (py/py-get-global results-var))
              session-id (py/py->clj (py/py-get-global session-var))]
          (when session-id
            (session/update-session! ling-id {:session-id session-id}))
          (doseq [msg (if (sequential? results) results [results])]
            (>!! out-ch {:type :message :phase phase :data msg})))
        (catch Exception e
          (log/error "[sdk.execution] Phase execution failed"
                     {:ling-id ling-id :phase phase :turn turn-count
                      :error (ex-message e)})
          (>!! out-ch {:type :error :phase phase :error (ex-message e)}))
        (finally
          (try
            (py/py-run (str "globals().pop('" results-var "', None)\n"
                            "globals().pop('" session-var "', None)\n"
                            "globals().pop('_hive_query_future_" safe-id "', None)\n"))
            (catch Exception _ nil))
          (close! out-ch)
          (session/update-session! ling-id {:phase-ended-at (System/currentTimeMillis)}))))
    out-ch))
