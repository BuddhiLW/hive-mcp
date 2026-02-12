(ns hive-mcp.agent.sdk.event-loop
  "Persistent asyncio event loop management for SDK sessions."
  (:require [hive-mcp.agent.sdk.python :as py]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn start-session-loop!
  "Start a persistent asyncio event loop in a background daemon thread."
  [safe-id]
  (let [loop-var (str "_hive_loop_" safe-id)
        thread-var (str "_hive_loop_thread_" safe-id)]
    (py/py-run (str "import asyncio\n"
                    "import threading\n"
                    "\n"
                    loop-var " = asyncio.new_event_loop()\n"
                    "\n"
                    "def _hive_run_loop_" safe-id "():\n"
                    "    asyncio.set_event_loop(" loop-var ")\n"
                    "    " loop-var ".run_forever()\n"
                    "\n"
                    thread-var " = threading.Thread(\n"
                    "    target=_hive_run_loop_" safe-id ",\n"
                    "    daemon=True,\n"
                    "    name='hive-sdk-loop-" safe-id "'\n"
                    ")\n"
                    thread-var ".start()\n"))
    (log/info "[sdk.event-loop] Persistent event loop started" {:safe-id safe-id})
    {:loop-var loop-var :thread-var thread-var}))

(defn connect-session-client!
  "Connect a ClaudeSDKClient on the persistent event loop."
  [safe-id options-obj loop-var]
  (let [client-var (str "_hive_client_" safe-id)]
    (py/py-set-global! "_hive_spawn_options" options-obj)
    (py/py-run (str "import asyncio\n"
                    "from claude_code_sdk import ClaudeSDKClient\n"
                    "\n"
                    "async def _hive_connect_" safe-id "():\n"
                    "    client = ClaudeSDKClient(options=_hive_spawn_options)\n"
                    "    await client.connect()\n"
                    "    globals()['" client-var "'] = client\n"
                    "    return True\n"
                    "\n"
                    "_hive_connect_future_" safe-id " = asyncio.run_coroutine_threadsafe(\n"
                    "    _hive_connect_" safe-id "(),\n"
                    "    " loop-var "\n"
                    ")\n"
                    "_hive_connect_future_" safe-id ".result(timeout=60)\n"))
    (try (py/py-run (str "globals().pop('_hive_spawn_options', None)\n"
                         "globals().pop('_hive_connect_future_" safe-id "', None)\n"))
         (catch Exception _ nil))
    (log/info "[sdk.event-loop] Client connected on persistent loop" {:safe-id safe-id})
    client-var))

(defn disconnect-session-client!
  "Disconnect the ClaudeSDKClient from the persistent event loop."
  [safe-id loop-var client-var]
  (try
    (py/py-run (str "import asyncio\n"
                    "\n"
                    "async def _hive_disconnect_" safe-id "():\n"
                    "    client = globals().get('" client-var "')\n"
                    "    if client is not None:\n"
                    "        await client.disconnect()\n"
                    "    globals().pop('" client-var "', None)\n"
                    "    return True\n"
                    "\n"
                    "_hive_dc_future_" safe-id " = asyncio.run_coroutine_threadsafe(\n"
                    "    _hive_disconnect_" safe-id "(),\n"
                    "    " loop-var "\n"
                    ")\n"
                    "_hive_dc_future_" safe-id ".result(timeout=30)\n"))
    (try (py/py-run (str "globals().pop('_hive_dc_future_" safe-id "', None)\n"))
         (catch Exception _ nil))
    (log/info "[sdk.event-loop] Client disconnected" {:safe-id safe-id})
    (catch Exception e
      (log/warn "[sdk.event-loop] Client disconnect failed"
                {:safe-id safe-id :error (ex-message e)}))))

(defn stop-session-loop!
  "Stop the persistent event loop and join the background thread."
  [safe-id loop-var]
  (try
    (py/py-run (str loop-var ".call_soon_threadsafe(" loop-var ".stop)\n"))
    (Thread/sleep 100)
    (py/py-run (str "globals().pop('" loop-var "', None)\n"
                    "globals().pop('_hive_loop_thread_" safe-id "', None)\n"))
    (log/info "[sdk.event-loop] Event loop stopped" {:safe-id safe-id})
    (catch Exception e
      (log/warn "[sdk.event-loop] Event loop stop failed"
                {:safe-id safe-id :error (ex-message e)}))))

(defn interrupt-session-client!
  "Send client.interrupt() via asyncio.run_coroutine_threadsafe."
  [client-var loop-var]
  (try
    (let [asyncio-mod (py/py-import "asyncio")
          client     (py/py-get-global client-var)
          loop-obj   (py/py-get-global loop-var)]
      (if (and client loop-obj (py/py-call loop-obj "is_running"))
        (let [coro       (py/py-call client "interrupt")
              future-obj (py/py-call asyncio-mod "run_coroutine_threadsafe"
                                     coro loop-obj)]
          (py/py-call future-obj "result" 10)
          {:success? true})
        {:success? false}))
    (catch Exception e
      (log/error "[sdk.event-loop] Interrupt exception" {:error (ex-message e)})
      {:success? false :error (ex-message e)})))
