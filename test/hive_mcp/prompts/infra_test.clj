(ns hive-mcp.prompts.infra-test
  "Pinning tests for D-Bus notification infrastructure.
   
   Tests verify:
   - notify! calls D-Bus with correct parameters
   - Graceful degradation when D-Bus unavailable
   - Specialized notification functions (permission-request, blocked, completed)
   
   Uses with-redefs to mock lambdaisland/dbus-client for isolated unit testing."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.prompts.infra :as infra]
            [lambdaisland.dbus.client :as dbus]))

;; =============================================================================
;; Test Fixtures & Helpers
;; =============================================================================

(def ^:dynamic *dbus-calls* nil)

(defn capture-dbus-calls-fixture
  "Fixture to capture D-Bus calls for verification."
  [f]
  (binding [*dbus-calls* (atom [])]
    (f)))

(use-fixtures :each capture-dbus-calls-fixture)

(defn mock-write-message
  "Mock D-Bus write-message that captures calls and returns success."
  [_client msg]
  (when *dbus-calls*
    (swap! *dbus-calls* conj msg))
  ;; Return a realized promise with notification ID
  (delay {:id 12345}))

(defn mock-init-client!
  "Mock D-Bus init-client! that returns a fake client."
  [_sock _handler]
  {:mock-client true})

(defn mock-session-sock
  "Mock session-sock that returns fake socket path."
  []
  "/run/user/1000/bus")

;; =============================================================================
;; Test: notify! Function
;; =============================================================================

(deftest test-notify!-sends-correct-dbus-message
  (testing "notify! sends correct D-Bus message structure"
    (with-redefs [dbus/session-sock mock-session-sock
                  dbus/init-client! mock-init-client!
                  dbus/write-message mock-write-message]
      ;; Reset the client to force re-initialization
      (reset! @#'infra/dbus-client nil)

      (infra/notify! "Test Title" "Test Body")

      (is (= 1 (count @*dbus-calls*)))
      (let [msg (first @*dbus-calls*)
            headers (:headers msg)
            body (:body msg)]
        ;; Verify D-Bus method call structure
        (is (= :method-call (:type msg)))
        (is (= "/org/freedesktop/Notifications" (:path headers)))
        (is (= "Notify" (:member headers)))
        (is (= "org.freedesktop.Notifications" (:interface headers)))
        (is (= "org.freedesktop.Notifications" (:destination headers)))

        ;; Verify body parameters (per freedesktop spec)
        (is (= "hive-mcp" (nth body 0))) ; app_name
        (is (= 0 (nth body 1))) ; replaces_id
        (is (= "" (nth body 2))) ; app_icon
        (is (= "Test Title" (nth body 3))) ; summary
        (is (= "Test Body" (nth body 4))) ; body
        (is (= [] (nth body 5))) ; actions
        (is (= {"urgency" 1} (nth body 6))) ; hints (default urgency)
        (is (= -1 (nth body 7))))))) ; timeout

(deftest test-notify!-with-custom-options
  (testing "notify! accepts custom urgency, app-name, icon, timeout"
    (with-redefs [dbus/session-sock mock-session-sock
                  dbus/init-client! mock-init-client!
                  dbus/write-message mock-write-message]
      (reset! @#'infra/dbus-client nil)

      (infra/notify! "Alert" "Critical message"
                     :urgency 2
                     :app-name "my-app"
                     :icon "/path/to/icon.png"
                     :timeout 5000)

      (is (= 1 (count @*dbus-calls*)))
      (let [body (:body (first @*dbus-calls*))]
        (is (= "my-app" (nth body 0))) ; custom app_name
        (is (= "/path/to/icon.png" (nth body 2))) ; custom icon
        (is (= {"urgency" 2} (nth body 6))) ; critical urgency
        (is (= 5000 (nth body 7))))))) ; custom timeout

(deftest test-notify!-dbus-unavailable-returns-nil
  (testing "notify! returns nil gracefully when D-Bus unavailable"
    (with-redefs [dbus/session-sock (fn [] (throw (Exception. "No D-Bus session")))
                  dbus/init-client! (fn [_ _] (throw (Exception. "Connection refused")))]
      (reset! @#'infra/dbus-client nil)

      (is (nil? (infra/notify! "Title" "Body"))))))

(deftest test-notify!-write-error-returns-nil
  (testing "notify! returns nil when D-Bus write fails"
    (with-redefs [dbus/session-sock mock-session-sock
                  dbus/init-client! mock-init-client!
                  dbus/write-message (fn [_ _] (throw (Exception. "Write failed")))]
      (reset! @#'infra/dbus-client nil)

      (is (nil? (infra/notify! "Title" "Body"))))))

(deftest test-notify!-reuses-client
  (testing "notify! reuses existing D-Bus client"
    (let [init-count (atom 0)]
      (with-redefs [dbus/session-sock mock-session-sock
                    dbus/init-client! (fn [_ _]
                                        (swap! init-count inc)
                                        {:mock-client true})
                    dbus/write-message mock-write-message]
        (reset! @#'infra/dbus-client nil)

        ;; Send multiple notifications
        (infra/notify! "First" "Message")
        (infra/notify! "Second" "Message")
        (infra/notify! "Third" "Message")

        ;; Client should only be initialized once
        (is (= 1 @init-count))
        (is (= 3 (count @*dbus-calls*)))))))

;; =============================================================================
;; Test: Specialized Notification Functions
;; =============================================================================

(deftest test-notify-permission-request!
  (testing "notify-permission-request! sends critical notification"
    (with-redefs [dbus/session-sock mock-session-sock
                  dbus/init-client! mock-init-client!
                  dbus/write-message mock-write-message]
      (reset! @#'infra/dbus-client nil)

      (infra/notify-permission-request! "agent-123" "file_edit" "Edit src/core.clj")

      (is (= 1 (count @*dbus-calls*)))
      (let [body (:body (first @*dbus-calls*))]
        ;; Should have lock emoji in title
        (is (clojure.string/includes? (nth body 3) "🔐"))
        (is (clojure.string/includes? (nth body 3) "file_edit"))
        ;; Body should include agent and summary
        (is (clojure.string/includes? (nth body 4) "agent-123"))
        (is (clojure.string/includes? (nth body 4) "Edit src/core.clj"))
        ;; Critical urgency (2)
        (is (= {"urgency" 2} (nth body 6)))
        ;; Never expire (0)
        (is (= 0 (nth body 7)))))))

(deftest test-notify-agent-blocked!
  (testing "notify-agent-blocked! sends normal urgency notification"
    (with-redefs [dbus/session-sock mock-session-sock
                  dbus/init-client! mock-init-client!
                  dbus/write-message mock-write-message]
      (reset! @#'infra/dbus-client nil)

      (infra/notify-agent-blocked! "agent-456" "Waiting for user input")

      (is (= 1 (count @*dbus-calls*)))
      (let [body (:body (first @*dbus-calls*))]
        ;; Should have pause emoji in title
        (is (clojure.string/includes? (nth body 3) "⏸️"))
        (is (clojure.string/includes? (nth body 3) "agent-456"))
        ;; Body should include reason
        (is (= "Waiting for user input" (nth body 4)))
        ;; Normal urgency (1)
        (is (= {"urgency" 1} (nth body 6)))))))

(deftest test-notify-agent-completed!
  (testing "notify-agent-completed! sends low urgency notification"
    (with-redefs [dbus/session-sock mock-session-sock
                  dbus/init-client! mock-init-client!
                  dbus/write-message mock-write-message]
      (reset! @#'infra/dbus-client nil)

      (infra/notify-agent-completed! "agent-789" "Tests passed: 42/42")

      (is (= 1 (count @*dbus-calls*)))
      (let [body (:body (first @*dbus-calls*))]
        ;; Should have checkmark emoji in title
        (is (clojure.string/includes? (nth body 3) "✅"))
        (is (clojure.string/includes? (nth body 3) "agent-789"))
        ;; Body should include summary
        (is (= "Tests passed: 42/42" (nth body 4)))
        ;; Low urgency (0)
        (is (= {"urgency" 0} (nth body 6)))))))

;; =============================================================================
;; Test: Edge Cases
;; =============================================================================

(deftest test-notify!-with-special-characters
  (testing "notify! handles special characters in title/body"
    (with-redefs [dbus/session-sock mock-session-sock
                  dbus/init-client! mock-init-client!
                  dbus/write-message mock-write-message]
      (reset! @#'infra/dbus-client nil)

      (infra/notify! "Title with <html> & \"quotes\""
                     "Body with\nnewlines\tand\ttabs")

      (is (= 1 (count @*dbus-calls*)))
      (let [body (:body (first @*dbus-calls*))]
        (is (= "Title with <html> & \"quotes\"" (nth body 3)))
        (is (= "Body with\nnewlines\tand\ttabs" (nth body 4)))))))

(deftest test-notify!-with-empty-body
  (testing "notify! handles empty body"
    (with-redefs [dbus/session-sock mock-session-sock
                  dbus/init-client! mock-init-client!
                  dbus/write-message mock-write-message]
      (reset! @#'infra/dbus-client nil)

      (infra/notify! "Title Only" "")

      (is (= 1 (count @*dbus-calls*)))
      (let [body (:body (first @*dbus-calls*))]
        (is (= "Title Only" (nth body 3)))
        (is (= "" (nth body 4)))))))

(deftest test-notify!-urgency-boundaries
  (testing "notify! accepts all valid urgency levels"
    (with-redefs [dbus/session-sock mock-session-sock
                  dbus/init-client! mock-init-client!
                  dbus/write-message mock-write-message]
      (reset! @#'infra/dbus-client nil)

      ;; Test each urgency level
      (doseq [urgency [0 1 2]]
        (infra/notify! "Test" "Body" :urgency urgency))

      (is (= 3 (count @*dbus-calls*)))
      (is (= [0 1 2]
             (map #(get-in % [:body 6 "urgency"]) @*dbus-calls*))))))
