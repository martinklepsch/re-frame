(ns re-frame.cofx
  (:require
    [re-frame.db           :refer [app-db]]
    [re-frame.interceptor  :refer [->interceptor]]
    [re-frame.registry     :as reg]
    [re-frame.loggers      :refer [console]]))


;; -- Registration ------------------------------------------------------------

(def kind :cofx)
(assert (re-frame.registry/kinds kind))


;; -- Interceptor -------------------------------------------------------------

(defn inject-cofx
  "Returns an interceptor which adds to a `context's` `:coeffects`.

  `coeffects` are the input resources required by an event handler
   to perform its job. The two most obvious ones are `db` and `event`.
   But sometimes a handler might need other resources.

   Perhaps a handler needs a random number or a GUID or the current datetime.
   Perhaps it needs access to the connection to a DataScript database.

   If the handler directly access these resources, it stops being as
   pure. It immedaitely becomes harder to test, etc.

   So the necessary resources are \"injected\" into the `coeffect` (map)
   given the handler.

   Given an `id`, and an optional value, lookup the registered coeffect
   handler (previously registered via `reg-cofx`) and it with two arguments:
   the current value of `:coeffects` and, optionally, the value. The registered handler
   is expected to return a modified coeffect.
   "
  ([registry id]
  (->interceptor
    :id      :coeffects
    :before  (fn coeffects-before
               [context]
               (update context :coeffects (reg/get-handler registry kind id)))))
  ([registry id value]
   (->interceptor
     :id     :coeffects
     :before  (fn coeffects-before
                [context]
                (update context :coeffects (reg/get-handler registry kind id) value)))))


;; -- Builtin CoEffects Handlers  ---------------------------------------------

(defn register-built-in!
  [{:keys [app-db registry] :as frame}]
  (let [register (partial reg/register-handler registry kind)]
    (register
     :db
     (fn db-coeffects-handler
       [coeffects]
       (assoc coeffects :db @app-db)))))

;; Because this interceptor is used so much, we reify it
;; (def inject-db (inject-cofx :db))
