(ns re-frame.core
  (:require
    [re-frame.events           :as events]
    [re-frame.subs             :as subs]
    [re-frame.interop          :as interop]
    [re-frame.db               :as db]
    [re-frame.fx               :as fx]
    [re-frame.cofx             :as cofx]
    [re-frame.router           :as router]
    [re-frame.loggers          :as loggers]
    [re-frame.registrar        :as registrar]
    [re-frame.registry         :as reg]
    [re-frame.interceptor      :as interceptor]
    [re-frame.std-interceptors :as std-interceptors :refer [db-handler->interceptor
                                                             fx-handler->interceptor
                                                             ctx-handler->interceptor]]
    [clojure.set               :as set]))


;; -- state
(def registry (reg/make-registry))
(def ev-queue (router/->EventQueue :idle interop/empty-queue {} registry))

;; --  dispatch
(def dispatch         (partial router/dispatch ev-queue))
(def dispatch-sync    (partial router/dispatch-sync ev-queue registry))


;; XXX move API functions up to this core level - to enable code completion and docs
;; XXX on figwheel reload, is there a way to not get the re-registration messages.


;; -- interceptor related
;; useful if you are writing your own interceptors
(def ->interceptor   interceptor/->interceptor)
(def enqueue         interceptor/enqueue)
(def get-coeffect    interceptor/get-coeffect)
(def get-effect      interceptor/get-effect)
(def assoc-effect    interceptor/assoc-effect)
(def assoc-coeffect  interceptor/assoc-coeffect)


;; --  standard interceptors
(def debug       std-interceptors/debug)
(def path        std-interceptors/path)
(def enrich      std-interceptors/enrich)
(def trim-v      std-interceptors/trim-v)
(def after       std-interceptors/after)
(def on-changes  std-interceptors/on-changes)


;; --  subscriptions
(defn reg-sub-raw
  "Associate a given `query id` with a given subscription handler function `handler-fn`
   which is expected to take two arguments: app-db and query vector, and return
   a `reaction`.

  This is a low level, advanced function.  You should probably be using reg-sub
  instead."
  [query-id handler-fn]
  (reg/register-handler registry subs/kind query-id handler-fn))

(def reg-sub      (partial subs/reg-sub registry))
(def subscribe    (partial subs/subscribe registry))

(def clear-sub    (partial reg/clear-handlers registry subs/kind))
(def clear-subscription-cache! subs/clear-subscription-cache!)

;; -- effects
(def reg-fx      (partial reg/register-handler registry fx/kind))
(def clear-fx    (partial reg/clear-handlers registry fx/kind))
(def fx-do-fx    (fx/do-fx registry))
(fx/register-built-in! registry ev-queue)

;; -- coeffects
(def reg-cofx    (partial reg/register-handler registry cofx/kind))
(def inject-cofx (partial cofx/inject-cofx registry))
(def clear-cofx  (partial reg/clear-handlers registry cofx/kind))
(def cofx-inject-db (cofx/inject-cofx registry :db))
(cofx/register-built-in! registry)


;; --  Events
(def clear-event (partial reg/clear-handlers registry events/kind))

(defn reg-event-db
  "Register the given `id`, typically a keyword, with the combination of
  `db-handler` and an interceptor chain.
  `db-handler` is a function: (db event) -> db
  `interceptors` is a collection of interceptors, possibly nested (needs flattening).
  `db-handler` is wrapped in an interceptor and added to the end of the chain, so in the end
   there is only a chain.
   The necessary effects and coeffects handler are added to the front of the
   interceptor chain.  These interceptors ensure that app-db is available and updated."
  ([id db-handler]
    (reg-event-db id nil db-handler))
  ([id interceptors db-handler]
   (events/register registry id [cofx-inject-db fx-do-fx interceptors (db-handler->interceptor db-handler)])))


(defn reg-event-fx
  ([id fx-handler]
   (reg-event-fx id nil fx-handler))
  ([id interceptors fx-handler]
   (events/register registry id [cofx-inject-db fx-do-fx interceptors (fx-handler->interceptor fx-handler)])))


(defn reg-event-ctx
  ([id handler]
   (reg-event-ctx id nil handler))
  ([id interceptors handler]
   (events/register registry id [cofx-inject-db fx-do-fx interceptors (ctx-handler->interceptor handler)])))


;; --  Logging -----
;; Internally, re-frame uses the logging functions: warn, log, error, group and groupEnd
;; By default, these functions map directly to the js/console implementations,
;; but you can override with your own fns (set or subset).
;; Example Usage:
;;   (defn my-fn [& args]  (post-it-somewhere (apply str args)))  ;; here is my alternative
;;   (re-frame.core/set-loggers!  {:warn my-fn :log my-fn})       ;; override the defaults with mine
(def set-loggers! loggers/set-loggers!)

;; If you are writing an extension to re-frame, like perhaps
;; an effects handler, you may want to use re-frame logging.
;;
;; usage:  (console :error "this is bad: " a-variable " and " anotherv)
;;         (console :warn "possible breach of containment wall at: " dt)
(def console loggers/console)


;; -- State Restoration For Unit Tests

(defn make-restore-fn
  "Checkpoints the state of re-frame and returns a function which, when
  later called, will restore re-frame to that checkpointed state.

  Checkpoint includes app-db, all registered handlers and all subscriptions.
  "
  []
  (let [handlers (-> registry :kind->id->handler deref)
        app-db   @db/app-db
        subs-cache @subs/query->reaction]
    (fn []
      ;; call `dispose!` on all current subscriptions which
      ;; didn't originally exist.
      (let [original-subs (set (vals subs-cache))
            current-subs  (set (vals @subs/query->reaction))]
        (doseq [sub (set/difference current-subs original-subs)]
          (interop/dispose! sub)))

      ;; Reset the atoms
      ;; We don't need to reset subs/query->reaction, as
      ;; disposing of the subs removes them from the cache anyway
      (reset! (:kind->id->handler registry) handlers)
      (reset! db/app-db app-db)
      nil)))


;; -- Event Processing Callbacks

(defn add-post-event-callback
  "Registers a function `f` to be called after each event is processed
   `f` will be called with two arguments:
    - `event`: a vector. The event just processed.
    - `queue`: a PersistentQueue, possibly empty, of events yet to be processed.

   This is useful in advanced cases like:
     - you are implementing a complex bootstrap pipeline
     - you want to create your own handling infrastructure, with perhaps multiple
       handlers for the one event, etc.  Hook in here.
     - libraries providing 'isomorphic javascript' rendering on  Nodejs or Nashorn.

  'id' is typically a keyword. Supplied at \"add time\" so it can subsequently
  be used at \"remove time\" to get rid of the right callback.
  "
  ([f]
   (add-post-event-callback f f))   ;; use f as its own identifier
  ([id f]
   (router/add-post-event-callback ev-queue id f)))


(defn remove-post-event-callback
  [id]
  (router/remove-post-event-callback ev-queue id))


;; --  Deprecation Messages
;; Assisting the v0.0.7 ->  v0.0.8 transition.
(defn register-handler
  [& args]
  (console :warn  "re-frame:  \"register-handler\" has been renamed \"reg-event-db\" (look for registration of " (str (first args)) ")")
  (apply reg-event-db args))

(defn register-sub
  [& args]
  (console :warn  "re-frame:  \"register-sub\" is deprecated. Use \"reg-sub-raw\" (look for registration of " (str (first args)) ")")
  (apply reg-sub-raw args))
