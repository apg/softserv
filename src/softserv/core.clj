(ns softserv.core
  (:import [java.util.concurrent ExecutorService Executors]
           [java.io BufferedReader BufferedWriter
            InputStreamReader OutputStreamWriter]
           [java.net Socket ServerSocket SocketException InetAddress]))

(defprotocol Shutdownable
  (shutdown [s]))

(extend-protocol Shutdownable
  Socket
  (shutdown [s]
    (when-not (.isClosed s)
      (doto s
        (.shutdownInput)
        (.shutdownOutput)
        (.close))))
  ServerSocket
  (shutdown [ss]
    (when-not (.isClosed ss)
      (.close ss)))
  ExecutorService
  (shutdown [es]
    (when-not (.isShutdown es)
      (.shutdown es))))

(defmacro with-shutdown
  "Ensures shutdown of Shutdownable resource `s` after body is run"
  [#^Shutdownable s & body]
  `(do (try
         ~@body
         (finally
          (shutdown ~s)))))

(defprotocol IServiceFn
  (add-method [this value fn]))

(deftype ServiceFn [method-name
                    #^clojure.lang.Ref method-map
                    #^clojure.lang.IFn dispatch-fn
                    #^clojure.lang.AFn generator-fn
                    #^clojure.lang.AFn error-fn]
  IServiceFn
  (add-method [this dvalue func]
    (dosync
     (ref-set method-map (assoc @method-map dvalue func))))

  clojure.lang.IFn
  (invoke [this s]
    (let [request (generator-fn s)
          dispatch-value (dispatch-fn request)
          method (@method-map dispatch-value)]
      (if method
        (method s request)
        (if-not error-fn
          (#(shutdown %1) s request)
          (error-fn s request))))))

(defmacro defservice
  "Creates a new socket service.

  Arguments:
    dispatch-fn: a function (or keyword) which when given a request object
                 (a map) returns a value associated with a handler
    generator-fn: a function which when given a socket returns a request object
                  (a map).
    error-fn: (optional) if there's no handler for the request, this function
              will be called. The default is shutsdown the Socket.
  "
  {:arglists '([name docstring? dispatch-fn generator-fn error-fn])}
  [s-name & args]
  (let [docstring (if (string? (first args))
                    (first args)
                    nil)
        [dispatch-fn generator-fn error-fn] (if docstring
                                              (next args)
                                              args)
        m (if-let [m s-name]
            (when docstring
              (assoc m :doc docstring))
            (if docstring
              {:doc docstring}
              {}))]
    `(let [v# (def ~s-name)]
       (when-not (and (.hasRoot v#) (instance? ServiceFn (deref v#)))
         (def ~(with-meta s-name m)
           (ServiceFn. ~(name s-name)
                       (ref {})
                       ~dispatch-fn
                       ~generator-fn
                       ~error-fn))))))

(defmacro defhandler [s-name dispatch-val & fn-tail]
  `(add-method ~s-name ~dispatch-val (fn ~@fn-tail)))

(defrecord SoftServer [socket pool thread]
  Shutdownable
  (shutdown [s]
    (do
      (shutdown (:socket s))
      (shutdown (:pool s))
      (.interrupt (:thread s)))))

(defn- create-server-aux
  [service-fn size ss]
  (let [pool (Executors/newFixedThreadPool size)
        thread (Thread.
                #(try
                   (let [s (.accept ss)]
                     (service-fn s)
                     (recur))
                   (catch SocketException e)))]
    (.start thread)
    (SoftServer. ss pool thread)))

(defn create-server
  "Creates a server"
  ([port service-fn size backlog #^InetAddress bind-addr]
     (create-server-aux service-fn size
                        (ServerSocket. port backlog bind-addr)))
  ([port service-fn size backlog]
     (create-server-aux service-fn size
                        (ServerSocket. port backlog)))
  ([port service-fn size]
     (create-server-aux service-fn size
                        (ServerSocket. port))))

(comment
  ;; an echo / date server
  (defn echo-date-parser [s]
    (binding [*in* (BufferedReader.
                     (InputStreamReader.
                      (.getInputStream s)))]
      (let [l (read-line)]
        (assoc {:data l} :type (if (= l "date") :date :echo)))))
    
  (defservice echo-date :type echo-date-parser)

  (defhandler echo-date :echo
    [s req]
    (binding [*out* (BufferedWriter.
                     (OutputStreamWriter.
                      (.getOutputStream s)))]
      (with-shutdown s
        (println (:data req)))))

  (defhandler echo-date :date
    [s req]
    (binding [*out* (BufferedWriter.
                     (OutputStreamWriter.
                      (.getOutputStream s)))]
      (with-shutdown s
        (println (str (java.util.Date.))))))

  (create-server 20000 echo-date 10))
