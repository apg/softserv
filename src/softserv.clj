(ns softserv
  (:use [clojure.contrib.logging :as log])
  (:import [java.util.concurrent ExecutorService Executors]
           [java.io BufferedReader BufferedWriter
            InputStreamReader OutputStreamWriter]
           [java.net Socket ServerSocket DatagramSocket
             DatagramPacket SocketException InetAddress]))

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
  DatagramSocket
  (shutdown [ds]
    (when-not (.isClosed ds)
      (.close ds)))
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
  [service-fn size ss connector-fn]
  (let [pool (Executors/newFixedThreadPool size)
        thread (Thread.
                (fn []
                  (loop []
                   (try
                     (let [s (connector-fn ss)]
                       (.execute pool #(service-fn s))
                       (recur))
                     (catch SocketException e
                       (log/error e))))))]
    (.start thread)
    (SoftServer. ss pool thread)))

(defn create-server
  "Creates a server"
  [& {:keys [port service backlog bind size]}]
  (create-server-aux service
                     (or size 8)
                     (ServerSocket. port
                                    (or backlog 5)
                                    (or bind (InetAddress/getLocalHost)))
                     #(.accept %)))

(defn create-udp-server
  "Creates a Datagram server"
  [& {:keys [port service bind size]}]
  (create-server-aux service
                     (or size 8)
                     (DatagramSocket. port
                                      (or bind (InetAddress/getByName "0.0.0.0")))
                     #(let [p (DatagramPacket. (byte-array 1024) 1024)]
                        (.receive % p)
                        p)))
