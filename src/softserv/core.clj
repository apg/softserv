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

(defrecord SoftServer [socket pool thread]
  Shutdownable
  (shutdown [s]
    (do
      (shutdown (:socket s))
      (shutdown (:pool s))
      (.interrupt (:thread s)))))

(defn- dispatch
  [genfun hmap s pool]
  (.submit pool
           (fn []
             (let [req (genfun s
                               (.getInputStream s)
                               (.getOutputStream s))
                   operation (:operation req)
                   handler (get hmap operation (fn [] (shutdown s)))]
               (handler req
                        s
                        (.getInputStream s)
                        (.getOutputStream s))))))

(defn- create-server-aux
  [genfun handler-map size ss]
  (let [pool (Executors/newFixedThreadPool size)
        thread (Thread.
                #(try
                   (let [s (.accept ss)]
                     (dispatch genfun
                               handler-map
                               s
                               pool)
                     (recur))
                   (catch SocketException e)))]
    (.start thread)
    (SoftServer. ss pool thread)))

(defn create-server
  "Creates a server"
  ([port genfun handler-map size backlog #^InetAddress bind-addr]
     (create-server-aux genfun handler-map size
                        (ServerSocket. port backlog bind-addr)))
  ([port genfun handler-map size backlog]
     (create-server-aux genfun handler-map size
                        (ServerSocket. port backlog)))
  ([port genfun handler-map size]
     (create-server-aux genfun handler-map size
                        (ServerSocket. port))))


(comment
;; an echo / date server
(defn req-reader
  [s in out]
  (pr (.getRemoteSocketAddress s))
  (binding [*in* (BufferedReader. (InputStreamReader. in))]
    (let [line (read-line)]
      (if (= line "date")
        {:operation :date
         :data line}
        {:operation :echo
         :data line}))))

(defn echo-handler
  [req s in out]
  (binding [*out* (BufferedWriter. (OutputStreamWriter. out))]
    (prn (:data req))
    (shutdown s)))

(defn date-handler
  [req s in out]
  (binding [*out* (BufferedWriter. (OutputStreamWriter. out))]
    (prn (str (new java.util.Date)))
    (shutdown s)))

(def handler-map {:echo echo-handler
                  :date date-handler})

(create-server 20000 req-reader handler-map 10))
