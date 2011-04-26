(ns softserv.examples.http-quick
  (:use [[softserv :only (defservice defhandler create-server)]
         [clojure.string :only (split)]] ))

(use '[clojure.string :only (split)])

(defn http-quick [s]
  (binding [*in* (BufferedReader.
                  (InputStreamReader.
                   (.getInputStream s)))]
    (let [l (read-line)]
      (let [[method path proto] (split l #"\s")]
        {:method method :path path :proto proto}))))

(defservice http-server :path http-quick
  (fn [s req]
    (binding [*out* (BufferedWriter.
                     (OutputStreamWriter.
                      (.getOutputStream s)))]
      (with-shutdown s
        (print"500 ERROR\r\n\r\nAn error occurred")))))

(defhandler http-server "/about"
  [s req]
  (binding [*out* (BufferedWriter.
                   (OutputStreamWriter.
                    (.getOutputStream s)))]
    (with-shutdown s
      (Thread/sleep 30000)
      (print "200 OK\r\n\r\nIndex"))))

(defhandler http-server "/"
  [s req]
  (binding [*out* (BufferedWriter.
                   (OutputStreamWriter.
                    (.getOutputStream s)))]
    (with-shutdown s
      (print "200 OK\r\n\r\nIndex"))))

(create-server :port 20000 :service http-server :size 1) 
