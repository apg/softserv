;; This is a horribly stupid example of a UDP "DNS" server.
;;
;; The way it works though is pretty simple, especially as
;; illustrated via netcat
;;
;; You'll need at least 2 terminals.
;;
;; In the first, run the results window:
;;
;; $ nc -u -l -p 20001
;;
;; In the second window, you'll make queries
;;
;; $ echo "slashdot.org" | nc -u 127.0.0.1 20001
;; ^C
;;
;; On standard out of the server you should see some log messages
;; and on the first terminal (which is listening on 20001) you'll
;; get the result.

(ns softserv.examples.stupiddns
  (:use [softserv :only (defservice defhandler create-udp-server)]
        [clojure.contrib.logging :as log])
  (:import [java.io BufferedReader BufferedWriter InputStreamReader]
           [java.net InetAddress DatagramPacket DatagramSocket]))

(def *sender* (DatagramSocket.))

(defn query-parser [p]
  (let [l (-> (.getData p) String. .trim)]
    (if (re-matches #"\d{1,3}(?:\.\d{1,3}){3}" l)
      {:type :echo :query l}
      {:type :domain :query l})))

(defn lookup [h]
  (try
    (-> h InetAddress/getByName .getHostAddress)
    (catch Exception e
      "NOT FOUND")))

(defn send-to [addr msg]
  (.send *sender*
         (let [bs (.getBytes msg)]
           (DatagramPacket. bs 0 (count bs) addr 20001))))

(defservice stupid-dns :type query-parser
  (fn [& args]
    (println "WHAT IS GOING ON????")))

(defhandler stupid-dns :echo
  [p req]
  (let [msg (str "That's the same: " {:query req} "\n")]
    (log/info msg)
    (send-to (.getAddress p) msg)))

(defhandler stupid-dns :domain
  [p req]
  (let [a (lookup (:query req))
        msg (str (:query req) " has the address: " a "\n")]
    (log/info msg)
    (send-to (.getAddress p) msg)))

;; (create-udp-server :port 20000 :service stupid-dns :size 10)

