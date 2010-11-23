# softserv

A simple replacement for contrib.socket-server

## Usage

To use, write a function that takes a socket and produces some sort of 
request object.

    (defn echo-date-parser [s]
       (with-in-reader s
         (let [l (read-line)]
           (assoc {:data l} :type (if (= l "date") :date :echo)))))
    
Next, use `defservice` giving it a name (in this case, `echo-date`, a
function which when given a request returns a dispatchable object, and
then the function you created to parse the request.

   (defservice echo-date :type echo-date-parser)

Once the service is defined, you can now create handlers that take that
request and continue processing it. The two handlers below handle `:echo`
requests, and `:date` requests

    (defhandler echo-date :echo
       [s req]
       (with-out-writer s
         (with-shutdown s
            (println (:data req)))))

    (defhandler echo-date :date
       [s req]
       (with-out-writer s
         (with-shutdown s
            (println (str (java.util.Date.))))))

Finally, setup a server on port 20000 which handles requests using the
`echo-date` service and runs in a thread pool of 10 threads.

   (create-server 20000 echo-date 10)
   
Other useful things.



## Installation

Add `[softserv "0.1-SNAPSHOT"]` to your `:dependencies` in project.clj

## License

Copyright (C) 2010 Andrew Gwozdziewycz

Distributed under the Eclipse Public License, the same as Clojure.
