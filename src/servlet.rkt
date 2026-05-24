#lang racket
(provide start)

(require web-server/servlet)
(require web-server/http/response-structs)
(require json)
(require "graph.rkt")
(require "html-gen.rkt")

(define cors-headers
  (list (make-header #"Access-Control-Allow-Origin"  #"*")
        (make-header #"Access-Control-Allow-Methods" #"GET, POST, OPTIONS")
        (make-header #"Access-Control-Allow-Headers" #"Content-Type")))

; Main servlet function: handles requests, extracts data, and produces responses
(define (start request)
  (define method (request-method request))

  (cond
    ; Browser preflight — must reply 200 with CORS headers and no body
    [(equal? method #"OPTIONS")
     (response/output #:code 200 #:headers cors-headers
                      (lambda (out) (void)))]

    [(equal? method #"POST")
       (define data      (bytes->jsexpr (request-post-data/raw request)))
       (define input-str (hash-ref data 'input))

       (define json-hash
         (hash 'result (tokenize-to-html input-str)
               'image  (genera-img input-str)))

       (response/output #:code 200 #:mime-type #"application/json"
                        #:headers cors-headers
                        (lambda (out) (write-json json-hash out)))]

    ; GET — serve the page
    [else
     (response/output #:code 200 #:mime-type #"text/html"
                      #:headers cors-headers
                      (lambda (out) (display (file->string "index.html") out)))]))

