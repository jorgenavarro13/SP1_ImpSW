#lang racket
(provide start)

(require web-server/servlet)
(require json)
(require "graph.rkt")
(require "html-gen.rkt")

; Main servlet function: handles requests, extracts data, and produces responses
(define (start request)
  (define method (request-method request))  ; extract the HTTP method (GET, POST)

  (cond
    [(equal? method #"POST")

       (define data  (bytes->jsexpr  (request-post-data/raw request) ))  ; extract JSON body
       (define input-str (hash-ref data 'input))                          ; get the input field

       ; build a hash map with the results:
       ; 'result' is the colored HTML, 'image' is the generated image
       (define json-hash
              (hash 'result (tokenize-to-html input-str)
                    'image    (genera-img input-str))
              )

       ; send the response, writing the hash map as JSON
       (response/output  #:code 200   #:mime-type #"application/json"
                     (lambda (out) (write-json json-hash out ) ) )
    ]

    ; if not POST, assume GET and serve the initial page
    [else
         ; response/output passes the output port to a lambda
         ; that writes what will be displayed
         (response/output  #:code 200   #:mime-type #"text/html"
                       (lambda (out) (display (file->string "index.html") out))  )
    ]
  )
)
