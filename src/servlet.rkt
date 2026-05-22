#lang racket
(provide start)

(require "src/graph.rkt")
(require "src/html-gen.rkt")

; Main servlet function: manage requests, extract data, and produce responses
(define (start request)
  (define method (request-method request))  ; extract the HTTP method (GET, POST)
 
  (cond
    [(equal? method #"POST")

       (define data  (bytes->jsexpr  (request-post-data/raw request) ))  ; extract json
       (define input-str (hash-ref data 'input))                 ; get the input field

       ; build a hash map with the results: 'result' is the colored HTML, 'image' is the generated image
       (define json-hash
              (hash 'result (tokenize-to-html input-str)
                    'image    (genera-img input-str))
              )
       
       ; response/output envia la respuesta, escribe el hashmap hacia el json
       (response/output  #:code 200   #:mime-type #"application/json"
                     (lambda (out) (write-json json-hash out ) ) )
    ]

    ; si no es POST, asume es GET, y abre la pagina inical
    [else
         ; response/output pasa el puerto out a una funcion lambda,
         ; que 'imprime' lo que se mostrara
         (response/output  #:code 200   #:mime-type #"text/html"
                       (lambda (out) (display (file->string "index.html") out))  )
    ]
  )
)