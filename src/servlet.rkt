#lang racket
(provide start)

(require racket/list)
(require racket/runtime-path)
(require web-server/servlet)
(require web-server/http/response-structs)
(require net/url)
(require json)
(require "graph.rkt")
(require "html-gen.rkt")
(require "lexer.rkt"
         "parser.rkt"
         "simulator.rkt")
(define-runtime-path src-dir ".")

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
      (define endpoint (url->string (request-uri request)))
      (cond 
        [(equal? endpoint "/simulate")
          (define body (bytes->string/utf-8 (request-post-data/raw request)))
          ; Extract data
          (define data (string->jsexpr body))
          (define definition (hash-ref data 'definition))
          (define input (hash-ref data 'input))

          (define result (Tokenizer definition))
          (define flat-tokens (first result))
          (define recursive-descent (Recursive-descent flat-tokens))
          (define success (first recursive-descent))
          (define recursive-result (second recursive-descent))
          (cond
            [(not success)
             (response/output #:code 400 #:mime-type #"application/json"
                              #:headers cors-headers
                              (lambda (out)
                                (write-json (hash 'error "syntax error in automaton definition") out) ))]
            [else
             ; Simulation — normalize to boolean so JSON serializes as true/false
             (define accepted? (if (simulate recursive-result input) #t #f))
             (response/output #:code 200 #:mime-type #"application/json"
                              #:headers cors-headers
                              (lambda (out)
                                (write-json (hash 'accepted accepted?) out)))])
        ]
        [else 
        (let* ([data      (bytes->jsexpr (request-post-data/raw request))]
          [input-str (hash-ref data 'input)]
          [json-hash
          (hash 'result (tokenize-to-html input-str)
                'image  (genera-img input-str))])

          (response/output #:code 200 #:mime-type #"application/json"
            #:headers cors-headers
            (lambda (out) (write-json json-hash out))))
        ]
      )
    ]

    ; GET — serve static files or the main page
    [else
     (define path-parts (map path/param-path (url-path (request-uri request))))
     (define last-seg   (if (null? path-parts) "" (last path-parts)))
     (cond
       [(equal? last-seg "index.js")
        (response/output #:code 200 #:mime-type #"application/javascript"
                         #:headers cors-headers
                         (lambda (out) (display (file->string (build-path src-dir "index.js")) out)))]
       [else
        (response/output #:code 200 #:mime-type #"text/html"
                         #:headers cors-headers
                         (lambda (out) (display (file->string (build-path src-dir "index.html")) out)))])]))

