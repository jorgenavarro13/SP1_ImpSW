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
          (define data (string->jsexpr body))
          (define definition (hash-ref data 'definition))
          (define input (hash-ref data 'input))

          (define (json-response payload)
            (response/output
              #:code 200
              #:mime-type #"application/json"
              #:headers cors-headers
              (lambda (out) (write-json payload out))))

          (with-handlers
            ([exn:fail? (lambda (e)
                          (json-response (hash 'error (exn-message e))))])
            (define flat-tokens (first (Tokenizer definition)))
            (define automaton (parse-tokens flat-tokens))
            (define accepted? (if (simulate automaton input) #t #f))
            (json-response (hash 'accepted accepted?)))
        ]
        [else
          (define data (bytes->jsexpr (request-post-data/raw request)))
          (define input-str (hash-ref data 'input))

          (define (json-response payload)
            (response/output #:code 200 #:mime-type #"application/json"
              #:headers cors-headers
              (lambda (out) (write-json payload out))))

          (with-handlers
            ([exn:fail? (lambda (e)
                          (json-response (hash 'error (exn-message e))))])
            (parse-tokens (first (Tokenizer input-str)))  ; validate — throws on syntax error
            (json-response
              (hash 'result (tokenize-to-html input-str)
                    'image  (genera-img input-str))))
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

