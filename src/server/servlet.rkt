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
(require "../core/lexer.rkt"
         "../core/parser.rkt"
         "../core/simulator.rkt")
(define-runtime-path src-dir ".")

(define cors-headers
  (list (make-header #"Access-Control-Allow-Origin" #"*")
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

        (define flat-tokens (first (Tokenizer definition)))
        (define parse-result (Recursive-descent flat-tokens))
        (define success (first parse-result))
        (define result-value (second parse-result))
        (cond
          [(not success)
           (response/output #:code 400 #:mime-type #"application/json"
                            #:headers cors-headers
                            (lambda (out)
                              (write-json (hash 'errors result-value) out)))]
          [else
           (define accepted? (if (simulate result-value input) #t #f))
           (response/output #:code 200 #:mime-type #"application/json"
                            #:headers cors-headers
                            (lambda (out)
                              (write-json (hash 'accepted accepted?) out)))])
        ]
       [else
        (define data (bytes->jsexpr (request-post-data/raw request)))
        (define input-str (hash-ref data 'input))
        (define flat-tokens (first (Tokenizer input-str)))
        (define parse-result (Recursive-descent flat-tokens))
        (define success (first parse-result))
        (define result-value (second parse-result))
        (cond
          [(not success)
           (response/output #:code 400 #:mime-type #"application/json"
                            #:headers cors-headers
                            (lambda (out)
                              (write-json (hash 'errors result-value) out)))]
          [else
           (response/output #:code 200 #:mime-type #"application/json"
                            #:headers cors-headers
                            (lambda (out)
                              (write-json (hash 'result (tokenize-to-html input-str)
                                                'image (generate-img result-value)
                                                'mode  (hash-ref result-value 'mode "dfa"))
                                          out)))])
        ]
       )
     ]

    ; GET — serve static files or the main page
    [else
     (define path-parts (map path/param-path (url-path (request-uri request))))
     (define last-seg (if (null? path-parts) "" (last path-parts)))
     (cond
       [(equal? last-seg "index.js")
        (response/output #:code 200 #:mime-type #"application/javascript"
                         #:headers cors-headers
                         (lambda (out) (display (file->string (build-path src-dir "../frontend/index.js")) out)))]
       [else
        (response/output #:code 200 #:mime-type #"text/html"
                         #:headers cors-headers
                         (lambda (out) (display (file->string (build-path src-dir "../frontend/index.html")) out)))])]))

