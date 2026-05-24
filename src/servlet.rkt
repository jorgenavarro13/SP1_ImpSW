#lang racket
(provide start)

(require web-server/servlet)
(require web-server/http/response-structs)
(require json)
(require "lexer.rkt")
(require "parser.rkt")
(require "html-gen.rkt")
(require "graph.rkt")

(define cors-headers
  (list (make-header #"Access-Control-Allow-Origin"  #"*")
        (make-header #"Access-Control-Allow-Methods" #"GET, POST, OPTIONS")
        (make-header #"Access-Control-Allow-Headers" #"Content-Type")))

; Convert the automaton alist returned by parse-tokens into a JSON-serializable hash.
; Fields missing due to parse errors fall back to empty defaults.
(define (automaton->hash aut)
  (define (get key default)
    (let ([pair (assoc key aut)])
      (if pair (or (cdr pair) default) default)))
  (hash 'name        (get 'name        "")
        'states      (get 'states      '())
        'alphabet    (get 'alphabet    '())
        'start       (get 'start       "")
        'end         (get 'end         "")
        'transitions (get 'transitions '())))

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

     ; Lex once; reuse the token stream for both HTML rendering and parsing
     (define lex-result  (Tokenizer input-str))
     (define tok-stream  (first  lex-result))
     (define lex-error   (second lex-result))

     (define parse-result (parse-tokens tok-stream))
     (define automaton    (first  parse-result))
     (define parse-errors (second parse-result))

     (define json-hash
       (hash 'result       (tokens->html tok-stream lex-error)
             'image        (genera-img input-str)
             'automaton    (automaton->hash automaton)
             'parseErrors  parse-errors))

     (response/output #:code 200 #:mime-type #"application/json"
                      #:headers cors-headers
                      (lambda (out) (write-json json-hash out)))]

    ; GET — serve the page
    [else
     (response/output #:code 200 #:mime-type #"text/html"
                      #:headers cors-headers
                      (lambda (out) (display (file->string "index.html") out)))]))
