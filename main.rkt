#lang racket
(require web-server/servlet-env)
(require "src/servlet.rkt")
    
(serve/servlet
            start
            #:launch-browser? #f
            #:listen-ip #f
            #:servlet-path "/"
            #:port (let ([p (getenv "PORT")]) (if p (string->number p) 8000))
)

; Example usage
; (require "src/lexer.rkt"
;          "src/parser.rkt"
;          "src/simulator.rkt")
; (define input "Automata\nstart q0\nstates [q0, q1, q2]\ntransitions [q0:0::q1, q1:1::q2, q2:1::q2]\nalphabet [0, 1]\nend q2")
; (define result (Tokenizer input))
; (define flat-tokens (first result))
; (define error-line (second result))
; (define automaton (parse-tokens flat-tokens))
; (displayln automaton)
; (displayln (simulate automaton "0111"))
