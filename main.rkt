#lang racket
(require "src/lexer.rkt"
         "src/parser.rkt"
         "src/simulator.rkt")

; Example usage
(define input "Automata\nstart q0\nstates [q0, q1, q2]\ntransitions [q0:0::q1, q1:1::q2, q2:1::q2]\nalphabet [0, 1]\nend q2")
(define result (Tokenizer input))
(define flat-tokens (first result))
(define error-line (second result))
(define automaton (parse-tokens flat-tokens))
(displayln automaton)
(displayln (simulate automaton "0111"))
