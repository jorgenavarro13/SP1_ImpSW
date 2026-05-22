#lang racket
(require rackunit
         "../src/lexer.rkt"
         "../src/parser.rkt"
         "../src/simulator.rkt")

; Shared fixture
(define input "Automata\nstart q0\nstates [q0, q1, q2]\ntransitions [q0:0::q1, q1:1::q2, q2:1::q2]\nalphabet [0, 1]\nend q2")
(define token-stream (Tokenizer input))
(define flat-tokens  (flatten-token token-stream))
(define automaton    (parse-tokens flat-tokens))

; Lexer tests
(test-case "Tokenizer produces non-empty output"
  (check-false (empty? token-stream)))

(test-case "flatten-token returns a flat list"
  (check-true (list? flat-tokens))
  (check-true (> (length flat-tokens) 0)))

; Parser tests
(test-case "parse-tokens: start state"
  (check-equal? (cdr (assoc 'start automaton)) "q0"))

(test-case "parse-tokens: end state"
  (check-equal? (cdr (assoc 'end automaton)) "q2"))

(test-case "parse-tokens: three states"
  (check-equal? (length (cdr (assoc 'states automaton))) 3))

(test-case "parse-transitions: three transitions"
  (check-equal? (length (cdr (assoc 'transitions automaton))) 3))

; Simulator tests
(test-case "simulate: accepts '0111'"
  (check-not-false (simulate automaton "0111")))

(test-case "simulate: accepts '011'"
  (check-not-false (simulate automaton "011")))

(test-case "simulate: rejects empty string"
  (check-false (simulate automaton "")))

(test-case "simulate: rejects '0'"
  (check-false (simulate automaton "0")))
