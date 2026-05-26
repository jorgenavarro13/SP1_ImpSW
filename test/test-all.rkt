#lang racket
(require rackunit
         "../src/core/lexer.rkt"
         "../src/core/parser.rkt"
         "../src/core/simulator.rkt")

; ── DFA fixture ───────────────────────────────────────────────────────────────
(define dfa-input
  "DFA myautomata [\nstates: [q0, q1, q2]\nalphabet: [0, 1]\nstart: q0\nend: [q2]\ntransitions: [q0::0::q1, q1::1::q2, q2::1::q2]\n]")

(define dfa-lex-result  (Tokenizer dfa-input))
(define dfa-flat-tokens (first dfa-lex-result))
(define dfa-error-line  (second dfa-lex-result))
(define dfa-parse       (Recursive-descent dfa-flat-tokens))
(define dfa-success     (first dfa-parse))
(define dfa-auto        (second dfa-parse))

; Lexer tests
(test-case "Tokenizer returns a list of two elements"
  (check-equal? (length dfa-lex-result) 2))

(test-case "Tokenizer: no error on valid DFA input"
  (check-false dfa-error-line))

(test-case "Tokenizer: flat-tokens is non-empty"
  (check-true (> (length dfa-flat-tokens) 0)))

(test-case "Tokenizer: error on invalid input"
  (define bad (Tokenizer "DFA x [\nstates: [q0]\n!!!bad\n]"))
  (check-not-false (second bad)))

; Parser tests
(test-case "Recursive-descent: DFA parses successfully"
  (check-true dfa-success))

(test-case "Recursive-descent: start state is q0"
  (check-equal? (hash-ref dfa-auto 'start) "q0"))

(test-case "Recursive-descent: end states contain q2"
  (check-not-false (member "q2" (hash-ref dfa-auto 'end))))

(test-case "Recursive-descent: three states declared"
  (check-equal? (length (hash-ref dfa-auto 'states)) 3))

; Simulator tests — DFA
(test-case "simulate DFA: accepts \"0111\""
  (check-true (simulate dfa-auto "0111")))

(test-case "simulate DFA: accepts \"01\""
  (check-true (simulate dfa-auto "01")))

(test-case "simulate DFA: rejects empty string"
  (check-false (simulate dfa-auto "")))

(test-case "simulate DFA: rejects \"0\""
  (check-false (simulate dfa-auto "0")))

; ── PDA fixture — a^n b^n ─────────────────────────────────────────────────────
(define pda-input
  "PDA anbn [\nstates: [q0, q1, q2]\nalphabet: [a, b]\nstackalpha: [A, Z]\nstackbottom: Z\nstart: q0\nend: [q2]\ntransitions: [q0::a,Z,AZ::q0, q0::a,A,AA::q0, q0::b,A,_::q1, q1::b,A,_::q1, q1::_,Z,Z::q2]\n]")

(define pda-lex-result  (Tokenizer pda-input))
(define pda-flat-tokens (first pda-lex-result))
(define pda-parse       (Recursive-descent pda-flat-tokens))
(define pda-success     (first pda-parse))
(define pda-auto        (second pda-parse))

(test-case "Tokenizer: PDA keyword tokenises as rw-pda"
  (define first-real (first (filter (lambda (t) (not (member (first t) '("blank_space" "newline")))) pda-flat-tokens)))
  (check-equal? (first first-real) "rw-pda"))

(test-case "Recursive-descent: PDA parses successfully"
  (check-true pda-success))

(test-case "Recursive-descent: PDA stackalpha has 2 symbols"
  (check-equal? (length (hash-ref pda-auto 'stackalpha)) 2))

(test-case "Recursive-descent: PDA stackbottom is Z"
  (check-equal? (hash-ref pda-auto 'stackbottom) "Z"))

(test-case "simulate PDA: accepts \"aabb\""
  (check-true (simulate pda-auto "aabb")))

(test-case "simulate PDA: accepts \"ab\""
  (check-true (simulate pda-auto "ab")))

(test-case "simulate PDA: rejects \"aab\""
  (check-false (simulate pda-auto "aab")))

(test-case "simulate PDA: rejects empty string"
  (check-false (simulate pda-auto "")))
