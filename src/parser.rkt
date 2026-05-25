#lang racket
(require racket/set)

(provide parse-transitions
         parse-tokens)

;Recursive-descent parser for the automaton language.
;
; Grammar (whitespace and newlines are stripped before parsing):
;
;   program         → rw-automata identifier '[' sections ']'
;   sections        → states-sec -> alphabet-sec -> start-sec -> end-sec -> transitions-sec
;   section         → states-sec | alphabet-sec | start-sec
;                   | end-sec   | transitions-sec
;   states-sec      → rw-states      '[' state-list      ']'
;   alphabet-sec    → rw-alphabet    '[' symbol-list     ']'
;   start-sec       → rw-start  stateId
;   end-sec         → rw-end    stateId
;   transitions-sec → rw-transitions '[' transition-list ']'
;   state-list      → stateId  (',' stateId)*  | ε
;   symbol-list     → symbol   (',' symbol)*   | ε
;   transition-list → transition (',' transition)* | ε
;   transition      → stateId ':' alphabet-symbol '::' stateId
;
; Note: '[' = right-straigth-parenthesis   ']' = left-straigth-parenthesis
;
; Returns: (list automaton-alist errors-list)
;   automaton-alist  — list of cons pairs keyed by 'name 'states 'alphabet
;                      'start 'end 'transitions  (compatible with simulator.rkt)
;   errors-list      — list of error strings; empty when syntax is valid

(define (tok-label t) 
  (first t)
)

(define (tok-value t)
  (second t)
)

(define (expect label tokens)
  (define token (car tokens))
  (if (equal? label (tok-label token))
    (values (tok-value token) (cdr tokens))
    (error "expected ~a but got ~a" label (tok-label token))
  )
)

(define (states-sec tokens)
  (let*-values
    (
      [(_ tokens) (expect "rw-states" tokens)]
      [(_ tokens) (expect "left-straigth-parenthesis" tokens)]
      [(s-list tokens) (parse-state-list tokens)]
      [(_ tokens) (expect "right-straigth-parenthesis" tokens)]
    )
    (values s-list tokens)
  )
)

(define (alphabet-sec tokens)
  (let*-values
    (
      [(_ tokens) (expect "rw-alphabet" tokens)]
      [(_ tokens) (expect "left-straigth-parenthesis" tokens)]
      [(sym-list tokens) (parse-symbol-list tokens)]
      [(_ tokens) (expect "right-straigth-parenthesis" tokens)]
    )
    (values sym-list tokens)
  )
)

(define (start-sec tokens)
  (let*-values
    (
      [(_ tokens) (expect "rw-start" tokens)]
      [(state-id tokens) (expect "identifier" tokens)]
    )
    (values state-id tokens)
  )
)

(define (end-sec tokens)
  (let*-values
    (
      [(_ tokens) (expect "rw-end" tokens)]
      [(state-id tokens) (expect "identifier" tokens)]
    )
    (values state-id tokens)
  )
)

(define (parse-sections tokens)
  (let*-values
    (
      [(states tokens) (states-sec tokens)]
      [(alphabet tokens) (alphabet-sec tokens)]
      [(start tokens) (start-sec tokens)]
      [(end tokens) (end-sec tokens)]
      [(transitions tokens) (transitions-sec tokens)]
    )
    (define sections-alist
      (list
        (cons 'states states)
        (cons 'alphabet alphabet)
        (cons 'start start)
        (cons 'end end)
        (cons 'transitions transitions)))
    (values sections-alist tokens)
  )
)

(define (parse-tokens tokens)
  (let*-values
    (
      [(_ tokens) (expect "rw-automata" tokens)]
      [(name tokens) (expect "identifier" tokens)]
      [(_ tokens) (expect "left-straigth-parenthesis" tokens)]
      [(sections tokens) (parse-sections tokens)]
      [(_ tokens) (expect "right-straigth-parenthesis" tokens)]
    )
    (define program-alist
      (cons (cons 'name name) sections))
    (values program-alist '())
  )
)
