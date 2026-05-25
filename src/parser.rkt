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
;   transition      → stateId '::' alphabet-symbol '::' stateId
;
; Note: '[' = left-straigth-parenthesis   ']' = right-straigth-parenthesis
;
; parse-tokens       → takes raw token stream, returns automaton-alist
; parse-transitions  → same but returns (list automaton-alist errors-list)

(define (tok-label t)
  (first t))

(define (tok-value t)
  (second t))

(define (expect label tokens)
  (define token (car tokens))
  (if (equal? label (tok-label token))
    (values (tok-value token) (cdr tokens))
    (error (format "expected '~a' but got '~a'" label (tok-label token)))))

; state-list → stateId (',' stateId)* | ε
(define (parse-state-list tokens)
  (if (equal? (tok-label (car tokens)) "stateId")
    (let*-values
      (
        [(state tokens) (expect "stateId" tokens)]
        [(rest  tokens) (parse-state-list-rest tokens)]
      )
      (values (cons state rest) tokens))
    (values '() tokens)))

(define (parse-state-list-rest tokens)
  (if (and (not (null? tokens))
           (equal? (tok-label (car tokens)) "coma"))
    (let*-values
      (
        [(_ tokens)     (expect "coma" tokens)]
        [(state tokens) (expect "stateId" tokens)]
        [(rest  tokens) (parse-state-list-rest tokens)]
      )
      (values (cons state rest) tokens))
    (values '() tokens)))

; Accepts either identifier or alphabet-symbol as an alphabet symbol
(define (parse-symbol tokens)
  (define label (tok-label (car tokens)))
  (cond
    [(equal? label "identifier")       (expect "identifier" tokens)]
    [(equal? label "alphabet-symbol")  (expect "alphabet-symbol" tokens)]
    [else (error (format "expected symbol but got '~a'" label))]))

; symbol-list → symbol (',' symbol)* | ε
(define (parse-symbol-list tokens)
  (define label (tok-label (car tokens)))
  (if (or (equal? label "identifier") (equal? label "alphabet-symbol"))
    (let*-values
      (
        [(sym  tokens) (parse-symbol tokens)]
        [(rest tokens) (parse-symbol-list-rest tokens)]
      )
      (values (cons sym rest) tokens))
    (values '() tokens)))

(define (parse-symbol-list-rest tokens)
  (if (and (not (null? tokens))
           (equal? (tok-label (car tokens)) "coma"))
    (let*-values
      (
        [(_ tokens)   (expect "coma" tokens)]
        [(sym  tokens) (parse-symbol tokens)]
        [(rest tokens) (parse-symbol-list-rest tokens)]
      )
      (values (cons sym rest) tokens))
    (values '() tokens)))

; transition → stateId '::' alphabet-symbol '::' stateId
(define (parse-transition tokens)
  (let*-values
    (
      [(from tokens) (expect "stateId" tokens)]
      [(_ tokens)    (expect "transition-sybol" tokens)]
      [(sym  tokens) (parse-symbol tokens)]
      [(_ tokens)    (expect "transition-sybol" tokens)]
      [(to   tokens) (expect "stateId" tokens)]
    )
    (values (list from sym to) tokens)))

; transition-list → transition (',' transition)* | ε
(define (parse-transition-list tokens)
  (if (equal? (tok-label (car tokens)) "stateId")
    (let*-values
      (
        [(trans tokens) (parse-transition tokens)]
        [(rest  tokens) (parse-transition-list-rest tokens)]
      )
      (values (cons trans rest) tokens))
    (values '() tokens)))

(define (parse-transition-list-rest tokens)
  (if (and (not (null? tokens))
           (equal? (tok-label (car tokens)) "coma"))
    (let*-values
      (
        [(_ tokens)     (expect "coma" tokens)]
        [(trans tokens) (parse-transition tokens)]
        [(rest  tokens) (parse-transition-list-rest tokens)]
      )
      (values (cons trans rest) tokens))
    (values '() tokens)))

(define (states-sec tokens)
  (let*-values
    (
      [(_ tokens)      (expect "rw-states" tokens)]
      [(_ tokens)      (expect "left-straigth-parenthesis" tokens)]
      [(s-list tokens) (parse-state-list tokens)]
      [(_ tokens)      (expect "right-straigth-parenthesis" tokens)]
    )
    (values s-list tokens)))

(define (alphabet-sec tokens)
  (let*-values
    (
      [(_ tokens)        (expect "rw-alphabet" tokens)]
      [(_ tokens)        (expect "left-straigth-parenthesis" tokens)]
      [(sym-list tokens) (parse-symbol-list tokens)]
      [(_ tokens)        (expect "right-straigth-parenthesis" tokens)]
    )
    (values sym-list tokens)))

(define (start-sec tokens)
  (let*-values
    (
      [(_ tokens)        (expect "rw-start" tokens)]
      [(state-id tokens) (expect "stateId" tokens)]
    )
    (values state-id tokens)))

(define (end-sec tokens)
  (let*-values
    (
      [(_ tokens)        (expect "rw-end" tokens)]
      [(state-id tokens) (expect "stateId" tokens)]
    )
    (values state-id tokens)))

; transitions-sec → rw-transitions '[' transition-list ']'
(define (transitions-sec tokens)
  (let*-values
    (
      [(_ tokens)      (expect "rw-transitions" tokens)]
      [(_ tokens)      (expect "left-straigth-parenthesis" tokens)]
      [(t-list tokens) (parse-transition-list tokens)]
      [(_ tokens)      (expect "right-straigth-parenthesis" tokens)]
    )
    (values t-list tokens)))

(define (parse-sections tokens)
  (let*-values
    (
      [(states      tokens) (states-sec tokens)]
      [(alphabet    tokens) (alphabet-sec tokens)]
      [(start       tokens) (start-sec tokens)]
      [(end         tokens) (end-sec tokens)]
      [(transitions tokens) (transitions-sec tokens)]
    )
    (values
      (list
        (cons 'states      states)
        (cons 'alphabet    alphabet)
        (cons 'start       start)
        (cons 'end         end)
        (cons 'transitions transitions))
      tokens)))

; Internal recursive-descent entry point — returns (values alist remaining-tokens)
(define (parse-program tokens)
  (let*-values
    (
      [(_ tokens)        (expect "rw-automata" tokens)]
      [(name tokens)     (expect "identifier" tokens)]
      [(_ tokens)        (expect "left-straigth-parenthesis" tokens)]
      [(sections tokens) (parse-sections tokens)]
      [(_ tokens)        (expect "right-straigth-parenthesis" tokens)]
    )
    (values (cons (cons 'name name) sections) tokens)))

(define (filter-whitespace token-stream)
  (filter (lambda (t)
            (not (member (tok-label t) '("blank_space" "newline"))))
          token-stream))

; Semantic validation: checks that every state/symbol reference exists in the declared sets.
(define (validate-automaton automaton)
  (define states-set (list->set (cdr (assoc 'states      automaton))))
  (define alpha-set  (list->set (cdr (assoc 'alphabet    automaton))))
  (define start      (cdr       (assoc 'start       automaton)))
  (define end        (cdr       (assoc 'end         automaton)))

  (unless (set-member? states-set start)
    (error (format "'~a' is not a declared state" start)))

  (unless (set-member? states-set end)
    (error (format "'~a' is not a declared state" end)))

  (for ([t (cdr (assoc 'transitions automaton))])
    (define from (first  t))
    (define sym  (second t))
    (define to   (third  t))
    (unless (set-member? states-set from)
      (error (format "'~a' is not a declared state" from)))
    (unless (set-member? alpha-set sym)
      (error (format "'~a' is not a declared alphabet symbol" sym)))
    (unless (set-member? states-set to)
      (error (format "'~a' is not a declared state" to)))))

; Public — takes raw token stream, returns automaton alist.
; This is what servlet.rkt calls directly.
(define (parse-tokens token-stream)
  (define-values (automaton _) (parse-program (filter-whitespace token-stream)))
  (validate-automaton automaton)
  automaton)

; Public — like parse-tokens but wraps errors; returns (list automaton-alist errors-list).
(define (parse-transitions token-stream)
  (with-handlers
    ([exn:fail? (lambda (e)
                  (list '() (list (exn-message e))))])
    (list (parse-tokens token-stream) '())))
