#lang racket
(provide parse-transitions
         parse-tokens)

; Recursive-descent parser for the automaton language.
;
; Grammar (whitespace and newlines are stripped before parsing):
;
;   program         → rw-automata identifier '[' sections ']'
;   sections        → section*
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

(define (parse-tokens raw-tokens)

  ; Strip whitespace and newlines — they carry no syntactic meaning
  (define tokens
    (filter (lambda (t) (not (member (first t) '("blank_space" "newline"))))
            raw-tokens))

  (define pos    0)
  (define errors '())

  ; ── Token access ────────────────────────────────────────────────────────────

  (define (eof?)      (>= pos (length tokens)))
  (define (current)   (if (eof?) '("EOF" "") (list-ref tokens pos)))
  (define (cur-label) (first  (current)))
  (define (cur-value) (second (current)))
  (define (advance)   (unless (eof?) (set! pos (+ pos 1))))

  (define (add-error expected)
    (set! errors
          (append errors
                  (list (format "Syntax error at pos ~a: expected <~a>, got '~a' (<~a>)"
                                pos expected (cur-value) (cur-label))))))

  ; Consume a token of the expected label and return its value.
  ; On mismatch: record an error, do NOT advance (let the caller recover).
  (define (expect label)
    (if (equal? (cur-label) label)
        (let ([v (cur-value)]) (advance) v)
        (begin (add-error label) #f)))

  ; ── Comma-separated list parsers ────────────────────────────────────────────

  ; Parse zero or more comma-separated tokens of item-label.
  ; Returns a list of their string values.
  (define (parse-list item-label)
    (define acc '())
    (when (equal? (cur-label) item-label)
      (set! acc (list (cur-value)))
      (advance)
      (let loop ()
        (when (equal? (cur-label) "coma")
          (advance)
          (if (equal? (cur-label) item-label)
              (begin
                (set! acc (append acc (list (cur-value))))
                (advance)
                (loop))
              (add-error item-label)))))
    acc)

  (define (parse-state-list)  (parse-list "stateId"))
  (define (parse-symbol-list) (parse-list "alphabet-symbol"))

  ; ── Transition ──────────────────────────────────────────────────────────────

  ; Parse one transition: stateId ':' alphabet-symbol '::' stateId
  (define (parse-transition)
    (define from (expect "stateId"))
    (expect "dots")
    (define sym  (expect "alphabet-symbol"))
    (expect "transition-sybol")
    (define to   (expect "stateId"))
    (and from sym to (list from sym to)))

  ; Parse zero or more comma-separated transitions
  (define (parse-transition-list)
    (define acc '())
    (when (equal? (cur-label) "stateId")
      (define t (parse-transition))
      (when t (set! acc (list t)))
      (let loop ()
        (when (equal? (cur-label) "coma")
          (advance)
          (define t2 (parse-transition))
          (when t2 (set! acc (append acc (list t2))))
          (loop))))
    acc)

  ; ── Bracketed helper ────────────────────────────────────────────────────────

  (define (parse-bracketed parse-inner)
    (expect "right-straigth-parenthesis")
    (define result (parse-inner))
    (expect "left-straigth-parenthesis")
    result)

  ; ── Sections ────────────────────────────────────────────────────────────────

  (define (parse-section)
    (define label (cur-label))
    (cond
      [(equal? label "rw-states")
       (advance)
       (cons 'states      (parse-bracketed parse-state-list))]

      [(equal? label "rw-alphabet")
       (advance)
       (cons 'alphabet    (parse-bracketed parse-symbol-list))]

      [(equal? label "rw-start")
       (advance)
       (cons 'start       (expect "stateId"))]

      [(equal? label "rw-end")
       (advance)
       (cons 'end         (expect "stateId"))]

      [(equal? label "rw-transitions")
       (advance)
       (cons 'transitions (parse-bracketed parse-transition-list))]

      [else
       ; Unknown token at section level: record error and skip to recover
       (add-error "section keyword (states / alphabet / start / end / transitions)")
       (advance)
       #f]))

  (define (parse-sections)
    (let loop ([acc '()])
      (if (member (cur-label) '("left-straigth-parenthesis" "EOF"))
          acc
          (let ([s (parse-section)])
            (loop (if s (append acc (list s)) acc))))))

  ; ── Top-level program ───────────────────────────────────────────────────────

  (define (parse-program)
    (expect "rw-automata")
    (define name     (expect "identifier"))
    (expect "right-straigth-parenthesis")
    (define sections (parse-sections))
    (expect "left-straigth-parenthesis")
    (cons (cons 'name name) sections))

  ; ── Run ─────────────────────────────────────────────────────────────────────

  (define automaton (parse-program))
  (list automaton errors))


; ── Legacy flat-list transition extractor (kept for backwards compat) ─────────

(define (parse-transitions flat-tokens)
  (define (aux tokens acc)
    (cond
      [(< (length tokens) 5) acc]
      [(and (equal? (first (first tokens))  "stateId")
            (equal? (first (second tokens)) "dots")
            (equal? (first (third tokens))  "alphabet-symbol")
            (equal? (first (fourth tokens)) "transition-sybol")
            (equal? (first (fifth tokens))  "stateId"))
       (aux (list-tail tokens 5)
            (append acc (list (list (second (first tokens))
                                    (second (third tokens))
                                    (second (fifth tokens))))))]
      [else (aux (cdr tokens) acc)]))
  (aux flat-tokens '()))

