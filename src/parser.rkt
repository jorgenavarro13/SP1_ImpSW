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
;   states-sec      → rw-states      ':' '[' state-list      ']'
;   alphabet-sec    → rw-alphabet    ':' '[' symbol-list     ']'
;   start-sec       → rw-start       ':'  stateId
;   end-sec         → rw-end         ':'  stateId
;   transitions-sec → rw-transitions ':' '[' transition-list ']'
;   state-list      → stateId  (',' stateId)*  | ε
;   symbol-list     → symbol   (',' symbol)*   | ε
;   transition-list → transition (',' transition)* | ε
;   transition      → stateId '::' alphabet-symbol '::' stateId
;
; Note: '[' = right-straigth-parenthesis   ']' = left-straigth-parenthesis
;
; Returns: (list automaton-alist errors-list)
;   automaton-alist  — list of cons pairs keyed by 'name 'states 'alphabet
;                      'start 'end 'transitions  (compatible with simulator.rkt)
;   errors-list      — list of error strings; empty when syntax is valid

; ── State: (list tokens pos errors) ──────────────────────────────────────────
;
; Every parser function takes a state and returns (values result new-state).

(define (make-st tokens pos errors) (list tokens pos errors))
(define (st-tokens st) (car   st))
(define (st-pos    st) (cadr  st))
(define (st-errors st) (caddr st))

; Walk the token list up to position pos and return the token there.
(define (list-at tokens pos)
  (if (= pos 0)
      (car tokens)
      (list-at (cdr tokens) (- pos 1))))

(define (st-eof? st)      (>= (st-pos st) (length (st-tokens st))))
(define (st-current st)   (if (st-eof? st) '("EOF" "") (list-at (st-tokens st) (st-pos st))))
(define (st-cur-label st) (car  (st-current st)))
(define (st-cur-value st) (cadr (st-current st)))

(define (st-advance st)
  (if (st-eof? st)
      st
      (make-st (st-tokens st) (+ (st-pos st) 1) (st-errors st))))

(define (st-add-error st expected)
  (make-st (st-tokens st)
           (st-pos st)
           (append (st-errors st)
                   (list (format "Syntax error at pos ~a: expected <~a>, got '~a' (<~a>)"
                                 (st-pos st) expected
                                 (st-cur-value st) (st-cur-label st))))))

; ── Token consumption ─────────────────────────────────────────────────────────

; Returns (values matched-value-or-#f new-state).
; On mismatch: records an error, does NOT advance (lets the caller recover).
(define (expect st label)
  (if (equal? (st-cur-label st) label)
      (values (st-cur-value st) (st-advance st))
      (values #f (st-add-error st label))))

; ── Comma-separated list parsers ──────────────────────────────────────────────

; Returns (values items-list new-state)
(define (parse-list st item-label)
  (if (not (equal? (st-cur-label st) item-label))
      (values '() st)
      (let collect-items ([acc (list (st-cur-value st))]
                          [st  (st-advance st)])
        (if (not (equal? (st-cur-label st) "coma"))
            (values acc st)
            (let ([st2 (st-advance st)])
              (if (equal? (st-cur-label st2) item-label)
                  (collect-items (append acc (list (st-cur-value st2))) (st-advance st2))
                  (values acc (st-add-error st2 item-label))))))))

(define (parse-state-list  st) (parse-list st "stateId"))
(define (parse-symbol-list st) (parse-list st "alphabet-symbol"))

; ── Transition ────────────────────────────────────────────────────────────────

; Parse one transition: stateId '::' alphabet-symbol '::' stateId
; Returns (values transition-or-#f new-state)
(define (parse-transition st)
  (let*-values ([(from st1) (expect st  "stateId")]
                [(_    st2) (expect st1 "transition-sybol")]
                [(sym  st3) (expect st2 "alphabet-symbol")]
                [(_    st4) (expect st3 "transition-sybol")]
                [(to   st5) (expect st4 "stateId")])
    (values (and from sym to (list from sym to)) st5)))

; Returns (values transitions-list new-state)
(define (parse-transition-list st)
  (if (not (equal? (st-cur-label st) "stateId"))
      (values '() st)
      (let-values ([(t st1) (parse-transition st)])
        (let collect-transitions ([acc (if t (list t) '())]
                                  [st  st1])
          (if (not (equal? (st-cur-label st) "coma"))
              (values acc st)
              (let ([st2 (st-advance st)])
                (let-values ([(t2 st3) (parse-transition st2)])
                  (collect-transitions (if t2 (append acc (list t2)) acc) st3))))))))

; ── Colon-bracketed helper ────────────────────────────────────────────────────

; Consumes ':' '[' <inner> ']'
(define (parse-colon-bracketed st parse-inner)
  (let*-values ([(_      st1) (expect st  "dots")]
                [(_      st2) (expect st1 "right-straigth-parenthesis")]
                [(result st3) (parse-inner st2)]
                [(_      st4) (expect st3 "left-straigth-parenthesis")])
    (values result st4)))

; ── Sections ──────────────────────────────────────────────────────────────────

(define (parse-section st)
  (define label (st-cur-label st))
  (cond
    [(equal? label "rw-states")
     (let-values ([(result st2) (parse-colon-bracketed (st-advance st) parse-state-list)])
       (values (cons 'states result) st2))]

    [(equal? label "rw-alphabet")
     (let-values ([(result st2) (parse-colon-bracketed (st-advance st) parse-symbol-list)])
       (values (cons 'alphabet result) st2))]

    [(equal? label "rw-start")
     (let*-values ([(_  st1) (expect (st-advance st) "dots")]
                   [(id st2) (expect st1 "stateId")])
       (values (cons 'start id) st2))]

    [(equal? label "rw-end")
     (let*-values ([(_  st1) (expect (st-advance st) "dots")]
                   [(id st2) (expect st1 "stateId")])
       (values (cons 'end id) st2))]

    [(equal? label "rw-transitions")
     (let-values ([(result st2) (parse-colon-bracketed (st-advance st) parse-transition-list)])
       (values (cons 'transitions result) st2))]

    [else
     (values #f (st-advance (st-add-error st "section keyword (states / alphabet / start / end / transitions)")))]))

(define (parse-sections st)
  (let collect-sections ([acc '()] [st st])
    (if (member (st-cur-label st) '("left-straigth-parenthesis" "EOF"))
        (values acc st)
        (let-values ([(s st2) (parse-section st)])
          (collect-sections (if s (append acc (list s)) acc) st2)))))

; ── Top-level program ─────────────────────────────────────────────────────────

(define (parse-program st)
  (let*-values ([(_        st1) (expect st  "rw-automata")]
                [(name     st2) (expect st1 "identifier")]
                [(_        st3) (expect st2 "right-straigth-parenthesis")]
                [(sections st4) (parse-sections st3)]
                [(_        st5) (expect st4 "left-straigth-parenthesis")])
    (values (cons (cons 'name name) sections) st5)))

; ── Entry point ───────────────────────────────────────────────────────────────

(define (parse-tokens raw-tokens)
  (let ([tokens (filter (lambda (t) (not (member (car t) '("blank_space" "newline"))))
                        raw-tokens)])
    (let-values ([(automaton final-st) (parse-program (make-st tokens 0 '()))])
      (list automaton (st-errors final-st)))))

; ── Legacy flat-list transition extractor (kept for backwards compat) ─────────
; Now expects stateId '::' alphabet-symbol '::' stateId

(define (parse-transitions flat-tokens)
  (let scan-transitions ([tokens flat-tokens] [acc '()])
    (cond
      [(< (length tokens) 5) acc]
      [(and (equal? (car (car         tokens)) "stateId")
            (equal? (car (cadr        tokens)) "transition-sybol")
            (equal? (car (caddr       tokens)) "alphabet-symbol")
            (equal? (car (cadddr      tokens)) "transition-sybol")
            (equal? (car (car (cddddr tokens))) "stateId")) 
       (scan-transitions (list-tail tokens 5)
                         (append acc (list (list (cadr (car         tokens))
                                                 (cadr (caddr       tokens))
                                                 (cadr (car (cddddr tokens)))))))]
      [else (scan-transitions (cdr tokens) acc)])))