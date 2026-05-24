#lang racket
; Parser: converts a flat token list into an automaton data structure

(provide parse-transitions
         parse-tokens)

(define (parse-transitions flat-tokens)
  ; Walk the list looking for groups of: stateId :: symbol :: stateId

  (define (parse-transitions-aux tokens acc)
    (cond
      ; BASE CASE: less than 5 tokens left, nothing more to parse
      [(< (length tokens) 5) acc]

      ; MATCH: found a transition pattern q0::0::q1
      [(and (equal? (first (first tokens))  "stateId")           ; from-state
            (equal? (first (second tokens)) "transition-sybol")   ; ::
            (member (first (third tokens))  '("alphabet-symbol" "identifier")) ; symbol
            (equal? (first (fourth tokens)) "transition-sybol")   ; ::
            (equal? (first (fifth tokens))  "stateId"))           ; to-state

       (let* ([from   (second (first tokens))]
              [symbol (second (third tokens))]
              [to     (second (fifth tokens))]
              [new-transition (list from symbol to)])
         ; Consume 5 tokens, add transition, recurse
         (parse-transitions-aux (list-tail tokens 5)
                                (append acc (list new-transition))))
      ]

      ; NO MATCH: skip one token and keep looking
      [else (parse-transitions-aux (cdr tokens) acc)]
    )
  )

  (parse-transitions-aux flat-tokens '())
)

; Parse
(define (parse-tokens raw-tokens)
  ; Strip whitespace tokens produced by the new Tokenizer
  (define tokens (filter (lambda (t) (not (equal? (first t) "blank_space"))) raw-tokens))
  ; Get all label values
  (define (get-all label)
    (remove-duplicates (map second (filter (lambda (t) (equal? (first t) label)) tokens)))
  )
  ; Get label value
  (define (get-first label)
    (let ([result (get-all label)])
      (if (empty? result)
          #f ; Return false if not found
          (first result))))

  ; Find the stateId that comes after a specific keyword, skipping any dots separator
  (define (get-state-after keyword)
    (let* ([token-list (dropf tokens (lambda (t) (not (equal? (first t) keyword))))]
           [rest       (dropf (if (pair? token-list) (cdr token-list) '())
                              (lambda (t) (equal? (first t) "dots")))])
      (if (and (pair? rest) (equal? (first (car rest)) "stateId"))
          (second (car rest))
          #f)))

  ; Build Automaton
  (list
    (cons 'name        (get-first "identifier"))
    (cons 'states      (get-all "stateId"))
    (cons 'start       (get-state-after "rw-start"))
    (cons 'end         (get-state-after "rw-end"))
    (cons 'alphabet    (get-all "alphabet-symbol"))
    (cons 'transitions (parse-transitions tokens))
  )
)
