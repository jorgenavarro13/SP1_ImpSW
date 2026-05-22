
; Generate token list
(define (flatten-token token-stream)
  (apply append token-stream)
)

(define (parse-transitions flat-tokens)
  ; First, isolate only the tokens inside transitions: [...]
  ; Walk the list looking for groups of: stateId : symbol :: stateId

  (define (parse-transitions-aux tokens acc)
    (cond
      ; BASE CASE: less than 5 tokens left, nothing more to parse
      [(< (length tokens) 5) acc]

      ; MATCH: found a transition pattern q0:0::q1
      [(and (equal? (first (first tokens))  "stateId")           ; from-state
            (equal? (first (second tokens)) "dots")               ; :
            (equal? (first (third tokens))  "alphabet-symbol")    ; symbol
            (equal? (first (fourth tokens)) "transition-sybol")  ; :: typo in original label
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
(define (parse-tokens tokens)
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
  
  ; Find the stateId that comes after a specific keyword
  (define (get-state-after keyword)
      (let ([token-list (dropf tokens (lambda (t) (not (equal? (first t) keyword))))])
        (if (and (pair? token-list) (pair? (cdr token-list)) (equal? (first (second token-list)) "stateId"))
            (second (second token-list))
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

(define (simulate automaton input)
  (define (simulate-aux current-state input)
    (cond
      [(empty? input) (member current-state (list (cdr (assoc 'end automaton))))]
      [else
        (let* ([symbol (car input)]
               [transition (findf 
                 (lambda (t) (and (equal? (first t) current-state)
                                  (equal? (second t) (string symbol))))
                 (cdr (assoc 'transitions automaton)))])
          (if transition
              (simulate-aux (third transition) (cdr input))
              #f))]))
  (simulate-aux (cdr (assoc 'start automaton)) (string->list input)))




; Example usage
(define input "Automata\nstart q0\nstates [q0, q1, q2]\ntransitions [q0:0::q1, q1:1::q2, q2:1::q2]\nalphabet [0, 1]\nend q2")
(define token-stream (Tokenizer input))
(define flat-tokens (flatten-token token-stream))
(define automaton (parse-tokens flat-tokens))
(displayln automaton)
(displayln (simulate automaton "0111"))
