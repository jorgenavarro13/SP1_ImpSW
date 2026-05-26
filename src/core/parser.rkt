#lang racket
(require racket/set)

(provide Recursive-descent
         strip-spaces)

; Parser: converts a flat token list into an automaton data structure

(define (strip-spaces raw-tokens) ; Helper function to remove blank spaces and newlines from the token stream
  (filter
    (lambda (t)
      (not (member (first t)
                   (list "blank_space" "newline"))))
    raw-tokens)
  )

(define (token-type tk) (first tk)) ; Accessor for the token type (label)
(define (token-lexeme tk) (second tk)) ; Accessor for the token lexeme (subStr)

; Parse result accessors: (list remaining-tokens auto errors)
(define (make-result tokens auto errors) (list tokens auto errors))
(define (result-tokens r) (first r))
(define (result-auto r) (second r))
(define (result-errors r) (third r))

; Helper function to add an error to the error list
(define (add-error errors expected received)
  (append errors
          (list
            (string-append "Expected " expected ", received " received)))
  )

; statesPrime ::= , stateId statesPrime | ]
(define (syntax-statesPrime tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "coma")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "stateId")
        (define lexem (token-lexeme (cadr tokens)))
        (define new-states (append (hash-ref auto 'states '()) (list lexem))) ; we
        (define new-auto (hash-set auto 'states new-states)) ; Create a new auto with the updated states list
        (syntax-statesPrime (cddr tokens) new-auto errors)] ; Recursive call to process the next stateId
       [else (make-result tokens auto (add-error errors "stateId" next-token))])]
    [(equal? current-token "left-straigth-parenthesis")
     (make-result (cdr tokens) auto errors)] ; Base case; if we find a closing bracket we return the list
    [else (make-result tokens auto (add-error errors ", or ]" current-token))]
    )
  )

; states ::= stateId statesPrime
(define (syntax-states tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "stateId")
     (define lexem (token-lexeme (car tokens)))
     (define new-auto (hash-set auto 'states (list lexem)))
     (syntax-statesPrime (cdr tokens) new-auto errors)] ; Keep looking for  more statess
    [else (make-result tokens auto (add-error errors "stateId" current-token))]
    )
  )

; statesList ::= [ states
(define (syntax-statesList tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "right-straigth-parenthesis")
     (syntax-states (cdr tokens) auto errors)] ; Look for states inside
    [else (make-result tokens auto (add-error errors "[" current-token))]
    )
  )

; statesDefinition ::= rw-states dots statesList
(define (syntax-statesDefinition tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "rw-states")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "dots")
        (syntax-statesList (cddr tokens) auto errors)] ; Check if the list of states is valid
       [else (make-result tokens auto (add-error errors ":" next-token))])]
    [else (make-result tokens auto (add-error errors "states" current-token))]
    )
  )

; alphabetPrime ::= , alphabet-symbol alphabetPrime | ]
(define (syntax-alphabetPrime tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "coma") ; First compare if there's a comma at the beggining and continue
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "alphabet-symbol")
        (define lexem (token-lexeme (cadr tokens)))
        (define new-alpha (append (hash-ref auto 'alphabet '()) (list lexem)))
        (define new-auto (hash-set auto 'alphabet new-alpha)) ; Replace the current hashmap with the new symbol of alphabet
        (syntax-alphabetPrime (cddr tokens) new-auto errors)] ; Look for more alphabet symbols on the list
       [else (make-result tokens auto (add-error errors "alphabet-symbol" next-token))])]
    [(equal? current-token "left-straigth-parenthesis")
     (make-result (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors ", or ]" current-token))]
    )
  )

; alphabet ::= alphabet-symbol alphabetPrime
(define (syntax-alphabet tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "alphabet-symbol") ; Does the alphabet actually starts with an alphabet symbol?
     (define lexem (token-lexeme (car tokens)))
     (define new-auto (hash-set auto 'alphabet (list lexem)))
     (syntax-alphabetPrime (cdr tokens) new-auto errors)] ; Recursive call to search more alphabet symbols
    [else (make-result tokens auto (add-error errors "alphabet-symbol" current-token))]
    )
  )

; alphabetList ::= [ alphabet
(define (syntax-alphabetList tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "right-straigth-parenthesis") ; Does the list starts with [ ?
     (syntax-alphabet (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors "[" current-token))]
    )
  )

; alphabetDefinition ::= rw-alphabet dots alphabetList
(define (syntax-alphabetDefinition tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "rw-alphabet") ; Does the alphabet section starts with alphabet : ?
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "dots")
        (syntax-alphabetList (cddr tokens) auto errors)]
       [else (make-result tokens auto (add-error errors ":" next-token))])]
    [else (make-result tokens auto (add-error errors "alphabet" current-token))]
    )
  )

; startDefinition ::= rw-start dots stateId
(define (syntax-startDefinition tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "rw-start") ; Does the structure begins with start : ?
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "dots")
        (define state-token (token-type (caddr tokens)))
        (cond
          [(equal? state-token "stateId")
           (define lexem (token-lexeme (caddr tokens)))
           (define new-auto (hash-set auto 'start lexem))
           (make-result (cdddr tokens) new-auto errors)]
          [else (make-result tokens auto (add-error errors "stateId" state-token))])]
       [else (make-result tokens auto (add-error errors ":" next-token))])]
    [else (make-result tokens auto (add-error errors "start" current-token))]
    )
  )

; endPrime ::= , stateId endPrime | ]
(define (syntax-endPrime tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "coma") 
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "stateId")
        (define lexem (token-lexeme (cadr tokens)))
        (define new-end (append (hash-ref auto 'end '()) (list lexem)))
        (define new-auto (hash-set auto 'end new-end))
        (syntax-endPrime (cddr tokens) new-auto errors)] ; Look up for more end states
       [else (make-result tokens auto (add-error errors "stateId" next-token))])]
    [(equal? current-token "left-straigth-parenthesis")
     (make-result (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors ", or ]" current-token))]
    )
  )

; endStates ::= stateId endPrime
(define (syntax-endStates tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "stateId")
     (define lexem (token-lexeme (car tokens)))
     (define new-auto (hash-set auto 'end (list lexem)))
     (syntax-endPrime (cdr tokens) new-auto errors)]
    [else (make-result tokens auto (add-error errors "stateId" current-token))]
    )
  )

; endList ::= [ endStates
(define (syntax-endList tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "right-straigth-parenthesis")
     (syntax-endStates (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors "[" current-token))]
    )
  )

; endDefinition ::= rw-end dots endList
(define (syntax-endDefinition tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "rw-end")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "dots")
        (syntax-endList (cddr tokens) auto errors)]
       [else (make-result tokens auto (add-error errors ":" next-token))])]
    [else (make-result tokens auto (add-error errors "end" current-token))]
    )
  )

; transitionsPrime ::= , transition transitionsPrime | ]   (mutually recursive with syntax-transition)
(define (syntax-transitionsPrime tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "coma")
     (syntax-transition (cdr tokens) auto errors)]
    [(equal? current-token "left-straigth-parenthesis")
     (make-result (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors ", or ]" current-token))]
    )
  )

; transition ::= stateId :: alphabet-symbol :: stateId  (then → transitionsPrime)
(define (syntax-transition tokens auto errors)
  (cond
    [(< (length tokens) 5)
     (make-result tokens auto (add-error errors "stateId :: alpha :: stateId" "end of input"))]
    [else
     (define from-type (token-type (list-ref tokens 0)))
     (define sym1-type (token-type (list-ref tokens 1)))
     (define alpha-type (token-type (list-ref tokens 2)))
     (define sym2-type (token-type (list-ref tokens 3)))
     (define to-type (token-type (list-ref tokens 4)))
     (cond
       [(and (equal? from-type "stateId")
             (equal? sym1-type "transition-sybol")
             (equal? alpha-type "alphabet-symbol")
             (equal? sym2-type "transition-sybol")
             (equal? to-type "stateId"))
        (define from (token-lexeme (list-ref tokens 0)))
        (define alpha (token-lexeme (list-ref tokens 2)))
        (define to (token-lexeme (list-ref tokens 4)))
        (define old-tr (hash-ref auto 'transitions (hash)))
        (define from-map (hash-ref old-tr from (hash)))
        (define dup-errors
          (if (and (equal? (hash-ref auto 'mode "") "dfa") (hash-has-key? from-map alpha))
              (append errors (list (string-append "DFA Error: state '" from "' has multiple transitions on symbol '" alpha "' (non-deterministic)")))
              errors))
        (define new-auto (hash-set auto 'transitions
                                   (hash-set old-tr from (hash-set from-map alpha to))))
        (syntax-transitionsPrime (list-tail tokens 5) new-auto dup-errors)]
       [else (make-result tokens auto (add-error errors "stateId :: alpha :: stateId" from-type))])]
    )
  )

; transitionsList ::= [ transition transitionsPrime
(define (syntax-transitionsList tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "right-straigth-parenthesis")
     (syntax-transition (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors "[" current-token))]
    )
  )

; transitionsDefinition ::= rw-transitions dots transitionsList
(define (syntax-transitionsDefinition tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "rw-transitions")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "dots")
        (syntax-transitionsList (cddr tokens) auto errors)]
       [else (make-result tokens auto (add-error errors ":" next-token))])]
    [else (make-result tokens auto (add-error errors "transitions" current-token))]
    )
  )

; ── PDA-specific grammar ─────────────────────────────────────────────────────

; stackAlphaPrime ::= , alphabet-symbol stackAlphaPrime | ]
(define (syntax-stackAlphaPrime tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "coma")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "alphabet-symbol")
        (define lexem (token-lexeme (cadr tokens)))
        (define new-sa (append (hash-ref auto 'stackalpha '()) (list lexem)))
        (syntax-stackAlphaPrime (cddr tokens) (hash-set auto 'stackalpha new-sa) errors)]
       [else (make-result tokens auto (add-error errors "stack symbol" next-token))])]
    [(equal? current-token "left-straigth-parenthesis")
     (make-result (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors ", or ]" current-token))]))

; stackAlpha ::= alphabet-symbol stackAlphaPrime
(define (syntax-stackAlpha tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "alphabet-symbol")
     (define lexem (token-lexeme (car tokens)))
     (syntax-stackAlphaPrime (cdr tokens) (hash-set auto 'stackalpha (list lexem)) errors)]
    [else (make-result tokens auto (add-error errors "stack symbol" current-token))]))

; stackAlphaList ::= [ stackAlpha
(define (syntax-stackAlphaList tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "right-straigth-parenthesis")
     (syntax-stackAlpha (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors "[" current-token))]))

; stackAlphaDefinition ::= rw-stackalpha dots stackAlphaList
(define (syntax-stackAlphaDefinition tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "rw-stackalpha")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "dots")
        (syntax-stackAlphaList (cddr tokens) auto errors)]
       [else (make-result tokens auto (add-error errors ":" next-token))])]
    [else (make-result tokens auto (add-error errors "stackalpha" current-token))]))

; stackBottomDefinition ::= rw-stackbottom dots alphabet-symbol
(define (syntax-stackBottomDefinition tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "rw-stackbottom")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "dots")
        (define sym-token (token-type (caddr tokens)))
        (cond
          [(equal? sym-token "alphabet-symbol")
           (define lexem (token-lexeme (caddr tokens)))
           (make-result (cdddr tokens) (hash-set auto 'stackbottom lexem) errors)]
          [else (make-result tokens auto (add-error errors "stack symbol" sym-token))])]
       [else (make-result tokens auto (add-error errors ":" next-token))])]
    [else (make-result tokens auto (add-error errors "stackbottom" current-token))]))

; pdaTransitionsPrime ::= , pdaTransition pdaTransitionsPrime | ]
(define (syntax-pdaTransitionsPrime tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "coma")
     (syntax-pdaTransition (cdr tokens) auto errors)]
    [(equal? current-token "left-straigth-parenthesis")
     (make-result (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors ", or ]" current-token))]))

; pdaTransition ::= stateId :: (alpha|_) , (alpha|_) , (alpha|id|_) :: stateId
(define (syntax-pdaTransition tokens auto errors)
  (cond
    [(< (length tokens) 9)
     (make-result tokens auto
                  (add-error errors "stateId :: sym,stackTop,push :: stateId" "end of input"))]
    [else
     (define from-t  (list-ref tokens 0))
     (define sym1-t  (list-ref tokens 1))
     (define input-t (list-ref tokens 2))
     (define com1-t  (list-ref tokens 3))
     (define stop-t  (list-ref tokens 4))
     (define com2-t  (list-ref tokens 5))
     (define push-t  (list-ref tokens 6))
     (define sym2-t  (list-ref tokens 7))
     (define to-t    (list-ref tokens 8))
     (cond
       [(and (equal? (token-type from-t)  "stateId")
             (equal? (token-type sym1-t)  "transition-sybol")
             (member (token-type input-t) '("alphabet-symbol" "lower-line"))
             (equal? (token-type com1-t)  "coma")
             (member (token-type stop-t)  '("alphabet-symbol" "lower-line"))
             (equal? (token-type com2-t)  "coma")
             (member (token-type push-t)  '("alphabet-symbol" "identifier" "lower-line"))
             (equal? (token-type sym2-t)  "transition-sybol")
             (equal? (token-type to-t)    "stateId"))
        (define from  (token-lexeme from-t))
        (define input (token-lexeme input-t))
        (define stop  (token-lexeme stop-t))
        (define push  (token-lexeme push-t))
        (define to    (token-lexeme to-t))
        (define old-tr (hash-ref auto 'pda-transitions '()))
        (define new-auto
          (hash-set auto 'pda-transitions
                    (append old-tr (list (list from input stop to push)))))
        (syntax-pdaTransitionsPrime (list-tail tokens 9) new-auto errors)]
       [else
        (make-result tokens auto
                     (add-error errors "stateId :: sym,stackTop,push :: stateId"
                                (token-type from-t)))])]))

; pdaTransitionsList ::= [ pdaTransition
(define (syntax-pdaTransitionsList tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "right-straigth-parenthesis")
     (syntax-pdaTransition (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors "[" current-token))]))

; pdaTransitionsDefinition ::= rw-transitions dots pdaTransitionsList
(define (syntax-pdaTransitionsDefinition tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "rw-transitions")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "dots")
        (syntax-pdaTransitionsList (cddr tokens) auto errors)]
       [else (make-result tokens auto (add-error errors ":" next-token))])]
    [else (make-result tokens auto (add-error errors "transitions" current-token))]))

; PDA body: states → alphabet → stackalpha → stackbottom → start → end → transitions
(define (syntax-pda-automaton tokens auto errors)
  (define id-type      (token-type (cadr tokens)))
  (define bracket-type (token-type (caddr tokens)))
  (cond
    [(not (equal? id-type "identifier"))
     (make-result tokens auto (add-error errors "identifier" id-type))]
    [(not (equal? bracket-type "right-straigth-parenthesis"))
     (make-result tokens auto (add-error errors "[" bracket-type))]
    [else
     (define auto+mode
       (hash-set
         (hash-set
           (hash-set auto 'mode "pda")
           'name (token-lexeme (cadr tokens)))
         'pda-transitions '()))
     (define states-r  (syntax-statesDefinition        (cdddr tokens)             auto+mode  errors))
     (define alpha-r   (syntax-alphabetDefinition      (result-tokens states-r)   (result-auto states-r)   (result-errors states-r)))
     (define salpha-r  (syntax-stackAlphaDefinition    (result-tokens alpha-r)    (result-auto alpha-r)    (result-errors alpha-r)))
     (define sbottom-r (syntax-stackBottomDefinition   (result-tokens salpha-r)   (result-auto salpha-r)   (result-errors salpha-r)))
     (define start-r   (syntax-startDefinition         (result-tokens sbottom-r)  (result-auto sbottom-r)  (result-errors sbottom-r)))
     (define end-r     (syntax-endDefinition           (result-tokens start-r)    (result-auto start-r)    (result-errors start-r)))
     (define trans-r   (syntax-pdaTransitionsDefinition (result-tokens end-r)     (result-auto end-r)      (result-errors end-r)))
     (define rem        (result-tokens trans-r))
     (define fin-auto   (result-auto   trans-r))
     (define fin-errors (result-errors trans-r))
     (cond
       [(and (not (null? rem))
             (equal? (token-type (car rem)) "left-straigth-parenthesis"))
        (make-result (cdr rem) fin-auto fin-errors)]
       [else
        (define got (if (null? rem) "EOF" (token-type (car rem))))
        (make-result rem fin-auto (add-error fin-errors "]" got))])]))

; automaton ::= (DFA | NFA) identifier [ statesDefinition alphabetDefinition startDefinition endDefinition transitionsDefinition ]
(define (syntax-automaton tokens auto errors)
  (define kw-type (token-type (car tokens)))
  (cond
    [(or (equal? kw-type "rw-dfa") (equal? kw-type "rw-nfa"))
     (define mode (if (equal? kw-type "rw-dfa") "dfa" "nfa"))
     (define id-type (token-type (cadr tokens)))
     (cond
       [(equal? id-type "identifier")
        (define bracket-type (token-type (caddr tokens)))
        (cond
          [(equal? bracket-type "right-straigth-parenthesis")
           (define auto+mode (hash-set (hash-set auto 'mode mode) 'name (token-lexeme (cadr tokens))))
           (define states-r (syntax-statesDefinition (cdddr tokens) auto+mode errors))
           (define alpha-r (syntax-alphabetDefinition (result-tokens states-r) (result-auto states-r) (result-errors states-r)))
           (define start-r (syntax-startDefinition (result-tokens alpha-r) (result-auto alpha-r) (result-errors alpha-r)))
           (define end-r (syntax-endDefinition (result-tokens start-r) (result-auto start-r) (result-errors start-r)))
           (define trans-r (syntax-transitionsDefinition (result-tokens end-r) (result-auto end-r) (result-errors end-r)))
           (define rem (result-tokens trans-r))
           (define fin-auto (result-auto trans-r))
           (define fin-errors (result-errors trans-r))
           (cond
             [(and (not (null? rem))
                   (equal? (token-type (car rem)) "left-straigth-parenthesis"))
              (make-result (cdr rem) fin-auto fin-errors)]
             [else
              (define got (if (null? rem) "EOF" (token-type (car rem))))
              (make-result rem fin-auto (add-error fin-errors "]" got))])]
          [else (make-result tokens auto (add-error errors "[" bracket-type))])]
       [else (make-result tokens auto (add-error errors "identifier" id-type))])]
    [(equal? kw-type "rw-pda")
     (syntax-pda-automaton tokens auto errors)]
    [else (make-result tokens auto (add-error errors "DFA, NFA or PDA" kw-type))]
    )
  )

; PDA semantic validation
(define (validate-pda-semantics auto)
  (define states-set  (list->set (hash-ref auto 'states '())))
  (define alpha-set   (list->set (hash-ref auto 'alphabet '())))
  (define salpha-set  (list->set (hash-ref auto 'stackalpha '())))
  (define start       (hash-ref auto 'start ""))
  (define end-list    (hash-ref auto 'end '()))
  (define bottom      (hash-ref auto 'stackbottom ""))
  (define pda-trans   (hash-ref auto 'pda-transitions '()))

  (define e1
    (if (and (not (equal? start "")) (not (set-member? states-set start)))
        (list (string-append "Start state '" start "' is not declared")) '()))

  (define e2
    (foldl (lambda (s acc)
             (if (set-member? states-set s) acc
                 (append acc (list (string-append "End state '" s "' is not declared")))))
           e1 end-list))

  (define e3
    (if (and (not (equal? bottom "")) (not (set-member? salpha-set bottom)))
        (append e2 (list (string-append "Stack bottom '" bottom "' is not in stackalpha")))
        e2))

  (foldl
    (lambda (t acc)
      (define from  (list-ref t 0))
      (define input (list-ref t 1))
      (define stop  (list-ref t 2))
      (define to    (list-ref t 3))
      (define push  (list-ref t 4))
      (define acc1
        (if (set-member? states-set from) acc
            (append acc (list (string-append "Transition: origin '" from "' is not a declared state")))))
      (define acc2
        (if (set-member? states-set to) acc1
            (append acc1 (list (string-append "Transition: destination '" to "' is not a declared state")))))
      (define acc3
        (if (or (equal? input "_") (set-member? alpha-set input)) acc2
            (append acc2 (list (string-append "Transition: input '" input "' is not in alphabet")))))
      (define acc4
        (if (or (equal? stop "_") (set-member? salpha-set stop)) acc3
            (append acc3 (list (string-append "Transition: stack symbol '" stop "' is not in stackalpha")))))
      (if (equal? push "_")
          acc4
          (foldl (lambda (c inner-acc)
                   (if (set-member? salpha-set (string c)) inner-acc
                       (append inner-acc
                               (list (string-append "Transition: push symbol '"
                                                    (string c)
                                                    "' is not in stackalpha")))))
                 acc4
                 (string->list push))))
    e3
    pda-trans))

; Semantic validation: every state/symbol referenced must be declared
(define (validate-automaton auto)
  (define mode (hash-ref auto 'mode ""))
  (if (equal? mode "pda")
      (validate-pda-semantics auto)
      (let ()
        (define states-set (list->set (hash-ref auto 'states '())))
        (define alpha-set (list->set (hash-ref auto 'alphabet '())))
        (define start (hash-ref auto 'start ""))
        (define end-list (hash-ref auto 'end '()))
        (define transitions (hash-ref auto 'transitions (hash)))
        (define e1
          (if (and (not (equal? start "")) (not (set-member? states-set start)))
              (list (string-append "Start state '" start "' is not declared"))
              '()))
        (define e2
          (foldl (lambda (s acc)
                   (if (set-member? states-set s) acc
                       (append acc (list (string-append "End state '" s "' is not declared")))))
                 e1 end-list))
        (for/fold ([acc e2])
                  ([(from from-map) (in-hash transitions)])
          (define acc1
            (if (set-member? states-set from) acc
                (append acc (list (string-append "Transition: origin '" from "' is not a declared state")))))
          (for/fold ([acc2 acc1])
                    ([(sym to) (in-hash from-map)])
            (define acc3
              (if (set-member? alpha-set sym) acc2
                  (append acc2 (list (string-append "Transition: symbol '" sym "' is not in the alphabet")))))
            (if (set-member? states-set to) acc3
                (append acc3 (list (string-append "Transition: destination '" to "' is not a declared state")))))))))

; Recursive descent entry point

(define (Recursive-descent token-stream)
  (define result (syntax-automaton (strip-spaces token-stream) (hash) '()))
  (define syntax-errors (result-errors result))
  (define auto (result-auto result))
  (if (null? syntax-errors)
      (let ([semantic-errors (validate-automaton auto)])
        (if (null? semantic-errors)
            (list #t auto)
            (list #f semantic-errors)))
      (list #f syntax-errors))
  )