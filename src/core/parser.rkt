#lang racket
(require racket/set)

(provide Recursive-descent
         strip-spaces)

; Parser: converts a flat token list into an automaton data structure
(define (strip-spaces raw-tokens)
  (filter
    (lambda (t)
      (not (member (first t)
                   (list "blank_space" "newline"))))
    raw-tokens)
  )

(define (token-type tk) (first tk))
(define (token-lexeme tk) (second tk))

; Parse result accessors: (list remaining-tokens auto errors)
(define (make-result tokens auto errors) (list tokens auto errors))
(define (result-tokens r) (first r))
(define (result-auto r) (second r))
(define (result-errors r) (third r))

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
        (define new-states (append (hash-ref auto 'states '()) (list lexem)))
        (define new-auto (hash-set auto 'states new-states))
        (syntax-statesPrime (cddr tokens) new-auto errors)]
       [else (make-result tokens auto (add-error errors "stateId" next-token))])]
    [(equal? current-token "left-straigth-parenthesis")
     (make-result (cdr tokens) auto errors)]
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
     (syntax-statesPrime (cdr tokens) new-auto errors)]
    [else (make-result tokens auto (add-error errors "stateId" current-token))]
    )
  )

; statesList ::= [ states
(define (syntax-statesList tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "right-straigth-parenthesis")
     (syntax-states (cdr tokens) auto errors)]
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
        (syntax-statesList (cddr tokens) auto errors)]
       [else (make-result tokens auto (add-error errors ":" next-token))])]
    [else (make-result tokens auto (add-error errors "states" current-token))]
    )
  )

; alphabetPrime ::= , alphabet-symbol alphabetPrime | ]
(define (syntax-alphabetPrime tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "coma")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "alphabet-symbol")
        (define lexem (token-lexeme (cadr tokens)))
        (define new-alpha (append (hash-ref auto 'alphabet '()) (list lexem)))
        (define new-auto (hash-set auto 'alphabet new-alpha))
        (syntax-alphabetPrime (cddr tokens) new-auto errors)]
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
    [(equal? current-token "alphabet-symbol")
     (define lexem (token-lexeme (car tokens)))
     (define new-auto (hash-set auto 'alphabet (list lexem)))
     (syntax-alphabetPrime (cdr tokens) new-auto errors)]
    [else (make-result tokens auto (add-error errors "alphabet-symbol" current-token))]
    )
  )

; alphabetList ::= [ alphabet
(define (syntax-alphabetList tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "right-straigth-parenthesis")
     (syntax-alphabet (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors "[" current-token))]
    )
  )

; alphabetDefinition ::= rw-alphabet dots alphabetList
(define (syntax-alphabetDefinition tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "rw-alphabet")
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
    [(equal? current-token "rw-start")
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
        (syntax-endPrime (cddr tokens) new-auto errors)]
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

; tapeAlphabetPrime ::= , alphabet-symbol tapeAlphabetPrime | ]
(define (syntax-tapeAlphabetPrime tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "coma")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "alphabet-symbol")
        (define lexem (token-lexeme (cadr tokens)))
        (define new-ta (append (hash-ref auto 'tape-alphabet '()) (list lexem)))
        (define new-auto (hash-set auto 'tape-alphabet new-ta))
        (syntax-tapeAlphabetPrime (cddr tokens) new-auto errors)]
       [else (make-result tokens auto (add-error errors "alphabet-symbol" next-token))])]
    [(equal? current-token "left-straigth-parenthesis")
     (make-result (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors ", or ]" current-token))]
    )
  )

; tapeAlphabet ::= alphabet-symbol tapeAlphabetPrime
(define (syntax-tapeAlphabet tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "alphabet-symbol")
     (define lexem (token-lexeme (car tokens)))
     (define new-auto (hash-set auto 'tape-alphabet (list lexem)))
     (syntax-tapeAlphabetPrime (cdr tokens) new-auto errors)]
    [else (make-result tokens auto (add-error errors "alphabet-symbol" current-token))]
    )
  )

; tapeAlphabetList ::= [ tapeAlphabet
(define (syntax-tapeAlphabetList tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "right-straigth-parenthesis")
     (syntax-tapeAlphabet (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors "[" current-token))]
    )
  )

; tapeAlphabetDefinition ::= rw-tape-alphabet dots tapeAlphabetList
(define (syntax-tapeAlphabetDefinition tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "rw-tape-alphabet")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "dots")
        (syntax-tapeAlphabetList (cddr tokens) auto errors)]
       [else (make-result tokens auto (add-error errors ":" next-token))])]
    [else (make-result tokens auto (add-error errors "tape_alphabet" current-token))]
    )
  )

; lba-transitionsPrime ::= , lba-transition lba-transitionsPrime | ]
(define (syntax-lba-transitionsPrime tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "coma")
     (syntax-lba-transition (cdr tokens) auto errors)]
    [(equal? current-token "left-straigth-parenthesis")
     (make-result (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors ", or ]" current-token))]
    )
  )

; lba-transition ::= stateId :: alpha :: stateId :: alpha :: alpha
(define (syntax-lba-transition tokens auto errors)
  (cond
    [(< (length tokens) 9)
     (make-result tokens auto
                  (add-error errors "stateId :: alpha :: stateId :: alpha :: dir" "end of input"))]
    [else
     (define from-type (token-type (list-ref tokens 0)))
     (define sym1-type (token-type (list-ref tokens 1)))
     (define read-type (token-type (list-ref tokens 2)))
     (define sym2-type (token-type (list-ref tokens 3)))
     (define to-type (token-type (list-ref tokens 4)))
     (define sym3-type (token-type (list-ref tokens 5)))
     (define write-type (token-type (list-ref tokens 6)))
     (define sym4-type (token-type (list-ref tokens 7)))
     (define dir-type (token-type (list-ref tokens 8)))
     (cond
       [(and (equal? from-type "stateId")
             (equal? sym1-type "transition-sybol")
             (equal? read-type "alphabet-symbol")
             (equal? sym2-type "transition-sybol")
             (equal? to-type "stateId")
             (equal? sym3-type "transition-sybol")
             (equal? write-type "alphabet-symbol")
             (equal? sym4-type "transition-sybol")
             (equal? dir-type "alphabet-symbol"))
        (define from (token-lexeme (list-ref tokens 0)))
        (define read (token-lexeme (list-ref tokens 2)))
        (define to (token-lexeme (list-ref tokens 4)))
        (define write (token-lexeme (list-ref tokens 6)))
        (define dir (token-lexeme (list-ref tokens 8)))
        (define dir-errors
          (if (member dir '("L" "R" "S"))
              errors
              (append errors (list (string-append
                                     "LBA transition: invalid direction '" dir "' (expected L, R, or S)")))))
        (define old-tr (hash-ref auto 'transitions (hash)))
        (define from-map (hash-ref old-tr from (hash)))
        (define new-auto
          (hash-set auto 'transitions
                    (hash-set old-tr from
                              (hash-set from-map read (list to write dir)))))
        (syntax-lba-transitionsPrime (list-tail tokens 9) new-auto dir-errors)]
       [else
        (make-result tokens auto
                     (add-error errors "stateId :: alpha :: stateId :: alpha :: dir" from-type))])
     ]
    )
  )

; lba-transitionsList ::= [ lba-transition lba-transitionsPrime
(define (syntax-lba-transitionsList tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "right-straigth-parenthesis")
     (syntax-lba-transition (cdr tokens) auto errors)]
    [else (make-result tokens auto (add-error errors "[" current-token))]
    )
  )

; lba-transitionsDefinition ::= rw-transitions dots lba-transitionsList
(define (syntax-lba-transitionsDefinition tokens auto errors)
  (define current-token (token-type (car tokens)))
  (cond
    [(equal? current-token "rw-transitions")
     (define next-token (token-type (cadr tokens)))
     (cond
       [(equal? next-token "dots")
        (syntax-lba-transitionsList (cddr tokens) auto errors)]
       [else (make-result tokens auto (add-error errors ":" next-token))])]
    [else (make-result tokens auto (add-error errors "transitions" current-token))]
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

; dfa-nfa-automaton ::= (DFA | NFA) identifier [ statesDefinition alphabetDefinition startDefinition endDefinition transitionsDefinition ]
(define (syntax-dfa-nfa-automaton tokens auto errors mode)
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
    [else (make-result tokens auto (add-error errors "identifier" id-type))]
    )
  )

; lba-automaton ::= LBA identifier [ statesDefinition alphabetDefinition tapeAlphabetDefinition startDefinition endDefinition lba-transitionsDefinition ]
(define (syntax-lba-automaton tokens auto errors)
  (define id-type (token-type (cadr tokens)))
  (cond
    [(equal? id-type "identifier")
     (define bracket-type (token-type (caddr tokens)))
     (cond
       [(equal? bracket-type "right-straigth-parenthesis")
        (define auto+mode (hash-set (hash-set auto 'mode "lba") 'name (token-lexeme (cadr tokens))))
        (define states-r (syntax-statesDefinition (cdddr tokens) auto+mode errors))
        (define alpha-r (syntax-alphabetDefinition (result-tokens states-r) (result-auto states-r) (result-errors states-r)))
        (define tape-r (syntax-tapeAlphabetDefinition (result-tokens alpha-r) (result-auto alpha-r) (result-errors alpha-r)))
        (define start-r (syntax-startDefinition (result-tokens tape-r) (result-auto tape-r) (result-errors tape-r)))
        (define end-r (syntax-endDefinition (result-tokens start-r) (result-auto start-r) (result-errors start-r)))
        (define trans-r (syntax-lba-transitionsDefinition (result-tokens end-r) (result-auto end-r) (result-errors end-r)))
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
    [else (make-result tokens auto (add-error errors "identifier" id-type))]
    )
  )

; automaton ::= (DFA | NFA | LBA) identifier [ ... ]
(define (syntax-automaton tokens auto errors)
  (define kw-type (token-type (car tokens)))
  (cond
    [(equal? kw-type "rw-dfa") (syntax-dfa-nfa-automaton tokens auto errors "dfa")]
    [(equal? kw-type "rw-nfa") (syntax-dfa-nfa-automaton tokens auto errors "nfa")]
    [(equal? kw-type "rw-lba") (syntax-lba-automaton tokens auto errors)]
    [else (make-result tokens auto (add-error errors "DFA, NFA, or LBA" kw-type))]
    )
  )

; Semantic validation: every state/symbol referenced must be declared
(define (validate-automaton auto)
  (define mode (hash-ref auto 'mode ""))
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

  (cond
    [(equal? mode "lba")
     (define tape-set (list->set (hash-ref auto 'tape-alphabet '())))
     (define e3
       (foldl (lambda (sym acc)
                (if (set-member? tape-set sym) acc
                    (append acc (list (string-append
                                        "Alphabet symbol '" sym "' is not in tape_alphabet")))))
              e2 (hash-ref auto 'alphabet '())))
     (for/fold ([acc e3])
               ([(from from-map) (in-hash transitions)])
       (define acc1
         (if (set-member? states-set from) acc
             (append acc (list (string-append "Transition: origin '" from "' is not a declared state")))))
       (for/fold ([acc2 acc1])
                 ([(read-sym tuple) (in-hash from-map)])
         (define to (first tuple))
         (define write (second tuple))
         (define acc3
           (if (set-member? tape-set read-sym) acc2
               (append acc2 (list (string-append
                                    "Transition: read symbol '" read-sym "' is not in tape_alphabet")))))
         (define acc4
           (if (set-member? tape-set write) acc3
               (append acc3 (list (string-append
                                    "Transition: write symbol '" write "' is not in tape_alphabet")))))
         (if (set-member? states-set to) acc4
             (append acc4 (list (string-append
                                  "Transition: destination '" to "' is not a declared state"))))))]
    [else
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
             (append acc3 (list (string-append "Transition: destination '" to "' is not a declared state"))))))]))

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