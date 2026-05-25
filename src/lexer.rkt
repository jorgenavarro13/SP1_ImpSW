#lang racket
; SP1 Implementation of computational methods

(provide reMatch
         getMatch
         getMax
         allRegex
         flush-line
         Tokenizer
         print-token-stream
         )

(define reMatch regexp-match-positions)

; (label, len-match, substr) GetMatch ((label regex))----------------
; This function returns a pair list of the match string
; based on the input and a regex pattern
(define (getMatch label-regex str)
    (define label (first label-regex))
    (define regex (second label-regex))
    (define match (reMatch regex str))

    (if match
        (let*
            (
                [lenMatch (cdr(first match))]
                [subStr (substring str 0 lenMatch)]
            )
            (list label lenMatch subStr)
        )

        (list "none" 0 "")
    )
)

; '(matches) getMax -> ('(label token))
; Function that get's the maximum match of a list of matches
(define (getMax allMatches)
    (let*
        (
            [longs (map second allMatches)]
            [maxLen (apply max longs)]
            [is-max? (lambda (row) (= maxLen (second row)))]
        )
        (first (filter is-max? allMatches))
    )
)

; Regular expresions of the automaton
(define allRegex
      '(  ("rw-automata" #rx"Automaton")
          ("rw-start" #rx"^start")
          ("rw-end" #rx"^end")
          ("rw-states" #rx"^states")
          ("rw-transitions" #rx"^transitions")
          ("rw-alphabet" #rx"^alphabet")
          ("right-straigth-parenthesis" #rx"^\\[")
          ("left-straigth-parenthesis" #rx"^\\]")
          ("newline" #rx"^\n")
          ("lower-line" #rx"^_")
          ("stateId" #rx"^q[0-9]+"  )
          ("dots"    #rx"^:")
          ("transition-sybol" #rx"^::")
          ("alphabet-symbol" #rx"^[a-zA-Z0-9]")
          ("identifier" #rx"^[a-zA-Z_][a-zA-Z0-9_]*")
          ("coma"    #rx"^,")
          ("semicol" #rx"^;")
          ("blank_space" #rx"^[ ]+")
       )
)

; consume characters until newline or eof
(define (flush-line remaining current-line)
  (cond
    [(= (string-length remaining) 0)
      current-line]

    [(char=? (string-ref remaining 0) #\newline)
      current-line]

    [else
      (flush-line
      (substring remaining 1)
      (string-append
        current-line
        (string (string-ref remaining 0))))]))

; ---------------TOKENIZER---------------------------------
; This function iterates trough the input, generation a token stream based on the
; longest match for each substring extracted, and as a result we obtain a token stream with
; the longest coincidences of strings and their respective label asociated

(define (Tokenizer input)

  (define (tokenizer input current-line current-line-tokens token-stream)

    (cond
      [(= (string-length input) 0)
       (list
        (append token-stream current-line-tokens)
        #f)]

      [else

       (let* (
               [all-matches (map (lambda (r) (getMatch r input)) allRegex)]
               [mayor (getMax all-matches)]
               [label (first mayor)]
               [len (second mayor)]
               [lexem (third mayor)]
             )

         (cond

           ; ERROR
           [(equal? label "none")

            ; discard current-line-tokens
            (list
             token-stream
             (flush-line input current-line))]

           ; NEWLINE
           [(equal? label "newline")

            (tokenizer
             (substring input len)
             ""
             '()
             (append token-stream
                     current-line-tokens
                     (list (list label lexem))))]

           ; NORMAL TOKEN
           [else

            (tokenizer
             (substring input len)
             (string-append current-line lexem)
             (append current-line-tokens
                     (list (list label lexem)))
             token-stream)]))]))

  (tokenizer input "" '() '()))

(define (print-token-stream token-stream)

  (define (print-aux stream)

    (cond
      [(empty? stream) (void)]

      [else
      (display "(")
      (display "\"")
       (display (caar stream))
      (display "\"")
      (display "  ")
      (display "\"")
      (if(equal? (caar stream) "newline")
       (display "\\n")
       (display (cadar stream)))
      (display "\"")
      (displayln ")")
       (print-aux (cdr stream))]))

  (print-aux token-stream))

(print-token-stream (car(Tokenizer "Automaton dfa [
  states :[ q0, q1, q2 ]
  alphabet: [ a, b, 0 ]
  start: q0
  end: q2
  transitions :[ 
    q0 :: a :: q1,
    q1 :: b :: q2,
    q2 :: 0 :: q0
  ]
]")))