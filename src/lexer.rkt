#lang racket
; SP1 Implementation of computational methods

; NEW CONCEPTS
; string-ref is a procedure used to access a specific character within a string by its index

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
          ("identifier" #rx"^[a-zA-Z_][a-zA-Z0-9_]*")
          ("alphabet-symbol" #rx"^[a-zA-Z0-9]")
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
        (string (string-ref remaining 0)))
      )
    ]
  )
)

; ---------------TOKENIZER---------------------------------
; This function iterates trough the input, generation a token stream based on the
; longest match for each substring extracted, and as a result we obtain a token stream with
; the longest coincidences of strings and their respective label asociated

(define (Tokenizer input)
  (define (tokenizer input current-line current-line-tokens token-stream)

    (cond ; BASE CASE: We don't have more characters on our input string
      [(= (string-length input) 0) 
       (list
        (append token-stream current-line-tokens) ; In this case we return the token stream plus the current line tokens
        #f)] ; False, indicating that there wasn't syntax error

      [else

       (let* (
               [all-matches (map (lambda (r) (getMatch r input)) allRegex)]
               [mayor (getMax all-matches)]
               [label (first mayor)]
               [len (second mayor)]
               [lexem (third mayor)]
             )

         (cond

           [(equal? label "none") ; Syntax error, there's no match alongside all possible regex

            
            (list
             token-stream ; Return the current token-stream 
             (flush-line input current-line))] ; Also the flush of the current line to show where the syntaxis error happened

           ; NEWLINE
           [(equal? label "newline") ; As we keep track on which line the possible error is going to be, 
           ; we need to "clean" that line

            (tokenizer
             (substring input len)
             ""
             '()
             (append token-stream
                     current-line-tokens
                     (list (list label lexem))))]

           [else ; NORMAL TOKEN

            (tokenizer
             (substring input len) ; Cropped input
             (string-append current-line lexem) ; Current line so far
             (append current-line-tokens 
                     (list (list label lexem))) ; Current line tokens
             token-stream)]))])) ; The token stream

  (tokenizer input "" '() '())) ; Our helper function

(define (print-token-stream token-stream)

  (define (print-aux stream)

    (cond
      [(empty? stream) (void)]

      [else
       (displayln (car stream))
       (print-aux (cdr stream))])
  )

  (print-aux token-stream))

; Generate token list
(define (flatten-token token-stream)
  (apply append token-stream)
)
