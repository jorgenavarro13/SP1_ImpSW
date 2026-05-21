#lang racket
(define reMatch regexp-match-positions)
; SP1 Implementation of computational methods

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
      '(  ("rw-automata" #rx"^Automata")
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
          ("alphabet-symbol" #rx"^[a-zA-Z0-9] ")
          ("coma"    #rx"^,")
          ("semicol" #rx"^;")
          ("identifier" #rx"^([A-Z]|[a-z])+")
       )
)

; ---------------TOKENIZER---------------------------------
; This function iterates trough the input, generation a token stream based on the
; longest match for each substring extracted, and as a result we obtain a token stream with 
; the longest coincidences of strings and their respective label asociated

(define (Tokenizer input) ; Auxiliar function for tokenizer
  (define (tokenizer input token-stream current-line) ; To generate a list of lists two simultaneous lists are being used
    (cond                                                                           
      ; BASE CASE
      [(= (string-length input) 0) (append token-stream (list current-line))]
      ; Skip blank spaces
      [(equal? (substring input 0 1) " ")
      (tokenizer (substring input 1) token-stream current-line)]
      
      [else
      (let* (
              [all-matches (map (lambda (r) (getMatch r input)) allRegex)]
              [mayor (getMax all-matches)]
              [label  (first mayor)]
              [len    (second mayor)]
              [lexem (third mayor)]￼
            )
        ( cond
            ; No match: Skip a character
            [(equal? label "none") (tokenizer (substring input 1) token-stream current-line)] ; Avoid infinite loop
            ; Skip to the next line
            [(equal? label "newline")(tokenizer(substring input len)  ; Add it to the list of lists 
                                                (append token-stream (list current-line))
                                                '()
                                      )
            ]
            ; Match : Consume and add token
            [else (tokenizer (substring input len)
                            token-stream 
                            (append current-line (list (list label lexem)))) ; Add it to the current line list
            ]
          )
      )]
    )
  )
  (tokenizer input '() '())
)

; Auxiliary function to print the token stream
(define (print-token-stream token-stream) 
  
  (define (print-token-stream-aux token-stream counter) ; Funcion auxiliar para no tener que pasar todos los parametros en la llamada a la funcion
    (cond
      [(empty? token-stream)]
      [else 
        (let* (
                [currentline  (car token-stream)]
                [tokens (map second currentline )]
                [restoflines (cdr token-stream)]
              )
      (display "\nLinea ")(display counter)(display ":") 
      (map (lambda (r) (display r)) tokens) (displayln "")
      (map (lambda (pair) (displayln pair )) currentline)    
      (print-token-stream-aux restoflines (+ 1 counter))
        )
      ]
    )
  )
 
  (print-token-stream-aux token-stream 1)
)