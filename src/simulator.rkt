#lang racket
; Simulator: runs an automaton against an input string

(provide simulate)

(define (simulate automaton input)
  (define start-state (hash-ref automaton 'start))
  (define end-states  (hash-ref automaton 'end))
  (define transitions (hash-ref automaton 'transitions (hash)))

  ; Expand one symbol from every state in the active set
  (define (step states symbol)
    (remove-duplicates
     (apply append
            (map (lambda (state)
                   (hash-ref (hash-ref transitions state (hash)) symbol '()))
                 states))))

  (define (simulate-aux states remaining)
    (cond
      [(null? states) #f]
      [(empty? remaining)
       (if (ormap (lambda (s) (member s end-states)) states) #t #f)]
      [else
       (simulate-aux (step states (string (car remaining))) (cdr remaining))]))

  (simulate-aux (list start-state) (string->list input)))
