#lang racket
; Simulator: runs an automaton against an input string

(provide simulate)

(define (simulate automaton input)
  (define start-state  (hash-ref automaton 'start))
  (define end-states   (hash-ref automaton 'end))
  (define transitions  (hash-ref automaton 'transitions (hash)))

  (define (simulate-aux current-state remaining)
    (cond
      [(empty? remaining) (if (member current-state end-states) #t #f)]
      [else
       (define symbol    (string (car remaining)))
       (define from-map  (hash-ref transitions current-state (hash)))
       (define next-state (hash-ref from-map symbol #f))
       (if next-state
           (simulate-aux next-state (cdr remaining))
           #f)]))

  (simulate-aux start-state (string->list input)))
