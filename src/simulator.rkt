#lang racket
; Simulator: runs an automaton against an input string

(provide simulate)

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
