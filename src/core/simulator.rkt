#lang racket
; Simulator: runs an automaton against an input string

(provide simulate)

(define (simulate automaton input)
  (define mode (hash-ref automaton 'mode "dfa"))
  (if (equal? mode "lba")
      (simulate-lba automaton input)
      (simulate-dfa-nfa automaton input)))

(define (simulate-dfa-nfa automaton input)
  (define start-state (hash-ref automaton 'start))
  (define end-states (hash-ref automaton 'end))
  (define transitions (hash-ref automaton 'transitions (hash)))

  (define (simulate-aux current-state remaining)
    (cond
      [(empty? remaining) (if (member current-state end-states) #t #f)]
      [else
       (define symbol (string (car remaining)))
       (define from-map (hash-ref transitions current-state (hash)))
       (define next-state (hash-ref from-map symbol #f))
       (if next-state
           (simulate-aux next-state (cdr remaining))
           #f)]))

  (simulate-aux start-state (string->list input)))

(define (simulate-lba automaton input)
  (define start-state (hash-ref automaton 'start))
  (define end-states (hash-ref automaton 'end))
  (define transitions (hash-ref automaton 'transitions (hash)))
  (define tape (list->vector (map string (string->list input))))
  (define tape-len (vector-length tape))
  (define max-steps (* 10 (max 1 tape-len)))

  (define (step state head steps)
    (cond
      [(> steps max-steps) #f]
      [(member state end-states) #t]
      [(or (< head 0) (>= head tape-len)) #f]
      [else
       (define cell (vector-ref tape head))
       (define from-map (hash-ref transitions state (hash)))
       (define tuple (hash-ref from-map cell #f))
       (if (not tuple)
           #f
           (let ([to (first tuple)]
                 [write (second tuple)]
                 [dir (third tuple)])
             (vector-set! tape head write)
             (define next-head
               (cond
                 [(equal? dir "R") (+ head 1)]
                 [(equal? dir "L") (- head 1)]
                 [else head]))
             (step to next-head (+ steps 1))))]))

  (step start-state 0 0))
