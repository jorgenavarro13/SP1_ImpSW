#lang racket
(require racket/set)

(provide simulate)

(define (simulate automaton input)
  (if (equal? (hash-ref automaton 'mode "") "pda")
      (simulate-pda automaton input)
      (simulate-dfa automaton input)))

; DFA / NFA — deterministic traversal (existing behaviour preserved)
(define (simulate-dfa automaton input)
  (define start-state (hash-ref automaton 'start))
  (define end-states  (hash-ref automaton 'end))
  (define transitions (hash-ref automaton 'transitions (hash)))

  (define (run current-state remaining)
    (cond
      [(empty? remaining) (if (member current-state end-states) #t #f)]
      [else
       (define symbol     (string (car remaining)))
       (define from-map   (hash-ref transitions current-state (hash)))
       (define next-state (hash-ref from-map symbol #f))
       (if next-state
           (run next-state (cdr remaining))
           #f)]))

  (run start-state (string->list input)))

; PDA — DFS with backtracking; ε-loop guard via visited-configuration set
(define (simulate-pda automaton input)
  (define start  (hash-ref automaton 'start))
  (define ends   (hash-ref automaton 'end))
  (define trans  (hash-ref automaton 'pda-transitions '()))
  (define bottom (hash-ref automaton 'stackbottom "Z"))

  ; Return all transitions whose (from, input, stackTop) match the given args.
  ; A "_" in the transition's input field matches any sym; same for stackTop.
  (define (find-matches state sym stack-top)
    (filter (lambda (t)
              (and (equal? (list-ref t 0) state)
                   (or (equal? (list-ref t 1) sym)
                       (equal? (list-ref t 1) "_"))
                   (or (equal? (list-ref t 2) stack-top)
                       (equal? (list-ref t 2) "_"))))
            trans))

  ; Pop stackTop (if not ε) then push the push-string onto the stack.
  ; Leftmost char of push-string ends up on top.
  (define (apply-stack t stack)
    (define pop  (list-ref t 2))
    (define push (list-ref t 4))
    (define popped
      (cond [(equal? pop "_") stack]
            [(empty? stack)   stack]
            [else             (cdr stack)]))
    (if (equal? push "_")
        popped
        (append (map string (string->list push)) popped)))

  (define (run state remaining stack visited)
    (define cfg (list state (length remaining) stack))
    (cond
      [(set-member? visited cfg) #f]
      [(and (empty? remaining) (member state ends)) #t]
      [else
       (define new-visited (set-add visited cfg))
       (define stack-top   (if (empty? stack) "_" (first stack)))
       ; Epsilon-input transitions (don't consume a character)
       (define eps-matches
         (filter (lambda (t) (equal? (list-ref t 1) "_"))
                 (find-matches state "_" stack-top)))
       ; Normal transitions (consume the current input character)
       (define normal-matches
         (if (empty? remaining) '()
             (filter (lambda (t) (not (equal? (list-ref t 1) "_")))
                     (find-matches state (string (first remaining)) stack-top))))
       (or
         (ormap (lambda (t)
                  (run (list-ref t 3) remaining (apply-stack t stack) new-visited))
                eps-matches)
         (ormap (lambda (t)
                  (run (list-ref t 3) (cdr remaining) (apply-stack t stack) new-visited))
                normal-matches))]))

  (run start (string->list input) (list bottom) (set)))
