#lang racket
(require racket/set) 

; Hello teacher ! If you're reviewing this it's me Jorge :)
(provide simulate) ; Main function, helps us to simulate the automatons 
                   ;  based on a hashmap of the structure 

(define (simulate automaton input)
  (if (equal? (hash-ref automaton 'mode "") "pda")
      (simulate-pda automaton input)
      (simulate-dfa automaton input)))

; DFA / NFA Both uses the same hashmap but the differences are when on a state 
; the same symbol has 2 or more symultaneous transitions
(define (simulate-dfa automaton input)
  (define start-state (hash-ref automaton 'start))
  (define end-states  (hash-ref automaton 'end))
  (define transitions (hash-ref automaton 'transitions (hash)))

  (define (run current-state remaining)
    (cond
      [(empty? remaining) (if (member current-state end-states) #t #f)] ; Base case
      ; Does the current state is on the final states? 
      ; Depending on this, it's accepted or not

      [else ; Keep looking the next transitions until the base case
       (define symbol     (string (car remaining)))
       (define from-map   (hash-ref transitions current-state (hash)))
       (define next-state (hash-ref from-map symbol #f))
       (if next-state
           (run next-state (cdr remaining))
           #f)]))

  (run start-state (string->list input))) ; This is a helper function to encapsulate the dfa logic

; PDA — DFS for this we used backtracking
; How does this automaton works? 
; 1.- The PDA starts at a defined initial state with the input converted to a character 
; list and a stack pre-loaded with a bottom marker symbol 
; 2.- At each step, find-matches scans all defined transitions looking for ones that match the current state, 
; the top of the stack, and either the current input symbol or a wildcard
; 3.- Stack manipulation — apply-stack updates the stack per the matched transition: 
; it optionally pops the top element and pushes a new string onto it 
; (leftmost char lands on top), or does nothing if the push/pop field is "_".
; 4.- run recursively explores all possible transitions using ormap
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

  ; Pop stackTop (if not epsilon) then push the push-string onto the stack.
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

  (run start (string->list input) (list bottom) (set))) ; Helper function

