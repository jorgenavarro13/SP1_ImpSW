#lang racket
(require racket/system)
(require racket/runtime-path)
(require racket/string)
(require net/base64)

(provide generate-img)

(define-runtime-path here ".")
(current-directory here)
(displayln (format "DIR: ~a" (current-directory)))

; Groups transitions by (from . to) pair so multiple symbols share one edge.
; Returns an immutable hash: (from . to) -> (list sym ...)
(define (group-transitions transitions)
  (foldl (lambda (t acc)
           (define key (cons (first t) (third t)))
           (define sym (second t))
           (hash-update acc key
                        (lambda (syms) (append syms (list sym)))
                        '()))
         (hash)
         transitions))

; Builds DOT source from the automaton hash.
(define (generate-dot automaton)
  (define states (hash-ref automaton 'states))
  (define start (hash-ref automaton 'start))
  (define end (hash-ref automaton 'end))
  (define trans-hash (hash-ref automaton 'transitions))

  ; Flatten nested hash (from -> (sym -> to)) into list of (from sym to) triples
  (define transitions
    (apply append
           (hash-map trans-hash
                     (lambda (from sym-hash)
                       (hash-map sym-hash
                                 (lambda (sym to) (list from sym to)))))))

  (define state-lines
    (map (lambda (s)
           (if (member s end)
               (format "  ~a [shape=doublecircle];" s)
               (format "  ~a [shape=circle];" s)))
         states))

  (define transition-lines
    (hash-map (group-transitions transitions)
              (lambda (key syms)
                (format "  ~a -> ~a [label=\"~a\"];"
                        (car key) (cdr key)
                        (string-join syms ",")))))

  (string-join
    (append
      (list "digraph DFA {"
            "  rankdir=LR;"
            "  start [shape=plaintext label=\"\"];"
            (format "  start -> ~a;" start))
      state-lines
      transition-lines
      (list "}"))
    "\n"))

(define (generate-img automaton)
  (define dot-text (generate-dot automaton))
  (parameterize ([current-directory here])
    (call-with-output-file "dfa.dot" #:exists 'replace
      (lambda (out) (display dot-text out)))
    (system "dot -Tpng dfa.dot -o dfa.png")
    (displayln "dfa.png generated")
    (bytes->string/latin-1 (base64-encode (file->bytes "dfa.png") #""))))
