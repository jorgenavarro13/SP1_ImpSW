#lang racket

(provide genera-img)
(require racket/system)
(require racket/runtime-path)
(require net/base64)

(define-runtime-path here ".")

; Group transitions into a (from . to) -> (sym ...) map so parallel edges share one label
(define (build-edge-map transitions)
  (for*/fold ([acc (hash)])
             ([(from from-map) (in-hash transitions)]
              [(sym  to-list)  (in-hash from-map)]
              [to              to-list])
    (define key (cons from to))
    (hash-set acc key (cons sym (hash-ref acc key '())))))

(define (genera-dot auto)
  (define states      (hash-ref auto 'states      '()))
  (define start       (hash-ref auto 'start       ""))
  (define end-states  (hash-ref auto 'end         '()))
  (define transitions (hash-ref auto 'transitions (hash)))
  (define edge-map    (build-edge-map transitions))

  (define state-lines
    (apply string-append
           (map (lambda (s)
                  (if (member s end-states)
                      (string-append "  " s " [shape=doublecircle];\n")
                      (string-append "  " s " [shape=circle];\n")))
                states)))

  (define edge-lines
    (apply string-append
           (for/list ([(key syms) (in-hash edge-map)])
             (define from  (car key))
             (define to    (cdr key))
             (define label (string-join (remove-duplicates syms) ","))
             (string-append "  " from " -> " to " [label=\"" label "\"];\n"))))

  (string-append
   "digraph G {\n"
   "  rankdir=LR;\n"
   "  __start__ [shape=plaintext label=\"\"];\n"
   "  __start__ -> " start ";\n"
   state-lines
   edge-lines
   "}\n"))

(define (genera-img auto)
  (define dot-text (genera-dot auto))
  (parameterize ([current-directory here])
    (call-with-output-file "automaton.dot" #:exists 'replace
                           (lambda (out) (display dot-text out)))
    (system "dot -Tpng automaton.dot -o automaton.png")
    (bytes->string/latin-1 (base64-encode (file->bytes "automaton.png") #""))))
