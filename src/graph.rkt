#lang racket

(provide genera-img)          ; export this function for use in other modules
(require racket/system)       ; for shell commands
(require racket/runtime-path) ; for path handling
(require net/base64)          ; for image encoding

; set the working directory to this file's location and print it
(define-runtime-path here ".")
(current-directory here)
(displayln (format "DIR: ~a" (current-directory)))

; returns the .dot file content
; note: \" for quotes inside the string
; automata-rep is a placeholder for now
(define (genera-dot automata-rep)
    (define dot-text
      "digraph DFA {
        rankdir=LR;

        start -> q0 [label=\"\"];
        q0 -> q1 [label=\"a\"];
        q1 -> q2 [label=\"b\"];
        q2 -> q2 [label=\"a,b\"];

        start [shape=plaintext];
        q1 [shape=doublecircle];
        q2 [shape=doublecircle];   }"
    )
    dot-text
)


(define  (genera-img  automata-rep )
  ; get .dot content by calling genera-dot
  (define dot-text (genera-dot automata-rep))

  ; ensure execution happens in the established directory
  (parameterize ([current-directory here])
      ; save the .dot file, replacing if it already exists
      (call-with-output-file "dfa.dot" #:exists 'replace
                             (lambda (out) (display dot-text out)))

      ; call graphviz to generate the image
      (system "dot -Tpng dfa.dot -o dfa.png")
      (displayln "dfa.png generated")

      ; return the base64-encoded image
      (bytes->string/latin-1 (base64-encode (file->bytes "dfa.png") #""))
  )
)
