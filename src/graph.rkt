#lang racket

(provide genera-img)          ; hace que esta función esté disponible para otros módulos
(require racket/system)       ; para el comando en terminal
(require racket/runtime-path) ; para manejo de rutas
(require net/base64)          ; para encoding de imagen

; establece la ruta actual con define-runtime-path  y current-directory
; luego imprimela
(define-runtime-path here ".")
(current-directory here)
(displayln (format "DIR: ~a" (current-directory)))

; devuleve el contenido del .dot
; notas: \"  para comillas dentro del string
; automata-rep es dummy... por ahora 
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
  ; contenido para el .dot: llamando a a genera-dot
  (define dot-text (genera-dot automata-rep))

  ; asegurar que ocurra en el directorio establecido... 
  (parameterize ([current-directory here])
      ; guardar archivo .dot, remplazando si ya existe
      (call-with-output-file "dfa.dot" #:exists 'replace
                             (lambda (out) (display dot-text out)))

     ; llamada a graphviz con el comando para generar la imagen
      (system "dot -Tpng dfa.dot -o dfa.png")
      (displayln "dfa.png generated")
    
     ; regresa imagen codificada
     (bytes->string/latin-1 (base64-encode (file->bytes "dfa.png") #""))
  )
)

; hace que la funcion genera-img este disponible desde fuera, con provide
(provide genera-img)

