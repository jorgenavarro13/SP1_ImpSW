#lang racket

(provide tokenize-to-html)

(require "lexer.rkt")

; =====================================================
; GENERACION DE HTML
; =====================================================

; Escapa caracteres especiales de HTML en un string
(define (html-encode str)
  (string-replace
   (string-replace
    (string-replace str "&" "&amp;")
    "<" "&lt;")
   ">" "&gt;"))

; Mapea etiqueta de token a clase CSS
(define (label->class label)
  (cond
    [(member label '("rw-automata" "rw-start" "rw-end"
                     "rw-states" "rw-transitions" "rw-alphabet")) "keyword"]
    [(equal? label "stateId")              "state-id"]
    [(equal? label "alphabet-symbol")      "symbol-id"]
    [(equal? label "dots")                 "dos-puntos"]
    [(equal? label "transition-sybol")     "flecha"]
    [(equal? label "coma")                 "coma"]
    [(equal? label "semicol")              "punto-coma"]
    [(member label '("right-straigth-parenthesis"
                     "left-straigth-parenthesis")) "parentesis"]
    [(equal? label "blank_space")          "blank"]
    [(equal? label "identifier")           "identifier"]
    [else                                  "unknown"]))

; Genera el elemento <span> HTML para un token
(define (token->span tok)
  (string-append "<span class=\"" (label->class (first tok)) "\">"
                 (html-encode (second tok))
                 "</span>"))

; Genera el HTML de una linea tokenizada (snippet para insertar inline)
(define (line->html-snippet line-pair)
  (let ([tokens (cdr line-pair)])
    (if (null? tokens)
        ""
        (string-append (apply string-append (map token->span tokens)) "<br>\n"))))

; ---- Para el servlet: devuelve snippet HTML de un string de entrada ----
(define (tokenize-to-html input-str)
  (let* (
         [tokenized (Tokenizer input-str)]
         [tokenized (first tokenized)]  ; obtenemos solo la parte de tokens, descartando error-line
         )
    (apply string-append (map token->span tokenized))))

; ---- Para standalone: genera documento HTML completo ----
(define html-styles
  (string-append
   "<style>\n"
   "  body        { background:#272822; color:#f8f8f2; font-family:monospace; font-size:15px; padding:30px; }\n"
   "  span        { font-weight:bold; }\n"
   "  .keyword    { color:#f92672; font-style:italic; }\n"
   "  .state-id   { color:#a6e22e; }\n"
   "  .symbol-id  { color:#e6db74; }\n"
   "  .dos-puntos { color:#ae81ff; }\n"
   "  .coma       { color:#f8f8f2; }\n"
   "  .punto-coma { color:#ae81ff; }\n"
   "  .flecha     { color:#66d9ef; }\n"
   "  .parentesis { color:#fd971f; }\n"
   "  .comment    { color:#75715e; font-style:italic; }\n"
   "</style>\n"))

(define (tokens->full-html tokenized-lines)
  (string-append
   "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"UTF-8\">\n<title>Token Stream</title>\n"
   html-styles
   "</head>\n<body>\n<pre>\n"
   (apply string-append (map line->html-snippet tokenized-lines))
   "</pre>\n</body>\n</html>"))


