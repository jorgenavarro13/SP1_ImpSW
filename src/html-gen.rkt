#lang racket

(provide tokenize-to-html tokenize-string tokenize-lines)

(require "src/lexer.rkt")

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
  (case label
    [(kw_states kw_start kw_accepting kw_alphabet kw_transitions) "keyword"]
    [(state_id)            "state-id"]
    [(symbol_id)           "symbol-id"]
    [(dos_puntos)          "dos-puntos"]
    [(coma)                "coma"]
    [(punto_coma)          "punto-coma"]
    [(flecha)              "flecha"]
    [(par_abre par_cierra) "parentesis"]
    [(comment)             "comment"]
    [else                  "unknown"]))

; Genera el elemento <span> HTML para un token
(define (token->span tok)
  (string-append "<span class=\"" (label->class (car tok)) "\">"
                 (html-encode (cdr tok))
                 "</span>"))

; Genera el HTML de una linea tokenizada (snippet para insertar inline)
(define (line->html-snippet line-pair)
  (let ([tokens (cdr line-pair)])
    (if (null? tokens)
        ""
        (string-append (apply string-append (map token->span tokens)) "<br>\n"))))

; ---- Para el servlet: devuelve snippet HTML de un string de entrada ----
(define (tokenize-to-html input-str)
  (let* ([lines     (string-split input-str "\n")]
         [tokenized (flatten-token lines)])
    (apply string-append (map line->html-snippet tokenized))))

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


