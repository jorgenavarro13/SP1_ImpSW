#lang racket

(provide tokenize-to-html)
(require "lexer.rkt")

; Escapes special HTML characters in a string
(define (html-encode str)
  (string-replace
   (string-replace
    (string-replace str "&" "&amp;")
    "<" "&lt;")
   ">" "&gt;"))

; Maps a token label to a CSS class
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

; Generates the HTML <span> element for a token
(define (token->span tok)
  (string-append "<span class=\"" (label->class (car tok)) "\">"
                 (html-encode (cdr tok))
                 "</span>"))

; Generates the HTML snippet for a tokenized line (for inline insertion)
(define (line->html-snippet line-pair)
  (let ([tokens (cdr line-pair)])
    (if (null? tokens)
        ""
        (string-append (apply string-append (map token->span tokens)) "<br>\n"))))

; ---- For the servlet: returns an HTML snippet from an input string ----
(define (tokenize-to-html input-str)
  (let* ([lines     (string-split input-str "\n")]
         [tokenized (flatten-token lines)])
    (apply string-append (map line->html-snippet tokenized))))

; ---- For standalone use: generates a full HTML document ----
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
