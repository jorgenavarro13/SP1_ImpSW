#lang racket

(provide tokenize-to-html)

(require "lexer.rkt")

; Escape special HTML characters in a string
(define (html-encode str)
  (string-replace
   (string-replace
    (string-replace str "&" "&amp;")
    "<" "&lt;")
   ">" "&gt;"))

; Map token label to a CSS class
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

; Generate the HTML <span> element for a token
(define (token->span tok)
  (string-append "<span class=\"" (label->class (first tok)) "\">"
                 (html-encode (second tok))
                 "</span>"))

; Generate HTML for a tokenized line (snippet for inline insertion)
(define (line->html-snippet line-pair)
  (let ([tokens (cdr line-pair)])
    (if (null? tokens)
        ""
        (string-append (apply string-append (map token->span tokens)) "<br>\n"))))


; For servlet: return an HTML snippet for an input string
(define (tokenize-to-html input-str)
  (let* ([result     (Tokenizer input-str)]
         [tokens     (first result)]
         [error-line (second result)]
         [token-html (apply string-append
                            (map (lambda (tok)
                                   (if (equal? (first tok) "newline")
                                       "<br>\n"
                                       (token->span tok)))
                                 tokens))])
    (if error-line
        (string-append token-html
                       "<br>\n"
                       "<span class=\"lexmessage\">Unknown character or phrase on this line:</span>" "<br>\n"
                       "<span class=\"error\">" (html-encode error-line) "</span>")
        token-html)))



; ---- For standalone: generate a complete HTML document ----
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
   "  .error      { color:#f8f8f2; text-decoration: underline;}\n"
   "</style>\n"))

(define (tokens->full-html tokenized-lines)
  (string-append
   "<!DOCTYPE html>\n<html>\n<head>\n<meta charset=\"UTF-8\">\n<title>Token Stream</title>\n"
   html-styles
   "</head>\n<body>\n<pre>\n"
   (apply string-append (map line->html-snippet tokenized-lines))
   "</pre>\n</body>\n</html>"))


