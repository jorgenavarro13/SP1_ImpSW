#lang racket
(require srfi/13)

; Read files
(define file "input.txt")
(define in (open-input-file file))
(define readed-data (sequence->list (in-lines in)) )
(close-input-port in)

(displayln "\nFile content")
(displayln readed-data)

(define (sys-show-file path)
  (cond
    [(equal? (system-type 'os) 'windows)
     (system (format "start \"\" \"~a\"" path))  ]
    [(equal? (system-type 'os) 'macosx)
     (system (format "open \"~a\"" path))  ]
    [else ; Linux / Unix
     (system (format "xdg-open \"~a\"" path))  ]
   )
)

(sys-show-file "paginaUNO.html")