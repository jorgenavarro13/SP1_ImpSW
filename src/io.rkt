#lang racket
(require srfi/13)

; IO utilities: file reading and system file opening

(provide sys-show-file
         read-file-lines)

; Read lines from a file and return them as a list of strings
(define (read-file-lines path)
  (define in (open-input-file path))
  (define readed-data (sequence->list (in-lines in)))
  (close-input-port in)
  readed-data)

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
