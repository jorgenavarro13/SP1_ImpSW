#lang racket
(require web-server/servlet-env)
(require "src/server/servlet.rkt")

(serve/servlet
  start
  #:launch-browser? #f
  #:listen-ip #f
  #:servlet-regexp #rx""
  #:port (let ([p (getenv "PORT")]) (if p (string->number p) 8000))
  )
