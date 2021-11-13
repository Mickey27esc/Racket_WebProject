#lang slideshow

;Funcion para encontrar un elemento en un lista

(define (contiene x L)
  (if (null? L) #f
      (if (= x (car L)) #t
          (contiene x (cdr L))
          )
      )
  )