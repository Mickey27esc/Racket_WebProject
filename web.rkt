#lang web-server/insta

;EJERCICIOS

(define (longitud L)
  (if (null? L) 0
      (+ 1 (longitud (cdr L)))
      )
  )

(define (contiene x L)
  (if (null? L) #f
      (if (= x (car L)) #t
          (contiene x (cdr L))
          )
      )
  )

(define (eliminar x L)
  (if (null? L) '()
      (if (= x (car L)) (cdr L)
          (append (list (car L)) (eliminar x (cdr L)))
          )
      )
  )

(define (invertir L)
  (if (null? L) '()
      (append (invertir (cdr L)) (list (car L)))
      )
  )

(define (pares L)
  (if (null? L) '()
      (if (even? (car L)) (append (list (car L)) (pares (cdr L)))
          (pares (cdr L))
          )
      )
  )


;El SOLUCIONARIO está conformadO por SOLUCIONES.
(struct solucion (s))

;SOLUCIONARIO : lista de soluciones 
(define SOLUCIONARIO
  (list (solucion "")
        )
  )

;start request -> response
(define (start request)
  (define a-solucion
    (if (can-parse-solucion? (request-bindings request)) (parse-solucion (request-bindings request))
      (list (solucion "") (solucion "") (solucion "")  (solucion "")  (solucion ""))
      )
    )
  (render-page a-solucion request)
  )


;can-parse-solucion?: bindings -> boolean
; Produces true if bindings contains values for every "solution"
(define (can-parse-solucion? bindings)
  (and (exists-binding? 'solucion1 bindings)
       (and (exists-binding? 'solucion2 bindings)
            (and (exists-binding? 'solucion3 bindings)
                 (and (exists-binding? 'solucion4 bindings)
                       (exists-binding? 'solucion5 bindings)
                       )
                 )
            )
       )
  )

;parse.-solucion: binding -> listof solucion
;Toma los bindings y devuelve una lista que sera utilizada para imprimir 
(define (parse-solucion bindings)
  (print (solucion-s (solucion (longitud (string-split (extract-binding/single 'solucion3 bindings))))))
  (list (solucion (pares (map string->number (string-split (extract-binding/single 'solucion1 bindings)))))
        (solucion (invertir (string-split (extract-binding/single 'solucion2 bindings))))
        (solucion (longitud (string-split (extract-binding/single 'solucion3 bindings))))
        (solucion (if (contiene (string->number (extract-binding/single 'contiene bindings)) (map string->number (string-split (extract-binding/single 'solucion4 bindings)))) "Verdadero" "Falso"))
        (solucion (eliminar (string->number (extract-binding/single 'eliminar bindings)) (map string->number (string-split (extract-binding/single 'solucion5 bindings)))))
        )
)


(define (render-page a-solucion request)
  (response/xexpr
   `(html (head (title "PROYECTO DE TEORIA "))
          (body
           (h1 "PROGRAMACION FUNCIONAL")
           ;Ejercicio 1
           (form
            (p "Función que devuelve los numeros pares de una lista")
            (small "Ingrese los valores enteros de la lista separados por un epsacio en blanco")
            (input ((name "solucion1")))
            (input ((type "submit")))
            (p ,(format "~v" (solucion-s (list-ref a-solucion 0))))
           ;Ejercicio 2
            (p "Función que devuelve la lista inversa")
            (input ((name "solucion2")))
            (input ((type "submit")))
            (p ,(format "~v" (solucion-s (list-ref a-solucion 1))))
           ;Ejercicio 3
            (p "Función que devuelve la longitud de una lista")
            (input ((name "solucion3")))
            (input ((type "submit")))
            (p ,(format "~v" (solucion-s (list-ref a-solucion 2))))
           ;Ejercicio 4
            (p "Función que determina si una lista numérica contiene un elemento. Ingrese el elemento a encontrar en el primer campo, y en el segundo ingrese los elementos, separados por espacios en blanco")
            (input ((name "contiene")))
            (input ((name "solucion4")))
            (input ((type "submit")))
            (p ,(solucion-s (list-ref a-solucion 3)))
           ;Ejercicio 5
            (p "Función que elimina un elemento numérico de una lista. Ingrese el elemento a eliminar en el primer campo, y en el segundo ingrese los elementos, separados por espacios en blanco")
            (input ((name "eliminar")))
            (input ((name "solucion5")))
            (input ((type "submit"))))
            (p ,(format "~v" (solucion-s (list-ref a-solucion 4))))
           )
       )
   )
)