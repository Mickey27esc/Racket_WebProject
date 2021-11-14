#lang web-server/insta

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
    (cond
      [(can-parse-solucion? (request-bindings request)) (cons SOLUCIONARIO (parse-solucion (request-bindings request)))]
      [else SOLUCIONARIO])
    )
  (render-page a-solucion request)
  )


;can-parse-solucion?: bindings -> boolean
; Produces true if bindings contains values for every "solution"
(define (can-parse-solucion? bindings)
  (and (exists-binding? 'solution1 bindings)
       (and (exists-binding? 'solution2 bindings)
            (and (exists-binding? 'solution3 bindings)
                 (and (exists-binding? 'solution4 bindings)
                       (exists-binding? 'solution5 bindings)
                       )
                 )
            )
       )
  )

;parse.-solucion: binding -> listof solucion
;Toma los bindings y devuelve una lista que sera utilizada para imprimir 
(define (parse-solucion binding)
  (list (solucion (extract-binding/single 'solucion1))
        (solucion (extract-binding/single 'solucion2))
        (solucion (extract-binding/single 'solucion3))
        (solucion (extract-binding/single 'solucion4))
        (solucion (extract-binding/single 'solucion5))
        )
)


(define (render-page a-solucion request)
  (response/xexpr
   `(html (head (title "PROYECTO DE TEORIA "))
          (body
           (h1 "PROGRAMACION FUNCIONAL")
           ;Ejercicio 1
           (p "Función que devuelve los numeros pares de una lista")
           (form
            (input ((name "solucion1")))
            (input ((type "submit"))))
           ;Ejercicio 2
           (p "Función que devuelve la lista inversa")
           (form
            (input ((name "solucion2")))
            (input ((type "submit"))))
           ;Ejercicio 3
           (p "Función ...")
           (form
            (input ((name "solucion3")))
            (input ((type "submit"))))
           ;Ejercicio 4
           (p "Función ...")
           (form
            (input ((name "solucion4")))
            (input ((type "submit"))))
           ;Ejercicio 5
           (p "Función ...")
           (form
            (input ((name "solucion5")))
            (input ((type "submit"))))
           )
       )
   )
)