#lang web-server/insta
 
; A blog is a (listof post)
; and a post is a (post title body)
(struct problem (description solution))

(struct set (p1 p2 p3 p4 p5))

;making problems
(define prob1 (problem "Función que devuelve los numeros pares de una lista" "solution"))
(define prob2 (problem "Función que devuelve una lista inversa" "solution2"))
(define prob3 (problem "Función que ..." "solution"))
(define prob4 (problem "Función que ..." "solution"))
(define prob5 (problem "Función que ..." "solution"))

; BLOG: blog
; The static blog.
(define SET (set prob1 prob2 prob3 prob4 prob5))


; start: request -> response
; Consumes a request and produces a page that displays all of the
; web content.
(define (start request)
  (define a-set
    (cond [(can-parse-problem? (request-bindings request))
           (cons (parse-problem (request-bindings request))
                 SET)]
          [else
           SET]))
  (render-set-page a-set request))
 
 
; can-parse-post?: bindings -> boolean
; Produces true if bindings contains values for 'title and 'body.
(define (can-parse-problem? bindings)
  (and (exists-binding? 'description bindings)
       (exists-binding? 'solution bindings)))
 
 
; parse-post: bindings -> post
; Consumes a bindings, and produces a post out of the bindings.
(define (parse-problem bindings)
  (problem (extract-binding/single 'description bindings)
           (extract-binding/single 'solution bindings)))
 
; render-blog-page: blog request -> response
; Consumes a blog and a request, and produces an HTML page
; of the content of the blog.
(define (render-set-page a-set request)
  (response/xexpr
   `(html (head (title "SET of Problems"))
          (body
           (h1 "Set of problems")
           
           (h1 (set-description a-set))
           (form
            (input ((name "solution 1")))
            (input ((type "submit")))
            )
           (form
            (input ((name "solution 2")))
            (input ((type "submit")))
            )
           (form
            (input ((name "solution 3")))
            (input ((type "submit")))
            )
           (form
            (input ((name "solution 4")))
            (input ((type "submit")))
            )
           (form
            (input ((name "solution 5")))
            (input ((type "submit")))
            )
           ))))
 
; render-post: post -> xexpr
; Consumes a post, produces an xexpr fragment of the post.
(define (render-problem a-prob)
  `(div ((class "problem"))
        ,(problem-description a-prob)
        (p ,(problem-solution a-prob))))
 
 
; render-posts: blog -> xexpr
; Consumes a blog, produces an xexpr fragment
; of all its posts.
(define (render-problems a-prob)
  `(div ((class "problems"))
        ,@(map render-problem a-prob)))