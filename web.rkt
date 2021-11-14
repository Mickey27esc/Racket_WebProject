#lang web-server/insta
 
; A blog is a (listof post)
; and a post is a (post title body)
(struct problem (description solution))
 
; BLOG: blog
; The static blog.
(define SET
  (list (problem "Descr 1" "solu 1")
        (problem "Descr 2" "solu 2")))
 
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
           ,(render-problems a-set)
           (form
            (input ((name "description")))
            (input ((name "solution")))
            (input ((type "submit"))))))))
 
; render-post: post -> xexpr
; Consumes a post, produces an xexpr fragment of the post.
(define (render-problem a-prob)
  `(div ((class "problem"))
        ,(problem-title a-post)
        (p ,(post-body a-post))))
 
 
; render-posts: blog -> xexpr
; Consumes a blog, produces an xexpr fragment
; of all its posts.
(define (render-posts a-blog)
  `(div ((class "posts"))
        ,@(map render-post a-blog)))