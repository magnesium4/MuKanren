#lang racket

; microkanren
; http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

(define (var c) (vector c)) ; define variable
(define (var? x) (vector? x)) ; checks if it's a variable
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0))) ; check if two variables are equal (the single element in the vector must be equal)

(define (walk u s) ; search for u in s
  (let ((pr (and (var? u) (assf (lambda (v) (var=? u v)) s))))
    (if pr (walk (cdr pr) s) u))) ; if u is not a variable just returns u or 

(define (ext-s x v s) (cons (cons x v) s)) ; adds binding of x->v to s

(define (=== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c)))) ; try to unify using given state
      (if s (unit (list (cons s (cdr s/c)))) mzero)))) ; return a goal that succeeds if the terms unify in state s/c, otherwise return empty stream

(define (unit s/c) (cons s/c mzero)) ; makes singleton stream
(define mzero '()) ; empty stream

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s))) ; walk u and v, using s
    (cond
      [(and (var? u) (var? v) (var=? u v)) s] ; if they're borth variables and equal, return s
      [(var? u) (ext-s u v s)] ; if one is a variable, add a binding from 
      [(var? v) (ext-s v u s)] ; it to the other term, and return s
      [(and (pair? u) (pair? v)) ; if they are both terms,
        (let ((s (unify (car u) (car v) s))) ; try to unify each half of the pair
          (and s (unify (cdr u) (cdr v) s)))] ; recursively, and return s
      [else (and (eq? u v) s)]))) ; if they are the same object, return true otherwise false. QUESTION: why would it ever reach this?

(define (call/fresh f) ; return goal
  (lambda (s/c)
    (let ((c (cdr s/c))) 
      ((f (var c)) (cons (car s/c) (+ c 1)))))) ; create a new variable satisfying satisfying f, given s/c

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c)))) ; states satisfying g1 given s/c + those satisfying g2 given s/c
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2))) ; find states satifying g1 given s/c, from those, find ones that satify g2 given s/c

(define (mplus s1 s2)
  (cond
    [(null? s1) s2] ; union of empty set and s2 is s2
    [(procedure? s1) (lambda () (mplus s2 (s1)))] ; if s1 is wrapped, unwrap it and apply mplus. QUESTION: the order here being swapped is important for trampoline?
    [else (cons (car s1) (mplus (cdr s1) s2))])) ; if it's unwrapped, we can take the first part of it and create the rest of the stream recursively

(define (bind s g)
  (cond
    [(null? s) mzero] ; if a streaam is null, it's doomed.
    [(procedure? s) (lambda () (bind (s) g))] ; if it's wrapped, unwrap it and try again.
    [else (mplus (g (car s)) (bind (cdr s) g))])) ; if it's unwrapped, take the states satisfying g, unioned with everything else in s we find (recursively).

; additional stuff

; Zzz
; conj+
; disj+
; conde
; pull
; take-all
; take n
; reification

(define empty-state (cons '() 0))

(define (call/empty-state g) (g empty-state))

; occurs-check
