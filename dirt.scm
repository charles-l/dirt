(use matchable)
(load "asm.scm")

(define word-size 4)

(define (emit-exit)
  '((movw %eax %ebx)
    (movw 1 %eax)
    (int #x80)))

(define (compile prog)
  (define stack (make-parameter '()))
  `((.text
      ,@(map (cut compile-expr <> stack) prog)
      ,@(emit-exit))))

(define (word? a)
  (and (exact? a)))

(define (lookup-si stack v)
  (* word-size (list-index (lambda (e) (eq? v (car e))) (stack))))

(define (lookup-type stack v)
  (cond
    ((assoc v stack) => cadr)
    (else
      (error "variable not defined" v))))

(define (def-var type name)
  `(,name . ,type))

(define s symbol-append)

(define (compile-expr expr stack)
  (match expr
         (((? word? a))
          `(movw ,a %eax))
         (('def name : type)
          (stack (cons (def-var type name) (stack)))
          `(pushw 0))
         (('def name : type val)
          (stack (cons (def-var type name) (stack)))
          `(pushw ,val))
         (('ref var)
          `(movw ,(cons (lookup-si stack var) '%esp) %eax))))
