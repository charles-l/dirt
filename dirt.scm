(use matchable)
(load "asm.scm")

(define (emit-exit)
  '((movw %eax %ebx)
    (movw 1 %eax)
    (int #x80)))

(define (compile prog)
  (define env (make-parameter '()))
  `((.text
      ,@(map (cut compile-expr <> env) prog)
      ,@(emit-exit))))

(define (word? a)
  (and (exact? a)))

(define (lookup env v)
  (cond
    ((assoc v (env)) => cadr)
    (else (error "no such variable defined" v))))

(define (lookup-si env v)
  (cdr (lookup env v)))

(define (def-var type name si)
  `(,name (,type . ,si)))

(define (compile-expr expr env)
  (match expr
         (((? word? a))
          `(movw ,a %eax))
         (('def name : type)
          (env (cons (def-var type name (length (env))) (env)))
          `(pushw 0))
         (('ref var)
          `(movw ,(cons (- (lookup-si env var)) '%esp) %eax))))
