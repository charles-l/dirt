(use matchable)
(load "asm.scm")

(define word-size 4)

(define (emit-exit)
  '((movw %eax %ebx)
    (movw 1 %eax)
    (int #x80)))

(define (compile-data prog)
  (concatenate (map (lambda (e)
                      `((label ,(cadr e))
                        (db ,(caddr e))))
                    (filter data-expr?
                            prog))))

(define (compile-text prog)
  (define stack (make-parameter '()))
  (concatenate (map (cut compile-expr <> stack)
                    (remove data-expr? prog))))

(define (compile prog)
  `((.data
      ,@(compile-data prog))
    (.text
      ,@(compile-text prog)
      ,@(emit-exit))))

(define (data-expr? e)
  (eq? 'data (car e)))

(define (word? a)
  (and (number? a) (exact? a)))

(define (stack-offset stack v)
  (* word-size (list-index (lambda (e) (eq? v (car e))) (stack))))

(define (lookup-type stack v)
  (cond
    ((assoc v stack) => cadr)
    (else
      (error "unbound variable" v))))

(define (def-var type name)
  `(,name . ,type))

(define (defined? stack n)
  (not (not (assoc n (stack)))))

(define (compile-expr expr stack)
  (match expr
         (('def name : type val)
          (if (defined? stack name) (error "already defined" name))
          (let ((o `(,@(compile-expr val stack)
                      (pushw %eax))))
            (stack (cons (def-var type name) (stack)))
            o))
         (('def name : type) ; alias
          (compile-expr (append expr '(0)) stack))

         (('val var)
          (unless (defined? stack var) (error "unbound variable" var))
          `((movw ,(cons (stack-offset stack var) '%esp) %eax)))

         (('ref var)
          (unless (defined? stack var) (error "unbound variable" var))
          `((movw %esp %eax)
            (addw %eax ,(stack-offset stack var))))

         (('+ o ...)
          ; TODO: assert numeric
          (append-map
            (lambda (a)
              `((pushw %eax) ; TODO: ensure that nothing could push %eax in the next step
                ,@(compile-expr a stack)
                (popw %ebx)
                (addw %ebx %eax))) o))

         (('asm o ...)
          `(,o))

         (((? word? a)) ; literal expression
          `((movw ,a %eax)))

         ((? word? a) ; literal
          `((movw ,a %eax)))
         (=>
           (error "failed to parse expr" expr))))
