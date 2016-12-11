;; refs
; http://ref.x86asm.net/
; http://bazaar.launchpad.net/~aghuloum/ikarus/ikarus.dev/annotate/head%3A/scheme/ikarus.intel-assembler.ss
; https://github.com/noelwelsh/assembler

(define regs
  ;  name  width  code
  '((%eax  32     0)
    (%ecx  32     1)
    (%edx  32     2)
    (%ebx  32     3)
    (%esp  32     4)
    (%ebp  32     5)
    (%esi  32     6)
    (%edi  32     7)))

(define (reg? r)
  (cond ((assoc r regs) #t)
        (else #f)))

(define (reg-code reg)
  (cond ((assoc reg regs) => caddr)
        (else
          (error "not a register" reg))))

;; immediates
(define (imm32? x)
  (exact? x))

(define (imm8? x)
  (and (exact? x)
       (<= 0 x 255)))

(define (immu8? x)
  (and (exact? x)
       (<= -128 x 127)))

(define (emit-byte byte)
  (write-byte byte (current-output-port)))

(define (code+reg code reg)
  (bitwise-ior code (reg-code reg)))

(define (code op ar)
  (list op ar))

; get last byte of some value
(define (byte val)
  (bitwise-and #xFF val))

; uh... figure out what this is doing (yay for copypasta)
(define (modr/m r1 r2)
  (bitwise-ior
    #b11000000
    (arithmetic-shift (reg-code r1) 3)
    (reg-code r2)))

(define (immediate v)
  (list (byte v)
        (byte (arithmetic-shift v -8))
        (byte (arithmetic-shift v -16))
        (byte (arithmetic-shift v -24))))

(define (x86 instr)
  (case instr
    ((ret)
     (lambda ()
       #xC3))
    ((mov)
     (lambda (r1 r2)
       (cond
         ((reg? r1)
          (list #x89 (modr/m r1 r2)))
         (else
           `(,(code+reg #xB8 r2)
              ,@(immediate r1))))))
    ((push)
     (lambda (r1)
       (cond
         ((reg? r1)
          (code+reg #x50 r1))
         ((imm8? r1)
          (list #x6A r1)))))
    ((pop)
     (lambda (r1)
       (code+reg #x58 r1)))
    ((cmp)
     (lambda (r1 r2)
       (cond
         ((eq? r2 '%eax)
           `(#x3D ,@(immediate r1)))
         ;((reg? r2)
         ; `(#x81 (reg-code r2) ,@(immediate r1)))
         )))
    (else
      (error "unknown x86 instruction " instr))))

(define (assemble asm)
  (map
    (lambda (l)
      (apply (x86 (car l)) (cdr l)))
    asm))

;; test

(map emit-byte
     (apply
       append
       (map (lambda (x) (if (list? x) x (list x)))
            (assemble '((push 3)
                        (mov 3 %eax)
                        (mov %eax %ebx)
                        (cmp 3 %eax)
                        (pop %eax)
                        (ret))))))
