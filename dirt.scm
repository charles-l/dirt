;; refs
; http://ref.x86asm.net/
; http://bazaar.launchpad.net/~aghuloum/ikarus/ikarus.dev/annotate/head%3A/scheme/ikarus.intel-assembler.ss
; https://github.com/noelwelsh/assembler

(use srfi-69)

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

(define labels (make-hash-table))

(define (label? a)
  (symbol? a))

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
  (and (number? x)
       (exact? x)
       (<= 0 x 255)))

(define (immu8? x)
  (and (number? x)
       (exact? x)
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

; http://wiki.osdev.org/X86-64_Instruction_Encoding#ModR.2FM_and_SIB_bytes
(define (modr/m reg rm)
  (bitwise-ior
    #b11000000 ; register-direct addressing mode
    (arithmetic-shift reg 3)
    rm))

; 32 bit values are encoded in reverse byte order
; i.e. 4th byte, 3rd byte, 2nd byte, 1st byte
(define (immediate v)
  (list (byte v)
        (byte (arithmetic-shift v -8))
        (byte (arithmetic-shift v -16))
        (byte (arithmetic-shift v -24))))

(define (x86 instr byte-len)
  (case instr
    ((ret)
     (lambda ()
       `(#xC3)))
    ((mov)
     (lambda (r1 r2)
       (cond
         ((and (reg? r1) (reg? r2))
          `(#x89 ,(modr/m (reg-code r1) (reg-code r2))))
         ((and (reg? r2) (imm32? r1))
          `(,(code+reg #xB8 r2)
             ,@(immediate r1)))
         (else
           (error "unknown/unimplemented mov command" (list instr r1 r2))))))
    ((push)
     (lambda (r1)
       (cond
         ((reg? r1)
          `(,(code+reg #x50 r1)))
         ((imm8? r1)
          `(#x6A ,r1))
         ((imm32? r1)
          `(#x68 ,@(immediate r1))))))
    ((pop)
     (lambda (r1)
       `(,(code+reg #x58 r1))))
    ((cmp)
     (lambda (r1 r2)
       (cond
         ((eq? r2 '%eax)
          `(#x3D ,@(immediate r1)))
         ((reg? r2)
          `(#x3B ,(modr/m (reg-code r2) (reg-code r1)))))))
    ((add)
     (lambda (r1 r2)
       (cond
         ((and (eq? r1 '%eax) (imm32? r2))
          `(#x05 ,@(immediate r2))))))
    ((sub)
     (lambda (r1 r2)
       (cond
         ((and (eq? r1 '%eax) (imm32? r2))
          `(#x2D ,@(immediate r2))))))
    ((imul)
     (lambda (r1 r2)
       `(#x0F #xAF ,(modr/m (reg-code r2) (reg-code r1)))))
    ((label)
     (lambda (l)
       (hash-table-set! labels l byte-len)
       '()))
    ((or)
     (lambda (r1 r2)
       (cond
         ((and (reg? r1) (imm32? r2))
          `(#x81 ,(modr/m 1 (reg-code r1)) ,@(immediate r2))))))
    ((and)
     (lambda (r1 r2)
       (cond
         ((and (reg? r1) (imm32? r2))
          `(#x81 ,(modr/m 4 (reg-code r1)) ,@(immediate r2))))))
    ((jmp)
     (lambda (l)
       ; get the relative location to the label
       (let ((j (- (hash-table-ref labels l) byte-len)))
         (cond
           ((immu8? j)
            ; subtract the length of the instr itself (- 2)
            `(#xEB ,(- j 2)))
           ((imm32? j)
            ; subtract the length of the instr itself (- 5)
            `(#xE9 ,@(immediate (- j 5))))))))
    ((je) ; TODO: dedup this code
     (lambda (l)
       ; get the relative location to the label
       (let ((j (- (hash-table-ref labels l) byte-len)))
         (cond
           ((immu8? j)
            ; subtract the length of the instr itself (- 2)
            `(#x74 ,(- j 2)))
           ((imm32? j)
            ; subtract the length of the instr itself (- 6)
            `(#x0F #x84 ,@(immediate (- j 6))))))))
    ((jne) ; TODO: dedup this code
     (lambda (l)
       ; get the relative location to the label
       (let ((j (- (hash-table-ref labels l) byte-len)))
         (cond
           ((immu8? j)
            ; subtract the length of the instr itself (- 2)
            `(#x75 ,(- j 2)))
           ((imm32? j)
            ; subtract the length of the instr itself (- 6)
            `(#x0F #x85 ,@(immediate (- j 6))))))))
    ((call)
     (lambda (l)
       ; TODO: handle pointer
       (cond
         ((label? l)
          `(#xE8 ,@(immediate (- (hash-table-ref labels l) byte-len 5))))
         (else
           (error "can't call" l)))))
    ((shl)
     (lambda (r1 r2)
       `(#xC1 ,(modr/m 4 (reg-code r1)) ,r2)))
    ((shr)
     (lambda (r1 r2)
       `(#xC1 ,(modr/m 5 (reg-code r1)) ,r2)))
    (else
      (error "unknown x86 instruction " instr))))

(define (assemble asm)
  (let ((byte-len 0))
    (map
     (lambda (l)
       (let ((o (apply (x86 (car l) byte-len) (cdr l))))
         (set! byte-len (+ byte-len (length o)))
         o))
     asm)))

;; test

(map emit-byte
     (apply
       append
       (map (lambda (x) (if (list? x) x (list x)))
            (assemble '((push 3)
                        (push 800)
                        (push %ecx)
                        (mov 3 %eax)
                        (mov %eax %ebx)
                        (label TEST)
                        (cmp 3 %eax)
                        (add %eax 4)
                        (label FOO)
                        (sub %eax 40000)
                        (cmp %eax %ebx)
                        (je TEST)
                        (jne FOO)
                        (call TEST)
                        (pop %eax)
                        (jmp TEST)
                        (or %eax 4)
                        (shl %eax 4)
                        (shr %eax 4)
                        (imul %eax %ebx)
                        (and %ebx 4)
                        (ret))))))
