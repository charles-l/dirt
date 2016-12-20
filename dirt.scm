;; refs
; http://ref.x86asm.net/
; http://bazaar.launchpad.net/~aghuloum/ikarus/ikarus.dev/annotate/head%3A/scheme/ikarus.intel-assembler.ss
; https://github.com/noelwelsh/assembler

(use srfi-69 matchable)

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

; allows forward reference of label during first pass
(define (label-addr labels l byte-len size post-thunk)
  (cons
    size
    (lambda ()
      (post-thunk (- (hash-table-ref labels l) byte-len size)))))

(define (reg? r)
  (cond ((and (symbol? r) (assoc r regs)) #t)
        (else #f)))

(define (mem? t)
  (pair? t))

(define (reg-code reg)
  (cond ((assoc reg regs) => caddr)
        (else
          (error "not a register" reg))))

;; immediates
(define (imm32? x)
  (and (number? x)
       (exact? x)))

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
(define (modr/m reg rm #!optional mod)
  (bitwise-ior
    (arithmetic-shift (or mod #b11) 6) ; default to register-direct addressing mode
    (arithmetic-shift reg 3)
    rm))

; 32 bit values are encoded in reverse byte order
; i.e. 4th byte, 3rd byte, 2nd byte, 1st byte
(define (immediate v)
  (list (byte v)
        (byte (arithmetic-shift v -8))
        (byte (arithmetic-shift v -16))
        (byte (arithmetic-shift v -24))))

(define (mem-access r m)
  (cond
    ((or (null? (cdr m)) (zero? (car m)))
     `(,(modr/m (reg-code r) (reg-code (car m)) #b00)))
    ((imm8? (car m))
     `(,(modr/m (reg-code r) (reg-code (cdr m)) #b01) ,(car m)))
    ((imm32? (car m))
     `(,(modr/m (reg-code r) (reg-code (cdr m)) #b10) ,@(immediate (car m))))))

(define (x86 expr byte-len)
  (match expr
         (('ret)
          `(#xC3))

         (('mov (? reg? r1) (? reg? r2))
          `(#x89 ,(modr/m (reg-code r1) (reg-code r2))))
         (('mov (? reg? r) (? mem? m))
          `(#x89 ,@(mem-access r m)))
         (('mov (? mem? m) (? reg? r))
          `(#x8B ,@(mem-access r m)))
         (('mov (? imm32? i) (? reg? r))
          `(,(code+reg #xB8 r) ,@(immediate i)))

         (('push (? reg? r))
          `(,(code+reg #x50 r)))
         (('push (? imm8? i))
          `(#x6A ,i))
         (('push (? imm32? i))
          `(#x68 ,@(immediate i)))

         (('pop (? reg? r))
          `(,(code+reg #x58 r)))

         (('cmp '%eax (? imm32? i))
          `(#x3D ,@(immediate i)))
         (('cmp (? reg? r1) (? reg? r2))
          `(#x3B ,(modr/m (reg-code r2) (reg-code r1))))

         (('add '%eax (? imm32? i))
          `(#x05 ,@(immediate i)))

         (('sub '%eax (? imm32? i))
          `(#x2D ,@(immediate i)))

         (('imul (? reg? r1) (? reg? r2))
          `(#x0F #xAF ,(modr/m (reg-code r2) (reg-code r1))))

         (('label (? symbol? l))
          (if (hash-table-ref/default labels l #f)
            (error "label already defined" l)
            (hash-table-set! labels l byte-len))
          '())

         (('or (? reg? r) (? imm32? i))
          `(#x81 ,(modr/m #b001 (reg-code r)) ,@(immediate i)))

         (('and (? reg? r) (? imm32? i))
          `(#x81 ,(modr/m #b100 (reg-code r)) ,@(immediate i)))

         (('jmp (? symbol? l))
          (label-addr labels l byte-len 5
                      (lambda (addr)
                        `(#xE9 ,@(immediate addr)))))

         (('je (? symbol? l))
          (label-addr labels l byte-len 6
                      (lambda (addr)
                        `(#x0F #x84 ,@(immediate addr)))))

         (('jne (? symbol? l))
          (label-addr labels l byte-len 6
                      (lambda (addr)
                        `(#x0F #x85 ,@(immediate addr)))))

         (('call (? symbol? l))
          ; TODO: handle pointer
          (label-addr labels l byte-len 6
                      (lambda (addr)
                        `(#xE8 ,@(immediate addr)))))

         (('shl (? reg? r1) (? imm8? i))
          `(#xC1 ,(modr/m #b100 (reg-code r1)) ,i))

         (('shr (? reg? r1) (? imm8? r2))
          `(#xC1 ,(modr/m #b101 (reg-code r1)) ,r2))

         (=>
           (error "failed to parse expression" expr))))

(define (assemble asm)
  (let ((byte-len 0))
    (map ; second pass
      (lambda (l)
        (cond
          ((procedure? l) (l)) ; fill in label addresses
          (else l)))
      (map ; first pass
       (lambda (expr)
         (let ((o (x86 expr byte-len)))
           (cond
             ((not (list? o))
              (set! byte-len (+ byte-len (car o)))
              (cdr o))
             (else
               (set! byte-len (+ byte-len (length o)))
               o))))
       asm))))

(define (emit-binary l #!optional filename)
  (with-output-to-port
    (if filename
      (open-output-file filename)
      (current-output-port))
    (lambda ()
      (map emit-byte
           (apply
             append
             (assemble l))))))
