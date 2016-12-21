;; refs
; http://ref.x86asm.net/
; http://bazaar.launchpad.net/~aghuloum/ikarus/ikarus.dev/annotate/head%3A/scheme/ikarus.intel-assembler.ss
; https://github.com/noelwelsh/assembler

(use srfi-69 srfi-1)

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

(define arches
  ; arch    bits  elf-arch-id
  '((i386   32    #x03)
    (ARM    32    #x28)
    (x86-64 64    #x3E)
    (ARM64  64    #xB7)))

(define arch 'i386)

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

;; u32s
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

; little endian 32 bit value
; i.e. 4th byte, 3rd byte, 2nd byte, 1st byte
(define (u32 v)
  (list (byte v)
        (byte (arithmetic-shift v -8))
        (byte (arithmetic-shift v -16))
        (byte (arithmetic-shift v -24))))

(define (u16 v)
  (list (byte v)
        (byte (arithmetic-shift v -8))))

(define (mem-access r m)
  (cond
    ((or (null? (cdr m)) (zero? (car m)))
     `(,(modr/m (reg-code r) (reg-code (car m)) #b00)))
    ((imm8? (car m))
     `(,(modr/m (reg-code r) (reg-code (cdr m)) #b01) ,(car m)))
    ((imm32? (car m))
     `(,(modr/m (reg-code r) (reg-code (cdr m)) #b10) ,@(u32 (car m))))))

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
          `(,(code+reg #xB8 r) ,@(u32 i)))

         (('push (? reg? r))
          `(,(code+reg #x50 r)))
         (('push (? imm8? i))
          `(#x6A ,i))
         (('push (? imm32? i))
          `(#x68 ,@(u32 i)))

         (('pop (? reg? r))
          `(,(code+reg #x58 r)))

         (('cmp '%eax (? imm32? i))
          `(#x3D ,@(u32 i)))
         (('cmp (? reg? r1) (? reg? r2))
          `(#x3B ,(modr/m (reg-code r2) (reg-code r1))))

         (('add '%eax (? imm32? i))
          `(#x05 ,@(u32 i)))

         (('sub '%eax (? imm32? i))
          `(#x2D ,@(u32 i)))

         (('imul (? reg? r1) (? reg? r2))
          `(#x0F #xAF ,(modr/m (reg-code r2) (reg-code r1))))

         (('label (? symbol? l))
          (if (hash-table-ref/default labels l #f)
            (error "label already defined" l)
            (hash-table-set! labels l byte-len))
          '())

         (('or (? reg? r) (? imm32? i))
          `(#x81 ,(modr/m #b001 (reg-code r)) ,@(u32 i)))

         (('and (? reg? r) (? imm32? i))
          `(#x81 ,(modr/m #b100 (reg-code r)) ,@(u32 i)))

         (('jmp (? symbol? l))
          (label-addr labels l byte-len 5
                      (lambda (addr)
                        `(#xE9 ,@(u32 addr)))))

         (('je (? symbol? l))
          (label-addr labels l byte-len 6
                      (lambda (addr)
                        `(#x0F #x84 ,@(u32 addr)))))

         (('jne (? symbol? l))
          (label-addr labels l byte-len 6
                      (lambda (addr)
                        `(#x0F #x85 ,@(u32 addr)))))

         (('call (? symbol? l))
          ; TODO: handle pointer
          (label-addr labels l byte-len 6
                      (lambda (addr)
                        `(#xE8 ,@(u32 addr)))))

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

;;; ELF

; see `man elf` for more info
(define (assemble-elf asm)
  ; ELF header
  (define elf-header-size 52) ; size of elf header in bytes
  (let ((elf-header (list
                      ;; ident
                      `(#x7F
                        ,(char->integer #\E)
                        ,(char->integer #\L)
                        ,(char->integer #\F))
                      `(,(case (cadr (assoc arch arches))
                           ((32) 1)
                           ((64) 2)))
                      '(1) ; little endian
                      '(1) ; version
                      '(0) ; abi (0 = sysv)
                      (make-list 8 0) ; then 8 bytes of padding

                      ;; type
                      (u16 2) ; 2 = static executable (3 for shared)

                      ;; machine
                      (u16 (caddr (assoc arch arches)))

                      ;; version
                      (u32 1)

                      ;; entry addr (32 bit)
                      (u32 #x8048060) ; default to this

                      ;; program header table offset (32 bit)
                      (u32 52)

                      ;; section header table offset (32 bit)
                      (u32 0)

                      ;; flags (none for x86)
                      (u32 0)

                      ;; header table size in bytes
                      (u16 52)

                      ;; size of header table entry
                      (u16 32)

                      ;; number of entries in the header table
                      (u16 1)

                      ;; section header size in bytes
                      (u16 0)

                      ;; number of entries in the section header table
                      (u16 0)

                      ;; section header table index of entry associated with the section name string table
                      '(0 0)))
        (program-header-table (list
                                (u32 1) ; type
                                (u32 0) ; offset
                                (u32 #x0848000) ; virtual addr
                                (u32 0) ; padding
                                (u32 102) ; size in file
                                (u32 102) ; size in memory
                                (u32 (bitwise-ior 1 4)) ; 1 = executible 4 = readable
                                (u32 #x1000)

                                ;; offset
                                '()))
        (text-section (assemble (cdr (assoc '.text asm)))))
    (append elf-header program-header-table text-section)))

(define (emit-binary l #!optional filename)
  (with-output-to-port
    (if filename
      (open-output-file filename)
      (current-output-port))
    (lambda ()
      (map emit-byte
           (apply
             append
             l)))))
