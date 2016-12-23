;; refs
; x86 encoding:
; http://ref.x86asm.net/
; http://bazaar.launchpad.net/~aghuloum/ikarus/ikarus.dev/annotate/head%3A/scheme/ikarus.intel-assembler.ss
; https://github.com/noelwelsh/assembler
; elf:
; http://wiki.osdev.org/ELF#Header
; http://wiki.osdev.org/ELF_Tutorial
; https://web.archive.org/web/20140130143820/http://robinhoksbergen.com/papers/howto_elf.html
; http://www.muppetlabs.com/~breadbox/software/tiny/teensy.html
; `man elf`

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

(define (reg? r)
  (cond ((and (symbol? r) (assoc r regs)) #t)
        (else #f)))

(define (label? l)
  (and (not (reg? l)) (symbol? l)))

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
(define (modr/m reg rm #!optional (mod #b11))
  (bitwise-ior
    (arithmetic-shift mod 6) ; default to register-direct addressing mode
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

(define (x86 expr addr)
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
         (('mov (? label? l) (? reg? r))
          (cons 5 (delay `(,(code+reg #xB8 r) ,@(u32 (hash-table-ref labels l))))))

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
            (hash-table-set! labels l addr))
          '())

         (('or (? reg? r) (? imm32? i))
          `(#x81 ,(modr/m #b001 (reg-code r)) ,@(u32 i)))

         (('and (? reg? r) (? imm32? i))
          `(#x81 ,(modr/m #b100 (reg-code r)) ,@(u32 i)))

         (('jmp (? symbol? l))
          ; TODO: find a different way of calculating the size of the program based on u16 and u32
          (cons 5 (delay `(#xE9 ,@(u32 (- (hash-table-ref labels l) addr 5))))))
         (('je (? symbol? l))
          (cons 6 (delay `(#x0F #x84 ,@(u32 (- (hash-table-ref labels l) addr 6))))))
         (('jne (? symbol? l))
          (cons 6 (delay `(#x0F #x85 ,@(u32 (- (hash-table-ref labels l) addr 6))))))

         (('call (? symbol? l))
          ; TODO: handle pointer
          (cons 5 (delay `(#xE8 ,@(u32 (- (hash-table-ref labels l) addr 5))))))

         (('shl (? reg? r1) (? imm8? i))
          `(#xC1 ,(modr/m #b100 (reg-code r1)) ,i))

         (('shr (? reg? r1) (? imm8? r2))
          `(#xC1 ,(modr/m #b101 (reg-code r1)) ,r2))

         (('int (? imm8? i))
          `(#xCD ,i))

         (('db (? imm8? i) ...)
          i)

         (=>
           (error "failed to parse expression" expr))))

(define (assemble asm #!optional (start-addr 0))
  (let ((addr start-addr))
    (map ; second pass
      (lambda (l)
        (cond
          ((promise? l) (force l)) ; fill in label addresses
          (else l)))
      (map ; first pass
       (lambda (expr)
         (let ((o (x86 expr addr)))
           (cond
             ((not (list? o))
              (set! addr (+ addr (car o)))
              (cdr o))
             (else
               (set! addr (+ addr (length o)))
               o))))
       asm))))

;;; ELF

(define (get-section section asm)
  (cond ((assoc section asm) => cdr)
        (else '())))

(define (assemble-elf asm)
  (define ehdr-size 52) ; elf header size
  (define phdr-size 32) ; progam header size
  (define phdr-n) ; number of program headers
  (define entry-point)
  ; ELF header
  (let* ((phdr-n (count (lambda (n) (not (null? n))) (map (cut get-section <> asm) '(.data .text))))
         (entry-point (+ #x8048000 (+ ehdr-size (* phdr-n phdr-size))))
         (data-addr #x08048096)

         (data (assemble (get-section '.data asm) data-addr))
         (text (assemble (get-section '.text asm) entry-point))

         (text-size (apply + (map length text)))
         (data-size (apply + (map length data)))


         (elf-header (list
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
                       (u32 entry-point) ; default to this

                       ;; program header table offset (32 bit)
                       ; located directly after the elf header
                       (u32 ehdr-size)

                       ;; section header table offset (32 bit)
                       (u32 0)

                       ;; flags (none for x86)
                       (u32 0)

                       ;; elf header size (bytes)
                       (u16 ehdr-size)

                       ;; size of header table entry (bytes)
                       (u16 phdr-size)

                       ;; number of entries in the header table
                       (u16 phdr-n)

                       ;; section header size in bytes
                       (u16 0)

                       ;; number of entries in the section header table
                       (u16 0)

                       ;; section header table index of entry associated with the section name string table
                       (u16 0)))

         (program-header-table
           (append
             (make-program-header entry-point
                                  (+ ehdr-size (* phdr-n phdr-size))
                                  text-size
                                  (bitwise-ior 1 4)) ; text
             (if (null? data)
               '()
               (make-program-header data-addr
                                    (+ ehdr-size (* phdr-n phdr-size) text-size)
                                    data-size
                                    4)))))
    (append elf-header
            program-header-table
            text
            data)))

(define (make-program-header virtual-addr offset sz flags)
  (list
    (u32 1) ; type (1 = load)
    (u32 offset)
    (u32 virtual-addr)
    (u32 0) ; physical addr (unused)
    (u32 sz) ; size in file
    (u32 sz) ; size in memory (dunno why these'd be different)
    (u32 flags) ; 1 = executible 2 = writeable 4 = readable
    (u32 #x1000)))

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
