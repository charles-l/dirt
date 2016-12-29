(load "dirt.scm")
(use utils srfi-13 test)

(define (objdump) (system "objdump -D -mi386 -b binary test"))

(test
  "6a0368200300008b198b24248b0c248b6d008b450089198999fdffffff8999409c000051b80300000089c33d0300000005040000002d409c00003bd80f84e9ffffffe8fbffffff0f85f5ffffff58e90600000081c804000000c1e004c1e8040fafd881e304000000c3"
  (begin
    (emit-binary
      (assemble '((pushb 3)
                  (pushw 800)
                  (movw (0 . %ecx) %ebx)
                  (movw (0 . %esp) %esp)
                  (movw (0 . %esp) %ecx)
                  (movw (0 . %ebp) %ebp)
                  (movw (0 . %ebp) %eax)
                  (movw %ebx (0 . %ecx))
                  (movw %ebx (-3 . %ecx))
                  (movw %ebx (40000 . %ecx))
                  (pushw %ecx)
                  (movw 3 %eax)
                  (movw %eax %ebx)
                  (label TEST)
                  (cmp %eax 3)
                  (add %eax 4)
                  (sub %eax 40000)
                  (cmp %eax %ebx)
                  (je TEST)
                  (label FOO)
                  (call FOO)
                  (jne FOO)
                  (pop %eax)
                  (jmp BLAH)
                  (or %eax 4)
                  (label BLAH)
                  (shl %eax 4)
                  (shr %eax 4)
                  (imul %eax %ebx)
                  (and %ebx 4)
                  (ret))) "test")
    (string-delete #\newline (call-with-input-pipe "xxd -p test" read-all))))

(test 42
      (begin
        (emit-binary
          (assemble-elf '((.text
                            (movw 1  %eax)
                            (movw 42 %ebx)
                            (int #x80)))) "test")
        (call-with-input-pipe "./test; echo $?" read)))

(test "Hello World!\n"
      (begin
        (emit-binary
          (assemble-elf `((.text
                            (movw 1 %ebx)
                            (movw 4 %eax)
                            (movw msg %ecx)
                            (movw 13 %edx)
                            (int #x80)

                            (movw 1 %eax)
                            (movw #x5D %ebx)
                            (int #x80))
                          (.data
                            (label msg)
                            (db "Hello World!\n")))) "test")
        (call-with-input-pipe "./test" read-all)))

(test "Hello Dudes!\n"
      (begin
        (emit-binary
          (assemble-elf `((.text
                            (movw 1 %ebx)
                            (movw 4 %eax)
                            (movw msg2 %ecx)
                            (movw 13 %edx)
                            (int #x80)

                            (movw 1 %eax)
                            (movw #x5D %ebx)
                            (int #x80))
                          (.data
                            (label some-data)
                            (db #x12 #x13 #x14)
                            (label msg2)
                            (db "Hello Dudes!\n")))) "test")
        (call-with-input-pipe "./test" read-all)))

(test 42
      (begin
        (emit-binary (assemble-elf (compile '((42)))) "test")
        (call-with-input-pipe "./test; echo $?" read)))

(test 0
      (begin
        (emit-binary (assemble-elf (compile '((def a : word)
                                              (ref a)))) "test")
        (call-with-input-pipe "./test; echo $?" read)))

(test-exit)
