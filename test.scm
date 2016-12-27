(load "dirt.scm")
(use utils srfi-13 test)

(define (objdump) (shell "objdump -D -mi386 -b binary test"))

(test
  "6a0368200300008b1989198999fdffffff8999409c000051b80300000089c33d0300000005040000002d409c00003bd80f84e9ffffffe8fbffffff0f85f5ffffff58e90600000081c804000000c1e004c1e8040fafd881e304000000c3"
  (begin
    (emit-binary
      (assemble '((push 3)
                  (push 800)
                  (mov (%ecx) %ebx)
                  (mov %ebx (%ecx))
                  (mov %ebx (-3 . %ecx))
                  (mov %ebx (40000 . %ecx))
                  (push %ecx)
                  (mov 3 %eax)
                  (mov %eax %ebx)
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
                            (mov 1  %eax)
                            (mov 42 %ebx)
                            (int #x80)))) "test")
        (call-with-input-pipe "./test; echo $?" read)))

(test "Hello World!\n"
      (begin
        (emit-binary
          (assemble-elf `((.text
                            (mov 1 %ebx)
                            (mov 4 %eax)
                            (mov msg %ecx)
                            (mov 13 %edx)
                            (int #x80)

                            (mov 1 %eax)
                            (mov #x5D %ebx)
                            (int #x80))
                          (.data
                            (label msg)
                            (db "Hello World!\n")))) "test")
        (call-with-input-pipe "./test" read-all)))

(test "Hello Dudes!\n"
      (begin
        (emit-binary
          (assemble-elf `((.text
                            (mov 1 %ebx)
                            (mov 4 %eax)
                            (mov msg2 %ecx)
                            (mov 13 %edx)
                            (int #x80)

                            (mov 1 %eax)
                            (mov #x5D %ebx)
                            (int #x80))
                          (.data
                            (label some-data)
                            (db #x12 #x13 #x14)
                            (label msg2)
                            (db "Hello Dudes!\n")))) "test")
        (call-with-input-pipe "./test" read-all)))

(test-exit)
