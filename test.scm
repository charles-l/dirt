(load "dirt.scm")
(use utils srfi-13 test)

(define outfile "test")
(define (objdump) (system (conc "objdump -D -mi386 -b binary " outfile)))

(define (run-test)
  (call-with-input-pipe (conc "./" outfile) read-all))

(define (run-test-get-status)
  (call-with-input-pipe (conc "./" outfile "; echo $?") read))

(test-group "assembler"
            (test
              "6a0368200300008b198b24248b0c248b6d008b45008b44240489198999fdffffff8999409c000051b80300000089c33d0300000005040000002d409c00003bd80f84e9ffffffe8fbffffff0f85f5ffffff58e90600000081c804000000c1e004c1e8040fafd881e304000000c3"
              (begin
                (emit-binary
                  (list (assemble '((pushb 3) ; TODO: organize these better
                                    (pushw 800)
                                    (movw (0 . %ecx) %ebx)
                                    (movw (0 . %esp) %esp)
                                    (movw (0 . %esp) %ecx)
                                    (movw (0 . %ebp) %ebp)
                                    (movw (0 . %ebp) %eax)
                                    (movw (4 . %esp) %eax)
                                    (movw %ebx (0 . %ecx))
                                    (movw %ebx (-3 . %ecx))
                                    (movw %ebx (40000 . %ecx))
                                    (pushw %ecx)
                                    (movw 3 %eax)
                                    (movw %eax %ebx)
                                    (label TEST)
                                    (cmp %eax 3)
                                    (addw %eax 4)
                                    (sub %eax 40000)
                                    (cmp %eax %ebx)
                                    (je TEST)
                                    (label FOO)
                                    (call FOO)
                                    (jne FOO)
                                    (popw %eax)
                                    (jmp BLAH)
                                    (or %eax 4)
                                    (label BLAH)
                                    (shl %eax 4)
                                    (shr %eax 4)
                                    (imul %eax %ebx)
                                    (and %ebx 4)
                                    (ret)))) outfile)
                (string-delete #\newline (call-with-input-pipe "xxd -p test" read-all))))

            (test
              "03d8c3"
              (begin
                (emit-binary
                  (list (assemble '((addw %eax %ebx)
                                    (ret)))) outfile)
                (string-delete #\newline (call-with-input-pipe "xxd -p test" read-all))))

            (test 42
                  (begin
                    (emit-binary
                      (assemble-elf '((.text
                                        (movw 1  %eax)
                                        (movw 42 %ebx)
                                        (int #x80)))) outfile)
                    (run-test-get-status)))

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
                                        (db "Hello World!\n")))) outfile)
                    (run-test)))

            (test "Hello Dudes!\n"
                  (begin
                    (emit-binary
                      (assemble-elf '((.text
                                        (movw 1 %ebx)
                                        (movw 4 %eax)
                                        (movw msg %ecx)
                                        (movw 13 %edx)
                                        (int #x80)

                                        (movw 1 %eax)
                                        (movw #x5D %ebx)
                                        (int #x80))
                                      (.data
                                        (label some-data)
                                        (db #x12 #x13 #x14)
                                        (label msg)
                                        (db "Hello Dudes!\n")))) outfile)
                    (run-test))))

(test-group "ir"
            (test 42
                  (begin
                    (emit-binary (assemble-elf (compile '((42)))) outfile)
                    (run-test-get-status)))

            (test 0
                  (begin
                    (emit-binary (assemble-elf (compile '((def a : w)
                                                          (val a)))) outfile)
                    (run-test-get-status)))

            (test 3
                  (begin
                    (emit-binary (assemble-elf (compile '((def a : w 3)
                                                          (def c : b 27)
                                                          (def d : w 800)
                                                          (val a)))) outfile)
                    (run-test-get-status)))

            (test 80
                  (begin
                    (emit-binary (assemble-elf (compile '((def a : w 3)
                                                          (def c : b 27)
                                                          (def d : w 80)
                                                          (val d)))) outfile)
                    (run-test-get-status)))

            (test "A"
                  (begin
                    (emit-binary (assemble-elf (compile '((def char : b 65)
                                                          (ref char)
                                                          (asm movw %eax %ecx)
                                                          (asm movw 1 %ebx)
                                                          (asm movw 4 %eax)
                                                          (asm movw 1 %edx)
                                                          (asm int #x80)))) outfile)
                    (run-test)))

            (test "heyo world!"
                  (begin
                    (emit-binary (assemble-elf (compile '((data msg "heyo world!")

                                                          (asm movw msg %ecx)
                                                          (asm movw 1 %ebx)
                                                          (asm movw 4 %eax)
                                                          (asm movw 11 %edx)
                                                          (asm int #x80)))) outfile)
                    (run-test)))
            (test 20
                  (begin
                    (emit-binary (assemble-elf (compile '((+ 5 10 5)))) outfile)
                    (run-test-get-status)))
            (test 5
                  (begin
                    (emit-binary (assemble-elf (compile '((+ -5 15 -5)))) outfile)
                    (run-test-get-status))))

(test-exit)
