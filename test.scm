(load "dirt.scm")
(use posix utils srfi-13 test)
(test
  "6a0368200300008b1989198999fdffffff8999409c000051b80300000089c33d0300000005040000002d409c00003bd80f84e9ffffff0f85faffffffe8ddffffff58e90600000081c804000000c1e004c1e8040fafd881e304000000c3"
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
                  (jne FOO)
                  (call TEST)
                  (pop %eax)
                  (jmp BLAH)
                  (or %eax 4)
                  (label BLAH)
                  (shl %eax 4)
                  (shr %eax 4)
                  (imul %eax %ebx)
                  (and %ebx 4)
                  (ret))) "test")
    ;(system "objdump -D -b binary -mi386 test")
    (system "") ; uhhh. WUT? Without this, call-with-input-pipe will run before test file has been written???
    ; this *might* break the 80 character limit...
    (string-delete #\newline (call-with-input-pipe "xxd -p test" read-all))))

(emit-binary
   (assemble-elf '((.text
                     (mov 5 %eax)
                     (ret)))) "test")

(print
   (assemble-elf '((.text
                     (mov 5 %eax)
                     (ret)))) "test")
(test-exit)
