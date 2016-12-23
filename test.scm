(load "dirt.scm")
(use utils srfi-13 test shell)
(test
  "6a0368200300008b1989198999fdffffff8999409c000051b80300000089c33d0300000005040000002d409c00003bd80f84e9ffffff0f85faffffffe8deffffff58e90600000081c804000000c1e004c1e8040fafd881e304000000c3"
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
    (run (cp test tmp))
    (string-delete #\newline (capture "xxd -p tmp"))))

(test 42
      (begin
        (emit-binary
          (assemble-elf '((.text
                            (mov 1  %eax)
                            (mov 42 %ebx)
                            (int #x80)))) "test")
        (run (cp test tmp))
        (with-input-from-string (capture "./tmp; echo $?") (lambda () (read)))))

(test "Hello World!\n"
      (begin
        (emit-binary
          (assemble-elf `((.text
                            (mov 1 %ebx)
                            (mov 4 %eax)
                            (mov #x08048096 %ecx)
                            (mov 13 %edx)
                            (int #x80)

                            (mov 1 %eax)
                            (mov #x5D %ebx)
                            (int #x80))
                          (.data
                            (db ,@(map char->integer '(#\H #\e #\l #\l #\o #\space #\W #\o #\r #\l #\d #\! #\newline)))))) "test")
        (run (cp test tmp))
        (with-input-from-string (capture "./tmp") (lambda () (read-all)))))

(run (rm tmp)) ; cleanup
(test-exit)
