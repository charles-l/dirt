# dirt

## usage

Make a flat x86 binary file:

```scheme
(load "asm.scm")

(emit-binary
  (list (assemble '((mov 4 %eax)
                    (ret)))) "test-file.bin")
```

Generate an ELF executable:

```scheme
(load "asm.scm")

(emit-binary
  (list (assemble-elf
          '((.text
             (movw 1  %eax)
             (movw 42 %ebx)
             (int #x80))))) "the-answer-to-everything")
```

You can check the status code with `echo $?`

## file structure

* `dirt.scm` - contains IR (which can/will be used for compiler development)
* `asm.scm` - low level ASM related code (elf/flat binary assembler)
* `test.scm` - tests
