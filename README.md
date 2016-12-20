# dirt

## usage

    (load "dirt.scm")
    (emit-binary
        '((mov 4 %eax)
          (ret)) "test-file.bin")
