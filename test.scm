(system "csi -s dirt.scm > test")
(system "objdump -D -b binary -mi386 test")
