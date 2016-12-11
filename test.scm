(system "csi -s guppy.scm > test")
(system "objdump -D -b binary -mi386 test")
