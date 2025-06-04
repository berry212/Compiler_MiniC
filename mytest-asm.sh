./build/minic -S -A -o ./tests/out/mytest.s ./tests/mytest.c
arm-linux-gnueabihf-gcc -static -g -o ./tests/out/mytest ./tests/out/mytest.s
qemu-arm-static ./tests/out/mytest