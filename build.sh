#!/bin/bash
dd if=/dev/zero of=disk.img bs=1k count=1440 # Create empty floppy image
dd if=loader.bin of=disk.img conv=notrunc # Add loader to floppy
printf "\x55\xAA" | dd of=disk.img bs=1 seek=510 conv=notrunc # Add magic bytes to end of boot sector
dd if=pong.bin of=disk.img bs=512 seek=1 conv=notrunc
#dd if=keybdtest.bin of=disk.img bs=512 seek=1 conv=notrunc
