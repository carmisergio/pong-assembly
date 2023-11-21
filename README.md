# Bootable PONG! in X86 assembly language

## Build instructions

### Assemble loader and pong

First, build `simplebootloader.asm` using emu8086
Place the resulting output file `output.bin` in the same folder as the source

Build `pong.asm` to `pong.bin` in the same folder

### Create bootable floppy image

The  previous step will have created the binary files `pong.bin` and `loader.bin`, containing the executable machine code respectively of the main program and the MBR boot loader
We now have to pack them into a bootable floppy image. The script `build.sh` will accomplsih precisely this task using DD

The script will produce a bootable floppy disk image named `disk.bin`

## Booting

### On a VM

Currently tested:

##### Oracle VM Virtualbox
Not working at the moment due to its wierd implementation of the VGA spec

##### VMWare Workstation Player
Working perfectly booting from floppy image

##### DosBox
Not really VM but working via command
```bat
boot disk.img
```

### On real hardware

##### USB boot

Flash the floppy image directly to a USB stick 

```bash
dd if=disk.img of=/dev/sdX bs=512
```

##### CD-ROM boot

First, create a bootable CD iso from the floppy image

```bash
mkisofs -pad -b disk.img -R -o cd.iso disk.img
```

Then, burn the iso to a CD

```bash
cdrecord -v -sao dev=/dev/sr0 cd.iso
```

##### Floppy Disk boot
If you are the real deal you may want to boot from the second best storage format there is (after magnetic tape): floppy disks.

To do this, you will have to find a way to get the image written directly to a disk. 
I personally have trasnfered `disk.img` to a windows98 host and written it to a floppy using [RawWrite for Windows](http://www.chrysocome.net/rawwrite)



