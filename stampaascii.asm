#make_boot#

org 7c00h      ; set location counter.

start:
    ; Initialize the stack
    mov     ax, 07c0h
    mov     ss, ax
    mov     sp, 03feh ; top of the stack.
    
    
    ; Set data segment to 0
    xor     ax, ax
    mov     ds, ax
    
    ; Set default video mode 80x25:
    mov     ah, 00h
    mov     al, 03h
    int     10h

    call prtstr
   
    
    INT 19h        ; reboot
    
                            
    
prtstr proc near
prtstr_start:

    mov al, 41h

prtstr_lp:


    mov ah, 0Eh
    int 10h
    
    inc al
    
    cmp al, 7Eh
    jg prtstr_start
    
    jmp prtstr_lp
    ret
prtstr endp

