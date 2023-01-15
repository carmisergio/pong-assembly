;-----------------------------------------------------------------------------------------------------------------------
; Pong
; Two player PONG! game                                                                           
; Author: Sergio Carmine 3CITI <mesergiocarmi.net>                                                       
; Date: 14/01/2023                                                                                                     
;-----------------------------------------------------------------------------------------------------------------------

; Directive to create bin file:
#make_bin#

; Emulator directives to simulate memory and CPU state after the bootloader has handed control
#load_segment=0800#
#load_offset=0000#
#al=0b#
#ah=00#
#bh=00#
#bl=00#
#ch=00#
#cl=02#
#dh=00#
#dl=00#
#ds=0800#
#es=0800#
#si=7c02#
#di=0000#
#bp=0000#
#cs=0800#
#ip=0000#
#ss=07c0#
#sp=03fe#

; Kernel is loaded at 0800:0000 by micro-os_loader
org 0000h

; Skip the data and function delaration section:
jmp start 

;------------------------------------------------------------------------------------------------------------
; Data section
;
    kbdbuf      db 128 dup (0)

    msg1 db "Press and hold ESC", 13, 10, 00h
    msg2 db "ESC pressed, release ESC", 13, 10, 00h
    msg3 db "ESC released", 13, 10, 00h
;
;------------------------------------------------------------------------------------------------------------ 

;------------------------------------------------------------------------------------------------------------
; Code section
; 

start:


    push cs
    pop ds
    
    ; Set extra segment to 0000h (IVT is at 0000h:0000h)
    xor     ax, ax
    mov     es, ax

    cli                             ; Momentarely disable interrupts
    
    ; Save previous addresses for keybord interrupt (IRQ9) handler
    ;push es:[9*4+2]                ; Every entry in the Interrupt Vector Table is 4 bytes             
    ;push es:[9*4]
    
    ; Update address with address to own interrupt handler
    mov es:[9*4], offset keybdisr   
    mov es:[9*4+2],cs
    sti                             ; Reenable interrupts
    
    ; Restore default interrupts address
    ;cli                 
    ;pop     word ptr es:[9*4]      
    ;pop     word ptr es:[9*4+2]
    ;sti
    
    jmp start

test0:
    lea si, msg1
    call prtstr                ; print "Press and hold ESC"

test1:
    mov     al, [kbdbuf + 1]    ; check Escape key state (Esc scan code = 1)
    or      al, al
    jz      test1               ; wait until it's nonzero (pressed/held)

    lea si, msg2
    call prtstr                 ; print "ESC pressed, release ESC"

test2:
    mov     al, [kbdbuf + 1]    ; check Escape key state (Esc scan code = 1)
    or      al, al
    jnz     test2               ; wait until it's zero (released/not pressed)

    lea si, msg3
    call prtstr

    ret

keybdisr:
    push ax
    push bx
    push cx
    push dx

    ; Read keyboard scan code
    in al, 60h

    ; Update keyboard state
    xor bh, bh
    mov bl, al
    and bl, 7Fh                 ; bx = scan code
    shr al, 7                   ; al = 0 if pressed, 1 if released
    xor al, 1                   ; al = 1 if pressed, 0 if released
    mov cs:[kbdstate+bx], al    ; Set key status in keyboard status buffer

    ; Send EOI to XT keyboard
    in      al, 61h
    mov     ah, al
    or      al, 80h
    out     61h, al
    mov     al, ah
    out     61h, al

    ; Send EOI to master PIC
    mov     al, 20h
    out     20h, al

    pop dx
    pop cx
    pop bx
    pop ax
    iret
    
;--------------------------------------------------------------------------------------------------
; Print string
; Prints a NULL terminated string to screen
; Parameters:
; - SI: pointer to string
;                        
prtstr proc near    
    push ax
    push si

prtstr_char_lp:
    mov al, [si]            ; Get next character
    cmp al, 0               ; Check if character is NULL
    jz prtstr_done           ; If NULL, we're done
    
    mov ah, 0Eh             ; Write character in teletype mode
    int 10h                 ; Call video BIOS interrupt
    
    inc si                  ; Next character
    
    jmp prtstr_char_lp       
    
prtstr_done:
    pop si
    pop ax

    ret
    
prtstr endp
;
;--------------------------------------------------------------------------------------------------  



;
;------------------------------------------------------------------------------------------------------------                                                                                                                    

; NOTES
; 300x200 Resolution means column ranges from 0000h to 013Fh
;                          row ranges from 0000h to 00C7h