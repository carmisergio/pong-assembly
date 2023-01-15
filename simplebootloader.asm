;-----------------------------------------------------------------------------------------------------------------------
; Simple Bootloader
; This program is meant to be put in the first sector of boot media.
; It will then load the main program from disk and pass control.                                                                               
; Author: Sergio Carmine 3CITI <mesergiocarmi.net>                                                       
; Date: 09/01/2023                                                                                                     
;-----------------------------------------------------------------------------------------------------------------------
name "loader"

; Directive to create boot file
#make_boot#

;==============================
; Memory table (HEX)
; -----------------------------
; 07C0:0000 | Boot Sector
; 07C0:01FF | (512 bytes)
; -----------------------------
; 07C0:0200 | Stack
; 07C0:03ff | (256 words)
; -----------------------------
; 0800:0000 | Loaded program
; 0800:XXXX | 
;==============================

;------------------------------------------------------------------------------------------------------------
; Code section
; 
org 7C00h      ; Set location counter. BIOS loads boot sector to address 7C00h

;--------------------------------------------------------------------------------------------------
; Start
;          
    ; Setup data segment
    xor ax, ax              ; Data segment is 0000h because data is loaded by BIOS in first segment
                            ; after code which starts at 7c00h
    mov ds, ax
    
    ; Initialize Stack
    mov ax, 07c0h           ; Stack Segment
    mov ss, ax
    mov sp, 03feh           ; Top of stack
   
    ; Set default video mode (80x25)
    mov ah, 00h             ; Set video mode function
    mov al, 03h             ; Video mode 03h (80x25)
    int 10h                 ; Call video BIOS interrupt
    
    ; Print message
    lea si, msg_loading       ; Load string address
    call prtstr             ; Print string
    
    ; Begin loading
load_retry:
    mov ah, 02h             ; Read function
    mov al, 10              ; How many sectors to read
    mov ch, 0               ; Cylinder
    mov cl, 2               ; Start sector
    mov dh, 0               ; Head
                            ; dl is drive number and is not changed because the BIOS passes us
                            ; the current drive in dl already
    mov bx, 0800h           
    mov es, bx              ; Segment of destinatino data
    mov bx, 0               ; Offset into that segment
    int 13h                 ; Read
    
    ;stc ; Simulate bad reads
    jnc load_succesful      ; CF is set if load unsuccesful
    
    cmp load_retries, 0          ; Check if there are retries left
    jz load_failed          ; The load operation has failed
    
    dec load_retries             ; Decrement retries left
    
    ; Print retrying message
    lea si, msg_load_retry  
    call prtstr
    
    jmp load_retry          ; Retry 

load_failed:
    ; Print load failed message
    lea si, msg_load_failed
    call prtstr
    
    ; Press any key to continue...
    mov ah, 0               ; Get character function
    int 16h                 ; BIOS input interrupt
    
    ; Reboot!
    mov [0472h], 1234h
    jmp 0FFFFh:0000h
    
load_succesful:    
    lea si, msg_load_success
    call prtstr
    
    ; Pass control to loaded program
    jmp 0800h:0000h  
;
;--------------------------------------------------------------------------------------------------

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
;------------------------------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------------------------------
; Data section
;
    ; Messages
    msg_loading db "## Simple Bootloader v0.1 - Sergio Carmine   ##", 0Ah, 0Dh
                db "Loading...", 0Ah, 0Dh, 00h
    msg_load_retry db "Read error, retrying...", 0Ah, 0Dh, 00h
    msg_load_failed db "Loading failed! Press any key to reboot.", 0Ah, 0Dh, 00h
    msg_load_success db "SUCCESS!", 0Ah, 0Dh, 00h
    
    ; Variables
    load_retries db 3
;
;------------------------------------------------------------------------------------------------------------ 

; Pad to end of boot sector
; db 510 - ($-$$) dup(0)
