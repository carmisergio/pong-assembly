;-----------------------------------------------------------------------------------------------------------------------
; Pong
; Two player PONG! game                                                                           
; Author: Sergio Carmine 3CITI <mesergiocarmi.net>                                                       
; Date: 14/01/2023                                                                                                     
;-----------------------------------------------------------------------------------------------------------------------
name "pong"

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
    ; Messages
    msg_hello db "PONG! Loaded succesfully! New message!", 0Ah, 0Dh, 00h
    
    msg1 db "Press and hold ESC", 13, 10, 00h
    msg2 db "ESC pressed, release ESC", 13, 10, 00h
    msg3 db "ESC released", 13, 10, 00h
    
    ; Variables
    ball_x dw 159           ; Current ball position
    ball_y dw 99
    ball_x_prev dw 3        ; Previous ball position
    ball_y_prev dw 3
    ball_dx dw 1            ; Ball movement delta in pixels
    ball_dy dw 1
    ball_start_dir db 1; 0 -> dx=1 dy=1, 1 -> dx=-1 dy=-1, 2 -> dx=1 dy=-1, 3 -> dx=-1 dy=1
    
    ball_stop db 0          ; Frames for how much the ball has to wait
    
    scorel db 0             ; Left player score
    scorer db 0             ; Right player score
    
    ; Paddle positions
    paddlel_y dw 95
    paddlel_y_prev dw 95 
    paddler_y dw 95
    paddler_y_prev dw 95     
    
    kbdstate db 128 dup(0)  ; Keyboard state buffer
    
    ; Score font
    score_font  db 0F8h, 8Fh, 11h, 1Fh, ; 5 Digit
                db 88h, 8Fh, 99h, 9Fh, ; 6 Digit
;
;------------------------------------------------------------------------------------------------------------ 

;------------------------------------------------------------------------------------------------------------
; Code section
; 

start:
    
    ; Set data segment:
    push cs
    pop ds
    
    ; Change interrupt handler for keyboard
    xor     ax, ax
    mov     es, ax          ; Set extra segment to 0000h (IVT is at 0000h:0000h)
    cli                     ; Momentarely disable interrupts  
    mov es:[9*4], offset keybdisr ; Update address with address to own interrupt handler  
    mov es:[9*4+2],cs
    sti                     ; Reenable interrupts
    
    
    ; Set video mode
    mov ah, 00h             ; Set video mode function
    mov al, 13h             ; Video mode 300x200 256 colors
    int 10h
    
    ; Prepare extra segment for use in addressing VRAM
    mov ax, 0A000h
    mov es, ax
    
    call drawscoredigit
    
lp:
    jmp lp
    
    ; Field borders
    call drawfld
    
; Main frame loop

main_lp:

    ; Handle keyboard input
    call handlekbd
    
    
    ; Check if the ball is in a wait state
    mov al, ball_stop
    or al, al           ; See if counter is 0
    jnz ball_wait
    
    ; Draw the ball
    call drawball
     
    ; Check if ball has bounced from top or bottom
    call checkbouncetb
    
    ; Check if ball has bounced from the paddle
    call checkbouncepaddles
    
    ; Check if a point has been scored
    call checkpoint
    
    ; Compute next ball position
    call compballpos
    
    ; Draw center line
    call drawctrln
    
    jmp ball_nowait
    
ball_wait:
    dec ball_stop
ball_nowait:

    
    ; Draw paddles
    call drawpaddles
    
    ; Frame delay
    mov ah, 86h
    xor cx, cx              ; High part of delay
    ;mov dx, 411Bh           ; Low part of delay
    mov dx, 211Bh           ; Low part of delay
    ;mov cx, 0001h
    int 15h                 ; Delay
    
    jmp main_lp
    
;--------------------------------------------------------------------------------------------------
; Check bounce top bottom
; Checks if ball has reached top or bottom and modifies deltas accordingly to make it bounce
;  
checkbouncetb proc near
    push ax
    push dx
    ; Get current ball position
    mov dx, ball_y
     
    ; See if ball has hit bottom
    cmp dx, 194
    ja checkbouncetb_bounce
    
    ; See if ball has hit top
    cmp dx, 2
    jbe checkbouncetb_bounce
    
    jmp checkbouncetb_nobounce
    
checkbouncetb_bounce:
    ; Invert Y delta of ball
    xor ax, ax
    sub ax, ball_dy
    mov ball_dy, ax
checkbouncetb_nobounce:
    pop dx
    pop ax
    ret
checkbouncetb endp
;
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Check bounce paddles
; Check if ball has bounced from paddles
;  
checkbouncepaddles proc near
    push ax
    push dx
    ; Get current ball position
    mov cx, ball_x 
    mov dx, ball_y
     
    ; See if ball has hit left
    cmp cx, 14
    jbe checkbouncepaddles_lvert
  
    ; See if ball has hit right
    cmp cx, 302
    ja checkbouncepaddles_rvert
    
    jmp checkbouncetb_nobounce
    
checkbouncepaddles_lvert:
     
    ; Check if we are behind paddles
    cmp cx, 9
    jb checkbouncepaddles_nobounce
    
    mov ax, paddlel_y       ; Get current paddle position
    
    jmp checkbouncepaddles_common
    
checkbouncepaddles_rvert:

    ; Check if we are behind paddles
    cmp cx, 310
    ja checkbouncepaddles_nobounce
    
    mov ax, paddler_y       ; Get current paddle position

checkbouncepaddles_common:    
    
    cmp dx, ax
    jb checkbouncepaddles_nobounce
    
    add ax, 15d
    cmp dx, ax
    ja  checkbouncepaddles_nobounce
    
    ; Invert X delta of ball
    xor ax, ax
    sub ax, ball_dx
    mov ball_dx, ax
    
checkbouncepaddles_nobounce:
    pop dx
    pop ax
    ret
checkbouncepaddles endp
;
;--------------------------------------------------------------------------------------------------    

;--------------------------------------------------------------------------------------------------
; Check point
; Checks whether the ball has hit the sides (a point has been scored)
; 
checkpoint proc near
    push ax
    push bx
    push cx
    push dx
    
    ; Get ball x position
    mov cx, ball_x
    
    ; See if ball has hit right
    cmp cx, 316
    ja checkpoint_hitright
    
    ; See if ball has hit left
    cmp cx, 0
    jbe checkpoint_hitleft
    
    jmp checkpoint_nohit           ; Ball has not hit anything
    
checkpoint_hitright:
    
    ; Increment left player score
    inc scorel
    jmp checkpoint_hit 
    
checkpoint_hitleft:
    
    ; Increment right player score
    inc scorer

checkpoint_hit:

    ; Reset ball position
    mov ball_x, 159d
    mov ball_y, 99d
    
    ; Set deltas based on direction
    mov al, ball_start_dir
    cmp al, 0
    jne checkpoint_dir_notzero:
    mov ball_dx, 1
    mov ball_dy, 1
    jmp checkpoint_dir_end
checkpoint_dir_notzero:
    cmp al, 1
    jne checkpoint_dir_notone
    mov ball_dx, -1
    mov ball_dy, -1
    jmp checkpoint_dir_end
checkpoint_dir_notone:
    cmp al, 2
    jne checkpoint_dir_nottwo
    mov ball_dx, 1
    mov ball_dy, -1
    jmp checkpoint_dir_end
checkpoint_dir_nottwo:
    mov ball_dx, -1
    mov ball_dy, 1
checkpoint_dir_end:
    inc al
    cmp al, 4
    jb checkpoint_dir_noreset 
    
    xor al, al                      ; Reset start direction
checkpoint_dir_noreset:    
    mov ball_start_dir, al          ; Save start direction 
    
   
    
    ;mov ah, 86h
    ;xor cx, cx              ; High part of delay
    ;mov dx, 411Bh           ; Low part of delay
    ;mov cx, 000Fh
    ;int 15h;                ; Delay After hit
    
    ; Instead of delaying, add a ball stop amount
    mov ball_stop, 120d
    
checkpoint_nohit:
    pop dx
    pop cx
    pop bx
    pop ax
    ret
checkpoint endp 
;
;--------------------------------------------------------------------------------------------------  

;--------------------------------------------------------------------------------------------------
; Compute ball position
; Computes next ball position based on deltas
;  
compballpos proc near
    ; Compute next ball position
    mov cx, ball_x
    mov ax, ball_dx
    add cx, ax
    mov ball_x, cx
    mov cx, ball_y
    mov ax, ball_dy
    add cx, ax
    mov ball_y, cx
    ret
compballpos endp
;
;--------------------------------------------------------------------------------------------------  
 
    
    
; ###################################### INPUT FUNCTIONS
    
;--------------------------------------------------------------------------------------------------
; Handle keyboard input
; Checks state of necessary keys and performs functions accordingly
;                        
handlekbd proc near    
    ; Ctrl Alt Del handler
    cmp [kbdstate+29], 0         ; Check for Ctrl
    jz handlekbd_not_ctrlaltdel
    cmp [kbdstate+56], 0         ; Check for Alt
    jz handlekbd_not_ctrlaltdel
    cmp [kbdstate+83], 0         ; Check for Del
    jz handlekbd_not_ctrlaltdel
    
    ; Reset system
    jmp far 0FFFFh:0
    
handlekbd_not_ctrlaltdel:

    ; Left paddle movement keys
    
    cmp [kbdstate+17], 0         ; Check for W
    jz handlekbd_not_w    
    ; Move paddle up
    mov ax, paddlel_y            
    dec ax   
    ; Check if paddle has reached top
    cmp ax, 2
    jae handlekbd_paddlel_nottopstop
    mov ax, 2                   ; Prevent paddle from going too much up
handlekbd_paddlel_nottopstop:
    mov paddlel_y, ax           ; Write new paddle position
    
handlekbd_not_w:
    cmp [kbdstate+31], 0         ; Check for S
    jz handlekbd_not_s
    ; Move paddle down
    mov ax, paddlel_y            
    inc ax   
    ; Check if paddle has reached top
    cmp ax, 183
    jbe handlekbd_paddlel_notbottomstop
    mov ax, 183                   ; Prevent paddle from going too much down
handlekbd_paddlel_notbottomstop:
    mov paddlel_y, ax           ; Write new paddle position

handlekbd_not_s:

    ; Right paddle movement keys
    
    cmp [kbdstate+72], 0         ; Check for UP
    jz handlekbd_not_up    
    ; Move paddle up
    mov ax, paddler_y            
    dec ax   
    ; Check if paddle has reached top
    cmp ax, 2
    jae handlekbd_paddler_nottopstop
    mov ax, 2                   ; Prevent paddle from going too much up
handlekbd_paddler_nottopstop:
    mov paddler_y, ax           ; Write new paddle position
    
handlekbd_not_up:
    cmp [kbdstate+80], 0         ; Check for DOWN
    jz handlekbd_not_down
    ; Move paddle down
    mov ax, paddler_y            
    inc ax   
    ; Check if paddle has reached top
    cmp ax, 183
    jbe handlekbd_paddler_notbottomstop
    mov ax, 183                   ; Prevent paddle from going too much down
handlekbd_paddler_notbottomstop:
    mov paddler_y, ax           ; Write new paddle position

handlekbd_not_down:     
    ret
    
handlekbd endp
;
;--------------------------------------------------------------------------------------------------  
  
  
; ###################################### GRAPHICS FUNCTIONS
    
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

;--------------------------------------------------------------------------------------------------
; Fill screen
; Fills video memory with a specific color
; Parameters:
; - al: color to fill with
;                        
fillscr proc near
    push ax
    push bx
    push cx
    push dx
    push si
    push es
    
    ; Setup extra segment
    mov bx, 0A000h
    mov es, bx
    
    ; Start from first address
    xor di, di
    
    ; Set bytes to write
    mov cx, 64000d
    
    ; Clear direction flag
    cld  

fillscr_lp:
    stosb
    loop fillscr_lp
   
    pop es 
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
fillscr endp
;
;--------------------------------------------------------------------------------------------------    

;--------------------------------------------------------------------------------------------------
; Draw rectangle XY
; Draws a rectangle parallel to the cartesian axies, with a certain width, height and position
; Parameters:
;  - ax: width of rectangle
;  - bx: height of rectangle
;  - cx: X position of top left corner of rectangle
;  - dx: Y position of top left corner of rectangle
;                        
drawrectxy proc near
    push ax
    push bx
    push cx
    push dx
    push si
    push es
    
    ; Compute final width accounting position offsets
    add ax, cx
    add bx, dx
    
    ; Save parameters locally
    mov drawrectxy_w, ax
    mov drawrectxy_h, bx
    mov drawrectxy_x, cx
    mov drawrectxy_y, dx
    
    ; Setup extra segment
    mov cx, 0A000h
    mov es, cx
    
    ; Start from first address
    xor si, si
    
    mov cx, drawrectxy_y    ;X starting position
drawrectxy_row_lp:
    
    mov dx, drawrectxy_x    ; Y starting position 
    
drawrectxy_col_lp:

    ; Compute offset into VRAM
    push dx                 ; dx will get modified, so save it
    mov ax, cx              ; Put current row in ax
    mov bx, 320d            ; Multiply by 320 to obtain offset to beginning of line
    mul bx
    pop dx                  ; Restore dx  
    add ax, dx              ; Add column to obtain pixel offset
    ;add ax, drawrectxy_x    ; Add column position
    mov si, ax              ; Put found value into si
    
    ;cmp si, 0FA00h          ; Check if we're trying to write somewhere out of VRAM
    ;jae drawrectxy_aov      ; Prevent from writing outside of VRAM
    
    mov es:[si], 0Fh        ; Draw pixel
    
;drawrectxy_aov:
        
    inc dx                  ; Next column
    
    ; Get width; Check if tere are still columns to draw
    mov ax, drawrectxy_w    
    cmp dx, ax              
    jb drawrectxy_col_lp
    
    inc cx                  ; Next row
    
    ; Check if tere are still columns to draw
    mov ax, drawrectxy_h
    cmp cx, ax
    jb drawrectxy_row_lp    

    pop es 
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
drawrectxy endp
; Local variables
drawrectxy_w dw ?
drawrectxy_h dw ?
drawrectxy_x dw ?
drawrectxy_y dw ?
;
;--------------------------------------------------------------------------------------------------  

;--------------------------------------------------------------------------------------------------
; Draw rectangle XY
; Draws a rectangle parallel to the cartesian axies, with a certain width, height and position
; Parameters:
;  - ax: width of rectangle
;  - bx: height of rectangle
;  - cx: X position of top left corner of rectangle
;  - dx: Y position of top left corner of rectangle
;                        
clearrectxy proc near
    push ax
    push bx
    push cx
    push dx
    push si
    push es
    
    ; Compute final width accounting position offsets
    add ax, cx
    add bx, dx
    
    ; Save parameters locally
    mov clearrectxy_w, ax
    mov clearrectxy_h, bx
    mov clearrectxy_x, cx
    mov clearrectxy_y, dx
    
    ; Setup extra segment
    mov cx, 0A000h
    mov es, cx
    
    ; Start from first address
    xor si, si
    
    mov cx, clearrectxy_y    ;X starting position
clearrectxy_row_lp:
    
    mov dx, clearrectxy_x    ; Y starting position 
    
clearrectxy_col_lp:

    ; Compute offset into VRAM
    push dx                 ; dx will get modified, so save it
    mov ax, cx              ; Put current row in ax
    mov bx, 320d            ; Multiply by 320 to obtain offset to beginning of line
    mul bx
    pop dx                  ; Restore dx  
    add ax, dx              ; Add column to obtain pixel offset
    ;add ax, drawrectxy_x    ; Add column position
    mov si, ax              ; Put found value into si
    
    ;cmp si, 0FA00h          ; Check if we're trying to write somewhere out of VRAM
    ;jae drawrectxy_aov      ; Prevent from writing outside of VRAM
    
    mov es:[si], 00h        ; Draw pixel
    
;drawrectxy_aov:
        
    inc dx                  ; Next column
    
    ; Get width; Check if tere are still columns to draw
    mov ax, clearrectxy_w    
    cmp dx, ax              
    jb clearrectxy_col_lp
    
    inc cx                  ; Next row
    
    ; Check if tere are still columns to draw
    mov ax, clearrectxy_h
    cmp cx, ax
    jb clearrectxy_row_lp    

    pop es 
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
clearrectxy endp
; Local variables
clearrectxy_w dw ?
clearrectxy_h dw ?
clearrectxy_x dw ?
clearrectxy_y dw ?
;
;--------------------------------------------------------------------------------------------------  

;--------------------------------------------------------------------------------------------------
; Draw centerline
; Draws field center line
;                        
drawctrln proc near
    push ax
    push bx
    push cx
    push dx
  
    mov cx, 159               ; Center column to put the squares on  
    
    mov dx, 4d                ; First row to put square on
    mov ax, 2d                ; Square width
    mov bx, 3d                ; Square height
drawctrln_row_lp:                       
    
    call drawrectxy           ; Draw rectangle

    add dx, 7d                ; Next row
    cmp dx, 198d
    jbe drawctrln_row_lp
    
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
drawctrln endp
;
;--------------------------------------------------------------------------------------------------  

;--------------------------------------------------------------------------------------------------
; Draw field
; Draws field borders 
;                        
drawfld proc near
    push ax
    push bx
    push cx
    push dx
    
    ; Horizontal field border
    mov ax, 320d
    mov bx, 2d 
    mov cx, 0d
    mov dx, 0d
    call drawrectxy
    mov dx, 198d
    call drawrectxy
    
    ; Vertical field border
    ;mov ax, 2d
    ;mov bx, 197d 
    ;mov cx, 0d
    ;mov dx, 2d
    ;call drawrectxy
    ;mov cx, 318d
    ;call drawrectxy
    
    pop ax
    pop bx
    pop cx
    pop dx 
    
    ret    
drawfld endp
;
;--------------------------------------------------------------------------------------------------  

;--------------------------------------------------------------------------------------------------
; Draw ball
; Clears previous ball position and draws new one 
; 
drawball proc near
    push ax
    push bx
    push cx
    push dx
    ; Delete previous ball
    mov ax, 3               ; Width
    mov bx, 3               ; Height
    mov cx, ball_x_prev     ; X position
    mov dx, ball_y_prev     ; Y position
    call clearrectxy
    
    ; Draw ball
    mov ax, 3               ; Width
    mov bx, 3               ; Height
    mov cx, ball_x          ; X position
    mov dx, ball_y          ; Y position
    call drawrectxy
    
    mov ball_x_prev, cx     ; Save current ball position as previous
    mov ball_y_prev, dx 
                      
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
drawball endp
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Draw paddles
; Clears previous paddle position and draws the new one 
; 
drawpaddles proc near
    push ax
    push bx
    push cx
    push dx
    
    ; Draw left paddle
    mov ax, 5d
    mov bx, 15d
    mov cx, 10d
    mov dx, paddlel_y_prev
    call clearrectxy
    mov dx, paddlel_y
    call drawrectxy 
    
    mov paddlel_y_prev, dx
    
    ; Draw right paddle
    mov cx, 304d
    mov dx, paddler_y_prev
    call clearrectxy
    mov dx, paddler_y
    call drawrectxy
    
    mov paddler_y_prev, dx 
                      
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
drawpaddles endp
;
;------------------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Draw score digit
; Draws a score digit at specific position 
; 
drawscoredigit proc near
    push ax
    push bx
    push cx
    push dx
    
    ; Setup extra segment
    mov bx, 0A000h          ; VRAM start address
    mov es, bx
    
    ; Setup VRAM start offset
    xor di, di
    
    ; Setup font start address
    lea si, score_font
    add si, 4
    
    ; Clear direction flag
    cld
    
    xor dl, dl              ; Parts counter
drawscoredigit_w_lp:    
    push dx
    lodsw                   ; Load first part of digit
    mov bx, ax
    
    mov cx, 64              ; Number of bits in the loaded word
    mov ax, 0F0Fh           ; Color of "one" pixels
    
    xor dl, dl              ; Count font blocks printed
    
    rol bx, 8
    
    
    
    ; Lines printed counter
    xor dh, dh
drawscoredigit_px_lp:

    rol bx, 1                  ; Put first digit on the left in carry
    jc drawscoredigit_px_one:
    
    add di, 4                  ; Increment si to move imaginary cursor
    
    jmp drawscoredigit_px_common
    
drawscoredigit_px_one:    
    
    stosw
    stosw
    
drawscoredigit_px_common:    
    inc dl
    
    ; Go to next line if necessary
    cmp dl, 4
    jl drawscoredigit_px_nonewline
    
    add di, 304             ; Add offset to make it go to new line
    xor dl, dl              ; Reset counter
    inc dh                  ; Count new line
    
    cmp dh, 4
    je drawscoredigit_px_line4
    ror bx, 4               ; Restore just printed line 
    jmp drawscoredigit_px_nonewline
    
drawscoredigit_px_line4:
    xor dh, dh
    
drawscoredigit_px_nonewline:
    loop drawscoredigit_px_lp
    
    pop dx
    inc dl
    ; Checks if there are still parts of digit to print
    cmp dl, 2
    jge drawscoredigit_done
    
    jmp drawscoredigit_w_lp
    
drawscoredigit_done:   
                      
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
drawscoredigit endp
;
;------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                             

; ###################################### INTERRUPT SERVICE ROUTINES
    
;--------------------------------------------------------------------------------------------------
; Keyboard interrupt (IRQ9) service routine
; 
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
;
;--------------------------------------------------------------------------------------------------              

; NOTES
; 300x200 Resolution means column ranges from 0000h to 013Fh
;                          row ranges from 0000h to 00C7h