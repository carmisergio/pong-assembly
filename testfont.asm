name "testfont"

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

; Kernel is loaded at 0800:0000 by simplebootloader
org 0000h

; Skip the data and function delaration section:
jmp start 

;------------------------------------------------------------------------------------------------------------
; Data section
;
    ; Messages
    msg_start db "Press [SPACE] to start...", 0h 
    msg_restart db "Press [SPACE] to restart...", 0h
    msg_left_wins db "LEFT wins!", 0h
    msg_right_wins db "RIGHT wins!", 0h
    
    ; Variables
    ball_x dw 159           ; Current ball position
    ball_y dw 99
    ball_x_prev dw 3        ; Previous ball position
    ball_y_prev dw 3
    ball_dx dw 2            ; Ball movement delta in pixels
    ball_dy dw 2
    ball_start_dir db 1; 0 -> dx=1 dy=1, 1 -> dx=-1 dy=-1, 2 -> dx=1 dy=-1, 3 -> dx=-1 dy=1
    
    ball_stop db 0          ; Frames for how much the ball has to wait
    
    scorel db 0             ; Left player score
    scorer db 0             ; Right player score
    scorel_prev db 88
    scorer_prev db 88
    
    win_score db 3         ; Winning score
    
    ; Paddle positions
    paddlel_y dw 95
    paddlel_y_prev dw 95 
    paddler_y dw 95
    paddler_y_prev dw 95  
    
    disable_sound db 0      ; Global sound disable
    tone_counter dw 0       ; Number of frames after which to stop playing tone 
    
    kbdstate db 128 dup(0)  ; Keyboard state buffer
    keypressed dw 0h        ; Last key pressed
    
    ; Score font
    score_font  db 0F9h, 99h, 99h, 9Fh, ; 0 Digit
                db 11h, 11h, 11h, 11h,  ; 1 Digit
                db 0F1h, 1Fh, 88h, 8Fh, ; 2 Digit
                db 0F1h, 1Fh, 11h, 1Fh, ; 3 Digit
                db 99h, 9Fh, 11h, 11h,  ; 4 Digit
                db 0F8h, 8Fh, 11h, 1Fh, ; 5 Digit
                db 88h, 8Fh, 99h, 9Fh,  ; 6 Digit
                db 0F1h, 11h, 11h, 11h, ; 7 Digit
                db 0F9h, 9Fh, 99h, 9Fh, ; 8 Digit
                db 0F9h, 9Fh, 11h, 11h, ; 9 Digit
    
    font_address dw 1200h   ; Location of font in memory  
    
    msg_test db "! !#$", 0h
;
;------------------------------------------------------------------------------------------------------------ 

;------------------------------------------------------------------------------------------------------------
; Code section
; 

start:
    
    ; Set data segment:
    push cs
    pop ds
    
    ; Set video mode
    mov ah, 00h             ; Set video mode function
    mov al, 13h             ; Video mode 300x200 256 colors
    int 10h
    
    ; Prepare extra segment for use in addressing VRAM
    mov ax, 0A000h
    mov es, ax  
    
            
;--------------------------------------------------------------------------------------------------
; Initial menu
;   
initial_menu:       

    ; Draw character
    lea si, msg_test       
    mov dh, 0Fh
    mov ax, 0d
    mov bx, 10d
    mov cl, 1
    call drawstring
    
    hlt
    

;
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Draw string
; Draws a string with a specific position and scale
; Parameters:
;  - si: pointer to string
;  - dh: color of digit
;  - cl: scale
;  - ax: X position
;  - bx: Y position
; 
drawstring proc near
    pusha  

drawstring_char_lp:    
    push ax             ; Save X position
    
    lodsb               ; Load next character of string
                                                       
    ; Check if end of string (NULL character)
    or al, al
    jz drawstring_end
    
    mov dl, al          ; Copy char to correct register
    pop ax              ; Recover X position
    
    ; Draw character                         
    call drawchar       
    
    ; Next position                     
   
    
    push cx
    xor ch, ch
          
drawstring_move_lp:        
     
    add ax, 5
    
    loop drawstring_move_lp      
    
    
    pop cx
    
 jmp drawstring_char_lp ; Go load next char
    
drawstring_end:
    pop ax              ; Pop because jz skips pop
    popa
    ret
drawstring endp
;
;------------------------------------------------------------------------------------------------------------       

;--------------------------------------------------------------------------------------------------
; Draw character
; Draws a character with a specific position and scale
; Parameters:
;  - dl: character to print
;  - dh: color of digit
;  - cl: scale
;  - ax: X position
;  - bx: Y position
; 
drawchar proc near
    pusha
    
    ; Save parameters
    mov drawchar_pos_x, ax
    mov drawchar_pos_y, bx
    mov drawchar_color, dh
    mov drawchar_scale, cl
    
    ; Setup font start address
    mov si, font_address
    
    ; Check for unprintable characters
    cmp dl, 20h
    jl drawchar_done
    cmp dl, 07Eh
    jg drawchar_done
    
    ; Locate character in fontset
    mov ax, dx      
    xor ah, ah      ; Clear unused part of character offset
    sub ax, 20h     ; Remove character set offset
    mov cl, 4       ; Each character takes up 4 bytes
    mul cl 
    add si, ax      ; Address of character
    
    ; Setup VRAM start offset
    xor di, di
    ; Calculate start offset in VRAM
    mov ax, drawchar_pos_y
    mov cx, 320             ; 320
    mul cx                  ; Calculate offset of start Y
    add di, ax              ; Add X to start offset
    add di, drawchar_pos_x  ; Add offset to pointer
    
    ; Setup extra segment
    mov bx, 0A000h          ; VRAM start address
    mov es, bx                                     
    
    ; Clear direction flag
    cld
    
    xor dl, dl              ; Parts counter
drawchar_w_lp:              ; Word loop - Gets called every time we need to load a new
                            ; byte of font data
    
    push dx    
    
    lodsw                   ; Load byte of font data
    mov bx, ax
    
    
    ; Compute of scale*1 sections in this word
    mov ax, 16
    mul drawchar_scale
    mov cx, ax              ; Number of bits in the loaded word
    
    mov ah, drawchar_color  ; Color of "one" pixels
    mov al, ah
    
    xor dl, dl              ; Count font blocks printed
    
    rol bx, 8
    
    
    
    ; Lines printed counter
    xor dh, dh
drawchar_px_lp:                ; Pixel loop - Gets called for every pixel

    rol bx, 1                  ; Put first digit on the left in carry
    jc drawchar_px_one:
    
    ; Pixel is 0
    push ax
    mov al, drawchar_scale
    xor ah, ah
    add di, ax                  ; Increment si to move imaginary cursor 
    pop ax
    
    jmp drawchar_px_common
    
drawchar_px_one:               ; Draw at this location 
    
    push cx
    xor ch, ch
    mov cl, drawchar_scale
    ; Pixel is 1     
drawchar_part_lp: 
    stosb 
    loop drawchar_part_lp
    
    pop cx
    
drawchar_px_common:    
    inc dl                     ; Next bit of digit
    
    ; Go to next line if necessary
    cmp dl, 4                
    jl drawchar_px_nonewline
    
    
    ; Compute next offset based on scale
    push ax                                               
    
    xor ax, ax      
    mov al, 4  
    mul drawchar_scale      ; Compute width of character
    add di, 320             ; Add offset to make it go to new line 
    sub di, ax              ; Subtract character width                                 
    
    pop ax
    
    
    xor dl, dl              ; Reset counter
    inc dh                  ; Count new line
    
    cmp dh, drawchar_scale               ; Check if we have printed the right number of lines for this scale
    je drawchar_px_line4
    ror bx, 4               ; Restore just printed line 
    jmp drawchar_px_nonewline
    
drawchar_px_line4:
    xor dh, dh              ; Reset lines printed counter
    
drawchar_px_nonewline:
    loop drawchar_px_lp
    
    pop dx                  ; Get back parts of digit
    inc dl
    ; Checks if there are still parts of digit to print
    cmp dl, 2
    jge drawchar_done       ; We have printed all parts of this digit
    
    jmp drawchar_w_lp
    
drawchar_done:      
                      
    popa
    
    ret
drawchar endp
; Local variables
drawchar_pos_x dw ?
drawchar_pos_y dw ?
drawchar_color db ?
drawchar_scale db ?
;
;------------------------------------------------------------------------------------------------------------      

;org 1200h 
db 1200h - ($-$$) dup(0) 

asdfsdfsfd  db 0F9h, 99h, 99h, 9Fh, ; 0 Digit
            db 11h, 11h, 11h, 11h,  ; 1 Digit
            db 0F1h, 1Fh, 88h, 8Fh, ; 2 Digit
            db 0F1h, 1Fh, 11h, 1Fh, ; 3 Digit
            db 99h, 9Fh, 11h, 11h,  ; 4 Digit
            db 0F8h, 8Fh, 11h, 1Fh, ; 5 Digit
            db 88h, 8Fh, 99h, 9Fh,  ; 6 Digit
            db 0F1h, 11h, 11h, 11h, ; 7 Digit
            db 0F9h, 9Fh, 99h, 9Fh, ; 8 Digit
            db 0F9h, 9Fh, 11h, 11h, ; 9 Digit
