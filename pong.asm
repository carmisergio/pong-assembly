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

; Kernel is loaded at 0800:0000 by simplebootloader
org 0000h

; Skip the data and function delaration section:
jmp start 

;------------------------------------------------------------------------------------------------------------
; Data section
;
    ; Messages 
    msg_pong db "PONG!", 0h
    msg_start db "Press [SPACE] to start...", 0h 
    msg_restart db "Press [SPACE] to restart...", 0h 
    msg_reset db "[ESC] to reset...", 0h
    msg_left_wins db "LEFT WINS!", 0h
    msg_right_wins db "RIGHT WINS!", 0h
    
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
    
    win_score db 11         ; Winning score
    
    ; Paddle positions
    paddlel_y dw 95
    paddlel_y_prev dw 95 
    paddler_y dw 95
    paddler_y_prev dw 95  
    
    disable_sound db 1      ; Global sound disable
    tone_counter dw 0       ; Number of frames after which to stop playing tone 
    
    kbdstate db 128 dup(0)  ; Keyboard state buffer
    keypressed dw 0h;       ; Last key pressed
    
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
    
            
;--------------------------------------------------------------------------------------------------
; Initial menu
;   
initial_menu:       
    
    ; Clear screen
    xor al, al
    call fillscr

    ; Field borders
    call drawfld 
    
    ; Draw pong logo
    lea si, msg_pong       
    mov dh, 0Fh
    mov ax, 72d
    mov bx, 50d
    mov cl, 8
    call drawstring
     
    ; Print start message
    lea si, msg_start
    mov dh, 19
    mov dl, 08
    call prtstr 
        
initial_menu_lp:

    
    call waitsync               ; Limit framerate
    
    ; Check if space pressed
    cmp keypressed, 57
    jne initial_menu_nobegin   
    
    xor ax, ax
    mov keypressed, ax          ; Clear key pressed
    jmp main_game               ; Begin game!
    
initial_menu_nobegin:

    call handle_ctrlaltdel      ; Handle CTRL ALT DEL
    call toneloop
    

    jmp initial_menu_lp
;
;-------------------------------------------------------------------------------------------------- 


;--------------------------------------------------------------------------------------------------
; Main Game
;  
main_game: 
    
    ; Reset first ball start direction
    mov ball_start_dir, 1      
    mov ball_dx, 2
    mov ball_dy, 2

main_game_restart:

    ; Clear screen
    xor al, al
    call fillscr
    
    ; Clear scores
    mov scorel, al
    mov scorer, al
    
    mov ball_stop, 40      
    
    ; Reset paddle positions
    mov ax, 95
    mov paddlel_y, ax
    mov paddlel_y_prev, ax 
    mov paddler_y, ax
    mov paddler_y_prev, ax
    
    ; Reset ball positions
    mov ball_x, 159  
    mov ball_y, 99
    mov ball_x_prev, 3
    mov ball_y_prev, 3

    ; Field borders
    call drawfld
    
    call waitsync
                                        
; Main frame loop
main_lp:

    ; Handle keyboard input
    call handlekbd   
    call handle_ctrlaltdel
    
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
    
    ; Check if a player has won
    call checkwin 
    
    ; Compute next ball position
    call compballpos
    
    jmp ball_nowait
    
ball_wait:
    dec ball_stop
ball_nowait:

    ; Draw center line
    call drawctrln
     
    ; Update scores
    call updtscoredsply
    
    ; Draw paddles
    call drawpaddles
    
    ; Frame delay
    ;mov ah, 86h
    ;xor cx, cx              ; High part of delay
    ;;mov dx, 411Bh          ; Low part of delay
    ;mov dx, 211Bh           ; Low part of delay
    ;;mov cx, 0001h
    ;int 15h                 ; Delay        
    
    ; Tone loop
    call toneloop
    
    ; Handle ESC   
    cmp keypressed, 01d         ; ESC
    jne main_game_notesc
    
    xor ax, ax
    mov keypressed, ax          ; Clear key pressed
    jmp initial_menu            ; Reset!

main_game_notesc:
    
    ; Syncronize to vertical blanking interval
    call waitsync
    
    jmp main_lp
;
;--------------------------------------------------------------------------------------------------    

;--------------------------------------------------------------------------------------------------
; Winner menu
;    
winner_menu:
    ; Clear screen
    xor al, al
    call fillscr

    ; Field borders
    call drawfld
    
    ; Update scores
    call updtscoredsply
    
    ; Set up for message print
    mov al, scorer
    cmp scorel, al
    jl winner_menu_right_wins
    
    lea si, msg_left_wins
    mov ax, 64                  ; Set correct scale
    
    jmp winner_menu_common

winner_menu_right_wins:
    lea si, msg_right_wins      ; Load correct message
    mov ax, 54                  ; Set correct scale

winner_menu_common:   
    
    ; Draw message       
    mov dh, 0Fh
    mov bx, 80
    mov cl, 4
    call drawstring
     
    ; Print message
    lea si, msg_restart
    mov dh, 19
    mov dl, 7
    call prtstr
    ; Print message
    lea si, msg_reset
    mov dh, 21
    mov dl, 13
    call prtstr 
    
    ; Prolong tone
    mov ax, tone_counter
    add ax, 30
    mov tone_counter, ax
        
winner_menu_lp:

    
    call waitsync               ; Limit framerate
    
    ; Handle SPACE
    cmp keypressed, 57d         ; Space
    jne winner_menu_notspace 
    
    ; Start game
    xor ax, ax
    mov keypressed, ax          ; Clear key pressed
    jmp main_game_restart       ; Begin game!
    
winner_menu_notspace:
           
    ; Handle ESC   
    cmp keypressed, 01d         ; ESC
    jne winner_menu_notesc
    
    xor ax, ax
    mov keypressed, ax          ; Clear key pressed
    jmp initial_menu            ; Reset!
    
winner_menu_notesc:

    call handle_ctrlaltdel      ; Handle CTRL ALT DEL 
    call toneloop               ; Handle stopping lingering tones
    

    jmp winner_menu_lp
;
;--------------------------------------------------------------------------------------------------

    
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
    
    ; Play wall bounce tone
    mov ax, 8000
    mov bx, 1
    call playtone
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
    
    ; Play paddle bounce tone
    mov ax, 6000
    mov bx, 1
    call playtone
    
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
    mov ball_dx, 2
    mov ball_dy, 2
    jmp checkpoint_dir_end
checkpoint_dir_notzero:
    cmp al, 1
    jne checkpoint_dir_notone
    mov ball_dx, -2
    mov ball_dy, -2
    jmp checkpoint_dir_end
checkpoint_dir_notone:
    cmp al, 2
    jne checkpoint_dir_nottwo
    mov ball_dx, 2
    mov ball_dy, -2
    jmp checkpoint_dir_end
checkpoint_dir_nottwo:
    mov ball_dx, -2
    mov ball_dy, 2
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
    
    ; Play score tone
    mov ax, 3000
    mov bx, 30
    call playtone
    
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
; Check win
; Check if a player has reached the winning score
;  
checkwin proc near      
    push ax
    
    ; Load winning score
    mov al, win_score
    
    cmp scorel, al
    je winner_menu
    
    cmp scorer, al
    je winner_menu
    
    pop ax
    ret
checkwin endp
;
;--------------------------------------------------------------------------------------------------  

;--------------------------------------------------------------------------------------------------
; Compute ball position
; Computes next ball position based on deltas
;  
compballpos proc near
    ; Compute next X position
    mov cx, ball_x
    mov ax, ball_dx
    add cx, ax     
                                      
    ; Clamp ball X
    cmp cx, 0
    jge compballpos_noxclamp1: 
    mov cx, 0               ; Set position to left most position 
compballpos_noxclamp1:
    cmp cx, 318   
    jle compballpos_noxclamp2
    mov cx, 318             ; Set position to right most position
compballpos_noxclamp2:
    
    mov ball_x, cx
                             
    ; Compute next Y position
    mov cx, ball_y
    mov ax, ball_dy
    add cx, ax
    
    ; Clamp ball Y
    cmp cx, 2    
    jge compballpos_noyclamp1: 
    mov cx, 2               ; Set position to top most position 
compballpos_noyclamp1:
    cmp cx, 195
    jle compballpos_noyclamp2
    mov cx, 195             ; Set position to bottom most position
compballpos_noyclamp2:
    
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
                       
    ; Left paddle movement keys
    
    cmp [kbdstate+17], 0         ; Check for W
    jz handlekbd_not_w    
    ; Move paddle up
    mov ax, paddlel_y            
    dec ax
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

;--------------------------------------------------------------------------------------------------
; Handle CTRL + ALT + DEL
;                        
handle_ctrlaltdel proc near    
    ; Ctrl Alt Del handler
    cmp [kbdstate+29], 0         ; Check for Ctrl
    jz handle_ctrlaltdel_not_ctrlaltdel
    cmp [kbdstate+56], 0         ; Check for Alt
    jz handle_ctrlaltdel_not_ctrlaltdel
    cmp [kbdstate+83], 0         ; Check for Del
    jz handle_ctrlaltdel_not_ctrlaltdel
    
    ; Reset system
    jmp far 0FFFFh:0
    
handle_ctrlaltdel_not_ctrlaltdel:
    
    ret
    
handle_ctrlaltdel endp
;
;-------------------------------------------------------------------------------------------------- 
  
  
; ###################################### GRAPHICS FUNCTIONS
    
;--------------------------------------------------------------------------------------------------
; Print string
; Prints a NULL terminated string to screen
; Parameters:
; - SI: pointer to string
; - DH: Start row
; - DL: Start column
;                        
prtstr proc near    
    push ax
    push bx
    push cx
    push dx
    push si
    
    xor bx, bx
    
    mov cx, 1                ; Write 1 copy of each character
    mov bl, 0Fh              ; Set text color


prtstr_char_lp:
    mov al, [si]            ; Get next character
    cmp al, 0               ; Check if character is NULL
    jz prtstr_done          ; If NULL, we're done
    
    mov ah, 02h             ; Set cursor position
    int 10h
    
    mov ah, 09h             ; Write character at cursor
    int 10h                 ; Call video BIOS interrupt
    
    inc si                  ; Next character 
    
    ; Move cursor
    inc dl
    
    jmp prtstr_char_lp       
    
prtstr_done:
    pop si  
    pop dx
    pop cx
    pop bx
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
; Parameters:
;  - dl: digit to print
;  - dh: color of digit
;  - ax: X position
;  - bx: Y position
; 
drawscoredigit proc near
    push ax
    push bx
    push cx
    push dx
    
     ; Save parameters
    mov drawscoredigit_color, dh
    mov drawscoredigit_x, ax
    mov drawscoredigit_y, bx
    
    
    ; Setup font start address
    lea si, score_font 
    ;mov si, font_address
    mov ax, dx
    xor ah, ah
    mov cl, 4
    mul cl 
    add si, ax  
    
    ; Setup VRAM start offset
    xor di, di
    ; Calculate start offset in VRAM
    mov ax, drawscoredigit_y
    mov cx, 320             ; 320
    mul cx                  ; Calculate offset of start Y
    add di, ax              ; Add X to start offset
    add di, drawscoredigit_x  ; Add offset to pointer
    
    ; Setup extra segment
    mov bx, 0A000h          ; VRAM start address
    mov es, bx                                     
    
    ; Clear direction flag
    cld
    
    xor dl, dl              ; Parts counter
drawscoredigit_w_lp:
    
    push dx    
    
    lodsw                   ; Load first part of digit
    mov bx, ax
    
    mov cx, 48              ; Number of bits in the loaded word
    
    mov ah, drawscoredigit_color           ; Color of "one" pixels
    mov al, ah
    
    xor dl, dl              ; Count font blocks printed
    
    rol bx, 8
    
    
    
    ; Lines printed counter
    xor dh, dh
drawscoredigit_px_lp:

    rol bx, 1                  ; Put first digit on the left in carry
    jc drawscoredigit_px_one:
    
    add di, 3                  ; Increment si to move imaginary cursor
    
    jmp drawscoredigit_px_common
    
drawscoredigit_px_one:    
    
    stosw
    stosb
    
drawscoredigit_px_common:    
    inc dl
    
    ; Go to next line if necessary
    cmp dl, 4
    jl drawscoredigit_px_nonewline
    
    add di, 308             ; Add offset to make it go to new line
    xor dl, dl              ; Reset counter
    inc dh                  ; Count new line
    
    cmp dh, 3
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
; Local variables
drawscoredigit_color db ?
drawscoredigit_x dw ?
drawscoredigit_y dw ?
;
;------------------------------------------------------------------------------------------------------------                                                                                                                                                                                                             

;--------------------------------------------------------------------------------------------------
; Update score display
; Updates the score display 
; 
updtscoredsply proc near
    push ax
    push bx
    push cx
    push dx
    
    xor ah, ah
    mov al, scorel_prev
    mov cl, 10d
    div cl
 
    ; Clear L least significant digit 
    mov dl, ah
    xor ah, ah
    push ax          
    mov dh, 00h
    mov ax, 137d
    mov bx, 12d
    call drawscoredigit
    
    pop ax
    
    ; Clear L most significant digit 
    mov dl, al          
    mov dh, 00h
    mov ax, 115d
    mov bx, 12d
    call drawscoredigit
    
    xor ah, ah
    mov al, scorer_prev
    mov cl, 10d
    div cl 
    
    ; Clear R least significant digit 
    mov dl, ah
    xor ah, ah
    push ax          
    mov dh, 00h
    mov ax, 193d
    mov bx, 12d
    call drawscoredigit
    
    pop ax
    
    ; Clear R most significant digit 
    mov dl, al          
    mov dh, 00h
    mov ax, 171d
    mov bx, 12d
    call drawscoredigit
    
    xor ah, ah
    mov al, scorel
    mov cl, 10d
    div cl    
    
    ; Draw L least significant digit 
    mov dl, ah
    xor ah, ah
    push ax          
    mov dh, 0Fh
    mov ax, 137d
    mov bx, 12d
    call drawscoredigit
    
    pop ax
    
    ; Draw L most significant digit 
    mov dl, al          
    mov dh, 0Fh
    mov ax, 115d
    mov bx, 12d
    call drawscoredigit
    
    mov al, scorer
    mov cl, 10d
    div cl   
    
    ; Draw R least significant digit 
    mov dl, ah
    xor ah, ah
    push ax          
    mov dh, 0Fh
    mov ax, 193d
    mov bx, 12d
    call drawscoredigit
    
    pop ax
    
    ; Draw R most significant digit 
    mov dl, al          
    mov dh, 0Fh
    mov ax, 171d
    mov bx, 12d
    call drawscoredigit
    
    mov dl, scorel
    mov scorel_prev, dl
    mov dl, scorer
    mov scorer_prev, dl   
                      
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
updtscoredsply endp
;
;------------------------------------------------------------------------------------------------------------

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


;--------------------------------------------------------------------------------------------------
; Syncronize to vertical blanking interval
;
waitsync proc near
    mov dx, 3DAH            ; Port 3DAH accesses the VGA status register
not_blank:     
    in  al, dx
    test al, 8              ; Test vertical retrace bit (the 8's bit in the status byte)
    jnz not_blank           ; If still in vertical retrace yet, wait

    mov dx, 3DAH            ; Port 3DAH accesses the VGA status register
blank: 
    in al,dx
    test al, 8              ; Test vertical retrace bit (the 8's bit in the status byte)
    jz blank                ; Wait for next vertical retrace
    
    ret
waitsync endp 
;
;--------------------------------------------------------------------------------------------------


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
    
    ; Only save presses, not releases
    or al, al
    jz keybdisr_wasrelease
    
    ; Save last key pressed
    mov keypressed, bx
    
keybdisr_wasrelease:

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
             
; ###################################### SOUND FUNCTIONS   

;--------------------------------------------------------------------------------------------------
; Tone loop
; Handle stopping tones
;   
toneloop proc near:
    push cx                    
                        
    mov cx, tone_counter    ; Get current value of tone counter            
    
    ; Check if tone has to be stopped
    cmp cx, 1
    jne no_stop_tone
    
    ; Stop tone
    call toneend
             
no_stop_tone:
    ; Check if the counter is already zero
    or cx, cx
    jz no_decrement_tone_counter
    
    ; Decrement tone counter
    dec cx
    mov tone_counter, cx 

no_decrement_tone_counter:
    
    pop cx
    ret
toneloop endp
;
;--------------------------------------------------------------------------------------------------

;--------------------------------------------------------------------------------------------------
; Play tone
; Play tone with specified frequency and duration (in frames) over PC speaker
; Parameters:
;  - AX: frequency
;  - BX: duration (frames)
;   
playtone proc near:
    
    push cx
    
    ; Check global disable
    mov cl, disable_sound
    or cl, cl
    jnz sound_disabled
    
    ; Set frame counter 
    inc bx
    mov tone_counter, bx;
    
    ; Start playing tone
    call tonestart
    
sound_disabled:
    
    pop cx 
    ret
playtone endp
;
;--------------------------------------------------------------------------------------------------                                                                                     
                                                                                                     
;--------------------------------------------------------------------------------------------------
; Tone start
; Start playing tone with specified frequency over PC speaker
; Parameters:
;  - AX: frequency
;   
tonestart proc near:
    push bx                
    
    ; Set frequency
    mov bx, ax          
    mov al, 182         ; PC speaker control register
    out 43h, al
    mov ax, bx
    out 42h, al         ; Lower byte of frequency
    mov al, ah
    out 42h, al         ; Higher byte of frequency
    
    ; Enable PC speaker output
    in al, 61h
    or al, 03h
    out 61h, al  
    
    pop bx  
    ret
tonestart endp
;
;--------------------------------------------------------------------------------------------------  

;--------------------------------------------------------------------------------------------------
; Tone end
; Stop playing tone over PC Speaker
;   
toneend proc near:
    push ax     
    
    ; Disable PC speaker output
    in al, 61h
    and al, 0FCh
    out 61h, al  
    
    pop ax
    ret
toneend endp
;
;-------------------------------------------------------------------------------------------------- 
    
; NOTES
; 300x200 Resolution means column ranges from 0000h to 013Fh
;                          row ranges from 0000h to 00C7h