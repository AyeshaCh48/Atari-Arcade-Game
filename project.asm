[org 0x0100]
    jmp menu_start

; ==============================================================================
;                                DATA SECTION
; ==============================================================================

; --- MENU STRINGS ---
t1: db 'WELCOME TO THE GAME!',0
t2: db 'Rules:',0
t3: db 'Score increases by hitting targets.',0
t4: db 'You have 3 lives. Avoid losing them.',0
t5: db 'Use Arrow Keys to Move.',0
t6: db 'Press Enter to Start Game',0
t7: db 'Press ESC to Exit Program',0

; --- GAME VARIABLES ---
plen: dw 10
prow: dw 21             ; Paddle Row
pcol: dw 37             ; Paddle Column

; --- BALL SETTINGS ---
brow: dw 14             ; Start in the middle
bcol: dw 40              
bdx:  db 0x01           ; Start Moving DOWN
bdy:  db 0x01           ; Start Moving RIGHT

; --- SPEED CONTROL ---
ball_timer_limit: dw 3000  ; Speed adjustment
ball_timer:       dw 0

score: dw 0
lives: dw 3             ; Start with 3 Lives

; --- GAME STRINGS ---
s:    db 'Score:'
l:    db 'Lives:'
t: db 'Time:'
slen: dw 6
game_over_txt:   db 'GAME OVER'
final_score_txt: db 'Final Score: '

row_char:  db 0
row_color: db 0

; --- BRICK ARRAYS ---
row1: db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
row2: db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
row3: db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
row4: db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1

; ==============================================================================
;                                MENU LOGIC
; ==============================================================================

menu_start:

    call clrscr
    call g              ; Draw ATARICADE logo
    call show_texts     ; Draw Instructions
    call wait_key       ; Wait for Enter or ESC

    mov ax,0x4C00
    int 0x21

; --- WAIT FOR ENTER / ESC ---
wait_key:
    mov ah,0
    int 16h
    cmp al,13           ; Check for ENTER
    je init_game        ; JUMP TO GAME!
    cmp al,27           ; Check for ESC
    je exit_program
    jmp wait_key

exit_program:
    mov ax,0x4C00
    int 0x21

; ==============================================================================
;                                GAME LOGIC
; ==============================================================================

init_game:
    call clrscr
	call boundary
    call dball
    call dbrick

gameloop:
    call scoring   	
    call dpaddle
	
    ; --- SPEED CONTROL ---
    mov ax, [ball_timer]
    inc ax
    mov [ball_timer], ax
   
    cmp ax, [ball_timer_limit]
    jl skip_ball_move      
   
    ; Reset timer and Move Ball
    mov word [ball_timer], 0
    call moveBall

skip_ball_move:
    mov ah,0x01
    INT 0x16        
    jz gameloop

    mov ah,0x00
    INT 0x16        

    ; ESC Key to Exit during game
    cmp ah, 0x01
    je exit_game_jump

    cmp ah,0x4B     ; Left Arrow
    je left
    cmp ah,0x4D     ; Right Arrow
    je right
    jmp gameloop

exit_game_jump:
    jmp game_over

; --- PADDLE MOVEMENT ---
left:
    mov ax,[pcol]
    cmp ax,3
    jle gameloop

    call eraseP
    dec word [pcol]
    call dpaddle
    jmp gameloop

right:
    mov ax, [pcol]
    add ax, [plen]
    cmp ax, 77        
    jge gameloop      
   
    call eraseP
    inc word [pcol]    
    call dpaddle      
    jmp gameloop

; ==============================================================================
;                             SHARED UTILITIES
; ==============================================================================
boundary:
 mov ax,0xb800
 mov es,ax
mov ax,07C4h
mov di,160
b1:
mov word[es:di],ax
add di,2
cmp di,320
jnz b1
sub di,2
add di,160
mov ax,07B3h
b2:
mov word[es:di],ax
add di,160
cmp di,3998
jnz b2
sub di,2
mov ax,07C4h
b3:
mov word[es:di],ax
sub di,2
cmp di,3840
jnz b3
sub di,160
mov ax,07B3h
b4:
mov word[es:di],ax
sub di,160
cmp di,160
jnz b4
ret
; --- CLEAR SCREEN (Used by both Menu and Game) ---
clrscr:
    mov ax,0xb800
    mov es,ax
    xor di,di
    mov ax,0x0720
    mov cx,2000
    rep stosw
    ret
	

; ==============================================================================
;                             MENU DRAWING FUNCTIONS
; ==============================================================================

; --- PRINT STRING HELPER ---
print_string:
next_char:
    lodsb
    cmp al,0
    je done_print
    stosw
    jmp next_char
done_print:
    ret

show_texts:
    mov ax,0xb800
    mov es,ax

    ; Row 12, Col 5 (t1)
    mov bx,12
    mov dx,5
    call calc_text_pos
    mov si,t1
	mov ah,0Eh
    call print_string

    ; Row 14, Col 5 (t2)
    mov bx,14
    mov dx,5
    call calc_text_pos
    mov si,t2
	mov ah,0Eh
    call print_string

    ; Row 16, Col 5 (t3)
    mov bx,16
    mov dx,5
    call calc_text_pos
    mov si,t3
	mov ah,0Eh
    call print_string

    ; Row 18, Col 5 (t4)
    mov bx,18
    mov dx,5
    call calc_text_pos
    mov si,t4
	mov ah,0Eh
    call print_string

    ; Row 20, Col 5 (t5)
    mov bx,20
    mov dx,5
    call calc_text_pos
    mov si,t5
	mov ah,0Eh
    call print_string

    ; Row 22, Col 5 (t6)
    mov bx,22
    mov dx,5
    call calc_text_pos
    mov si,t6
	mov ah,07h
    call print_string

    ; Row 24, Col 5 (t7)
    mov bx,24
    mov dx,5
    call calc_text_pos
    mov si,t7
	mov ah,07h
    call print_string
    ret

calc_text_pos:
    mov di,bx
    shl di,7
    mov si,bx
    shl si,5
    add di,si
    shl dx,1
    add di,dx
    ret

g:
    push bp
    mov bp,sp
    mov ax,0xb800
    mov es,ax
    mov ax,0x0CDB

    ; DRAWING ATARICADE LOGO
    mov word[es:810],ax
    mov word[es:812],ax
    mov word[es:814],ax
    mov word[es:816],ax
    mov word[es:970],ax
    mov word[es:1130],ax
    mov word[es:1290],ax
    mov word[es:1450],ax
    mov word[es:1132],ax
    mov word[es:1134],ax
    mov word[es:1136],ax
    mov word[es:976],ax
    mov word[es:1136],ax
    mov word[es:1296],ax
    mov word[es:1456],ax

    ; T
    mov word[es:820],ax
    mov word[es:822],ax
    mov word[es:824],ax
    mov word[es:826],ax
    mov word[es:828],ax
    mov word[es:984],ax
    mov word[es:1144],ax
    mov word[es:1304],ax
    mov word[es:1464],ax

    ; A
    mov word[es:832],ax
    mov word[es:834],ax
    mov word[es:836],ax
    mov word[es:838],ax
    mov word[es:992],ax
    mov word[es:1152],ax
    mov word[es:1312],ax
    mov word[es:1472],ax
    mov word[es:1154],ax
    mov word[es:1156],ax
    mov word[es:1158],ax
    mov word[es:998],ax
    mov word[es:1158],ax
    mov word[es:1318],ax
    mov word[es:1478],ax

    ; R
    mov word[es:842],ax
    mov word[es:1002],ax
    mov word[es:1162],ax
    mov word[es:1322],ax
    mov word[es:1482],ax
    mov word[es:844],ax
    mov word[es:846],ax
    mov word[es:848],ax
    mov word[es:850],ax
    mov word[es:1010],ax
    mov word[es:1170],ax
    mov word[es:1168],ax
    mov word[es:1166],ax
    mov word[es:1164],ax
    mov dx,0x0C5C
    mov word[es:1324],dx
    mov word[es:1488],dx

    ; I
    mov word[es:854],ax
    mov word[es:856],ax
    mov word[es:858],ax
    mov word[es:860],ax
    mov word[es:862],ax
    mov word[es:1018],ax
    mov word[es:1178],ax
    mov word[es:1338],ax
    mov word[es:1494],ax
    mov word[es:1496],ax
    mov word[es:1498],ax
    mov word[es:1500],ax
    mov word[es:1502],ax

    ; A
    mov word[es:870],ax
    mov word[es:872],ax
    mov word[es:874],ax
    mov word[es:876],ax
    mov word[es:1030],ax
    mov word[es:1190],ax
    mov word[es:1350],ax
    mov word[es:1510],ax
    mov word[es:1192],ax
    mov word[es:1194],ax
    mov word[es:1196],ax
    mov word[es:1036],ax
    mov word[es:1196],ax
    mov word[es:1356],ax
    mov word[es:1516],ax

    ; R
    mov word[es:880],ax
    mov word[es:1040],ax
    mov word[es:1200],ax
    mov word[es:1360],ax
    mov word[es:1520],ax
    mov word[es:882],ax
    mov word[es:884],ax
    mov word[es:886],ax
    mov word[es:888],ax
    mov word[es:1048],ax
    mov word[es:1208],ax
    mov word[es:1206],ax
    mov word[es:1204],ax
    mov word[es:1202],ax
    mov dx,0x0C5C
    mov word[es:1362],dx
    mov word[es:1526],dx

    ; C
    mov word[es:892],ax
    mov word[es:894],ax
    mov word[es:896],ax
    mov word[es:898],ax
    mov word[es:1058],ax
    mov word[es:1052],ax
    mov word[es:1212],ax
    mov word[es:1372],ax
    mov word[es:1532],ax
    mov word[es:1534],ax
    mov word[es:1536],ax
    mov word[es:1538],ax
    mov word[es:1378],ax

    ; A
    mov word[es:902],ax
    mov word[es:904],ax
    mov word[es:906],ax
    mov word[es:908],ax
    mov word[es:1062],ax
    mov word[es:1222],ax
    mov word[es:1382],ax
    mov word[es:1542],ax
    mov word[es:1224],ax
    mov word[es:1226],ax
    mov word[es:1228],ax
    mov word[es:1068],ax
    mov word[es:1228],ax
    mov word[es:1388],ax
    mov word[es:1548],ax

    ; D
    mov word[es:912],ax
    mov word[es:914],ax
    mov word[es:916],ax
    mov word[es:918],ax
    mov word[es:1072],ax
    mov word[es:1232],ax
    mov word[es:1392],ax
    mov word[es:1552],ax
    mov word[es:1554],ax
    mov word[es:1556],ax
    mov word[es:1558],ax
    mov word[es:1078],ax
    mov word[es:1238],ax
    mov word[es:1398],ax

    ; E
    mov word[es:922],ax
    mov word[es:924],ax
    mov word[es:926],ax
    mov word[es:928],ax
    mov word[es:1082],ax
    mov word[es:1242],ax
    mov word[es:1402],ax
    mov word[es:1562],ax
    mov word[es:1564],ax
    mov word[es:1566],ax
    mov word[es:1568],ax
    mov word[es:1244],ax
    mov word[es:1246],ax
    mov word[es:1248],ax

    pop bp
    ret

; ==============================================================================
;                             GAME FUNCTIONS
; ==============================================================================


;-----------------Paddle------------------
dpaddle:
    mov ax,0xb800
    mov es,ax
    mov ax, [prow]
    mov bx, 80
    mul bx              
    add ax, [pcol]        
    shl ax, 1        
    mov di, ax  
    mov dx,0x0EDB
    mov si,0
p1:
    add si,1
    mov word[es:di],dx
    add di,2
    cmp si,[plen]
    jnz p1
    ret

eraseP:
    push dx
    mov ax,0xb800
    mov es,ax
    mov ax, [prow]
    mov bx, 80
    mul bx              
    add ax, [pcol]        
    shl ax, 1        
    mov di, ax  
    mov dx,0x0D20
    mov si,0
    mov cx,[plen]
e1:
    mov word[es:di],dx
    add di,2
    loop e1
    pop dx
    ret

scoring:
    push es
    push di
    push si
    push ax
    push bx
    push cx
    push dx

    mov ax, 0xb800
    mov es, ax
   
    ; Display "Score:"
    mov di, 6
    mov si, s
    mov cx, [slen]
    mov ah, 0x0E
    cld
nextchar:
    lodsb
    stosw
    loop nextchar
   
    add di, 2
    mov ax, [score]
    call print_number  

    ; Display "Lives:"
   mov di,86
    mov si, l
    mov cx, [slen]
    mov ah, 0x0E
    cld
next:
    lodsb
    stosw
    loop next

    add di, 2
	push di
	mov cx,3
	mov ax,0x0720
	 n1:
    mov word [es:di], ax
	add di,4
	loop n1
	
	pop di
    mov cx, [lives]
    mov ax,0E03h
   n2:
    mov word [es:di], ax
	add di,4
	loop n2
	
    pop dx
    pop cx
    pop bx
    pop ax
    pop si
    pop di
    pop es
    ret

print_number:
    push ax
    push bx
    push cx
    push dx
   
    mov bx, 10
    mov cx, 0
d1_lbl:
    mov dx, 0
    div bx
    push dx
    add cx, 1
    cmp ax, 0
    jnz d1_lbl
d2_lbl:
    pop dx
    add dl, 30h
    mov dh, 0x0E
    mov word [es:di], dx
    add di, 2
    loop d2_lbl

    pop dx
    pop cx
    pop bx
    pop ax
    ret
;----------------------BALL---------------------
dball:
    mov ax,0xb800
    mov es,ax
    mov ax,[brow]
    mov bx,80
    mul bx
    add ax,[bcol]
    shl ax,1
    mov di,ax
    mov dh,0x03
    mov dl,9
    mov word[es:di],dx
    ret

eraseball:
    mov ax,0xb800
    mov es,ax
    mov ax,[brow]
    mov bx,80
    mul bx
    add ax,[bcol]
    shl ax,1
    mov di,ax
    mov dx,0x0020
    mov word[es:di],dx
    ret

flip_bxdx: ;vertical   +ve down -ve up
    push ax
    mov al, [bdx]
    xor al, 0xFF        
    inc al              
    mov [bdx], al
    pop ax
    ret

flip_bxdy:  ;horizontal  +ve right -ve left
    push ax
    mov al, [bdy]
    xor al, 0xFF        
    inc al              
    mov [bdy], al
    pop ax
    ret

moveBall:
    call eraseball
   
    ; Update Position
    mov al, [bdx]
    add [brow], al      
    mov al, [bdy]
    add [bcol], al      

    ; Wall collision (Left/Right)
    cmp word[bcol], 2
    je bounceH
    cmp word[bcol], 77
    je bounceH
    jmp checkV

bounceH:
    call flip_bxdy
    jmp check_bricks

checkV:
    ; Top Ceiling Check
    cmp word [brow], 2
    je bounceV
   
    ; Paddle Check
    cmp word [brow], 21
    je checkPaddle  
    jmp checkBottom

bounceV:
    call flip_bxdx
    jmp check_bricks

checkPaddle:
    mov ax, [bcol]
    cmp ax, [pcol]          ; Left edge of paddle
    jl checkBottom          ; If Left of paddle, it misses
   
    mov ax, [bcol]
    mov bx, [pcol]
    add bx, [plen]          ; Right edge of paddle
    cmp ax, bx              
    jg checkBottom          ; If Right of paddle, it misses
   
    ; HIT PADDLE
	call play_paddle     ;SOUND
    ;mov byte [bdx], 0xFF    ; Bounce UP (-1)
	call flip_bxdx
    jmp check_bricks        

checkBottom:
    cmp word [brow], 23
    jge lose_life
    jmp check_bricks

lose_life:
    call eraseball
    mov ax, [lives]
    dec ax
    mov [lives], ax
    cmp ax, 0
    jnz respawn_ball        
    jmp game_over          

respawn_ball:
    mov word [brow], 14    
    mov word [bcol], 40    
    mov byte [bdx], 0x01    
    mov byte [bdy], 0x01    
   
    call dball              
    call scoring            
   
    ; Small delay
    mov cx, 0
pause_loop:
    inc cx
    cmp cx, 0xFFFF
    jne pause_loop
   
    ret                    

check_bricks:
    call removeBrick
    call dball              
    ret

; -------------BRICK FUNCTIONS -------------
row_pattern:
    push di
    push cx
    push si
pattern_loop:
    mov al, [row_char]
    mov ah, [row_color]
    cmp byte[si], 0
    je skip_brick
    mov [es:di], ax
    add di, 2
    mov [es:di], ax
    add di, 2
	 mov [es:di], ax
    add di, 2
	 mov [es:di], ax
    add di, 2
	 mov [es:di], ax
    add di, 2
	 mov [es:di], ax
    add di, 2
	 mov [es:di], ax
    add di, 2
    mov al, ' '
    mov ah, [row_color]
    mov [es:di], ax
    add di, 2
    jmp next_brick_iter
skip_brick:
    mov ax, 0x0720
	mov bx,8
	skip1:
    mov [es:di], ax
    add di, 2
    dec bx
	cmp bx,0
	jnz skip1
next_brick_iter:
    add si, 1
    loop pattern_loop
    pop si
    pop cx
    pop di
    ret

row_offset:
    mov cx, si
	mov di,0
	f1:
	add di,160
	loop f1
    add di, 6
    ret

dbrick:
    mov ax, 0B800h
    mov es, ax

    mov si, 3        
    call row_offset  
    mov byte [row_char], 219
    mov byte [row_color], 0Ch  
    mov cx, 9
    mov si, row1
    call row_pattern

    mov si, 5        
    call row_offset
    mov byte [row_char], 219
    mov byte [row_color], 0Eh  
    mov cx, 9
    mov si, row2
    call row_pattern

    mov si, 7        
    call row_offset
    mov byte [row_char], 219
    mov byte [row_color], 0Ah  
    mov cx, 9
    mov si, row3
    call row_pattern

    mov si, 9        
    call row_offset
    mov byte [row_char], 219
    mov byte [row_color], 0Bh  
    mov cx, 9
    mov si, row4
    call row_pattern
    ret

removeBrick:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es              

    mov ax, 0xb800  
    mov es, ax  
    mov ax, [brow]
    cmp ax, 3
    je r1
    cmp ax, 5
    je r2
    cmp ax, 7
    je r3
    cmp ax, 9
    je r4
    jmp d_exit

r1:
    mov si, 3
    call row_offset
    mov si, row1
    mov cx, 9
    jmp calc_pos
r2:
    mov si, 5
    call row_offset
    mov si, row2
    mov cx, 9
    jmp calc_pos
r3:
    mov si, 7
    call row_offset
    mov si, row3
    mov cx, 9
    jmp calc_pos
r4:
    mov si, 9
    call row_offset
    mov si, row4
    mov cx, 9
    jmp calc_pos

calc_pos:
    mov ax, [brow]
    mov bx, 80
    mul bx
    add ax, [bcol]
    shl ax, 1

l1_scan:
    cmp di, ax
    je swap
    add di, 2
    cmp di, ax
    je swap
	add di, 2
    cmp di, ax
    je swap
	add di, 2
    cmp di, ax
    je swap
	add di, 2
    cmp di, ax
    je swap
	add di, 2
    cmp di, ax
    je swap
	add di, 2
    cmp di, ax
    je swap
    add di, 4
    add si, 1
    loop l1_scan
    jmp d_exit

swap:
    cmp word[es:di], 0x0720  
    je d_exit
   call play_brick
    ; HIT DETECTED
    mov byte [si], 0        
    add word [score], 20  
   
    call flip_bxdx          
    call dbrick              
   
d_exit:
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

; ==============================================================================
;                                GAME OVER SCREEN
; ==============================================================================

game_over:
    call clrscr
    mov ax, 0xb800
    mov es, ax

    mov di, 1990        
    mov si, game_over_txt
    mov cx, 9          
    mov ah, 0x0E        
print_go:
    lodsb
    stosw
    loop print_go

    mov di, 2150        
    mov si, final_score_txt
    mov cx, 13
    mov ah, 0x0E       
print_final:
    lodsb
    stosw
    loop print_final

    mov ax, [score]
    call print_number  

    mov ah, 0
    int 0x16

    mov ax, 0x4c00
    int 0x21

; ==============================================================================
;                               SOUND UTILITIES
; ==============================================================================

play_brick:
    push ax
    push bx
    push cx
    push dx

    ; 1. SET FREQUENCY (500 Hz)
    ; Divisor D = 1193180 / Freq (200) = 5965
    mov ax, 795        ; Divisor (0x04A9)
    mov bl, 0xB6        ; Control word for PIT (Channel 2, LSB then MSB, Mode 3)
    
    out 0x43, al        ; Write control word to PIT Command Port
    mov al, bl
    out 0x42, al        ; Write LSB of divisor to PIT Channel 2
    mov al, ah
    out 0x42, al        ; Write MSB of divisor to PIT Channel 2

    ; 2. TURN SPEAKER ON
    in al, 0x61         ; Read current state of PPI Port B
    or al, 0x03         ; Set bits 0 and 1 (Enable PIT Channel 2 and Speaker)
    out 0x61, al        ; Write new value back

    ; 3. DELAY (Approx 100ms)
    mov cx, 3000        ; Loop count for a short delay
delay_loop:
    loop delay_loop     ; Use LOOP for efficient decrement and jump

    ; 4. TURN SPEAKER OFF
    in al, 0x61         ; Read current state of PPI Port B
    and al, 0xFC        ; Clear bits 0 and 1 (Disable PIT Channel 2 and Speaker)
    out 0x61, al        ; Write new value back

    pop dx
    pop cx
    pop bx
    pop ax
    ret
	
	play_paddle:
    push ax
    push bx
    push cx
    push dx

    ; Divisor D = 1193180 / Freq (250) = 1193
    mov ax, 4773        
    mov bl, 0xB6       
    
    out 0x43, al        
    mov al, bl
    out 0x42, al        
    mov al, ah
    out 0x42, al        

    in al, 0x61         
    or al, 0x03        
    out 0x61, al        

    mov cx, 5000       
delay_loopP:
    loop delay_loop    
 
    in al, 0x61         
    and al, 0xFC       
    out 0x61, al        

    pop dx
    pop cx
    pop bx
    pop ax
    ret