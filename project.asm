IDEAL
MODEL small
STACK 100h
DATASEG
    Black equ 0h
    Red equ 0Ch
    Green equ 0Ah
    TileL equ 20d
    KeyboardBufferHead equ 001Ch
    KeyboardBufferTail equ 001Ah

    TitleText db  0Ah, 0Ah, 0Ah, 0Ah, "        Press Any Key To Start$"
    ScoreNumber dw 0FF99h
    ScoreText db '000$'
    Rand dw ?
    FruitPos dw 0508h
    HeadPos dw 0508h
    TailPos dw 0508h
    PrevTailPos dw ?
    HeadDirection dw 0h
    TailDirection dw 0h
    Switcher dw 0FF00h, 0FFh, 0100h, 01h
    Board db 160 dup(5d)

    GameOverText db 0Ah, 0Ah, "Game Over! Press any key to restart.$"
CODESEG
proc CONVERT_POS
    xor ah, ah
    mov al, ch
    shl al, 4
    adc al, cl
    mov di, ax
    ret
endp CONVERT_POS

proc DELAY
    xor ah, ah
    int 1Ah
    mov bx, dx
delayLoop:
    xor ah, ah
    int 1Ah
    sub dx, bx
    cmp dx, si
    jl delayLoop
    ret
endp DELAY

proc DRAW_TILE
    push dx
    push cx
    push di
    xor ah, ah
    mov bh, TileL
    mov al, ch
    mul bh
    mov dx, ax
    mov al, cl
    mul bh
    mov di, ax
    mov al, bl
    mov ah, 0Ch
    xor bh, bh
    mov cx, TileL
row:
    push cx
    mov cx, TileL
line:
    push cx
    mov cx, di
    int 10h
    pop cx
    inc di
    loop line
    pop cx
    inc dx
    sub di, TileL
    loop row
    pop di
    pop cx
    pop dx
    ret
endp DRAW_TILE

proc MOVE
    push bx
    push dx
    mov cx, [word ptr HeadPos]
    mov ax, [word ptr Rand]
    add al, cl
    mov [word ptr Rand], ax
    cmp [word ptr HeadDirection], 0h
    jz erase_tail
    cmp dl, dh
    jz erase_tail
add_tile:
    pop dx
    inc dl
    push dx
    jmp skip
erase_tail:
    mov cx, [word ptr TailPos]
    mov [word ptr PrevTailPos], cx
    call CONVERT_POS
    mov al, [byte ptr ds:bp+di]
    cmp al, 3d
    ja no_direction_change
    shl ax, 1
    mov si, ax
    mov dx, [word ptr bx+si]
    mov [word ptr TailDirection], dx
no_direction_change:
    mov [byte ptr ds:bp+di], 5d
    mov bx, Black
    call DRAW_TILE
    mov ax, [word ptr TailDirection]
    add cl, al
    add ch, ah
    mov [word ptr TailPos], cx
skip:
    mov cx, [word ptr HeadPos]
    mov ax, [HeadDirection]
    add cl, al
    add ch, ah
    call CONVERT_POS
    mov [word ptr HeadPos], cx
    mov bx, Green
    call DRAW_TILE
    cmp cl, 0Fh
    ja lose
    cmp ch, 09h
    ja lose
    cmp [byte ptr ds:bp+di], 5d
    mov [byte ptr ds:bp+di], 4d
    jl lose
    cmp cx, [word ptr FruitPos]
    jz new_fruit
    jmp continue
lose:
    mov ax, 0
    int 10h
    mov ah, 09h
    lea dx, [GameOverText]
    int 21h
    mov [ScoreNumber], 0FF99h
    mov [FruitPos], 0508h
    mov [HeadPos], 0508h
    mov [TailPos], 0508h
    mov [HeadDirection], 0
    mov [TailDirection], 0
    lea bx, [Board]
    xor si, si
    mov cx, 160d
reset_board:
    mov [byte ptr bx+si], 5d
    inc si
    loop reset_board
    xor ah, ah
    int 16h
    jmp setup
new_fruit:
    mov cx, 3d
new_fruit_loop:
    xor di, di
    mov bx, [word ptr Rand]
    mov ax, bx
    and ax, 127d
    add di, ax
    add bl, bh
    mov ax, bx
    and ax, 31d
    add di, ax
    add bl, bh
    mov ax, bx
    and ax, 1d
    add di, ax
    cmp [byte ptr ds:bp+di], 4d
    ja use_random
    rol [word ptr Rand], 1
    loop new_fruit_loop
    mov cx, [PrevTailPos]
    jmp finally
use_random:
    mov ax, di
    mov cx, di
    shr ax, 4d
    mov ch, al
    shl al, 4d
    sub cl, al
finally:
    mov [word ptr FruitPos], cx
    mov bx, Red
    call DRAW_TILE
    pop dx
    inc dh
    push dx
    mov ax, [word ptr ScoreNumber]
    clc
    inc ax
    daa
    adc ah, 0h
    mov [word ptr ScoreNumber], ax
    lea bx, [ScoreText]
    mov dh, ah
    mov ah, al
    and ah, 0Fh
    shr al, 04h
    or ax, 3030h
    mov [word ptr bx+1], ax
    mov ah, dh
    and ah, 0Fh
    or ah, 30h
    mov [byte ptr bx], ah
continue:
    xor bh, bh
    mov dx, 013h
    mov ah, 02h
    int 10h
    mov ah, 09h
    lea dx, [ScoreText]
    int 21h
    push 0040h
    pop es
    mov ax, [es:KeyboardBufferTail]
    mov [es:KeyboardBufferHead], ax
    mov si, 04h
    call DELAY
    pop dx
    pop bx
    ret
endp MOVE

start:
    mov ax, @data
    mov ds, ax
    mov ax, 13h
    int 10h
    xor bh, bh
    mov dx, 060Ah
    mov ah, 02h
    int 10h
    mov ah, 09h
    lea dx, [TitleText]
    int 21h
    xor ah, ah
    int 16h
setup:
    mov ax, 13h
    int 10h
    xor ah, ah
    int 1Ah
    mov [word ptr Rand], dx
    lea bp, [Board]
    lea bx, [Switcher]
    mov dx, 0201h
until_press:
    call MOVE
movement_skip:
    mov ah, 1h
    int 16h
    jz until_press
check_keypress:
    xor ah, ah
    sub ax, 69h
    cmp ax, 3h
    ja until_press
    shl ax, 1
    mov si, ax
    mov ax, [word ptr bx+si]
    test ax, [word ptr HeadDirection]
    jnz until_press
    mov [word ptr HeadDirection], ax
    mov cx, [word ptr HeadPos]
    call CONVERT_POS
    mov ax, si
    shr ax, 1
    mov [byte ptr ds:bp+di], al
    jmp until_press
exit:
    mov ax, 4c00h
    int 21h
END start
