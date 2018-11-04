;;; TODO:
;;; - address TODO/FIXME in file
;;; - make sure procedures preserve ax when they call other procedures that
;;;   return in ax

	;; Null pointer.
	%define NULL 0x0000

	;; Carriage ret char, new line char.
	%define NEWLINE 0x0d, 0x0a

	;; Prefix for interpreter commands.
	%define CMD_PREFIX ':'

	;; Lisp object and object heap sizes.
	%define OBJ_SIZE 8
	%define OBJ_HEAP_SIZE OBJ_SIZE * 50  ; TODO: find good size for demo
	%define LAST_OBJ OBJ_HEAP_SIZE - OBJ_SIZE

	;; Lisp object types.
	%define TYPE_UNIQUE 0x01
	%define TYPE_INT 0x02
	%define TYPE_SYMBOL 0x03
	%define TYPE_PAIR 0x04

	;; Lisp object field offsets.
	%define TYPE 0
	%define VAL 1
	%define NAME 1
	%define CAR 1
	%define CDR 3

	;; Maximum size for a Lisp symbol's name.
	%define MAX_NAME_SIZE OBJ_SIZE - NAME

	BITS 16

	;; General sources used throughout project:
	;; - 16-bit x86 addressing modes: https://stackoverflow.com/a/12474190
	;; - zero-extension: https://stackoverflow.com/a/32836665
	;; - Lecture notes:
	;;   - https://www.cs.uaf.edu/2017/fall/cs301/lecture/09_11_registers.html
	;;   - https://www.cs.uaf.edu/2017/fall/cs301/lecture/09_15_strings_arrays.html
	
;;; ===========================================================================
;;; Boot sector
;;; ===========================================================================

	;; Special thanks to Michael Petch for his help with the boot sector!
	;; https://stackoverflow.com/users/3857942/michael-petch

	;; General sources:
	;; - https://blog.benjojo.co.uk/post/interactive-x86-bootloader-tutorial
	;; - https://en.wikipedia.org/wiki/INT_13H#INT_13h_AH=02h:_Read_Sectors_From_Drive
	;; - https://wiki.osdev.org/Real_Mode#The_Stack
	;; - Michael Petch:
	;;   - https://stackoverflow.com/q/52461308
	;;   - https://stackoverflow.com/q/52463695

	section boot, vstart=0x0000

	lisp_load_start equ 0x0060

	;; Set up the stack above where Lisp is loaded.
	;; 
	;; Set SS (the Stack Segment register) to lisp_load_start and SP
	;; (the Stack Pointer) to 0x0000 so that the stack grows down from
	;; lisp_load_start:0xFFFF toward the "top" of the stack at
	;; lisp_load_start:0x0000.
	mov ax, lisp_load_start
	mov ss, ax
	xor sp, sp

	;; Number of 512B sectors to read from the drive.
        mov al, (lisp_end-lisp_start+511)/512

        mov ch, 0  ; cylinder
        mov cl, 2  ; starting sector
        mov dh, 0  ; drive head

	;; Set ES (the Extra Segment register) to lisp_load_start and BX to
	;; 0x0000 so that we load the sectors into memory starting at
	;; lisp_load_start:0x0000.
	;; 
	;; https://stackoverflow.com/a/32705076
	;; http://www.c-jump.com/CIS77/ASM/Memory/lecture.html#M77_0120_reg_names
        mov bx, lisp_load_start 
        mov es, bx  
        xor bx, bx

	;; Read the sectors.
        mov ah, 0x02
        int 0x13

	;; Far jump to lisp_load_start:0x0000.
	;; 
	;; Set CS (the Code Segment register) to lisp_load_start and the
	;; instruction pointer to 0x0000, so the CPU begins executing
	;; instructions at lisp_load_start:0x0000.
	;; 
	;; https://wiki.osdev.org/Segmentation#Far_Jump
	;; https://stackoverflow.com/a/47249973
	;; http://www.c-jump.com/CIS77/ASM/Memory/lecture.html#M77_0120_reg_names
        jmp lisp_load_start:0x0000

	;; Pad boot sector to boot signature.
	times 510-($-$$) db 0
	db 0x55
	db 0xaa

	section lisp, vstart=0x0000
lisp_start:	

	;; Set DS (the Data Segment register) to lisp_load_start.
	;; 
	;; Michael Petch: https://stackoverflow.com/q/52461308
	;; http://www.c-jump.com/CIS77/ASM/Memory/lecture.html#M77_0120_reg_names
	mov ax, lisp_load_start
	mov ds, ax

	jmp main


;;; ===========================================================================
;;; Main
;;; ===========================================================================

lisp_crash:
;;; Crash the Lisp interpreter.
	jmp .start

	.str db "Lisp has crashed.",0

	.start:

	mov di, .str
	call println

	jmp reboot

main:
;;; Run the Lisp interpreter.
	call init_freelist
	call make_initial_objs

	;; --------------------------------------------------------------------
	;; TODO: temp

	push ax
	push di

	jmp .skiptestobjs

	.test1 dw 0x0000
	.test2 dw 0x0000
	.test3 dw 0x0000

	.symstr1 dw "abcdefg",0
	.symstr2 dw "ttt",0
	.symstr3 dw "bl",0

	.skiptestobjs:

	mov di, .symstr1
	call get_sym
	mov [.test1], ax

	mov di, .symstr2
	call get_sym
	mov [.test2], ax

	mov di, .symstr3
	call get_sym
	mov [.test3], ax

	call print_newline
	mov di, [.test1]
	call print_obj

	call print_newline
	mov di, [.test2]
	call print_obj

	call print_newline
	mov di, [.test3]
	call print_obj

	call print_newline

	pop di
	pop ax
	;; --------------------------------------------------------------------

	jmp .start

	.welcome_str:
	db "Welcome to Lisp!"
	db NEWLINE
	db "Run ",CMD_PREFIX,"help for a list of special commands."
	db NEWLINE
	db 0

	.expected_end_str db "Parse error: expected end of input",0

	.prompt db "> ",0

	.start:

	mov di, .welcome_str
	call println

	;; REPL.
	.loop:

	mov di, .prompt
	call println

	mov di, input_buffer
	call getstr

	;; Check for empty input.
	cmp BYTE [di], 0
	je .loop

	;; Check for a command.
	cmp BYTE [di], CMD_PREFIX
	jne .parse
	call execute_command
	jmp .loop

	;; Parse the input expression.
	.parse:

	;; Fulfill parse's pre.
	call skipspace

	call parse
	cmp ax, NULL
	je .loop

	;; Error if we encounter more input after the parsed expression.
	cmp BYTE [di], 0
	je .print
	call badinput
	mov di, .expected_end_str
	call println
	jmp .loop

	;; TODO: comment: print (the parsed expr? the result of eval?)
	.print:
	
	mov di, ax
	call print_newline
	call print_obj

	jmp .loop


;;; ===========================================================================
;;; Object construction
;;; ===========================================================================

make_initial_objs:
;;; Construct the initial set of Lisp objects.
	call make_emptylist
	ret

make_emptylist:
;;; Construct the Lisp empty list object.
	;; save
	push ax
	push di

	mov BYTE dl, TYPE_UNIQUE
	call get_obj

	mov WORD [emptylist], ax

	;; restore
	pop di
	pop ax

	ret

get_int:
;;; Construct a Lisp int.
;;; Pre: di contains the int value.
;;; Post: ax points to the object.
	;; save
	push bx

	push di  ; Save int value.
	mov BYTE dl, TYPE_INT
	call get_obj
	pop di  ; Restore int value.

	mov bx, ax
	mov WORD [bx+VAL], di

	;; restore
	pop bx

	ret

get_sym:
;;; Construct a Lisp symbol.
;;;
;;; Rather than store a pointer to the string that represents its name, the
;;; constructed symbol object stores the string directly. That is, the space
;;; allocated for the symbol object actually contains the char array that
;;; represents the symbol's name. The obvious disadvantage of this solution is
;;; that the size of the symbol's name cannot exceed OBJ_SIZE.
;;; 
;;; To overcome this limitation, we would need to implement a separate heap for
;;; strings. It's beyond the scope of this project to handle memory
;;; fragmentation, so the easiest solution would be to partition the heap into
;;; equal-sized chunks and allocate one chunk per string, as we do for Lisp
;;; objects. However, we would need to choose a relatively small chunk size
;;; (comparable to OBJ_SIZE) in order to conserve the little memory available
;;; to us in 16-bit mode, so we would lose the advantage of allowing symbol
;;; names to exceed OBJ_SIZE. Thus, symbols may as well store their names
;;; directly.
;;; 
;;; Pre:
;;; - di points to the symbol str.
;;; - The symbol str has length where 0 < length <= MAX_NAME_SIZE.
;;; - The symbol str terminates on 0.
;;; 
;;; Post:
;;; - ax points to the symbol object.
;;; - The copied symbol str begins at the address of the symbol + NAME.
;;; - The copied symbol str terminates on 0 unless its length is MAX_NAME_SIZE.

	;; save
	push bx
	push cx
	push di
	push si

	;; Construct the symbol object.
	push di  ; Save symbol str.
	mov BYTE dl, TYPE_SYMBOL
	call get_obj
	pop di  ; Restore symbol str.

	;; Iterate through the given symbol str and copy it into the symbol
	;; object.

	;; Set si to the address of the symbol + NAME.
	mov si, ax
	add si, NAME

	;; Index into the symbol object.
	xor bx, bx

	jmp .test
	.loop:

	;; Copy the current char from the given symbol str to the current
	;; position in the symbol object.
	mov BYTE cl, [di]
	mov BYTE [si+bx], cl

	inc di  ; Advance to the next char in the given symbol str.
	inc bx  ; Increment index.

	;; Continue the loop until we encounter the given symbol str's
	;; terminating 0.
	.test:
	cmp BYTE [di], 0
	jne .loop

	;; Skip appending the terminating 0 if the symbol str has length equal
	;; to MAX_NAME_SIZE.
	cmp bx, MAX_NAME_SIZE
	je .return

	;; Append the terminating 0.
	mov BYTE [si+bx], 0

	.return:

	;; restore
	pop si
	pop di
	pop cx
	pop bx

	ret

cons:
;;; Construct a Lisp pair.
;;; Pre: di and si contain car and cdr.
;;; Post: ax points to the object.
	;; save
	push bx

	push di  ; Save car.
	mov BYTE dl, TYPE_PAIR
	call get_obj
	pop di  ; Restore car.

	mov bx, ax
	mov WORD [bx+CAR], di
	mov WORD [bx+CDR], si

	;; restore
	pop bx

	ret

get_obj:
;;; Construct a Lisp object with the specified type.
;;; Pre: dl contains the type.
;;; Post: ax points to the object.
	;; save
	push bx
	push si

	jmp .start

	.nofreestr db "Error: no free memory for new object",0

	.start:

	;; Check if the free list is empty.
	cmp WORD [freelist], NULL
	je .nofree

	;; Pop the head off the free list.
	mov WORD bx, [freelist]
	mov WORD si, [bx+CDR]
	mov WORD [freelist], si
	dec WORD [freecount]

	;; Set the object's type and return the object.
	mov BYTE [bx+TYPE], dl
	mov ax, bx
	jmp .return

	.nofree:
	mov di, .nofreestr
	call println
	jmp lisp_crash

	.return:

	;; restore
	pop si
	pop bx

	ret

init_freelist:
;;; Construct the initial list of free objects.
	;; save
	push bx
	push cx

	;; Iterate through the object heap, pointing each object's CDR at the
	;; next object in the heap.

	;; Current object index.
	xor bx, bx

	jmp .test
	.loop:

	;; Point cx at the next object.
	mov cx, obj_heap
	add cx, bx
	add cx, OBJ_SIZE

	;; Point the current object's CDR at the next object.
	mov WORD [obj_heap+bx+CDR], cx

	;; Increment free objects count.
	inc WORD [freecount]

	;; Increment current object index.
	add bx, OBJ_SIZE
	
	;; Continue the loop until we reach the last object.
	.test:
	cmp bx, LAST_OBJ
	jne .loop

	;; Set the last object's CDR to NULL.
	mov WORD [obj_heap+bx+CDR], NULL
	inc WORD [freecount]

	;; Set the head of the free list to the first object in the object
	;; heap.
	mov WORD [freelist], obj_heap

	;; TODO: temp
	call print_freelist

	;; restore
	pop cx
	pop bx

	ret


;;; ===========================================================================
;;; Parse
;;; ===========================================================================

parse:
;;; Convert part of the input str to a Lisp object.
;;; Pre: di points to a non-space char in the input str.
;;; Post:
;;; - ax points to the parsed object.
;;; - di points to the first non-space char after the parsed substr.
;;; On error: Return NULL.

	jmp .start

	.badinputstr db "Parse error: char unrecognized in this context",0

	.start:

	cmp BYTE [di], '('
	jne .skiplist
	inc di
	call skipspace
	call parse_list
	jmp .return

	.skiplist:

	cmp BYTE [di], 0x30
	jl .skipint
	cmp BYTE [di], 0x39
	jg .skipint
	call parse_num
	jmp .return

	.skipint:

	call badinput

	push di
	mov di, .badinputstr
	call println
	pop di

	mov ax, NULL

	.return:
	ret

parse_list:
;;; Convert part of the input str to a Lisp list.
;;; Pre: di points to a non-space char in the input str.
;;; Post:
;;; - ax points to the parsed object.
;;; - di points to the first non-space char after the parsed substr.
;;; On error: Return NULL.

	;; save
	push bx
	push si

	jmp .start

	.incomplete_list_str db "Parse error: incomplete list",0

	.start:

	;; If the current char is ')', we've reached the end of the list.
	cmp BYTE [di], ')'
	jne .not_end

	;; Fulfill post.
	inc di
	call skipspace

	;; Return the empty list object.
	mov ax, [emptylist]
	jmp .return

	.not_end:

	;; If the current char is 0, the list is incomplete.
	cmp BYTE [di], 0
	jne .not_incomplete

	;; Notify the user.
	call badinput
	push di  ; Save input pointer.
	mov di, .incomplete_list_str
	call println
	pop di  ; Restore input pointer.

	;; Return NULL.
	mov ax, NULL
	jmp .return

	.not_incomplete:

	;; Parse the list's car.
	call parse

	;; Check if parse signaled an error.
	cmp ax, NULL
	je .return

	;; Save car.
	mov bx, ax

	;; Parse the list's cdr, which must be another list.
	call parse_list

	;; Check if parse_list signaled an error.
	cmp ax, NULL
	je .return

	push di  ; Save input pointer.

	;; Construct the list object.
	mov di, bx  ; car
	mov si, ax  ; cdr
	call cons

	pop di  ; Restore input pointer.

	.return:

	;; Restore.
	pop si
	pop bx

	ret

parse_num:
;;; Convert part of the input str to a Lisp int.
;;; Pre: di points to a char in the range 0x30-0x39 in the input str.
;;; Post:
;;; - ax points to the parsed object.
;;; - di points to the first non-space char after the parsed substr.
;;; On error: Return NULL.

	;; save
	push bx
	push cx
	push dx
	push si

	jmp .start

	.strbegin dw 0x0000
	.strend dw 0x0000

	.badinputstr db "Parse error: non-numeric char",0
	.overflowstr db "Parse error: number exceeds size limit",0

	.start:

	;; Save the position of the first char for reporting overflow.
	mov WORD [.strbegin], di

	;; Advance to the end of the string by finding the first char that does
	;; not represent a digit 0-9.
	mov bx, di
	.loop_find_end:

	;; Advance to the next char.
	inc bx

	cmp BYTE [bx], '('
	je .exit
	cmp BYTE [bx], ')'
	je .exit
	cmp BYTE [bx], ' '
	je .exit
	cmp BYTE [bx], 0
	je .exit

	cmp BYTE [bx], 0x30
	jl .error
	cmp BYTE [bx], 0x39
	jg .error

	jmp .loop_find_end

	.error:
	mov di, bx
	call badinput
	mov di, .badinputstr
	call println
	mov ax, NULL
	jmp .return

	.exit:

	;; Save the end of the parsed substr so we can fulfill our post.
	mov WORD [.strend], bx

	;; Now go back through the string, adding up the values of the digits
	;; until we have our integer:

	;; Running total.
	xor si, si

	;; Current place in the number, starting at 0.
	;; 10 raised to the current place gives us the place value.
	xor cx, cx

	;; Points to 1B before the start of our string (so we know where to
	;; stop).
	dec di

	;; Start the loop.
	jmp .test

	.loop_add_digits:

	;; Convert the current digit from char to int.
	movzx dx, [bx]
	sub dx, 0x30

	push di  ; save string terminator
	push si	 ; save running total

	;; Calculate the current place value by finding 10 raised to the
	;; current place.
	mov di, 10
	mov si, cx  ; current place
	call power

	pop si  ; restore running total
	pop di  ; restore string terminator

	jo .overflow

	;; Multiply the current digit by the place value and add the result to
	;; our running total.
	imul dx, ax
	jo .overflow
	add si, dx
	jo .overflow

	;; Increment the current place.
	inc cx

	.test:

	;; Move back one char and continue the loop if we haven't reached the
	;; pointer to 1B before the start of our string.
	dec bx
	cmp bx, di
	jg .loop_add_digits

	;; Construct the int object.
	mov di, si  ; The final value of the parsed int.
	call get_int

	;; Fulfill post.
	mov WORD di, [.strend]
	call skipspace
	jmp .return

	.overflow:
	mov WORD di, [.strbegin]
	call badinput
	mov di, .overflowstr
	call println
	mov ax, NULL

	.return:

	;; restore
	pop si
	pop dx
	pop cx
	pop bx

	ret

skipspace:
;;; Move to the next non-space char in the input str.
;;; Pre: di points to the current position in the input str.
;;; Post: di points to the next non-space char.
	jmp .test
	.loop:
	inc di

	.test:
	cmp BYTE [di], ' '
	je .loop

	ret

badinput:
;;; Print an arrow indicating a bad char in the input str.
;;; Pre: di points to the char.
	;; save
	push cx

	jmp .start

	.prestr db "  ",0

	.start:

	;; Print a number of spaces equal to the width of the REPL prompt.
	push di  ; Save input pointer.
	mov di, .prestr
	call println
	pop di  ; Restore input pointer.

	;; Print a number of spaces equal to the bad char's offset from the
	;; start of the input str.

	;; Offset counter.
	mov cx, di
	sub cx, input_buffer

	jmp .test
	.loop:

	mov BYTE dl, ' '
	call putc

	dec cx

	.test:
	cmp cx, 0
	jg .loop

	;; Print an arrow underneath the bad char.
	mov BYTE dl, '^'
	call putc

	;; restore
	pop cx

	ret


;;; ===========================================================================
;;; Print
;;; ===========================================================================

print_obj:
;;; Print a Lisp object.
;;; 
;;; Pre:
;;; - di points to the object.

	;; save
	push di

	jmp .start

	.emptystr db "()",0

	.bugstr:
	db "You have found a bug: cannot print object of unrecognized type",0

	.start:

	;; Empty list:
	
	cmp di, [emptylist]
	jne .skipempty

	mov di, .emptystr
	call print
	jmp .return

	.skipempty:

	;; Pair:

	cmp BYTE [di+TYPE], TYPE_PAIR
	jne .skippair

	call print_pair
	jmp .return

	.skippair:

	;; Symbol:

	cmp BYTE [di+TYPE], TYPE_SYMBOL
	jne .skipsym

	call print_sym
	jmp .return

	.skipsym:

	;; Int:

	cmp BYTE [di+TYPE], TYPE_INT
	jne .skipint

	mov WORD di, [di+VAL]
	call print_num
	jmp .return

	.skipint:

	;; Handle unrecognized type.
	mov di, .bugstr
	call println
	jmp lisp_crash

	.return:
	
	;; restore
	pop di

	ret

print_sym:
;;; Print a symbol.
;;; 
;;; Pre:
;;; - di points to the symbol.
;;; - The symbol's name str has length where 0 < length <= MAX_NAME_SIZE.
;;; - The symbol's name str terminates on 0 unless its length is MAX_NAME_SIZE.

	;; save
	push ax
	push bx
	push di

	add di, NAME  ; Point di at the symbol str.
	xor bx, bx    ; Index into the symbol str.
	mov ah, 0x0e  ; For int 0x10.

	.loop:

	;; Print the current char from the symbol str.
	mov BYTE al, [di+bx]
	int 0x10

	;; Advance to the next char.
	inc bx

	;; Break if we've encountered the terminating 0.
	cmp BYTE [di+bx], 0
	je .break

	;; Break if the symbol str index is MAX_NAME_SIZE.
	cmp bx, MAX_NAME_SIZE
	je .break

	jmp .loop

	.break:

	;; restore
	pop di
	pop bx
	pop ax
	
	ret

print_num:
;;; Print a number.
;;; Pre: di contains the number.

	;; save
	push ax
	push bx
	push cx
	push dx

	;; Set ax to the number.
	mov ax, di

	cmp ax, 0
	jge .positive

	;; ax < 0

	;; Set ax to its absolute value.
	xor bx, bx
	sub bx, ax
	mov ax, bx

	;; Print a minus sign.
	push ax  ; Save our number.
	mov BYTE al, '-'
	mov ah, 0x0e
	int 0x10
	pop ax  ; Restore our number.

	.positive:

	;; ax >= 0

	;; Number of digits pushed onto the stack.
	xor cx, cx

	.parseloop:
	
	;; div divides dx:ax by the operand. ax stores the quotient and dx
	;; stores the remainder.
	;; source: https://stackoverflow.com/a/8022107

	;; Divide our number (ax) by 10.
	xor dx, dx
	mov bx, 10
	div bx

	;; Convert the remainder from int to char and push it.
	add dx, 0x30
	push dx
	inc cx  ; Number of digits pushed onto the stack.

	;; Continue the loop if the quotient != 0.
	cmp ax, 0
	jne .parseloop

	;; Done parsing the number. Now print it:

	jmp .test
	.printloop:

	;; Pop a digit and print it.
	pop dx
	mov BYTE al, dl
	mov ah, 0x0e
	int 0x10

	dec cx  ; Number of digits left on the stack.

	.test:
	cmp cx, 0
	jg .printloop

	;; restore
	pop dx
	pop cx
	pop bx
	pop ax

	ret

print_pair:
;;; Print a chain of Lisp objects beginning with the given pair.
;;; Pre: di points to the pair.
	;; save
	push di

	jmp .start

	.dotstr db " . ",0

	.start:

	;; Iterate through the chain, printing each object's CAR until we
	;; encounter an object that is not a pair.

	;; Print the opening '('.
	push di  ; Save the current object.
	mov dl, '('
	call putc
	pop di  ; Restore the current object.

	.loop:

	;; Invariant: the current object is a pair.

	;; Print the current object's CAR.
	push di  ; Save the current object.
	mov WORD di, [di+CAR]
	call print_obj
	pop di  ; Restore the current object.

	;; Set the current object to its CDR.
	mov WORD di, [di+CDR]

	;; Exit the loop if the current object is not a pair (so it must be the
	;; last object in the chain).
	cmp BYTE [di+TYPE], TYPE_PAIR
	jne .break

	;; Print a space.
	push di  ; Save the current object.
	mov dl, ' '
	call putc
	pop di  ; Restore the current object.

	jmp .loop

	.break:

	;; If the last object in the chain is the empty list, then the chain is
	;; a list and we're done. Otherwise, the chain is not a list and we
	;; must indicate this by printing a dot followed by the last object in
	;; the chain.
	cmp di, [emptylist]
	je .end

	push di
	mov di, .dotstr
	call print
	pop di

	call print_obj

	.end:

	;; Print the closing ')'.
	mov dl, ')'
	call putc

	;; restore
	pop di

	ret


;;; ===========================================================================
;;; Interpreter commands
;;; ===========================================================================

execute_command:
;;; Call a command given an input string.
;;; Pre: di points to the input string.

	;; save
	push bx
	push si

	mov bx, command_table
	jmp .test

	.loop:

	;; Advance to the next command string.
	add bx, 4

	.test:

	;; Compare the current command string with the input string.
	mov WORD si, [bx]
	call compare_strings

	;; Loop if the strings are not equal.
	cmp ax, 0
	je .loop

	;; The command and input strings are equal, so call the procedure that
	;; follows the command string in the table.
	add bx, 2
	call [bx]

	;; restore
	pop si
	pop bx

	ret


;;; ---------------------------------------------------------------------------
;;; Command procedures
;;; ---------------------------------------------------------------------------

help:
;;; Print a list of commands.

	;; save
	push ax
	push bx
	push di

	xor ax, ax
	mov bx, command_table
	jmp .test

	.loop:

	;; Print the current command string.
	mov WORD di, [bx]
	call println

	;; Advance to the next command string.
	add bx, 4
	inc ax

	.test:

	cmp ax, [help_list_len]
	jl .loop

	;; restore
	pop di
	pop bx
	pop ax

	ret

keymap:
;;; Toggle between QWERTY and Dvorak.
	push di  ; save
	jmp .start

	.qwertystr db "Layout: QWERTY",0
	.dvorakstr db "Layout: Dvorak",0

	.start:

	cmp BYTE [dvorak_mode], 0
	je .dvorak

	mov di, .qwertystr
	call println
	mov BYTE [dvorak_mode], 0
	jmp .return

	.dvorak:

	mov di, .dvorakstr
	call println
	mov BYTE [dvorak_mode], 1

	.return:
	pop di  ; restore
	ret

;;; TODO: don't print "See you soon!" if rebooting because lisp crashed
reboot:
;;; Reboot.
	jmp .start

	.str1 db "See you soon!",0
	.str2 db "Press any key to reboot.",0

	.start:

	mov di, .str1
	call println

	mov di, .str2
	call print_newline
	call println

	;; Wait for a keypress.
	mov ah, 0
	int 0x16

	;; Reboot.
	;; https://stackoverflow.com/a/32686533
	db 0x0ea
	dw 0x0000
	dw 0xffff
	
invalid_command:
;;; Handle an invalid command.
	push di  ; save
	jmp .print

	.str db "Invalid command.",0

	.print:
	mov di, .str
	call println

	pop di  ; restore
	ret


;;; ---------------------------------------------------------------------------
;;; Command data
;;; ---------------------------------------------------------------------------

	;; The help command prints the first help_list_len commands from the
	;; command table.
	help_list_len dw 4  ; TODO: check val is correct

;;; Command strings:

	freelist_str db CMD_PREFIX,"free",0
	help_str db CMD_PREFIX,"help",0
	keymap_str db CMD_PREFIX,"keymap",0
	reboot_str db CMD_PREFIX,"restart",0

command_table:
	dw freelist_str
	dw print_freelist

	dw help_str
	dw help

	dw keymap_str
	dw keymap

	dw reboot_str
	dw reboot

	;; Allows execute_command to always call invalid_command if the input
	;; string does not match any of the above command strings.
	dw input_buffer
	dw invalid_command


;;; ===========================================================================
;;; Debugging utilities
;;; ===========================================================================

print_freelist:
;;; Print the list of free Lisp objects.
	;; save
	push bx
	push cx
	push di

	jmp .start

	.colonstr db ": ",0
	.ptrstr db " -> ",0
	.nullstr db "NULL",0
	.freecountstr db "free objects: ",0

	.start:

	call print_newline

	;; Point bx at the head of the free list.
	mov WORD bx, [freelist]

	;; Free objects count, only used for printing.
	xor cx, cx

	jmp .test
	.loop:

	;; Print current object's position in free list.
	mov di, cx
	call print_num

	mov di, .colonstr
	call print

	;; Print address of current object.
	mov di, bx
	call print_num

	mov di, .ptrstr
	call print

	;; Point bx at the next object.
	mov WORD bx, [bx+CDR]

	;; Increment free objects count.
	inc cx

	;; Continue the loop until the current object is NULL.
	.test:
	cmp bx, NULL
	jne .loop

	mov di, .nullstr
	call print

	call print_newline

	;; Print the stored free objects count.
	mov di, .freecountstr
	call println
	mov WORD di, [freecount]
	call print_num

	call print_newline

	;; restore
	pop di
	pop cx
	pop bx

	ret


;;; ===========================================================================
;;; System utilities
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Input
;;; ---------------------------------------------------------------------------

;;; TODO: prevent buffer overflow
getstr:
;;; Read a string from keyboard input.
;;; Pre: di points to an array.
;;; Post: di points to the same array, which now contains the string.

	;; save
	push ax
	push bx

	mov bx, 0  ; input array index

	;; Get one char at a time, adding each one to the input array and
	;; printing it to the screen. Exit upon encountering a carriage return.
	.loop:
	
	;; Read a char to al.
	mov ah, 0
	int 0x16

	;; Check for backspace.
	cmp al, 0x08
	je .backspace

	;; Check for carriage ret (enter).
	cmp al, 0x0d
	je .return

	;; Skip unprintable chars.
	cmp al, 0x20
	jl .loop
	cmp al, 0x7e
	jg .loop

	;; Convert the char from QWERTY to Dvorak if Dvorak is enabled.
	cmp BYTE [dvorak_mode], 0
	je .skipconvert

	call convert_char

	.skipconvert:

	;; Add the char to the input array.
	mov BYTE [di+bx], al

	;; Print the char in al.
	mov ah, 0x0e
	int 0x10

	jmp .increment

	;; Handle a backspace.
	.backspace:

	cmp bx, 0
	je .loop

	dec bx
	mov BYTE [di+bx], 0
	call cursor_backspace
	jmp .loop

	.increment:
	
	inc bx
	jmp .loop

	.return:

	;; Append the null-terminator.
	mov BYTE [di+bx], 0

	;; restore
	pop bx
	pop ax

	ret

cursor_backspace:
;;; Move the cursor back one column and print a null character.

	;; save
	push ax
	push bx
	push cx
	push dx

	;; source:
	;; - https://wiki.osdev.org/Text_Mode_Cursor#Get_Cursor_Data
	;; - https://wiki.osdev.org/Text_Mode_Cursor#Moving_the_Cursor

	;; From source: bh is the "display page (usually, if not always 0)".
	xor bh, bh

	;; Get cursor data. Row and column are returned in dh and dl. (Values
	;; are also returned in ch and cl, which is why this procedure must
	;; preserve cx.)
	mov ah, 0x03
	int 0x10

	;; Move the cursor back one column. Row and column are passed in dh and
	;; dl.
	dec dl
	mov ah, 0x02
	int 0x10

	;; Print a null character. The cursor automatically moves forward one
	;; column.
	mov al, 0
	mov ah, 0x0e
	int 0x10

	;; Move the cursor back one column.
	mov ah, 0x02
	int 0x10

	;; restore
	pop dx
	pop cx
	pop bx
	pop ax

	ret

convert_char:
;;; Convert a character from QWERTY to Dvorak.
;;; Pre: al contains the character as it was entered with QWERTY.
;;; Post: al contains the corresponding Dvorak character.
	push bx  ; save

	;; chars < 0x21 don't need conversion
	cmp al, 0x21
	jl .return

	;; Use the QWERTY char as an index into the Dvorak keymap.
	movzx bx, al
	mov BYTE al, [.dvorak_keymap+bx-0x21]

	.return:
	pop bx  ; restore
	ret

	.dvorak_keymap:
	db "!_#$%&-()*}w[vz0123456789SsW]VZ@AXJE>UIDCHTNMBRL",0x22,"POYGK<QF:/"
	db "\=^{`axje.uidchtnmbrl'poygk,qf;?|+~",0x7f


;;; ---------------------------------------------------------------------------
;;; Output
;;; ---------------------------------------------------------------------------

println:
;;; Print a string on a new line.
;;; Pre: di points to the beginning of the string.
	call print_newline
	call print
	ret

print:
;;; Print a string.
;;; Pre: di points to the beginning of the string.

	;; save
	push ax
	push bx
	
	mov ah, 0x0e

	;; Print each char until encountering a null terminator.

	mov bx, 0
	jmp .test

	.loop:

	mov BYTE al, [di+bx]
	int 0x10
	inc bx

	.test:

	cmp BYTE [di+bx], 0
	jne .loop

	;; restore
	pop bx
	pop ax
	
	ret

print_newline:
;;; Move the cursor to the start of the next line.
	;; save
	push di

	jmp .print
	.str db NEWLINE,0

	.print:
	mov di, .str
	call print

	;; restore
	pop di

	ret

putc:
;;; Print a char.
;;; Pre: dl contains the char.
	;; save
	push ax

	mov ah, 0x0e
	mov BYTE al, dl
	int 0x10

	;; restore
	pop ax

	ret


;;; ---------------------------------------------------------------------------
;;; String operations
;;; ---------------------------------------------------------------------------

compare_strings:
;;; Compare two strings.
;;; Pre: di and si point to the strings.
;;; Post: ax contains 1 if the strings are equal and 0 otherwise.
	push bx  ; save

	mov bx, 0  ; index into each string

	;; Loop through the strings, comparing each pair of chars with the same
	;; index.
	.loop:

	;; Exit the loop and return 0 if the current two chars are not equal.
	mov BYTE al, [si+bx]
	cmp BYTE [di+bx], al
	jne .false

	;; The chars are equal, so exit the loop and return true if we've
	;; reached the null terminator.
	cmp BYTE [di+bx], 0
	je .true

	;; Advance to the next two chars and continue the loop.
	inc bx
	jmp .loop

	.true:
	mov ax, 1
	jmp .return

	.false:
	mov ax, 0

	.return:
	pop bx  ; restore
	ret


;;; ---------------------------------------------------------------------------
;;; Arithmetic
;;; ---------------------------------------------------------------------------

divide:
;;; Divide the first operand by the second operand.
;;; Pre: di contains the first operand and si the second operand.
;;; Post: ax contains the quotient and dx the remainder.

	;; div divides dx:ax by the operand. ax stores the quotient and dx
	;; stores the remainder.
	;; source: https://stackoverflow.com/a/8022107

	;; idiv divides signed numbers in a similar manner. cwd sign-extends ax
	;; into dx:ax.
	;; source: https://stackoverflow.com/a/9073207

	mov ax, di  ; first operand
	cwd
	idiv si  ; second operand

	ret

power:
;;; Raise the first operand to the power of the second operand.
;;; Pre: di contains the first operand and si the second operand.
;;; Post: ax contains the result.
	push si  ; save

	;; Return 1 if the exponent is 0.
	mov ax, 1
	cmp si, 0
	je .return

	mov ax, di  ; running total

	.loop:

	;; Exit the loop if the exponent is 1.
	cmp si, 1
	je .return

	imul ax, di  ; Multiply running total by first operand.
	jo .return
	dec si	     ; Decrement the exponent.
	jmp .loop

	.return:
	pop si  ; restore
	ret


;;; ===========================================================================
;;; Global data
;;; ===========================================================================

	input_buffer times 256 db 0
	dvorak_mode db 1  ; TODO: back to 0 before submit project

	freelist dw 0x0000
	freecount dw 0x0000

	emptylist dw 0x0000

	;; TODO: comment why align (use low bits, e.g. mark in mark-and-sweep)
	;; TODO: may only need align 2; see notebook
	align OBJ_SIZE
	obj_heap times OBJ_HEAP_SIZE db 0

lisp_end:	
