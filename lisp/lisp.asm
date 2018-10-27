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
	%define OBJ_HEAP_SIZE 80  ; TODO: bigger
	%define LAST_OBJ OBJ_HEAP_SIZE - OBJ_SIZE

	;; Lisp object types.
	%define TYPE_UNIQUE 0x01
	%define TYPE_INT 0x02
	%define TYPE_PAIR 0x03

	;; Lisp object field offsets.
	%define TYPE 0
	%define VAL 1
	%define NAME 1
	%define CAR 1
	%define CDR 3

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

	.skiptestobjs:

	mov di, 111
	call get_int
	mov [.test1], ax

	mov di, 222
	call get_int
	mov [.test2], ax

	mov di, [.test1]
	mov si, [.test2]
	;; mov si, [emptylist]
	call cons
	mov [.test3], ax

	call print_newline
	mov di, [emptylist]
	call print_obj

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

	;; TODO: error if [di] not 0

	;; TODO: comment: print (the parsed expr? the result of eval?)
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

;;; TODO: fulfill non-space post
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

;;; TODO:
;;; - fulfill non-space post
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
	push di
	push dx
	push si

	mov bx, di

	;; Advance to the end of the string by finding the first char that does
	;; not represent a digit 0-9.
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
	mov ax, NULL
	jmp .return

	.exit:

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

	jo .return

	;; Multiply the current digit by the place value and add the result to
	;; our running total.
	imul dx, ax
	jo .return
	add si, dx
	jo .return

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

	.return:

	;; restore
	pop si
	pop dx
	pop di
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
;;; Pre: di points to the object.
	;; save
	push di

	jmp .start

	.emptystr db "()",0

	.bugstr:
	db "You have found a bug: cannot print object of unrecognized type",0

	.start:

	cmp di, [emptylist]
	jne .skipempty
	mov di, .emptystr
	call print
	jmp .return

	.skipempty:

	cmp BYTE [di+TYPE], TYPE_INT
	jne .skipint
	mov WORD di, [di+VAL]
	call print_num
	jmp .return

	.skipint:

	cmp BYTE [di+TYPE], TYPE_PAIR
	jne .skippair
	call print_pair
	jmp .return

	.skippair:

	mov di, .bugstr
	call println
	jmp lisp_crash

	.return:
	
	;; restore
	pop di

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

;;; TODO: test w/ longer chains
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
	push di
	mov di, ' '
	call putc
	pop di

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
