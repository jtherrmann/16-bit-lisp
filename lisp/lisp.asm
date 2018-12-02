;;; TODO:
;;; - address TODO/FIXME in file
;;; - make sure procedures preserve ax when they call other procedures that
;;;   return in ax
;;; - update docstring comments for consistent formatting

	BITS 16

	;; General sources used throughout project:
	;; - 16-bit x86 addressing modes: https://stackoverflow.com/a/12474190
	;; - zero-extension: https://stackoverflow.com/a/32836665
	;; - Lecture notes:
	;;   - https://www.cs.uaf.edu/2017/fall/cs301/lecture/09_11_registers.html
	;;   - https://www.cs.uaf.edu/2017/fall/cs301/lecture/09_15_strings_arrays.html

;;; ===========================================================================
;;; Macros
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; General
;;; ---------------------------------------------------------------------------

	;; Null pointer.
	%define NULL 0x0000

	;; Carriage ret char, new line char.
	%define NEWLINE 0x0d, 0x0a

	;; Prefix for interpreter commands.
	%define CMD_PREFIX ':'

	;; Input buffer size.
	%define INPUT_SIZE 64


;;; ---------------------------------------------------------------------------
;;; Lisp object
;;; ---------------------------------------------------------------------------

;;; A Lisp object is a chunk of memory OBJ_SIZE bytes in size:

	%define OBJ_SIZE 8


;;; Each of its fields is offset from the start of the object by some number of
;;; bytes. All objects have a TYPE field with an offset of 0:

	;; Object type: any
	;; Field type:  Lisp object type
	;; Field size:  BYTE
	%define TYPE 0


;;; The remaining fields overlap in memory and each is specific to a particular
;;; type of object:

	;; Object type: TYPE_INT
	;; Field type:  integer
	;; Field size:  WORD
	%define VAL 1

	;; Object type: TYPE_SYMBOL
	;; Field type:  string
	;; Field size:  0 < size <= MAX_NAME_SIZE
	;; 
	;; For a detailed explanation of how and why symbols store their names,
	;; see the documentation for the get_sym function.
	%define NAME 1

	;; Object types: TYPE_BUILTIN_1, TYPE_BUILTIN_2
	;; Field type:   pointer to assembly function
	;; Field size:   WORD
	%define FUNC 1

	;; Object types: TYPE_BUILTIN_1, TYPE_BUILTIN_2
	;; Field type:   pointer to Lisp symbol
	;; Field size:   WORD
	%define FUNC_NAME 3

	;; Object type: TYPE_PAIR
	;; Field type:  pointer to Lisp object
	;; Field size:  WORD
	%define CAR 1
	%define CDR 3

	;; Object type: TYPE_PAIR
	;; Field type:  boolean (0 or 1)
	;; Field size:  BYTE
	%define IS_LIST 5


;;; Lisp object types:

	%define TYPE_UNIQUE 0x01
	%define TYPE_INT 0x02
	%define TYPE_SYMBOL 0x03
	%define TYPE_PAIR 0x04
	%define TYPE_BUILTIN_1 0x05  ; 1-arg builtin function
	%define TYPE_BUILTIN_2 0x06  ; 2-arg builtin function


;;; Symbol name size:

	%define MAX_NAME_SIZE OBJ_SIZE - NAME


;;; Lisp object heap size:

	%define OBJ_HEAP_SIZE OBJ_SIZE * 200  ; TODO: find good size for demo

	
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

	.msg db "Lisp has crashed.",0

	.start:

	mov di, .msg
	jmp reboot_comp

main:
;;; Run the Lisp interpreter.

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

	call init_freelist
	call make_initial_objs

	mov di, .welcome_str
	call println

	;; REPL.
	.loop:

	mov di, .prompt
	call println

	call getstr
	mov di, input_buffer

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
	je .eval
	call badinput
	mov di, .expected_end_str
	call println
	jmp .loop

	;; Evaluate the parsed expression.
	.eval:

	mov di, ax
	mov BYTE [toplevel], 1
	call eval

	cmp ax, NULL
	je .loop
	
	;; Print eval's return value.
	mov di, ax
	call print_newline
	call print_obj

	jmp .loop


;;; ===========================================================================
;;; Object construction
;;; ===========================================================================

make_initial_objs:
;;; Construct the initial set of Lisp objects.

	;; save
	push ax
	push di
	push si

	jmp .start

	;; Special form symbols.
	.quotestr db "quote",0
	.definestr db "define",0
	.condstr db "cond",0
	.lambdastr db "lambda",0

	;; Builtin function names.
	.consstr db "cons",0
	.carstr db "car",0
	.cdrstr db "cdr",0
	.evalstr db "eval",0

	.start:

	;; Make the empty list.
	call make_emptylist

	;; Initialize the global env by setting it to the empty list.
	mov WORD di, [emptylist]
	mov WORD [globalenv], di

	;; Make the quote symbol.
	mov di, .quotestr
	call get_sym
	mov WORD [quotesym], ax

	;; Make the define symbol.
	mov di, .definestr
	call get_sym
	mov WORD [definesym], ax

	;; Make the cond symbol.
	mov di, .condstr
	call get_sym
	mov WORD [condsym], ax

	;; Make the lambda symbol.
	mov di, .lambdastr
	call get_sym
	mov WORD [lambdasym], ax

	;; Make the builtin function cons.
	mov di, cons
	mov si, .consstr
	mov dl, TYPE_BUILTIN_2
	call make_builtin_function

	;; Make the builtin function car.
	mov di, builtin_car
	mov si, .carstr
	mov dl, TYPE_BUILTIN_1
	call make_builtin_function

	;; Make the builtin function cdr.
	mov di, builtin_cdr
	mov si, .cdrstr
	mov dl, TYPE_BUILTIN_1
	call make_builtin_function

	;; Make the builtin function eval.
	mov di, eval
	mov si, .evalstr
	mov dl, TYPE_BUILTIN_1
	call make_builtin_function

	;; restore
	pop si
	pop di
	pop ax

	ret

make_emptylist:
;;; Construct the Lisp empty list object.
	;; save
	push ax
	push dx

	mov BYTE dl, TYPE_UNIQUE
	call get_obj

	mov WORD [emptylist], ax

	;; restore
	pop dx
	pop ax

	ret

get_int:
;;; Construct a Lisp int.
;;; Pre: di contains the int value.
;;; Post: ax points to the object.
	;; save
	push bx
	push dx

	mov BYTE dl, TYPE_INT
	call get_obj

	mov bx, ax
	mov WORD [bx+VAL], di

	;; restore
	pop dx
	pop bx

	ret

get_sym:
;;; Construct a Lisp symbol.
;;;
;;; Rather than store a pointer to the string that represents its name, the
;;; constructed symbol object stores the string directly. That is, the space
;;; allocated for the symbol object also stores the char array that represents
;;; the symbol's name. The obvious disadvantage of this solution is that the
;;; size of the symbol's name cannot exceed OBJ_SIZE.
;;; 
;;; In order to overcome this limitation we would need to implement a separate
;;; heap for strings. Handling memory fragmentation is beyond the scope of this
;;; project, so the easiest workaround would be to partition the heap into
;;; equal-sized chunks and allocate one chunk per string, as we do for Lisp
;;; objects. However, we would need to choose a small chunk size in order to
;;; conserve the little memory available to us in 16-bit mode, so we would lose
;;; the advantage of allowing symbol names to exceed OBJ_SIZE.
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
	push dx
	push si

	;; Construct the symbol object.
	mov BYTE dl, TYPE_SYMBOL
	call get_obj

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
	pop dx
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
	push dx

	mov BYTE dl, TYPE_PAIR
	call get_obj

	;; Set the pair's car and cdr.
	mov bx, ax
	mov WORD [bx+CAR], di
	mov WORD [bx+CDR], si

	;; A list is the empty list or a pair whose cdr is a list. Determine
	;; whether the new pair is a list.

	;; The new pair is a list if its cdr is the empty list.
	cmp WORD si, [emptylist]
	je .islist

	;; Otherwise, the new pair is not a list if its cdr is not a pair.
	cmp BYTE [si+TYPE], TYPE_PAIR
	jne .notlist

	;; Otherwise, the new pair is a list if its cdr is a list.
	cmp BYTE [si+IS_LIST], 1
	je .islist

	.notlist:
	mov BYTE [bx+IS_LIST], 0
	jmp .return

	.islist:
	mov BYTE [bx+IS_LIST], 1

	.return:

	;; restore
	pop dx
	pop bx

	ret

make_builtin_function:
;;; Construct a builtin Lisp function and bind it to its name.
;;;
;;; Pre:
;;; - di points to the assembly function.
;;; - si points to a string representing the function's name.
;;; - dl contains TYPE_BUILTIN_1 or TYPE_BUILTIN_2, depending on whether the
;;;   assembly function takes 1 or 2 arguments.

	;; save
	push ax
	push bx
	push di
	push si

	;; Construct a builtin function of the given type.
	call get_obj
	mov bx, ax

	;; Store the given assembly function pointer with the builtin function.
	mov WORD [bx+FUNC], di
	
	;; Construct a symbol representing the builtin function's name.
	mov di, si  ; Given string.
	call get_sym

	;; Store the symbol with the builtin function.
	mov WORD [bx+FUNC_NAME], ax

	;; Bind the symbol to the builtin function.
	mov di, ax  ; Symbol.
	mov si, bx  ; Builtin function.
	call bind

	;; restore
	pop si
	pop di
	pop bx
	pop ax

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
	%define LAST_OBJ OBJ_HEAP_SIZE - OBJ_SIZE

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

	;; restore
	pop cx
	pop bx

	ret


;;; ===========================================================================
;;; Parse
;;; ===========================================================================

parse:
;;; Convert part of the input str to a Lisp object.
;;; 
;;; Pre:
;;; - di points to a non-space char in the input str.
;;; 
;;; Post:
;;; - ax points to the parsed object.
;;; - di points to the first non-space char after the parsed substr.
;;; 
;;; On error:
;;; - Return NULL.

	jmp .start

	.badinputstr db "Parse error: char unrecognized in this context",0

	.start:

	;; List:

	cmp BYTE [di], '('
	jne .skiplist

	inc di
	call skipspace
	call parse_list
	jmp .return

	.skiplist:

	;; Symbol:

	call is_sym_start_char
	cmp ax, 0
	je .skipsym

	call parse_sym
	jmp .return

	.skipsym:

	;; Int:

	cmp BYTE [di], 0x30
	jl .skipint
	cmp BYTE [di], 0x39
	jg .skipint

	call parse_int
	jmp .return

	.skipint:

	;; Unrecognized type:

	call badinput

	push di  ; Save input pointer.
	mov di, .badinputstr
	call println
	pop di  ; Restore input pointer.

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

parse_sym:
;;; Convert part of the input str to a Lisp symbol.
;;; 
;;; Pre:
;;; - di points to a char, in the input str, for which is_sym_start_char is
;;;   true.
;;; 
;;; Post:
;;; - di points to the first non-space char after the parsed substr.
;;; 
;;; On error:
;;; - Return NULL.

	;; save
	push bx
	push cx

	jmp .start

	;; Temporary symbol str buffer.
	.symstr times MAX_NAME_SIZE+1 db 0

	.badinputstr db "Parse error: symbol contains invalid char",0
	.toolongstr db "Parse error: symbol exceeds maximum length",0

	.start:

	;; Copy the input symbol str to the temporary buffer.

	;; Index into the temporary buffer.
	xor bx, bx

	.loop:

	;; Break if we encounter the beginning of a list.
	cmp BYTE [di], '('
	je .break

	;; Break if we encounter the end of a list.
	cmp BYTE [di], ')'
	je .break

	;; Break if we encounter a space.
	cmp BYTE [di], ' '
	je .break

	;; Break if we encounter the end of the input expression.
	cmp BYTE [di], 0
	je .break

	;; Check if we've exceeded the maximum allowable size for symbol names.
	cmp bx, MAX_NAME_SIZE
	je .toolong

	;; Check if the current char is a valid symbol char.
	call is_sym_char
	cmp ax, 0
	je .badinput

	;; Copy the current char from the input symbol str to the temporary
	;; buffer.
	mov BYTE cl, [di]
	mov BYTE [.symstr + bx], cl

	inc di	; Advance to the next char in the input symbol str.
	inc bx  ; Increment temporary buffer index.
	jmp .loop

	;; The input symbol str is too long. Notify the user and return NULL.
	.toolong:

	call badinput

	push di  ; Save input pointer.
	mov di, .toolongstr
	call println
	pop di	 ; Restore input pointer.

	mov ax, NULL
	jmp .return

	;; The input symbol str contains an invalid char. Notify the user and
	;; return NULL.
	.badinput:

	call badinput

	push di  ; Save input pointer.
	mov di, .badinputstr
	call println
	pop di	 ; Restore input pointer.

	mov ax, NULL
	jmp .return

	;; Exit the loop.
	.break:

	;; Append a terminating 0 to the symbol str stored in the temporary
	;; buffer.
	mov BYTE [.symstr + bx], 0

	;; Fulfill post.
	call skipspace

	;; Construct the symbol object.
	push di  ; Save input pointer.
	mov di, .symstr
	call get_sym
	pop di  ; Restore input pointer.

	.return:
	
	;; restore
	pop cx
	pop bx

	ret

is_sym_char:
;;; Return whether a symbol can contain the given char.
;;;
;;; A valid symbol char is any char for which is_sym_start_char returns true or
;;; any char in the range '0'-'9'.
;;; 
;;; Pre:
;;; - di points to the char.
;;; 
;;; Post:
;;; - ax is 1 (true) or 0 (false).

	call is_sym_start_char
	cmp ax, 0
	jne .true

	cmp BYTE [di], 0x30
	jl .false

	cmp BYTE [di], 0x39
	jg .false

	.true:
	mov ax, 1
	jmp .return

	.false:
	mov ax, 0

	.return:
	ret

is_sym_start_char:
;;; Return whether a symbol can start with the given char.
;;;
;;; A symbol can start with any lowercase or uppercase letter.
;;; 
;;; Pre:
;;; - di points to the char.
;;; 
;;; Post:
;;; - ax is 1 (true) or 0 (false).

	cmp BYTE [di], 'a'
	jl .not_lowercase

	cmp BYTE [di], 'z'
	jg .not_lowercase

	jmp .true

	.not_lowercase:

	cmp BYTE [di], 'A'
	jl .false

	cmp BYTE [di], 'Z'
	jg .false

	.true:
	mov ax, 1
	jmp .return

	.false:
	mov ax, 0

	.return:
	ret

parse_int:
;;; Convert part of the input str to a Lisp int.
;;; 
;;; Pre:
;;; - di points to a char in the range 0x30-0x39 in the input str.
;;; 
;;; Post:
;;; - ax points to the parsed object.
;;; - di points to the first non-space char after the parsed substr.
;;; 
;;; On error:
;;; - Return NULL.
;;;
;;; TODO:
;;; - parse negative ints

	;; save
	push bx
	push cx
	push dx
	push si

	jmp .start

	.strbegin dw 0x0000
	.strend dw 0x0000

	.badinputstr db "Parse error: non-numeric char",0
	.overflowstr db "Parse error: int exceeds size limit",0

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

	;; Current place in the int, starting at 0.
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
	push dx

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
	pop dx
	pop cx

	ret


;;; ===========================================================================
;;; Eval
;;; ===========================================================================

eval:
;;; Evaluate an expression.
;;;
;;; Pre:
;;; - di points to the Lisp object that represents the expression.
;;; - [toplevel] is 1 if the expression is being evaluated at the top level of
;;;   the program and 0 otherwise.
;;;
;;; Post:
;;; - ax points to the Lisp object that represents the result.
;;; 
;;; On error:
;;; - Return NULL.

	;; save
	push bx
	push cx
	push si

	jmp .start

	.notpairstr1 db "You have found a bug: ",0
	.notpairstr2 db " is not a pair.",0
	.notliststr db " is not a list",0
	.1argstr db " takes 1 argument",0
	.2argstr db " takes 2 arguments",0
	.builtinerrstr db " signaled an error",0
	.notfuncstr db " is not a function",0
	.bugstr:
	db "You have found a bug: somehow reached the end of eval (should "
	db "never happen).",0

	.start:

	;; --------------------------------------------------------------------
	;; Self-evaluating expressions
	;; --------------------------------------------------------------------

	;; If expr is a self-evaluating expression, return expr.

	cmp BYTE [di+TYPE], TYPE_INT
	je .selfeval

	cmp WORD di, [emptylist]
	je .selfeval

	cmp BYTE [di+TYPE], TYPE_BUILTIN_1
	je .selfeval

	cmp BYTE [di+TYPE], TYPE_BUILTIN_2
	je .selfeval

	jmp .skipselfeval

	.selfeval:

	mov ax, di
	jmp .return

	.skipselfeval:


	;; --------------------------------------------------------------------
	;; Symbols
	;; --------------------------------------------------------------------

	;; If expr is a symbol, return the value to which it's bound.

	jmp .symstart

	.syminvalidstr db " is undefined",0

	.symstart:

	;; Check if expr is a symbol.
	cmp BYTE [di+TYPE], TYPE_SYMBOL
	jne .skipsym

	;; Get the value to which expr is bound.
	call get_def

	;; Return the result unless it's NULL.
	cmp ax, NULL
	jne .return

	;; expr is not bound to a value. Notify the user and return NULL:

	call invalid_expr

	call print_obj
	push di  ; Save expr.
	mov di, .syminvalidstr
	call print
	pop di  ; Restore expr.

	jmp .return

	.skipsym:

	;; --------------------------------------------------------------------


	;; Check if expr is a pair.
	cmp BYTE [di+TYPE], TYPE_PAIR
	je .ispair

	;; expr is not a pair. This is a bug. Notify the user and crash:

	push di  ; Save expr.
	mov di, .notpairstr1
	call println
	pop di  ; Restore expr.

	call print_obj

	mov di, .notpairstr2
	call print

	jmp lisp_crash

	;; expr is a pair.
	.ispair:


	;; Check if expr is a list.
	cmp BYTE [di+IS_LIST], 1
	je .islist

	;; expr is not a list and so cannot be evaluated. Notify the user and
	;; return NULL:

	call invalid_expr

	call print_obj

	push di  ; Save expr.
	mov di, .notliststr
	call print
	pop di  ; Restore expr.

	mov ax, NULL
	jmp .return

	;; expr is a list.
	.islist:


	;; --------------------------------------------------------------------
	;; special form: quote
	;; --------------------------------------------------------------------

	;; If (car expr) is the quote symbol, return (car (cdr expr)).
	;; For example, (quote (1 2 3)) evaluates to (1 2 3).

	;; Check if (car expr) is the quote symbol.
	push di  ; Save expr.
	mov WORD di, [di+CAR]
	mov WORD si, [quotesym]
	call equal
	pop di  ; Restore expr.

	;; If not, don't evaluate expr as a quotation.
	cmp ax, 0
	je .skipquote

	;; Get the length of (cdr expr).
	push di  ; Save expr.
	mov WORD di, [di+CDR]
	call length
	pop di  ; Restore expr.

	;; expr is invalid if length (cdr expr) != 1.
	cmp ax, 1
	jne .quoteinvalid

	;; Return (car (cdr expr)).
	push di  ; Save expr.
	mov WORD di, [di+CDR]
	mov WORD ax, [di+CAR]
	pop di  ; Restore expr.
	jmp .return

	;; expr is invalid. Notify the user and return NULL.
	.quoteinvalid:
	call invalid_expr

	push di  ; Save expr.

	mov WORD di, [quotesym]
	call print_obj

	mov di, .1argstr
	call print

	pop di  ; Restore expr.

	;; Return NULL.
	mov ax, NULL
	jmp .return

	.skipquote:


	;; --------------------------------------------------------------------
	;; special form: define
	;; --------------------------------------------------------------------

	;; If (car expr) is the define symbol, expr must be of the form
	;; (define <name> <definition>), where <name> is a symbol and
	;; <definition> is an expression.
	;; 
	;; Bind <name> to (eval <definition>) and return NULL.

	jmp .definestart

	.namenotsymbolstr db " is not a symbol",0
	.toplevelstr db " can only occur at the top level",0

	.definestart:

	;; Check if (car expr) is the define symbol.
	push di  ; Save expr.
	mov WORD di, [di+CAR]
	mov WORD si, [definesym]
	call equal
	pop di  ; Restore expr.

	;; If not, don't evaluate expr as a definition.
	cmp ax, 0
	je .skipdefine

	;; Check if expr is being evaluated at the top level of the program.
	cmp BYTE [toplevel], 1
	jne .nottoplevel

	;; Get the length of (cdr expr).
	push di  ; Save expr.
	mov WORD di, [di+CDR]
	call length
	pop di  ; Restore expr.

	;; expr is invalid if length (cdr expr) != 2.
	cmp ax, 2
	jne .defineargsnum

	;; Check if (car (cdr expr)) is a symbol.
	push di  ; Save expr.
	mov WORD di, [di+CDR]
	mov WORD di, [di+CAR]
	cmp BYTE [di+TYPE], TYPE_SYMBOL
	pop di  ; Restore expr.

	;; If not, expr is invalid.
	jne .namenotsymbol

	;; Bind <name> to (eval <definition>):

	push di  ; Save expr.

	;; Set di to the definition: (car (cdr (cdr expr))).
	mov WORD di, [di+CDR]
	mov WORD di, [di+CDR]
	mov WORD di, [di+CAR]

	;; Eval the definition.
	mov BYTE [toplevel], 0
	call eval

	pop di  ; Restore expr.

	;; Check if eval returned NULL.
	cmp ax, NULL
	je .return

	;; Set si to the result.
	mov si, ax

	push di  ; Save expr.

	;; Set di to the name: (car (cdr expr)).
	mov WORD di, [di+CDR]
	mov WORD di, [di+CAR]

	;; Bind the name to the value of the definition.
	call bind

	pop di  ; Restore expr.

	;; Return.
	jmp .defineret

	;; expr is not being evaluated at the top level of the program. Notify
	;; the user.
	.nottoplevel:
	call invalid_expr

	push di  ; Save expr.

	mov WORD di, [definesym]
	call print_obj

	mov di, .toplevelstr
	call print

	pop di	 ; Restore expr.

	;; Return.
	jmp .defineret

	;; Wrong number of arguments. Notify the user.
	.defineargsnum:
	call invalid_expr

	push di  ; Save expr.

	mov WORD di, [definesym]
	call print_obj

	mov di, .2argstr
	call print

	pop di  ; Restore expr.

	;; Return.
	jmp .defineret

	;; <name> is not a symbol. Notify the user.
	.namenotsymbol:
	call invalid_expr

	push di  ; Save expr.

	mov WORD di, [di+CDR]
	mov WORD di, [di+CAR]
	call print_obj

	mov di, .namenotsymbolstr
	call print

	pop di  ; Restore expr.

	;; Return NULL regardless of whether expr is valid.
	.defineret:
	mov ax, NULL
	jmp .return

	.skipdefine:


	;; --------------------------------------------------------------------
	;; special form: cond
	;; --------------------------------------------------------------------

	;; If (car expr) is the cond symbol, print a placeholder message and
	;; return NULL.

	;; Check if (car expr) is the cond symbol.
	push di  ; Save expr.
	mov WORD di, [di+CAR]
	mov WORD si, [condsym]
	call equal
	pop di  ; Restore expr.

	;; If not, don't evaluate expr as a conditional.
	cmp ax, 0
	je .skipcond

	;; Print a placeholder message:

	call invalid_expr

	push di  ; Save expr.
	mov WORD di, [condsym]
	call special_form_placeholder
	pop di  ; Restore expr.

	;; Return NULL.
	mov ax, NULL
	jmp .return

	.skipcond:


	;; --------------------------------------------------------------------
	;; special form: lambda
	;; --------------------------------------------------------------------

	;; If (car expr) is the lambda symbol, print a placeholder message and
	;; return NULL.

	;; Check if (car expr) is the lambda symbol.
	push di  ; Save expr.
	mov WORD di, [di+CAR]
	mov WORD si, [lambdasym]
	call equal
	pop di  ; Restore expr.

	;; If not, don't evaluate expr as a lambda expression.
	cmp ax, 0
	je .skiplambda

	;; Print a placeholder message:

	call invalid_expr

	push di  ; Save expr.
	mov WORD di, [lambdasym]
	call special_form_placeholder
	pop di  ; Restore expr.

	;; Return NULL.
	mov ax, NULL
	jmp .return

	.skiplambda:


	;; --------------------------------------------------------------------
	;; Function application
	;; --------------------------------------------------------------------

	;; If we've reached this point then expr is a list that is not a
	;; special form, so expr must represent a function application.

	;; Eval (car expr) to get the function.
	push di  ; Save expr.
	mov WORD di, [di+CAR]
	mov BYTE [toplevel], 0
	call eval
	pop di  ; Restore expr.

	;; Check if eval returned NULL.
	cmp ax, NULL
	je .return

	;; Check if the function is a 1-arg builtin.
	mov bx, ax
	cmp BYTE [bx+TYPE], TYPE_BUILTIN_1
	jne .skipbuiltin1

	;; Get the length of (cdr expr).
	push di  ; Save expr.
	mov WORD di, [di+CDR]
	call length
	pop di  ; Restore expr.

	;; expr is invalid if length (cdr expr) != 1.
	cmp ax, 1
	jne .builtin1argsnum

	;; Eval (car (cdr expr)) to get the arg.
	push di  ; Save expr.
	mov WORD di, [di+CDR]
	mov WORD di, [di+CAR]
	call eval
	pop di  ; Restore expr.

	;; Check if eval returned NULL.
	cmp ax, NULL
	je .return

	;; Call the assembly function.
	push di  ; Save expr.
	mov di, ax  ; Arg.
	call WORD [bx+FUNC]
	pop di  ; Restore expr.

	;; Return the result.
	jmp .builtinreturn

	;; Wrong number of arguments. Notify the user and return NULL.
	.builtin1argsnum:
	call invalid_expr

	push di  ; Save expr.

	mov di, bx  ; Builtin function.
	call print_obj

	mov di, .1argstr
	call print

	pop di  ; Restore expr.

	;; Return NULL.
	mov ax, NULL
	jmp .return

	.skipbuiltin1:

	;; Check if the function is a 2-arg builtin.
	mov bx, ax
	cmp BYTE [bx+TYPE], TYPE_BUILTIN_2
	jne .skipbuiltin2

	;; Get the length of (cdr expr).
	push di  ; Save expr.
	mov WORD di, [di+CDR]
	call length
	pop di  ; Restore expr.

	;; expr is invalid if length (cdr expr) != 2.
	cmp ax, 2
	jne .builtin2argsnum

	;; Eval (car (cdr expr)) to get the first arg.
	push di  ; Save expr.
	mov WORD di, [di+CDR]
	mov WORD di, [di+CAR]
	call eval
	pop di  ; Restore expr.

	;; Check if eval returned NULL.
	cmp ax, NULL
	je .return

	;; Save first arg.
	mov cx, ax

	;; Eval (car (cdr (cdr expr))) to get the second arg.
	push di  ; Save expr.
	mov WORD di, [di+CDR]
	mov WORD di, [di+CDR]
	mov WORD di, [di+CAR]
	call eval
	pop di  ; Restore expr.

	;; Check if eval returned NULL.
	cmp ax, NULL
	je .return

	;; Call the assembly function.
	push di  ; Save expr.
	mov di, cx  ; First arg.
	mov si, ax  ; Second arg.
	call WORD [bx+FUNC]
	pop di  ; Restore expr.

	;; Return the result.
	jmp .builtinreturn

	;; Wrong number of arguments. Notify the user and return NULL.
	.builtin2argsnum:
	call invalid_expr

	push di  ; Save expr.

	mov di, bx  ; Builtin function.
	call print_obj

	mov di, .2argstr
	call print

	pop di  ; Restore expr.

	;; Return NULL.
	mov ax, NULL
	jmp .return

	;; Return the result of applying a builtin function.
	.builtinreturn:

	;; Return the result unless it's NULL.
	cmp ax, NULL
	jne .return

	;; The result is NULL. Notify the user and return NULL:

	call invalid_expr

	push di  ; Save expr.

	mov di, bx  ; Builtin function.
	call print_obj

	mov di, .builtinerrstr
	call print

	pop di  ; Restore expr.

	;; Return NULL.
	jmp .return

	.skipbuiltin2:

	;; The value of (car expr) is not a function. Notify the user and
	;; return NULL:

	call invalid_expr

	push di  ; Save expr.

	mov di, bx
	call print_obj

	mov di, .notfuncstr
	call print

	pop di  ; Restore expr.

	;; Return NULL.
	mov ax, NULL
	jmp .return

	;; --------------------------------------------------------------------


	;; We should never reach this point. Notify the user and crash.
	mov di, .bugstr
	call println
	jmp lisp_crash


	.return:

	;; restore
	pop si
	pop cx
	pop bx

	ret

length:
;;; Return the length of a Lisp list.
;;;
;;; Pre:
;;; - di points to the list.

	;; save
	push di

	xor ax, ax  ; length
	jmp .test

	.loop:
	inc ax  ; Increment length.
	mov WORD di, [di+CDR]  ; Set di to (cdr di).

	;; Continue the loop until di is the empty list.
	.test:
	cmp WORD di, [emptylist]
	jne .loop

	;; restore
	pop di

	ret

invalid_expr:
;;; Handle an invalid expression.
;;;
;;; Pre:
;;; - di points to the expression.
	jmp .start

	.errstr db "Invalid expression:",NEWLINE,NEWLINE,"  ",0

	.start:

	;; Print the error string.
	push di  ; Save expr.
	mov di, .errstr
	call println
	pop di  ; Restore expr.

	;; Print expr.
	call print_obj
	call print_newline
	call print_newline

	ret

special_form_placeholder:
;;; Print a placeholder message for a special form that has not been implemented.
;;;
;;; Pre:
;;; - di points to the special form's symbol.

	jmp .start

	.message1 db "Special form '",0
	.message2 db "' not yet implemented",0

	.start:

	push di  ; Save symbol.
	mov di, .message1
	call print
	pop di  ; Restore symbol.

	;; Print the special form's symbol.
	call print_obj

	push di  ; Save symbol.
	mov di, .message2
	call print
	pop di  ; Restore symbol.

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

	.builtinstr1 db "#<function: ",0
	.builtinstr2 db ">",0

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

	;; Builtin function:

	cmp BYTE [di+TYPE], TYPE_BUILTIN_1
	je .builtin

	cmp BYTE [di+TYPE], TYPE_BUILTIN_2
	je .builtin

	jmp .skipbuiltin

	.builtin:

	push di  ; Save object.
	mov di, .builtinstr1
	call print
	pop di  ; Restore object.

	mov WORD di, [di+FUNC_NAME]
	call print_obj

	mov di, .builtinstr2
	call print

	jmp .return

	.skipbuiltin:

	;; Unrecognized type:

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
	push dx

	jmp .start

	.dotstr db " . ",0

	.start:

	;; Iterate through the chain, printing each object's CAR until we
	;; encounter an object that is not a pair.

	;; Print the opening '('.
	mov dl, '('
	call putc

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
	mov dl, ' '
	call putc

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
	pop dx
	pop di

	ret


;;; ===========================================================================
;;; Object comparison
;;; ===========================================================================

equal:
;;; Return whether two Lisp objects are equal.
;;; 
;;; Pre:
;;; - di points to the first object.
;;; - si points to the second object.
;;;
;;; Post:
;;; - ax contains 1 if the objects are equal and 0 otherwise.

	;; save
	push cx

	jmp .start

	.bugstr:
	db "You have found a bug: cannot compare objects of unrecognized type",0

	.start:

	;; Return true if the objects are the same object.
	cmp di, si
	je .true

	;; Return false if the objects have different types.
	mov BYTE cl, [di+TYPE]
	cmp BYTE cl, [si+TYPE]
	jne .false

	;; If the objects are of type unique, return false because we already
	;; know they're not the same object.

	cmp BYTE [di+TYPE], TYPE_UNIQUE
	jne .skipunique

	jmp .false

	.skipunique:

	;; If the objects are of type int, return whether they have the same
	;; value.

	cmp BYTE [di+TYPE], TYPE_INT
	jne .skipint

	mov WORD cx, [di+VAL]
	cmp WORD cx, [si+VAL]
	je .true
	jmp .false

	.skipint:

	;; If the objects are of type symbol, return the result of sym_cmp.

	cmp BYTE [di+TYPE], TYPE_SYMBOL
	jne .skipsym

	call sym_cmp
	jmp .return

	.skipsym:

	;; If the objects are of type pair, return whether their CARs and CDRs
	;; are equal.

	cmp BYTE [di+TYPE], TYPE_PAIR
	jne .skippair

	push di  ; Save first object.
	push si	 ; Save second object.

	mov WORD di, [di+CAR]
	mov WORD si, [si+CAR]
	call equal

	pop si  ; Restore first object.
	pop di	; Restore second object.

	cmp ax, 0
	je .return

	push di  ; Save first object.
	push si	 ; Save second object.

	mov WORD di, [di+CDR]
	mov WORD si, [si+CDR]
	call equal

	pop si  ; Restore first object.
	pop di	; Restore second object.

	;; Return the result of the second call to equal.
	jmp .return

	.skippair:

	;; Cannot compare objects. Notify the user and crash.
	mov di, .bugstr
	call println
	jmp lisp_crash

	.true:
	mov ax, 1
	jmp .return

	.false:
	mov ax, 0

	.return:

	;; restore
	pop cx

	ret

sym_cmp:
;;; Return whether the symbols are equal.
;;;
;;; Pre:
;;; - di points to the first symbol.
;;; - si points to the second symbol.
;;;
;;; Post:
;;; - ax contains 1 if the symbols are equal and 0 otherwise.

	;; save
	push bx
	push di
	push si

	add di, NAME  ; Start of the first symbol's string.
	add si, NAME  ; Start of the second symbol's string.
	mov bx, 0  ; Index into each string.

	;; Loop through the strings, comparing each pair of chars with the same
	;; index.
	.loop:

	;; Exit the loop and return false if the current two chars are not
	;; equal.
	mov BYTE al, [si+bx]
	cmp BYTE [di+bx], al
	jne .false

	;; The chars are equal, so exit the loop and return true if we've
	;; reached the null terminator.
	cmp BYTE [di+bx], 0
	je .true

	;; Advance to the next two chars.
	inc bx

	;; Exit the loop and return true if we've reached the maximum name
	;; size.
	cmp bx, MAX_NAME_SIZE
	je .true

	jmp .loop

	.true:
	mov ax, 1
	jmp .return

	.false:
	mov ax, 0

	.return:

	;; restore
	pop si
	pop di
	pop bx

	ret


;;; ===========================================================================
;;; Global environment
;;; ===========================================================================

;;; The global environment is just a list of name-value pairs. For example, the
;;; list of pairs ((nums 1 2 3) (y . 2) (x . 1)) represents an enviroment in
;;; which x is defined as 1, y as 2, and nums as (1 2 3).

;;; To retrieve the value bound to a given name, we must perform a linear
;;; search on the list of name-value pairs. To bind a value to a given name, we
;;; also perform a linear search to determine whether the name is already bound
;;; to some value.

;;; In the future, it would be a good idea to use a table implementation with
;;; more efficient insertion and retrieval; for example, the Lisp-in-C
;;; (https://notabug.org/jtherrmann/lisp-in-c) precursor to this project uses
;;; a hash table.

bind:
;;; Bind a name to a value.
;;;
;;; Pre:
;;; - di points to a symbol object representing the given name.
;;; - si points to an object representing the given value.

	;; save
	push ax
	push di
	push si

	;; Get the name-value pair for the given name.
	call lookup

	;; If the name has no binding, create a new binding for it.
	cmp ax, NULL
	je .newbinding

	;; Otherwise, just set the binding's CDR to the new value and return.
	mov WORD di, ax
	mov WORD [di+CDR], si
	jmp .return

	;; Create the new name-value pair and insert it at the beginning of the
	;; list representing the global environment.
	.newbinding:

	;; Construct the name-value pair.
	call cons

	;; Construct a list whose car is the name-value pair and whose cdr is
	;; the list that represents the current global environment.
	mov di, ax
	mov WORD si, [globalenv]
	call cons

	;; Set the global environment to the newly constructed list.
	mov WORD [globalenv], ax

	.return:

	;; restore
	pop si
	pop di
	pop ax

	ret

get_def:
;;; Return the value bound to the given name, or NULL if the name is not bound
;;; to a value.
;;;
;;; Pre:
;;; - di points to a symbol object representing the given name.
;;;
;;; Post:
;;; - ax points to the object representing the value.

	;; Get the name-value pair for the given name.
	call lookup

	;; Return NULL if the name is unbound.
	cmp ax, NULL
	je .return

	;; Return the value from the name-value pair.
	push di  ; save
	mov WORD di, ax
	mov WORD ax, [di+CDR]
	pop di  ; restore

	.return:
	ret

lookup:
;;; Return the name-value pair for the given name, or NULL if the name is
;;; unbound.
;;;
;;; Pre:
;;; - di points to a symbol object representing the given name.
;;;
;;; Post:
;;; - ax points to the name-value pair.

	;; save
	push si

	;; Iterate through the list that represents the global environment
	;; until we find a name matching the given name. If we reach the end
	;; of the list without finding a matching name, return NULL.

	;; Set si to the global env.
	mov WORD si, [globalenv]

	jmp .test
	.loop:

	push si  ; Save current position in global env.

	;; Set si to the next name-value pair in the global env.
	mov WORD si, [si+CAR]

	;; Set si to the name in the name-value pair.
	mov WORD si, [si+CAR]

	;; Compare the given name, in di, with the name from the name-value
	;; pair.
	call equal

	pop si  ; Restore current position in global env.

	;; Jump to the next iteration if the names are not equal.
	cmp ax, 0
	je .next

	;; The names are equal, so return the name-value pair.
	mov WORD ax, [si+CAR]
	jmp .return

	;; Increment the current position in the global env.
	.next:
	mov WORD si, [si+CDR]

	;; Continue the loop until we reach the empty list at the end of the
	;; global env.
	.test:
	cmp WORD si, [emptylist]
	jne .loop

	;; We did not find a name matching the given name, so return NULL.
	mov ax, NULL

	.return:

	;; restore
	pop si

	ret


;;; ===========================================================================
;;; Misc builtin functions
;;; ===========================================================================
;;; 
;;; Builtin functions that do not belong in one of the previous sections.

builtin_car:
;;; Builtin function car.
;;;
;;; Pre:
;;; - di points to the argument.
;;;
;;; Post:
;;; - ax points to the result.
;;;
;;; On error:
;;; - Return NULL.
	;; save
	push dx
	push si

	mov si, type_pair_str
	mov BYTE dl, TYPE_PAIR
	call typecheck

	cmp ax, 0
	je .error

	mov WORD ax, [di+CAR]
	jmp .return

	.error:
	mov ax, NULL

	.return:

	;; restore
	pop si
	pop dx

	ret

builtin_cdr:
;;; Builtin function cdr.
;;;
;;; Pre:
;;; - di points to the argument.
;;;
;;; Post:
;;; - ax points to the result.
;;;
;;; On error:
;;; - Return NULL.
	;; save
	push dx
	push si

	mov si, type_pair_str
	mov BYTE dl, TYPE_PAIR
	call typecheck

	cmp ax, 0
	je .error

	mov WORD ax, [di+CDR]
	jmp .return

	.error:
	mov ax, NULL

	.return:

	;; restore
	pop si
	pop dx

	ret

typecheck:
;;; Typecheck an argument to a builtin function.
;;;
;;; Pre:
;;; - di points to the argument.
;;; - si points to a string representing the type's name.
;;; - dl contains the type.
;;;
;;; Post:
;;; - ax is 1 if the argument is of the specified type and 0 otherwise.
	jmp .start

	.str1 db "Type error: ",0
	.str2 db " is not a ",0

	.start:
	
	;; Return true if the arg is of the specified type.
	cmp BYTE [di+TYPE], dl
	je .true

	;; The arg is not of the specified type. Notify the user and return
	;; false:

	push di  ; Save arg.
	mov di, .str1
	call println
	pop di  ; Restore arg.

	call print_obj

	push di  ; Save arg.

	mov di, .str2
	call print

	mov di, si  ; Type name str.
	call print

	pop di  ; Restore arg.

	;; Return false.
	mov ax, 0
	jmp .return

	;; Return true.
	.true:
	mov ax, 1

	.return:
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

reboot:
;;; Reboot.
	jmp .start

	.msg db "See you soon!",0

	.start:

	mov di, .msg
	jmp reboot_comp
	
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
	help_list_len dw 6  ; TODO: check val is correct

;;; Command strings:

	freecount_str db CMD_PREFIX,"free",0
	freelist_str db CMD_PREFIX,"freelist",0
	globalenv_str db CMD_PREFIX,"genv",0
	help_str db CMD_PREFIX,"help",0
	keymap_str db CMD_PREFIX,"keymap",0
	reboot_str db CMD_PREFIX,"restart",0

command_table:
	dw freecount_str
	dw print_freecount

	dw freelist_str
	dw print_freelist

	dw globalenv_str
	dw print_global_env

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

print_freecount:
;;; Print the number of free Lisp objects.
	;; save
	push di

	jmp .start

	.str db "free objects: ",0

	.start:

	mov di, .str
	call println

	mov WORD di, [freecount]
	call print_num

	;; restore
	pop di

	ret

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

	;; Print NULL.
	mov di, .nullstr
	call print

	call print_newline

	;; Print the number of free objects.
	call print_freecount
	call print_newline

	;; restore
	pop di
	pop cx
	pop bx

	ret

print_global_env:
;;; Print the global environment.
	;; save
	push di

	call print_newline

	mov WORD di, [globalenv]
	call print_obj

	;; restore
	pop di

	ret


;;; ===========================================================================
;;; System utilities
;;; ===========================================================================

;;; ---------------------------------------------------------------------------
;;; Input
;;; ---------------------------------------------------------------------------

getstr:
;;; Read a string from keyboard input.

	;; save
	push ax
	push bx
	push di

	mov di, input_buffer  ; Input buffer.
	mov bx, 0  ; Input buffer index.

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

	;; Restart the loop if the input buffer index is INPUT_SIZE, to prevent
	;; buffer overflow.
	cmp bx, INPUT_SIZE
	je .loop

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
	pop di
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

	;; Exit the loop and return false if the current two chars are not
	;; equal.
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


;;; ---------------------------------------------------------------------------
;;; Power management
;;; ---------------------------------------------------------------------------

reboot_comp:
;;; Reboot the computer.
;;; Print the given message, wait for a keypress, and reboot the computer.
;;;
;;; Pre:
;;; - di contains the message.
	jmp .start

	.waitstr db "Press any key to reboot.",0

	.start:

	;; Print the given message.
	call println
	call print_newline

	;; Wait for a keypress.
	mov di, .waitstr
	call println
	mov ah, 0
	int 0x16

	;; Reboot.
	;; Source: https://stackoverflow.com/a/32686533
	db 0x0ea
	dw 0x0000
	dw 0xffff


;;; ===========================================================================
;;; Global data
;;; ===========================================================================

	;; The input buffer.
	;; Use INPUT_SIZE+1 to allocate an extra byte for the null terminator.
	input_buffer times INPUT_SIZE+1 db 0

	;; Whether Dvorak mode is enabled.
	dvorak_mode db 1  ; TODO: back to 0 before submit project

	;; Whether we are currently evaluating an expression at the top level
	;; of the program.
	toplevel db 0

	;; The list of free objects.
	freelist dw 0x0000
	freecount dw 0x0000

	;; The list representing the global environment.
	globalenv dw 0x0000

	;; The empty list object.
	emptylist dw 0x0000

	;; Special form symbols.
	quotesym dw 0x0000
	definesym dw 0x0000
	condsym dw 0x0000
	lambdasym dw 0x0000

	;; Type names used for printing type errors.
	type_pair_str db "pair",0

	;; The object heap.
	obj_heap times OBJ_HEAP_SIZE db 0

lisp_end:	
