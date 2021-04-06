; PIRATA bugs fixed, code optimized and smaller
; Rui Ribeiro - 2021
;
; https://github.com/ruyrybeyro/pirate128/blob/main/pirataMOD2021/PirataMOD2021.tap
;

; border cyan/red - insert tape to copy
;                   SPACE to end, jump to save
;
; border yellow   - insert blank tape and press 1
;                   (and some other keys, including space,
;                   see BIT0KEY)
;
; border green    - 1 make another copy
;                   2 start over
;
;


; FORMAT of tape block in memory from $4000 onwards
;
;  BYTE 1 - LSB size of block
;  BYTE 2 - MSB size of block
;  BYTE 3 - flag identifying type of tape block
;  -------- block of data

; if BYTE1 and BYTE2 = 0, no more blocks, otherwise another block follows

; 141 bytes ROM + 8 bytes for stack

; ROM CALL - alternate LD_BYTES entry point
; https://skoolkid.github.io/rom/asm/0556.html
;
; for not returning to SA_LD_RET at the end of load.
; Interrupts must be disabled
;
; A' - header block + Z=1
; IX = start address
; DE = block length
;
; returns from LD_FLAG if different flag
; returns when SPACE pressed or end of data on tape
;
; Returns upon completion:
;
; DE = number of bytes loaded + 1
; IX = IX + number of bytes loaded + 1
;
LD_BYTES2       EQU     $0562

; ROM CALL - LD_FLAG alternate entry point
;
; after loading headers and byte of flag
;
; if flag was different than 0
; we need to reenter load routine
;
; returns when SPACE pressed or end of data on tape
;
; Returns upon completion:
;
; DE = number of bytes loaded + 1
; IX = IX + number of bytes loaded + 1
;
LD_FLAG3        EQU     $05B7

; ROM CALL - SA-BYTES subroutine alternate entry
; https://skoolkid.github.io/rom/asm/04C2.html
;
; alternate entry, does not return using SA_LD_RET
; does not reenable interrupts
;
;A  = flag, usually 00 header block, $FF data block
;DE = block length
;IX = start address
;
SA_BYTES2       EQU     $04C6

; constants

RAM_BEGIN	EQU	$4000		; first byte of RAM (screen)
MAX_SIZE	EQU	$FFFF-8+1 - RAM_BEGIN - 5


                ;
                ; entry point of PIRATA MC subroutine
                ; originally stored in a REM statement in BASIC
                ; transfered to ORG address via LDIR
                ;
                ; ORG   14446
		ORG 	$386E

;
; ===========================
;  Initialisation/setup
; ===========================
;

START:	
		DI			; Disabling interrupts
					; only needed once
		LD	SP,0		; stack to end of RAM

;
; ===========================
;  Block(s) loading
; ===========================
;
; load blocks from tape
; until SPACE pressed
;

BEGIN:		LD	HL,RAM_BEGIN	; beggining of RAM

L_NEXTBLK:		
		PUSH    HL	
		INC	HL		; size of block LSB
		INC	HL              ; size of block MSB
		XOR	A		; header block
		LD      (HL),A		; flag type of block
		INC	HL              
		LD	DE,MAX_SIZE 	; max number of bytes
		PUSH	HL
		POP	IX

		; set carry flag for LOADing
		SCF

		; setup for the ROM instructions we are skipping
                ; Z'=1, A'=code block flag (0)
                ;
                ; better be 0, because header blocks expect a shorter tone
                ; in the ROM LD_BYTES routine
                ;
		INC	D
		EX	AF,AF'
		DEC	D

		LD	A,$0F		; border white/MIC off
		OUT	($FE),A		
		; end of "ROM" code

		CALL	LD_BYTES2    	; call $0562 - load block
		JR	Z,END_BLOCK	; end of tape byte stream

		; read keyboard port $7FFE
                ; select keyboard half row BNM Symbol SPACE
		LD	A,$7F
		IN 	A,($FE)
		RR	A
		JR	NC,END_LOAD	; if bit 0 = 0
                                        ; SPACE pressed, finish block(s) loading loop
                                        ; and jump to save

                ; if this block is executed is because
                ; the block flag is different than 0
                ; though at the LD_BYTES routine point
                ; it is stored now in L (and not A)
                ; (L is used to store/put together the last loaded byte/bits
                ; at LD_BYTES 8-bit loading loop)

		LD	(IX-01),L	; save flag block identifier byte in RAM
                                        ; after block size
                                        ; IX = first data byte

		XOR	A		; Z=1
                                        ; we need to change Z flag
                                        ; as it is used ahead on ROM

		CALL	$LD_FLAG3	; $05B7 - load entry after flag check.

;
; ===========================
;  Loading block housekeeping
; ===========================
;
; Calculate loaded bytes, and store the length.
; Restore IX.
; Jump to load another block.
;

                    ; subtract from max number of bytes / original length
                    ; to get bytes loaded
                    ; eg. original DE calling LD_BYTES vs new DE returning from LD_BYTES
                    ; DE (length) = original DE - new DE - 1

END_BLOCK:	LD	HL,MAX_SIZE
		OR	A		; carry =0
		SBC	HL,DE		; HL=bytes loaded

		LD	D,H
		LD	E,L		; DE=HL=bytes loaded(+checksum)
		DEC	DE		; DE=bytes loaded

		POP	HL		; POP IX before loading, beggining of block
		    
		
		LD	(HL),E          ; (HL) = size (word) at beggining of block
		INC	HL
		LD	(HL),D
		PUSH	IX
		POP	HL		; HL=IX
		DEC	HL		; loading leaves IX at IX+1, DEC it
		JR	L_NEXTBLK	; next block

;
; ===========================
;  Finished loading all blocks
; ===========================
;
; Execute this code after SPACE is pressed.
;
; (Next) block with size of word 0, means END.
;

END_LOAD:	POP	HL		; POP IX before loading

		; (latter IX) = 0 word, no more blocks
		XOR	A
		LD	(HL),A		; (latter IX) = LSB 0 word
		INC	HL		; point to next byte
		LD	(HL),A		; (latter IX) = MSB 0 word

;
; ===========================
;  Waiting for user input
; ===========================
;
; Border yellow
; waits for a key for saving loaded blocks.
;


		LD	A,06            ; border yellow
		OUT	($FE),A

		CALL	DELAY	

                ; wait for a pressed key
                ; works for not only for "1"
                ; but also SPACE, ENTER, P, 0, Q, A, Shift
                ;

BIT0KEY:	XOR	A		; select all keyboard half-rows
		IN 	A,($FE)		; IN A,($00FE)
		RR	A		; rotate 1 bit to the right
		JR 	C,BIT0KEY	; if bit 0=1 (no key pressed), try again

;
; =========================
;  Block(s) saving
; =========================
;
; save all blocks
; with a delay of 0.9 seconds between them

BEGIN_SBLOCK:	LD	HL,RAM_BEGIN	; first RAM address
NEXT_SBLOCK:	CALL	DELAY	
		LD	E,(HL)		; LSB size of block
		INC	HL
		LD	D,(HL)		; MSB size of block
					; DE = size of block kept on RAM
		LD	A,E
		OR	D
		JR	Z,EXIT_SAVE	; size = 0, jump to EXIT_SAVE
		INC	HL
		LD	A,(HL)		; load flag
		INC	HL

		PUSH	HL
		POP	IX

                ; IX is now pointing at data to be saved
                ; DE has length of data to be saved

		CALL	SA_BYTES2 	; CALL	$04C6

		PUSH	IX
		POP	HL		; HL=IX
		DEC	HL		; IX+1 returned from SA_BYTES needs to be corrected
		JR	NEXT_SBLOCK	; save next block

;
; =========================
;  After Saving all blocks
; =========================
;
; border green
; Reads keyboard:
;
; "1" - saves blocks again
; "2" - starts from scratch loading

EXIT_SAVE:	LD	A,04         	; border green
		OUT	($FE),A

		; read keyboard port $F7FE
		LD	BC,$F7FE	; keyboard half row 1-5
WAIT_1_2:	IN	A,(C)
		BIT	1,A	    	; key "2"
		JR	Z,BEGIN      	; begin from scratch
		BIT	0,A         	; key "1"
		JR	Z,BEGIN_SBLOCK	; save again another copy
		JR	WAIT_1_2	; read keyboard port again

;
; =========================
;  Delay routine
; =========================
;
; delay of aprox 0.9 seconds

DELAY:		LD	BC,$FFFF	    
DLOOP:		DEC	BC		; decrement BC $FFFF/65535 times
		LD	A,B		; till BC=0
		OR	C
		JR	NZ,DLOOP	; JR if BC not equal 0
		RET

		; 8 bytes at the end of RAM empty
;STACK:		;DEFS	(8)	
		END	
