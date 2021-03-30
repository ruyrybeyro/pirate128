; Commented (dis)assembly listing of Pirata - https://spectrumcomputing.co.uk/entry/36321/ZX-Spectrum/Pirata
; Rui Ribeiro - 2021
;
; border cyan/red - insert tape to copy
;                   SPACE to force end?
;
; border yellow   - insert blank tape and press 1 
;                   (and some other keys, including space,
;                   see BIT0KEY)
;
; border green    - 1 make another copy
;                   2 start over
;
; FORMAT of tape block in memory from $4000 onwards
;
;  BYTE 1 - LSB size of block
;  BYTE 2 - MSB size of block	
;  BYTE 3 - flag identifying type of tape block
;  -------- block of data 

; if BYTE1 and BYTE2 = 0, no more blocks, otherwise another block follows

; 171 bytes = 155 bytes + 16 bytes for stack (of which 14 are available)

; ROM CALL - alternate LD_BYTES entry point
; https://skoolkid.github.io/rom/asm/0556.html
;
; for not returning to SA_LD_RET at the end of load.
; Interrupts must be disabled
; 
; A' - header block + Z=1
; IX = start address
; DE = block lenght
;
; returns in LD_FLAG if different flag
; returns when SPACE pressed or end of data on tape
;
; upon completion:
;
; DE = number of bytes loaded + 1 
; IX = IX + number of bytes loaded + 1
;
LD_BYTES2	EQU	$0562

; ROM CALL - LD_FLAG alternate entry point
;
; after loading headers and byte of flag
;
; if flag was different than 0
; we need to reenter load routine
;
LD_FLAG2	EQU	$05B6

; ROM CALL - SA-BYTES subroutine
; https://skoolkid.github.io/rom/asm/04C2.html
;
;A  = flag, usually 00 header block, $FF data block
;DE = block length
;IX = start address
;
SA_BYTES	EQU	$04C2


	
                ;	
		; entry point of PIRATA MC subroutine
		; originally stored in a REM statement in BASIC
		; transfered to $FF54 via LDIR
                ;
		; ORG	65364
		ORG 	$FF54

;
; ===========================
;  Initialisation/setup
; ===========================
;
; Moving stack out of way
; Disabling interrupts.
	
BEGIN:		LD	SP,$FFFF	; stack pointer - end of RAM
					; should be 0000
	
		DI			; DI can be moved to before the SP
					; it is only needed the first time
                                        ; more one byte to load


;
; ===========================
;  Block(s) loading 
; ===========================
;
; load blocks from tape
; until SPACE pressed
;

		LD	IX,$4000	; beginning of RAM/screen
					; "MAINLOOP"

L_NEXTBLK:	LD	(IX+02),00	; init flag to zero
					; it won't be set if header block 0

		PUSH	IX
		INC	IX		; size of block LSB
		INC	IX              ; size of block MSB
		INC	IX              ; flag
		LD	A,00		; header block	- Could be XOR A, one less byte
		LD	DE,BEGIN-0x4000	; $BF54/48980 max number of bytes
					; could be MAINLOOP-0x4000 
					; for more free bytes 
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

		CALL	$LD_BYTES2    	; load block
		JR	Z,END_LOAD	; end of tape byte stream

		; read keyboard port $7FFE
                ; select keyboard half row BNM Symbol SPACE
		LD	A,$7F		
		IN 	A,($FE)
		RR	A
		JR	NC,SAVE_SECTION	; if bit 0 = 0
					; SPACE pressed, finish block(s) loading loop
					; and jump to save

		; if we arrive here
		; block flag is different than 0
		; though at the LD_BYTES routine point
		; it is stored now in L (and not A) 
                ; (L is used to store/put together the last loaded byte/bits)

		LD	(IX-01),L	; save flag block identifier byte in RAM
					; after block size
					; IX = first data byte


		XOR	A               ; Z=1
		CALL	$LD_FLAG2	; force reentry into load tape routine
                                        ; right after where it left
					; load entry after flag check.
                                        ;
					; delete XOR A and change CALL to
					; CALL $05B7
                                        ; less one byte

;
; ===========================
;  Loading block housekeeping
; ===========================
;
; Calculate loaded bytes, and store the lenght.
; Restore IX.
; Jump to load another block.
;


		    ; subtract from max number of bytes / original lenght
                    ; to get bytes loaded
		    ; eg. original DE given to LD_BYTES - new DE - 1

END_LOAD:	LD	HL,BEGIN-0x4000 ; $BF54  max number of bytes
					; could be MAINLOOP-0x4000

		OR	A		; carry =0
		SBC	HL,DE		; HL=bytes loaded

		PUSH	HL
		POP	DE		; DE=HL=bytes loaded(+checksum)
		DEC	DE		; DE=bytes loaded

		POP	HL		; POP IX before loading, beggining of block
		    
		; store lenght in memory of this block, before data loaded
		LD	(HL),E          ; (HL) = size (word) at beggining of block
		INC	HL		; point to next byte
		LD	(HL),D

		DEC	IX		; loading leaves IX at IX+1, DEC it
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

SAVE_SECTION:	POP	HL		; POP IX before loading

		LD	(HL),00		; (IX) = 0 word, no more blocks
		INC	HL		; point to next byte
		LD	(HL),00		; could be XOR A,LD (HL),A,INC HL,LD (HL),A
					; less one byte
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

		CALL	DELAY		; 0.9 seconds delay	

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

BEGIN_SBLOCK:	LD	IX,$4000	; begin of RAM
NEXT_SBLOCK:	CALL	DELAY	
		LD	A,(IX+00)       ; LSB size of block	
		LD	D,(IX+01)       ; MSB size of block
		OR	D
		JR	Z,EXIT_SAVE	; size = 0, jump to FFD2
		LD	E,(IX+00)	; DE = size of block kept on RAM
		LD	A,(IX+02)	; A = block flag
					
					; could be
					; LD E,(IX+0)
					; LD D,(IX+1)
                                        ; LD A,E
					; OR D
					; JR ....
                                        ; LD A,(IX+2)
                                        ; less 3 bytes
		; Add 3 to get past word block SIZE + flag
		INC	IX
		INC	IX		
		INC	IX

		; IX is now pointing at data to be saved
		; DE has lenght of data to be saved

		CALL	SA_BYTES 	; CALL	SA_BYTES
					; change it to CALL $04C6
					; and delete DI - one less byte
					; and does not PUSH SA/LD-RET into the stack
					; so it does not enable interrupts back.
					; As it is, EI is executed at $054F, after
					; saving a block

                DI                      ; disabling interrupts, enabled in SA/LD-RET
                                        ; returning from SA_BYTES

				        ; if an interrupt comes before DI
					; it needs 20 bytes stack
					; if a interrupt happens and a key is pressed
					; machine code will be corrupted
					; at the DELAY function

					; nevertheless, an interrupt happening
					; just before DI
					; even if not overflowing stack
					; there will be data corruption, at least
					; 1 or 2 bytes at FRAMES ($5C78)
					; (and it happens occasionally) 


		DEC	IX
		JR	NEXT_SBLOCK	; saves next block

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
		JP	Z,BEGIN      	; begin from scratch
					; ought be MAINLOOP

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
DLOOP:		DEC	BC		; $FFFF DECs
		LD	A,B		; till BC=0
		OR	C
		JR	NZ,DLOOP	; JR if BC not equal 0
		RET

;FFEF		15 bytes for stack, so 14 available without corrupting MC 
;		(SP should be initialized to 0 and not $FFFF for using 16 bytes)
		
