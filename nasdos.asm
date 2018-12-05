; z80dasm 1.1.2
; command line: z80dasm.exe -v --origin=0xd000 --sym-output=dossym.asm --sym-input=nasdefs.asm --block-def=blocks.txt -l -t -o nasdos.asm nasdos.rom

;
; Re assemblable version of NAS-DOS version 1.4.
; Created Jan 2014, Phill Harvey-Smith.
; Originally disassembled Jan 2014 from the MESS nasdos.rom, which was a modified copy
; of nasdos 1.4.  Updated 2018-12-05 to match v1.4 ROMS dumpred from PHS' Nascom 3.
;
; To be built with z80asm : https://www.nongnu.org/z80asm/
;

	org	0d000h
DOSCold:
	jp DoDOSCold		; Cold start
DOSWarm:
	jp DoDOSWarm		; warm start
DOSInit:
	jp DoDOSInit		; Init drive 0
DOSFRMT:
	jp DoDOSFRMT		; Format a disk 
DOSRSCT:
	jp DoDOSRSCT		; Read a sector
DOSRTRK:
	jp DoDOSRTRK		; Read a track
DOSWSCT:
	jp DoDOSWSCT		; Write a sector
DOSWTRK:
	jp DoDOSWTRK		; Write a track
DOSCMND:
	jp DoDOSCMND		; Write a command to the FDC
DOSDSEL:
	jp DoDOSDSEL		; Drive select
	jp 0ffffh		;d01e	c3 ff ff 	. . . 
	jp 0ffffh		;d021	c3 ff ff 	. . . 

; BLOCK 'data00' (start 0xd024 end 0xd076)
DLSPD:	
	defb DOSSides		; Sides per disk
DLTPS:
	defb DOSTrkPerDsk	; Tracks per side 
DLDPS:
	defb 007h		; Drives per system

; Workspace pointer, to block of 8 bytes in RAM.
WORKSP:
	defw WKSPACE		

; Dos version no as ASCII?
	defb 031h		;d029	31 	1 
	defb 02eh		;d02a	2e 	. 
	defb 034h		;d02b	34 	4 
	defb 010h		;d02c	10 	. 

; Re-enable ints and return
DoEIRET:
	ei
	ret

; Unknow and un jumped to routine, seems to just print the following text and
; return to nas-sys. Something to do with DOS serial no perhaps ?
PrintSerialno:
	rst	$28

	defb	'(C)Copyright 1982 & 1983 '
	defb	'Lucas Logic Ltd.'
	defb	'by'
	defb	00dh
	defb	'Dove Computer Services'
	defb	00dh,000h
	
; Original version (hack) from MESS had the copyright text removed!
;                0123456789ABCDEF
;	defb	'                '
;	defb	'                '
;	defb	'                '
;	defb	' 51103',0dh,00h
	
;	defb 000h, 000h, 000h, 000h
;	defb 000h, 000h, 000h, 000h
;	defb 000h, 000h, 000h, 000h
	
	rst	$18
	defb	MRET		; Return to NAS-SYS
;
; Initialize DOS.
;

DoDOSInit:
	ld a,WDCmdForceInt	; Force interrupt, reset FDC 
	out (CmdStatReg),a	; Send command
	ld a,CTRLBMotor+CTLBDS0	; Turn motor on and select drive 0 (021h)
	out (CTRLReg),a		
	
	in a,(CTRLReg)		; Read status reg
	and MaskDriveSide	; Select just drive id & side (01fh)
	dec a			; Check for zero : drive 0, side 0
	ld a,01fh		
	ret nz			; not drive 0 side 0
	
	in a,(CmdStatReg)	; read status
	push hl			; save HL
	ld hl,(WORKSP)		; get pointer to workspace in RAM 
	ld (hl),0ffh		; Set first byte
	pop hl			; Restore HL & return 
	ret			
	
;
; Format a disk.
; Entry :
;	A = logical drive no to format.	
;
 
DoDOSFRMT:
	push af			; Save AF
ld091h:
	rst 28h			; Display string
	
	defb	'Skew? (1-8) ',000h
	
ld09fh:
	rst 18h			; Get character from keyboard wait & blink 
	defb	BLINK
	
	cp 031h			; Check between 1..8
	jr c,ld091h		; lower : prompt again
	cp 039h			; > 8
	jr nc,ld091h		; higher : prompt again
	
	rst 30h			; Display typed character
	
	push af			; Save skew
	rst 18h			; Output CRLF
	defb	CRLF
	
	pop af			; Restore skew
	and 00fh		; Convert to binary by masking off MSN
	ld hl,DOSFormatBuf	; Format buffer at 1000h 
	push hl			; Save buffer ptr

; I suspect the following code is a bunch of loops that generate the raw sectors for
; each track to be formatted	

	ld b,DOSSecPerTrk	; 16 bytes of 0 
ld0b6h:
	ld (hl),000h		 
	inc hl			
	djnz ld0b6h		
	pop hl			; Restore buffer ptr
	
; fill in a table of 16 bytes containing the sector numbers of the sectors on the track
; with the appropreate skew factor

	ld b,000h		; Zero B, BC now contains 16 bit skew 
	ld c,a			; Skew into c
	ld d,001h		; Start sector no
ld0c1h:
	ld a,(hl)		; Get byte from buffer
	inc hl			; point to next byte
	or a			; Set flags
	jr nz,ld0cfh		; End : skip
	
	dec hl			; Move back 1 
	ld (hl),d		; store sector no ?
	inc d			; next sector no
	ld a,DOSSecPerTrk	; Number of sectors / track 
	sub d			; Calculate number left
	jr c,ld0e3h		; done all : exit
	add hl,bc		; add skew to pointer
ld0cfh:
	push hl			; Save buffer ptr & skew
	push bc			;
	ld bc,DOSRawSecBuf	; point to end of buffer
	or a			;
	sbc hl,bc		; Subtract end from current pos, will be -ve if not end of buffer
	pop bc			; Restore buffer ptr & skew
	pop hl			
	jr c,ld0c1h		; not past end of buffer, continue
	
	push bc			; Save skew
	ld bc,0-DOSSecPerTrk	; move HL back inside buffer by adding 64K-16
	add hl,bc		; this will wrap around back inside buffer  
	pop bc			; restore skew
	jr ld0c1h		; do next


ld0e3h:
	pop af			; Restore & re-save logical drive ID
	push af			
	call LogToPhysDrv	; Convert to physical drive
	or CTRLBMotor		; Mask in motor on
	out (CTRLReg),a		; Send to control reg
	
	call CheckDriveReady	; Check that the drive has come on and has a disk in
	jr nz,ld107h		; Error : 
	
	rst 10h			; RCAL
	defb 	(ld14d-$)-1	
	
	jr nz,ld107h		; Error : exit 
	
	ld a,(DLSPD)		; Get sides per disk
	dec a			; Test double sided
	jr z,ld107h		; nope : skip
 
	in a,(CTRLReg)		; Read control reg
	and MaskDrives		; Mask out all but drive ID
	or CTRLBMotor + CTLBSide ; Turn on motor select side 1	
	out (CTRLReg),a		; Do it
	rst 18h			; SCAL 
	defb	CRLF		; Print EOL
	
	rst 10h			; RCAL 
	defb	(ld14d-$)-1	; destination offset
	
	
ld107h:
	ld c,a			; 
	pop af			; Restore drive
	call DoDOSDSEL		; Select it
	
	ld a,c			;
	push af			; Save 
	rst 28h			; Print stringz
	
	defb	00dh
	defb	'Creating Directory',000h
	
	ld hl,DOSRawSecBuf	; Point to buffer
	push hl			; Save   
	ld (hl),0ffh		; fill in first byte
	ld de,DOSRawSecBuf+1	; point to second byte in buffer
	ld bc,00fffh		; count
	ldir			; fast fill !
	pop hl			; Restore
	
	ld de,00001h		; 
	ld b,DOSSecPerTrk	; Sector count
ld137h:
	call sub_d396h		; Write the sector out
	jr nz,ld143h		; 
	
	inc e			; 
	djnz ld137h		; More sectors to do, do them
	
	rst 18h			; SCAL 
	defb	CRLF		; output EOL
	
	pop af			; Restore & return
	ret			
ld143h:
	rst 28h			; Print stringz 
	defb	'-',000h
	
	rst 18h			; SCAL
	defb 	ERRM		; exit with error
	
	pop af			; Restore 
	ld a,0ffh		; Flag error
	or a			
	ret			

ld14d:
				; restore to track 0
	ld a,WDCmdRestore + WDCHeadLoad + WDCStepRate  		
	call DoDOSCMND		; Send to FDC
	ld hl,FormatData	; Point to data for format
	ld de,DOSRawSecBuf	; Point just past sector number buffer

ld158h:
	ld c,(hl)		
	ld a,c			; get block count
	or a			; end ?
	jr z,ld175h		; yep : exit
	
	inc hl			
ld15eh:
	push hl			; Save pointer for later 
ld15fh:
	pop hl			
	push hl			
	ld b,(hl)		; Get bytecount
	inc hl			
ld163h:
	ld a,(hl)		; Get value
	inc hl			
ld165h:
	ld (de),a		; Put byte in format buffer
	inc de			; point to next byte of buffer
	djnz ld165h		; Loop until no more
	
	ld b,(hl)		; get next byte count 
	inc hl			
	ld a,b			
	or a			
	jr nz,ld163h		; if nonzero loop again
	
	dec c			; Decrement block count
	jr nz,ld15fh		; Keep going if more to do
	
	pop af			;
	jr ld158h		;

ld175h:
	ld a,(DLTPS)		; Get tracks per side
	ld e,a			;
				; restore to track 0
	ld a,WDCmdRestore + WDCHeadLoad + WDCStepRate  		
	call DoDOSCMND		; Send to FDC
	jp c,RetDOSErrInvFmt		; Jump on error
	
;
; Scan over the raw sector data in RAM, and fill in the sector ID
; and track numbers.
;
	
ld181h:
	push de			
	ld hl,01040h		; Offset in buffer
	ld de,00170h		; Size of each (raw sector) entry ?
	ld a,DOSSecPerTrk	; Sector count ?
	ld b,a			; Process this many sectors
	ld ix,DOSFormatBuf	; Buffer base (secor ids)
ld18fh:
	in a,(TrackReg)		; Read track 
	ld (hl),a		; Put in sector header
	inc hl			; Next byte in buffer 
	and 001h		; Mask in odd / even bit
	rlca			; a=a*8
	rlca			;
	rlca			;
	ld c,a			; save a

	in a,(CTRLReg)		; Read control reg
	and MaskDriveSide	; Mask out all but drive & side 
	or CTRLBMotor		; Mask in motor on
	out (CTRLReg),a		; Do it 

	and CTLBSide		; Mask out all but side
	rrca			; Shift it to bit 0
	rrca			
	rrca			
	rrca			

	ld (hl),a		; Put it in buffer
	inc hl			; Next byte
	ld a,(ix+000h)		; Get sector ID 
	dec a			; decrement (sector id 0..f)
	xor c			; mask in track odd / even	. 
	inc a			; inc id (01..10)
	inc ix			; Point to next id 
	ld (hl),a		; Save in raw sector
	add hl,de		; move to next entry
	djnz ld18fh		; loop if more to do
	
	pop de			; Restore
	ld hl,DOSRawSecBuf	; Point to raw data buffer
	push de			; Save regs
	push hl			
	ld a,WDCmdWriteTrack + WDCDelay5ms ; Write the track
	call SendCMDandData	; Send the command and format data
	
	pop de			; Restore
	push af			; Save AF while we print
	or a			;
	sbc hl,de		;
	in a,(TrackReg)		; Read trackreg
	
	rst 18h			; SCAL
	defb	B2HEX		; Print track as hex 
	
	rst 28h			; Print string
	defb	'-',000h
	
	rst 18h			; SCAL 
	defb	TBCD3		; Output HL as hex
	
	pop af			; Restore status from FDC
	pop de			
	jr nz,RetDOSErrInvFmt	; Error : exit error ff	

; HL at this point contains the last byte written to the track.	

	push hl			; save byte count 
	ld bc,017e0h		; low vbalue for last byte
	or a			; clear flags
	sbc hl,bc		; check it
	pop hl			; restore hl
	jr c,RetDOSErrInvFmt	; too few bytes written disk spinning too fast!
	
	push hl			; save byte count
	ld bc,018a0h		; high value for last byte written
	or a			; clear flags
	sbc hl,bc		; check it
	pop hl			; restore hl
	jr nc,RetDOSErrInvFmt	; too many bytes written disk spinning too slow!
	
	xor a			;d1e6	af 	. 
	dec e			;d1e7	1d 	. 
	ret z			;d1e8	c8 	.
 
	ld a,WDCmdStepIn + WDCTrkUpdate+ WDCHeadLoad + WDCStepRate
	call DoDOSCMND		; Step to next track
	
	cp 020h			; Error ?
	jr z,ld181h		; Nope : do next

RetDOSErrInvFmt:
	ld a,DOSErrInvFmt	; Error : Invalid format
	or a			; Set flags
	ret			; return to caller

; Raw sector data definition table
; byte defs  count value 	
FormatData:
; beginning of track pre-amble
	defb 001h  		; 1 repeat
	defb 020h, 04eh 	; byte def(s)
	defb 000h		; terminator
	
; sector data
	defb DOSSecPerTrk	; 10h repeats
	defb 00ch, 000h		
	defb 003h, 0f5h 
	defb 001h, 0feh 	; ID address mark
	defb 004h, 001h 	; Track, side, sector id, sector len (filled in later)
	defb 001h, 0f7h 	; CRC calculated and written by FDC 	
	defb 016h, 04eh 	
	defb 00ch, 000h		
	defb 003h, 0f5h
	defb 001h, 0fbh		; Data address mark
	defb 080h, 040h		; data in 2 blocks of 128		
	defb 080h, 040h
	defb 001h, 0f7h		; CRC calculated and written by FDC 
	defb 036h, 04eh
	defb 000h		; terminator
	
; track filler
	defb 003h 		; 03h repeats
	defb 0ffh, 04eh		; byte def(s)
	defb 000h		; terminator
	
	defb 000h		; table terminator

; Entry : A=logical drive no
; Exit  : A=physical drive no (ds bit).
LogToPhysDrv:
	push bc			; Save regs
	ld b,a			; Save drive no
	ld a,(DLSPD)		; Get logical sides / disk
	dec a			; dec side count 
	jr z,ld227h		; Zero : exit
	sla b			; drive no = drive no * 2  
	jr c,ld22dh		; 
ld227h:
	ld a,b			; Recover logical drive no
	and 00fh		; Mask out MSN
	cp b			; MSN set ? 
	jr z,ld230h		; No : continue
ld22dh:
	pop bc			; Restore
	scf			; Flag error
	ret			; Return
	
ld230h:
	push af			; Save drive no
	srl a			; 
	ld b,a			;d233	47 	G 
	inc b			;d234	04 	. 
	xor a			;d235	af 	. 
	scf			;d236	37 	7 
ld237h:
	rla			;d237	17 	. 
	djnz ld237h		;d238	10 fd 	. . 
	ld b,a			;d23a	47 	G 
	pop af			;d23b	f1 	. 
	bit 0,a		;d23c	cb 47 	. G 
	ld a,b			;d23e	78 	x 
	pop bc			;d23f	c1 	. 
	ret z			;d240	c8 	. 
	or 010h		;d241	f6 10 	. . 
	ret			;d243	c9 	. 

;
; Select a drive
;
; Entry :
; 	A = logical drive number 0..7
; Exit :
; 	A = FDC status
;

DoDOSDSEL:
	push af			; Save Regs
	call LogToPhysDrv	; Translate logical drive no to physical
	or CTRLBMotor		; Mask in motor control
	out (CTRLReg),a		; Select drive and turn on motor 
	
	call CheckDriveReady	; Is the drive up to speed and loaded ?
	jr nz,ld294h		; Nope : error exit
	
	pop af			; Restore logical drive nr
	call LogToPhysDrv	; Translate logical drive no to physical
	and 00fh		; Make sure it's valid
	push bc			; Save reg
	ld b,a			; B=physical drive
	push hl			; Save HL
	ld hl,(WORKSP)		; Get workspace pointer in HL
	ld a,(hl)		; Get first byte of workspace
	pop hl			; Restore HL
	cp b			; Is saved drive same as current ?
	ld a,b			; Copy current drive back to a
	pop bc			; Restore BC
	jr z,ld292h		; Current drive = saved
 
	push hl			; Save HL
	ld hl,(WORKSP)		; Get workspace pointer in HL
	ld (hl),a		; Set saved drive = current
	inc hl			;
	inc hl			;
	push bc			; Save BC
	ld b,005h		; retry count
ld26eh:
	ld a,WDCmdReadAddr+WDCDelay5ms	; Read address
	call sub_d2f0h		; Give command to FDC
	
	jr z,ld288h		; Address read ok ? yep : skip
	djnz ld26eh		; Decrement count, and try again if not 0
	
	in a,(CTRLReg)		; Read control reg
	and MaskDrives		; Mask out all but drive id 
	or CTRLBMotor		; Mask in motor control
	out (CTRLReg),a		; Select drive and turn on motor 
	
	ld a,WDCmdRestore + WDCHeadLoad + WDCStepRate ; Restore to track 0
	call DoDOSCMND		; Do it!
	pop bc			; Restore regs 
	pop hl			
	jr ld296h		; return error 01fh
ld288h:
	pop bc			; Restore BC 
	ld hl,(WORKSP)		; Point HL at workspace
	inc hl			; +2
	inc hl			
	ld a,(hl)		; Get first byte of ID (track no)
	pop hl			; Restore HL
	out (TrackReg),a	; Update track register 
ld292h:
	xor a			; Zero A & flag no error
	ret			
ld294h:
	pop bc			; Restore regs
	pop af			 
ld296h:
	ld a,DOSErr1F		; Error code 
	or a			; Flag error
	ret			 

; Check for drive being ready, waits with timeout.
; Exit: A=Error code flags.nz on error
CheckDriveReady:
	push bc			; Save regs
	ld bc,00000h		; Zero counter?
ld29eh:
	in a,(INTReg)		; Read int masks
	bit 1,a			; Is drive ready ?
	jr z,ld2adh		; Yes : exit
	call ShortDelay		; Wait a little while 
	jr nz,ld29eh		; if count nozero, loop again
	
	ld a,DOSErr1F		; Set error code
	jr ld2aeh		
ld2adh:
	xor a			; Make sure A=0
ld2aeh:
	pop bc			; Restore 
	or a			; Set flags to flag error (or not) 
	ret			

;
; Read a track / Read sector.
; Entry :
; 	 A = Drive
; 	 D = Track
;	 E = Sector
;	HL = Address of buffer
; Exit :
; 	 A = FDC Error code.
;
	
DoDOSRTRK:
	call DoDOSDSEL		; Select drive
	ret nz			; Error : exit
	ld a,WDCmdReadTrack+WDCDelay5ms	; Read track command
	jr ld2bfh		

DoDOSRSCT:
	call DoDOSDSEL		; Select drive
	ret nz			; Error : exit
	ld a,WDCmdReadSec	; Read a sector

ld2bfh:
	push hl			; Save buffer pointer
	ld hl,(WORKSP)		; Get pointer to workspace
	inc hl			; +1
	ld (hl),a		; Save command
	pop hl			; Restore buffer pointer
	
	call sub_d2e2h		; Read data to buffer
	ret z			; Error : exit
	
; Error so re-calibrate the drive and try again !
	
	; Step in
	ld a,WDCmdStepIn + WDCTrkUpdate + WDCHeadLoad + WDCStepRate		
	call sub_d2deh		; Send to FDC
	ret z			; Error : exit 

	; Restore to track 0 & verify
	ld a,WDCmdRestore + WDCHeadLoad + WDCVerifyTrk + WDCStepRate 
	call sub_d2deh		; Send to FDC
	ret z			; Error : exit 

	; Step out
	ld a,WDCmdStepOut + WDCTrkUpdate + WDCHeadLoad + WDCStepRate		
	call sub_d2deh		; Send to FDC
	ret z			; Error : exit 

	; Restore to track 0 
	ld a,WDCmdRestore + WDCHeadLoad + WDCStepRate
sub_d2deh:
	call DoDOSCMND		; Send to FDC
	ret c			; ok : return

sub_d2e2h:
	call SeekToSideTrack	; Seek to the required track
	ret nz			; Return on error
	
	ld a,e			; Get sector no
	out (SectorReg),a	; Send it to FDC
	push hl			; Save buffer ptr
	ld hl,(WORKSP)		; Point to workspace
	inc hl			; offset 1
	ld a,(hl)		; Get current WD command
	pop hl			; Restore buffer ptr
	
; Falls through to.... 

sub_d2f0h:
	di			; Ints off
	push bc			; Save regs
	push hl			
	ld c,INTReg		; Read intreg when done
	out (CmdStatReg),a	; Send command to FDC
	jr ld2fbh		; Skip
ld2f9h:
	ld (hl),a		; Save read byte in memory
	inc hl			; point to next
ld2fbh:
	in b,(c)		; Poll int reg
	jr z,ld2fbh		; Wait until int happens
	
	in a,(DataReg)		; Read FDC Data reg
	jp m,ld2f9h		; IntRQ active ?, branch if not
	
	in a,(CmdStatReg)	; Read status reg
	or a			; Set flags
	jr z,ld30bh		; Status ok ==0 ?
	
	pop hl			; Recover & resave HL
	push hl			
ld30bh:
	pop bc			; Drop saved HL
	pop bc			; Recover BC
	jp DoEIRET		; Re enable ints & return
	

; 
; Seek to the track in D, setting side as needed.
;
SeekToSideTrack:
	push bc			; Save regs
	push de			
	ld a,(DLTPS)		; Get tracks per side
ld315h:
	dec a			; Max track TPS -1
	cp d			; Check specified track 
	jr nc,SeekToTrack	; Les than max, seek to it
	
	ld a,(DLSPD)		; Get logical sides / disk
	dec a			; decrement
	jr z,ld35fh		; Zero : error track to large
	
	ld a,(DLTPS)		; Get tracks per side
	push af			; Save AF
	rla			; A=A*2
	dec a			; -1
	sub d			; -target track
	ld d,a			; into D 
	dec d			; -1
	pop af			; Restore AF
	dec a			; Max track TPS -1
	cp d			; Cheack again
	jr c,ld35fh		; Still greater : Error
	
	in a,(CTRLReg)		; Read control reg
	and MaskDriveSide	; Mask out all but drives & side
	or CTLBSide + CTRLBMotor ; Select second side & turn on motor
	out (CTRLReg),a		

SeekToTrack:
	in a,(CTRLReg)		; Read control reg
	and MaskDriveSide	; Mask out all but drives & side
	ld c,a			; Take a copy for later
	or CTRLBMotor		; Mask in motor
	out (CTRLReg),a		; Send it
	
	in a,(CTRLReg)		; Read control reg
	and MaskDriveSide	; Mask out all but drives & side
	cp c			; Same as saved copy from above
	jr nz,ld359h		; nope !
	
	in a,(TrackReg)		; Read track reg
	cp d			; Are we already on required track ? 
	jr z,ld35ch		; yes : skip
	
	ld a,d			; get required track in a
	out (DataReg),a		; put it in data reg
				
	ld a,WDCmdSeek + WDCHeadLoad + WDCVerifyTrk + WDCStepRate		
	call DoDOSCMND		; Send seek command to FDC 
	jr c,ld359h		; Error : exit
	
	bit 4,a			; Check for seek error
	scf			;
	jr z,ld35ch		; nope : return ok
ld359h:
	ld a,DOSErrTimeOut		; Return error
	or a			; set flags 
ld35ch:
	pop de			; Restore & return
	pop bc			
	ret			
	
ld35fh:
	ld a,DOSErrInvDat	; Flag error
	or a			
	jr ld35ch		

;
; Execute an FDC command.
; Entry :
;	A = FDC command byte
; Exit :
;	A = FDC status byte. Carry set on error.
;
	
DoDOSCMND:
	di			; Disable ints
	push bc			; Save BC 
	ld bc,08000h		; Setup timeout count (was 4000h in MESS version).
	out (CmdStatReg),a	; Send command to FDC
ld36bh:
	in a,(INTReg)		; Check for int
	rra			; get IntRQ flag into carry 
	jr nc,ld377h		; No int yet ? : continue waiting
	
	in a,(CmdStatReg)	; Read FDC status
	or a			; Set flags 
ld373h:
	pop bc			; Restore BC 
	jp DoEIRET		; Enable ints and return
	
ld377h:
	call ShortDelay		; Wait a while
	jr nz,ld36bh		; if no timeout : continue waiting
	
	ld a,DOSErrTimeOut		; Return error
	or a			; Set flags
	scf			; Flag error
	jr ld373h		; return

; Entry : BC=counter
; Exit  : BC=BC-1, flags set for BC==0
ShortDelay:
	dec bc			; Decrement count 
	ld a,b			; Set flags so we know when bc=0
	or c			
	ex (sp),hl		; burn some time
	ex (sp),hl			 
	ex (sp),hl			
	ex (sp),hl			
	ret			

;
; Write a track / Write sector.
; Entry :
; 	 A = Drive
; 	 D = Track
;	 E = Sector
;	HL = Address of buffer
; Exit :
; 	 A = FDC Error code.
;

	
DoDOSWTRK:
	call DoDOSDSEL		; Select drive
	ret nz			; Error : exit
	ld a,WDCmdWriteTrack + WDCDelay5ms ; Write track command
	jr ld398h		
	
DoDOSWSCT:
	call DoDOSDSEL		; Select drive
	ret nz			; Error : exit
sub_d396h:
	ld a,WDCmdWriteSec	; Write sector command
ld398h:
	push hl			; Save buffer pointer
	ld hl,(WORKSP)		; Get pointer to workspace
	inc hl			; +1
	ld (hl),a		; Save command
	pop hl			; Restore buffer pointer

	call sub_d3bbh		;d39f	cd bb d3 	. . . 
	ret z			; Error : exit

; Error so re-calibrate the drive and try again !
	
	; Step in
	ld a,WDCmdStepIn + WDCTrkUpdate + WDCHeadLoad + WDCStepRate		
	call sub_d3b7h		; Send to FDC
	ret z			; Error : exit 

	; Restore to track 0 & verify
	ld a,WDCmdRestore + WDCHeadLoad + WDCVerifyTrk + WDCStepRate 
	call sub_d3b7h		; Send to FDC
	ret z			; Error : exit 

	; Step out
	ld a,WDCmdStepOut + WDCTrkUpdate + WDCHeadLoad + WDCStepRate		
	call sub_d3b7h		; Send to FDC
	ret z			; Error : exit 

	; Restore to track 0 
	ld a,WDCmdRestore + WDCHeadLoad + WDCStepRate
sub_d3b7h:
	call DoDOSCMND		; Send to FDC
	ret c			; ok : return

sub_d3bbh:
	call SeekToSideTrack	; Seek to the required track
	ret nz			; Return on error
	
	ld a,e			; Get sector no
	out (SectorReg),a	; Send it to FDC
	push hl			; Save buffer ptr
	ld hl,(WORKSP)		; Point to workspace
	inc hl			; offset 1
	ld a,(hl)		; Get current WD command
	pop hl			; Restore buffer ptr

	jr nc,SendCMDandData		; skip
	cp WDCmdWriteTrack + WDCDelay5ms ; Is it write track ?
	jr z,SendCMDandData		; Yes : skip 
	ld a,WDCmdWriteSec + WDCDelay5ms  ; no : add delay to write sector
	
;
; Disable ints, the send command (in A) to FDC followed by data at HL.
;
	
SendCMDandData:
	di			; Ints off
	push bc			; Save regs
	push hl			
	ld c,INTReg		; Read intreg when done
	out (CmdStatReg),a	; Send command to FDC
ld3d8h:
	ld a,(hl)		; Get data byte to send to FDC
	inc hl			; update data ptr
ld3dah:
	in b,(c)		; Poll int reg
	jr z,ld3dah		; Wait until int happens
	
	out (DataReg),a		; output data reg
	jp m,ld3d8h		; IntRQ active yet ? no : do next byte
	
	dec hl			; decrement data ptr
	in a,(CmdStatReg)	; read status register
	or a			; set flags
	jr z,ld3ebh		; no error ? yep : return
	pop hl			; Restore data ptr
	push hl			; resave
ld3ebh:
	pop bc			; Restore BC
	pop bc			
	jp DoEIRET		; Enable ints and return
	
	rst 38h			;d3f0	ff 	. 
	rst 38h			;d3f1	ff 	. 
	rst 38h			;d3f2	ff 	. 
	rst 38h			;d3f3	ff 	. 
	rst 38h			;d3f4	ff 	. 
	rst 38h			;d3f5	ff 	. 
	rst 38h			;d3f6	ff 	. 
sub_d3f7h:
	ret			;d3f7	c9 	. 
	nop			;d3f8	00 	. 
	nop			;d3f9	00 	. 
sub_d3fah:
	ret			;d3fa	c9 	. 
	nop			;d3fb	00 	. 
	nop			;d3fc	00 	. 
ld3fdh:
	ret			;d3fd	c9 	. 
	nop			;d3fe	00 	. 
	nop			;d3ff	00 	.

 
DoDOSCold:
	jp ld407h		
	
DoDOSWarm:
	rst 10h			; RCAL
	defb	(ld43eh-$)-1	; Routine offset

	jr ld463h		

;
; Cold start routine.
;

ld407h:
	ld sp,01000h		; Setup stack pointer 
	call 0000dh		; Init NAS-SYS 
	call sub_d803h		;d40d	cd 03 d8 	. . . 
	
	ld hl,DoDOSWarm		; Point to warm start routine
	ld a,(USRMCP)		; Check USRMCP vector 
	cp OpCodeJP		; has it been set ? (is it a jp instruction).  
	jr nz,ld41dh		; nope skip, use DoDOSWarm
	ld hl,(USRMCP+1)	; get vector address into HL
ld41dh:
	push hl			; Save USRMCP address
	ld a,OpCodeJP		; Save a jp in cold start vector
	ld (WFDC),a		
	ld (USRMCP),a		; and warm start vector
	
	ld hl,DoDOSWarm		; Point to warm start routine 
	ld (USRMCP+1),hl	; Set jump address
	
	ld hl,DOSCold		; Point to cold start routine
	ld (WFDC+1),hl		; Set cold start jump address
 
	ld a,05dh		; set command character to ]
	ld (CMDCHR),a
	
	ld hl,CMDTAB		; Point to command table
	ld (WCMTAB),hl		; Save it
	ret			

ld43eh:	
	pop hl			; restore USRMCP	. 
	ld (SUOUT+1),hl		; Save in user output jump
	rst 28h			; Display signon message

	defb	'-- NAS-DOS 1 -->'	; Signon message
	defb	'D40238'	; Dos serial number, prited on manual.
	defb	'<',00h
	
; Hacked version from MESS contained :
;	defb 	'Disc Operating System  ',0

; at exit from RST $28 a will contain zero.

	call DOSARH		; Call dos assembler routine handler function 0
	ld bc,055dfh		; ????
	rst 18h			; Call NAS-SYS 
	defb	MRET		; return to NAS-SYS
	
	
;
; Scan iput line for disk command ?
;
ld463h:
	push af			; Save AF 
	cp 00dh			; End of input line ?
	jr z,ld46ah		; Yep : scan it
ld468h:
	pop af			; Nope: restore & return 
	ret
	
ld46ah:
	ld hl,(CURSOR)		; Get pos of cursor
	rst 18h			; SCAL 	
	defb	CPOS		; Get address of first character of it's line	
	
	ld a,(CMDCHR)		; Get command character
	cp (hl)			; Is it our command character ?
	jr nz,ld468h		; Nope : exit
	
	ld b,a			; b= command char
	ld (hl),020h		; replace with space  
	rst 18h			; SCAL 	
	defb	CRLF		; Output CR LF
	
	ld hl,(CURSOR)		; Get pos of cursor 
	ld de,0ffc0h		; DE=-40h
	add hl,de		; HL=HL-40h, point to line before
	ld a,b			; recover command char
	ld (hl),a		; put it back in line

	ex de,hl		; point de at command line
	inc de			; move past command char
	ld a,(de)		; get char of actual command 
	cp 041h			; is it A or after ?
	jr c,ld468h		; nope : exit
	cp 05bh			; is it before Z
	jr nc,ld468h		; nope : exit
 
	ld (00ffeh),sp		; Make some room on stack
	ld sp,00ffeh		
	push af			;
	sub 041h		; convert command to binary
	sla a			; convert to table offset
	push de			; save command line pointer
	
	ld e,a			; Transfer offset to DE
	ld d,000h		; Zero MSB
	ld hl,(WCMTAB)		; get address of table
	add hl,de		; point to table entry for this command
	ld e,(hl)		; Get address of routine into DE
	inc hl			
	ld d,(hl)		
	ex de,hl		; save address of comand into hl 
	pop de			; restore command line pointer
	inc de			; move to next character on command line 
	pop af			; restore
	ld bc,DOSCmdOk		; 
	push bc			
	jp (hl)			; execute command routine
	
DOSCmdErr:
	rst 18h			; Output 'Error'
	defb 	ERRM
	
	jr ld4b8h		; Exit : command done
DOSCmdOk:
	rst 28h			; Display ok message.
	defb	'Ok.', 00dh, 000h
	
ld4b8h:
	ld sp,(00ffeh)		; Restore Stack pointer
	pop af			; Restore AF
	xor a			; Zero A	. 
	ret			; Return to caller

;
; Load a basic program
; ]Bd:fname
; on entry DE points to the command line after the command byte
;
	
DOSCmdB:
	call ReadDriveID	; Read drive ID & set drive
	call ReadFilename	; Read filename
	jr nc,ld4dah		; Found : skip on
 
 ; Filename not found on command line, look for it on the first line of 
 ; basic program
 
	ld de,(BASTXT)		; Get pointer to start of basic text
	inc de			; Increment past link & line number
	inc de			
	inc de			
	inc de			
	ld a,(de)		; Get a byte from basic 
	cp TokREM		; Is the first line a REM ? 
	jr nz,DOSCmdErr		; Nope : error
	inc de			; Move past it 
	call ReadFilename	; Try to read filename from basic
	jr c,DOSCmdErr		; Error if not found
ld4dah:
	ld hl,(PROGND)		; Get address of end of program
	ld (VAREND),hl		; Set end of variables & arrays to same address
	ld (ARREND),hl		
	ex de,hl		; Save end of save area
	ld hl,BASBaseAddr	; Point to base address of basic
	ld bc,ld7fdh		;
	jp ld65eh		;

;
; Clean directory
; ]Cd
; Warning corrupts memory from 01000h to 02c00h
;

DOSCmdC:	
	call ReadDriveID	; Read drive ID & set drive
	call DOSCmdSure		; Verify user wants to proceed, exit if not

	; fill 01e00h-02c00h with 0FFh
	ld hl,01e00h		; Beginning of block
	ld (hl),0ffh		; Set first byte
	ld de,01e01h		; dest 1 byte further on
	ld bc,00dffh		; Count
	ldir			; fill memory
	
	ld hl,DIRCleanAddr	; Load at 1000h
	ld de,DIRFirstSec	; Track (D) =0 , first sector (E)
	ld bc,DIRSize		; Sectors in dir
	ld a,(WDRV)		; Get drive
	
	push af			; Save params for later
	push bc			
	push de			
	call DOSARH		; Call dos to read dir
	defb 	AREAD
	
	ld hl,DIRCleanAddr	; Scan loaded dir
	ld de,DIRNewAddr	; Into new buffer as cleaned
	push de			; Save new buff ptr
	ld b,DIREntryCount	; Process up to this many entries
	
ld51ch:
	ld a,(hl)		; Fetch a byte fromm buffer
	cp DIRDeleted		; Deleted file ?
	jr z,ld52ch		; Yes : skip past
	jr nc,ld534h		; End of dir entries ? yep : exit 
	
	push bc			; Save entry count
	ld bc,DIREntryLen	; Copy entry to new buffer
	ldir			
	pop bc			; Restore entry count
	jr ld532h		; skip
	
ld52ch:
	push de			; Save new buff ptr
	ld de,DIREntryLen	; Skip past deleted in source
	add hl,de			
	pop de			; Restore new buff ptr
ld532h:
	djnz ld51ch		; if more entries keep going
ld534h:
	pop hl			; Restore loacatio, size etc of dir 
	pop de			
	pop bc			
	pop af			
	call DOSARH		; Re-save cleaned dir.
	defb	ASAVE
	
	ret
	
;
; display the directory from a drive
; ]Dd
;

DOSCmdD:
	call ReadDriveID	; Read drive ID & set drive
	rst 28h			; Print text 
	defb	' Id:',000h
	
	call	DOSARH		; Read driectory header
	defb	AREADH
	 
	rst 10h			; RCAL
	defb	(OutputFilename-$)-1	; display filename
	
	call DOSARH		; Open directory
	defb	AOPEND
	
	ld hl,00000h		; 
	push hl			;
ld554h:
	rst 28h			; Print text
	defb 00dh
	defb '    File name Exec Load Size Tr Sc'
	defb 00dh, 000h
	
	ld d,00ch		; Print 12 lines per page
ld57ch:
	call DOSARH		; Read directory
	defb 	AREADD
	
	jr c,ld5d5h		; Done all : exit
	
	rst 28h			; Print stringz
	defb '  ',000h		; left margin
	
	ld a,(WDDRV)		; Get working drive
	or 030h			; convert to ASCII
	rst 30h			; print it
ld58ch:
	ld a,DOSSeperator	; Print seperator
	rst 30h			
 
	rst 10h			; RCAL 
	defb (OutputFilename-$)-1	; offset
	
	rst 28h			; Print stringz
	defb '  ',000h		; space after filename
	
	ld l,(ix+000h)		; Retrieve exec address
	ld h,(ix+001h)		
	push de			; Save DE
	ld e,(ix+002h)		; Retrieve load address
	ld d,(ix+003h)		
	
	rst 18h			; SCAL
	defb	TX1		; Output HL (exec) and DE (load) as hex
	
	pop de			; Restore DE 
ld5a5h:
	ld l,(ix+006h)		; Retrieve size
	ld h,(ix+007h)		;
	push hl			; Save it 
	
	rst 18h			; SCAL
	defb	TBCD3		; output size as hex
	
	ld a,(ix+004h)		; Retrieve start track
	rst 18h			; SCAL
	defb	B2HEX		; Output it
	
	rst 18h			; SCAL 
	defb	SPACE		; output a space

	ld a,(ix+005h)		; Retrieve start sector
	rst 18h			; SCAL
	defb	B2HEX		; Output it
	
	rst 18h			; SCAL
	defb	CRLF		; Output EOL
	
	pop bc			; Restore regs
	pop hl			; running total of used sectors
	add hl,bc		; Add current file total to running total
	push hl			; Save running total
	
	dec d			; done all files in this page ? 
	jr nz,ld57ch		; nope : loop again
	
	call WaitKeyWithCursor	; Wait for a keypress
	jr ld554h		; do next page

OutputFilename:
	ld b,DOSMaxFNLen	; filename length
ld5cah:
	ld a,(ix+000h)		; get character from buffer
	and 07fh		; make it 7 bit ASCII
	rst 30h			; Print it 
	inc ix			; increment buffer pointer
	djnz ld5cah		; loop if more characters
	ret			; return
	
ld5d5h:
	pop de			; Restore DE
	ld hl,DOSDataSecs	; Max data sectors per disk
	or a			; clear carry 
	sbc hl,de		; Calculate free sectors
	
	rst 18h			; SCAL
	defb	TBCD3		; Output HL as hex

	rst 28h			; output stringz
	defb	'sctrs',00dh,000h
	ret			

;
; Execute a file
; ]Ed:filename
;
DOSCmdE:	
	call ReadIDFilename	; Read drive ID and filename 
	call DOSARH		; Load / Chain file
	defb	ACHAIN		
	jr ld62ch		; Error if it returns (fails).

;
; Format a disk
; ]Fd:diskname:
;

DOSCmdF:
	call ReadIDFilename	; Read drive ID and filename 
	call DOSCmdSure		; Prompt user to confirm
	ld a,(WDRV)		; Get drive id
	
	call DOSARH		; Tell dos to format disk
	defb 	AFORM
	
	push af			; Save drive id
	call DOSARH		; Read dir header
	defb	AREADH
	
	push ix			; de=ix, loaded sec address
	pop de			 
	ld bc,00008h		; 8 bytes
	ld hl,(WFILE)		; point at disk filename 
	ldir			; copy into sector
	
	ld hl,ld7c6h		; Point at 'random' number 
	ld bc,00003h		; 3 bytes
	ldir			; copy into sector
	
	call DOSARH		; resave disk header
	defb	ASAVEH
	
	pop af			;d619	f1 	. 
	inc a			;d61a	3c 	< 
	ret nz			;d61b	c0 	. 
	
	rst 28h			;d61c	ef 	. 
	defb	'Fmt.',000h
	jr ld62ch		;d622	18 08 	. . 

;
; Load a file, do not execute.
; ]Ld:filename
;

DOSCmdL:
	call ReadIDFilename	; Read drive ID and filename 
	call DOSARH		; Read whole file 
	defb	AREADF		
	ret nc			; Return if no error else fall through

ld62ch:
	jp DOSCmdErr		;d62c	c3 ae d4 	. . . 

;
; Save object code
; ]Od:filename:llll hhhh eeee (low addr, high addr, exec addr).
;
	
DOSCmdO:	
	call ReadIDFilename	; Read drive ID and filename 
	rst 18h			; SCAL
	defb	RLIN		; Read HEX words to ARGS
	
	jr c,ld62ch		; Error :
	
	ld a,(ARGN)		; Get the number of arguments converted
	cp 003h			; must be 3
	jr nz,ld62ch		; Error : invalid no of args
	
	rst 18h			; SCAL 
	defb 	ARGS		; Collect args
	
	rst 10h			; RCAL
	defb	(ld65eh-$)-1	; Save the file
	
	jp ClearARGS		; Skip

;
; Save a NAS-PEN file
; ]Pd:filename
;
	
DOSCmdP:	
	call ReadDriveID	; Read drive ID from command line
	call ReadFilename	; Read filename from command line
	jr nc,ld654h		; Got it, skip
	
	ld de,01020h		; Try to read from NAS-PEN source
	call ReadFilename	
	jr c,ld62ch		; not found : error
ld654h:
	ld hl,PENBaseAddr	; Base address of NAS-PEN area
	ld de,(PENEndText)	; Last used byte
	ld bc,PENWarm		; Set exec addr to NAS-PEN warm start

ld65eh:
	call DOSARH		; Save the file
	defb ASAVEF
	ret

;
; Read directly from disk.
; ]Rllll nnnn tt ss d
; llll	addr 
; nnnn	no sectors
; tt	track
; ss	sector
; d	drive
;

DOSCmdR:	
	rst 18h			; SCAL
	defb	RLIN		; read command line to ARGN, ARGx 
	
	jr c,ld62ch		; Error :exit
	
	call CheckRWArgs	; Check for correct number & format of args
	ld hl,(ARG1)		; Get load address
	ld bc,(ARG2)		; Get number of sectors
	
	call sub_d6f6h		; Get track, start sec & drive no
	
	call DOSARH		; Read sector
	defb	AREAD
	
	jr ClearARGS		; Skip
	
;
; Scratch (delete) a file
; ]Sd:filename
;

DOSCmdS:
	call ReadIDFilename	; Read drive ID and filename 
	call DOSCmdSure		; Get confirmation from user
	
	call DOSARH		; Call dos to scratch the file
	defb	ASCRAT		
	
	jp c,DOSCmdErr		; Flag error else return
	ret			

;
; Load user boot
; ]Ud
;
	
DOSCmdU:
	call ReadDriveID	; Get drive ID
	ld de,00001h		; Track 0 (D), Sector 1 (E)
	ld bc,00001h		; Read 1 sector 
	ld hl,DOSUserAddr	; Load / Exec addr
	push hl			; Push load addr on stack so we can jump to it with ret
	call DOSARH		; Read the boot sector
	defb	AREAD		
	ret			; Jump to boot sector
	
;
; Verify a file
; ]Vd:filename
;
DOSCmdV:
	call ReadIDFilename	; Read drive ID and filename 
	call DOSARH		; Scan directory for file
	defb	ASCAND
	
	jr c,ld62ch		; Error : exit
	ld b,(ix+DIRFlen+1)	; BC = file length
	ld c,(ix+DIRFlen)	
	ld d,(ix+DIRTrack)	; Get start track
	ld e,(ix+DIRSector)	; Get start sector
	ld hl,DOSCold		; Load address : rom so nothing gets over-written.
ld6b2h:
	push bc			; Save sector count
	ld bc,00001h		; Read 1 sector
	ld a,(WDRV)		; Get current drive
	call DOSARH		; Read the sector
	defb	AREAD
	
	pop bc			; Retrieve sector count 
	inc e			; Move to next sector
	ld a,e			; Check for end of track
	cp DOSSecPerTrk+1	
	jr nz,ld6c7h		; nope : do next on this track
	
	ld e,001h		; Start new track, sector = 1
	inc d			; increment track number
ld6c7h:
	dec bc			; decrement sector count
	ld a,b			; Check for all done, BC=0
	or c			
	jr nz,ld6b2h		; More to do : loop again
	ret			
;
; Write to disk
; ]Wllll hhhh tt ss d
; llll	start addr 
; hhhh 	end addr
; tt	track
; ss	sector
; d	drive
;
DOSCmdW:	
	rst 18h			; SCAL
	defb	RLIN		; read command line to ARGN, ARGx 

	call CheckRWArgs	; Check for correct number & format of args
	rst 18h			; Collect args
	defb	ARGS

	push hl			; Save buffer addr 
	ex de,hl		; Swap start and end addr 
	scf			
	sbc hl,de		; Work out differenc in bytes
	ld bc,00000h		; Init sector count 
	ld de,DOSSecSize	; Sector size in bytes
	
ld6dfh:
	sbc hl,de		; decrement buffer length
	inc bc			; increment sector count
	jr nc,ld6dfh		; keep going if buffer left
	pop hl			; restore start address
	
	rst 10h			; RCAL
	defb	(sub_d6f6h-$)-1	; offset
	
	call DOSARH		; Write the data
	defb 	ASAVE

; Zero out the Nassys ARGS area
ClearARGS:	
	ld hl,ARG1		; point at base of ARGS
	ld b,ARGLEN		; length of ARGS
ld6f0h:
	ld (hl),000h		; Zero a byte 
	inc hl			; move to next 
	djnz ld6f0h		; More to do keep going 
	ret			;

; Get track sector and drive from ARGS into d, e & a
sub_d6f6h:
	ld a,(ARG3)		; Get track
	ld d,a			
	ld a,(ARG4)		; Get start sector 
	ld e,a			
	ld a,(ARG5)		; Get drive
	ret			
	
;
; Save Zeap source file
; ]Zd:filename
;

DOSCmdZ:
	rst 10h			; RCAL
	defb	(ReadDriveID-$)-1	; Get ID
	
	rst 10h			; RCAL
	defb 	(ReadFilename-$)-1	; Get filename
	
	jr nc,ld71ah		; Filename ok : skip on
	
	ld hl,(ZEPBaseAddrPtr)	; Get address of first Zeap line
	ld de,00007h		; offset within line
	add hl,de		
	ld a,(hl)		; Get a byte from line
	cp 03bh			; Is it a colon ?
	jr nz,ld787h		; nope : error 
	
	inc hl			; Point at flename
	ex de,hl		; 
	
	rst 10h			; RCAL
	defb	(ReadFilename-$)-1	; read it
	
	jr c,ld787h		; Error : exit
	
ld71ah:
	ld hl,(ZEPBaseAddrPtr)	; Get addr of beginning of source
	push hl			; Save it
	ld e,(hl)		; Get source length into de
	inc hl			
	ld d,(hl)		
	dec hl			
	add hl,de		; Calculate end address
	
	ex de,hl		; end addr into de
	pop hl			; recover start addr
	
	ld bc,01003h		; Set exec address
	jp ld65eh		; Go save it

ReadIDFilename:
	rst 10h			; RCAL 
	defb	(ReadDriveID-$)-1	; Read drive ID
	
	rst 10h			;d72d	d7 	. 
	defb	(ReadFilename-$)-1
	
	
	jr c,ld787h		;d72f	38 56 	8 V 
	ret			;d731	c9 	. 

;
; Read drive ID from command line and update drive to access (WDRV).
; Entry
;	de = command line pointer.
; Exit
;	de = updated command line ptr, WDRV set.
;

ReadDriveID:
	xor a			; Zero A
	ld (WDRV),a		; Set drive to be accessed to 0
ld736h:
	ld a,(de)		; Get next byte from command line
	inc de			; increment command line ptr
	cp 020h			; Space
	jr z,ld736h		; yes : try next
	
	dec de			; decrement ptr 
	cp 030h			; chgaracter before '0' ? 
	ret c			; yep: return
	cp 038h			; character after '7' ?
	ret nc			; yep return
	
	and 00fh		; Convert drive digit to binary
	ld (WDRV),a		; Save drive id
	inc de			; Move cmd line ptr past digit
	ret			; return
	
;
; Get filename from command line and into buffer at WFILENAME
; Entry
;	de = command line pointer.
; Exit
;	de = updated command line ptr, WFILENAME, WFILE  set.
;
ReadFilename:
	ld b,DOSMaxFNLen	; Filename max length
	ld hl,WFILENAME		; Pointer to filename buffer
	ld (WFILE),hl		; Set it
	push hl			; Save
ld753h:
	ld (hl),020h		; Put space in buffer
	inc hl			; do next 
	djnz ld753h		; loop while more to do
	
	pop hl			; Restore buffer ptr 
	ld b,DOSMaxFNLen	; Filename max length
ld75bh:
	ld a,(de)		; Get a byte from command line
	and 07fh		; Make sure it's > 128
	inc de			; icrement cmd line ptr
	cp 020h			; Space ?
	ret c			; yep : return
	cp DOSSeperator		; is it a : ? 
	jr nz,ld75bh		; yep, skip and get next
ld766h:
	ld a,(de)		; Get next char  
	cp 020h			; Space ?
	ccf			;  
	ret nc			; yep : return
	cp DOSSeperator		; Seperator ?
ld76dh:
	inc de			; icrement cmd line ptr 
	ret z			; Seperator : return
	
	ld (hl),a		; Put char in filename buffer
	inc hl			; increment fn buff ptr
	djnz ld766h		; if more characters to do loop again 
	
	ld a,(de)		; get next character from buffer 
	cp DOSSeperator		; Seperator ?
	jr z,ld76dh		; Yep move past it & return
	or a			; Retrurn
	ret			;

; Display 'Sure?' 
DOSCmdSure:
	rst $28
	defb	'Sure?', 00dh, 000h

	rst 10h			; RCAL 
	defb	(WaitKeyWithCursor-$)-1	
	
	cp	$59		; Check for 'Y'
	ret z			; Yep: return else drop through to error
 
ld787h:
	jp DOSCmdErr		;d787	c3 ae d4 	. . . 

WaitKeyWithCursor:
	push de			; Save regs
	push hl			
	rst 18h			; Get a character in A with cursor	 
	defb	BLINK
	
	cp 00dh			; Enter ?
	jr nz,ld796h		; Nope: output it 
	ld hl,(CURSOR)		; Get address of cursor 
	ld a,(hl)		; get character there into A
ld796h:
	rst 30h			; Display it
	pop hl			; Restore regs 
	pop de			; 
	cp 01bh			; is it ESC ?
	jp z,DOSCmdOk		; Yep : exit ok
	
	push af			; Save regs
	rst 18h			; Output CRLF
	defb	CRLF			
	pop af			; Restore and return 
	ret			
	
CheckRWArgs:
	ld a,(ARGN)		; Get returned argument count
	cp 005h			; Check for 5 args
	jr nz,ld787h		; Not 5 : error
	
	ld ix,ARG3+1		; Check upper bytes of ARG3..5
	ld b,003h		; 3 bytes
ld7b0h:
	ld a,(ix+000h)		; get byte
	inc ix			; move to next 
	inc ix			
	or a			; Test for 0
	jr nz,ld787h		; nope : error 
	djnz ld7b0h		; loop if more
	ret			; return 
	
ld7bdh:
	ld hl,ld7c3h		;d7bd	21 c3 d7 	! . . 
	jp 0e411h		;d7c0	c3 11 e4 	. . . 
ld7c3h:
	nop			;d7c3	00 	. 
	adc a,c			;d7c4	89 	. 
	nop			;d7c5	00 	. 

; 3 bytes transfered to disk header after disk name when formatting a disk.
; their function is unknown.
ld7c6h:
	defb	0C7h,0FDh,02bh	; From PHS v1.4 ROM.
	
;	defb	099h,0ffh,02bh	; original bytes from MESS hacked dos rom.

; NAS-DOS interactive command handler tables, one address per handler 
; 26 command handlers ]A .. ]Z some of these just point to error
; routines.
CMDTAB:
	defw DOSCmdErr		; Command A
	defw DOSCmdB		; Command B
	defw DOSCmdC		; Command C
	defw DOSCmdD		; Command D
	defw DOSCmdE		; Command E
	defw DOSCmdF		; Command F
	defw DOSCmdErr		; Command G
	defw DOSCmdErr		; Command H
	defw DOSCmdErr		; Command I
sub_d7dbh:
	defw DOSCmdErr		; Command J
	defw DOSCmdErr		; Command K
	defw DOSCmdL		; Command L
	defw DOSCmdErr		; Command M 
	defw DOSCmdErr		; Command N
	defw DOSCmdO		; Command O
	defw DOSCmdP		; Command P 
	defw DOSCmdErr		; Command Q 
	defw DOSCmdR		; Command R 
	defw DOSCmdS		; Command S 
	defw DOSCmdErr		; Command T 
	defw DOSCmdU		; Command U 
	defw DOSCmdV		; Command V
	defw DOSCmdW		; Command W 
	defw DOSCmdErr		; Command X
	defw DOSCmdErr		; Command Y 
	defw DOSCmdZ		; Command Z 
ld7fdh:
	jp ld7bdh		

; Assembler routine handler
DOSARH:
	jp DoDOSARH		
; Init routine called by DOS Cold start
sub_d803h:
	jp ld880h		
; Basic routine handler
DOSBRH:
	jp DoDOSBRH		
	
; Assembler routine handler
DoDOSARH:
	ex af,af'		; Swap to alt AF pair
	ex (sp),hl		; save HL on stack, & get return address 
	ld a,(hl)		; Get parameter byte
	inc hl			; increment past it
	ex (sp),hl		; put new return address in 
	push bc			; Save regs
	push de			
	push hl			
	exx			; Swap to alternate reg set.
	ld e,a			; e=call number
	ld hl,(WDTAB)		; get pointer to ARH tab
	rst 10h			; Relative call
	defb	(ScanTable-$)-1	; scan call table
	
	jp c,ErrorCodeExit	; Error, function not found :  
	ld hl,ld833h		; Return to here after function execution ?
	push hl			; save return address and function address on stack
	push de			 
	exx			; Use Alt regs
	ex af,af'		
	ret			; jump to function

;
; Scan ARH/BRH table, entries consist of 3 bytes :
; offset 	size 	function
; 0		1	Function call number of this entry
; 1		2	Address of handler
; The table is terminated with an entry with function 0.
;
; On entry : 
;	HL points to the table to scan, 
;	E contains the function number
;
; On exit :
;	DE contains the address of the function handler
;	CF is set on error (end of table reached).
;

ScanTable:
	ld a,(hl)		; Get a byte from table
	inc hl			; increment past it
	or a			; Is A == 0 ?
	scf			; flag error
	ret z			; Yes : end of table
	
	cp e			; Is this the function we want ?
	jr z,ld82fh		; Yep : skip on
	inc hl			; Skip past handler address
	inc hl			
	jr ScanTable		; try next 
ld82fh:
	ld e,(hl)		; Get handler address into DE & return
	inc hl			
	ld d,(hl)		
	ret			
 
ld833h:
	pop hl			; Restore regs and return to caller 
	pop de			
	pop bc			
	ret			

ARHTable:
	defb AINITD		; Initialize drive
	defw DOSARH01
	
	defb AFORM		; Format a disk		
	defw DOSFRMT		
	
	defb ACHAIN		; Chain another program	
	defw DOSARH03		
	
	defb ACRATE		; Create a directory entry
	defw DOSARH04		
	
	defb ASCRAT		; Scratch a directory entry		
	defw DOSARH05		
	
	defb ASCAND		; Scan directory		
	defw DOSARH06		
	
	defb AOPEND		; Open a directory		
	defw DOSARH0A		
	
	defb AOPENI		; Open input file		
	defw DOSARH0B		
	
	defb AOPENO		; Open output file		
	defw DOSARH0C		
	
	defb APREPO		; Prepare new output file		
	defw DOSARH0D		
	
	defb AREAD		; Direct read		
	defw DOSARH14		
	
	defb AREADD		; Read directory		
	defw DOSARH15		
	
	defb AREADS		; Read sequential		
	defw DOSARH16		
	
	defb AREADR		; Read random	
	defw DOSARH17		
	
	defb AREADF		; Read whole file		
	defw DOSARH18		
	
	defb AREADH		; Read directory header		
	defw DOSARH19		
	
	defb ASAVE		; Direct save		
	defw DOSARH1E		
	
	defb ASAVED		; Directory saveh		
	defw DOSARH1F		
	
	defb ASAVES		; Save sequential		
	defw DOSARH20		
	
	defb ASAVER		; Save random		
	defw DOSARH21		
	
	defb ASAVEF		; Save whole file		
	defw DOSARH22		
	
	defb ASAVEH		; Save directory header		
	defw DOSARH23		
	
	defb ACLSEI		; Close input file
	defw DOSARH28		
	
	defb ACLSEO		; Close output file		
	defw DOSARH29		
	
	defb 000h		; end of table

ld880h:
	xor a			; Set default drive to 0
	ld (WDRV),a		
	dec a			; Mark input & output buffers invalid
	ld (WFPIN),a		
	ld (WFPOT),a		
	ld (WDDRV),a		; And director drive
	ld a,OpCodeJP		; Setup user disk io jump
	ld (USDIO),a		
	ld hl,DisplayErrExit	; and address
	ld (USDIO+1),hl		 
	ld hl,DOSMaxPrep	; Set max prep sectors
	ld (WSIZE),hl		
	ld hl,ARHTable		; Setup ARH table pointer
	ld (WDTAB),hl		
	ld hl,00f00h		; Top of NAS-SYS RAM ?
ld8a8h:
	inc h			; Try next page of RAM
	ld a,h			; Reached beginning of disk rom ?
	cp 0d0h			 
	jr z,ld8b6h		; Yep break out
	
	; Test for ram in this page
	ld a,(hl)		; get a byte from ram
	ld b,a			; save it 
	cpl			; flip all bits
	ld (hl),a		; put it back in ram
	cp (hl)			; did it save ok ?
	ld (hl),b		; put original back 
	jr z,ld8a8h		; yep saved, try next page
	
ld8b6h:
	dec h			; Point to last page where save worked
	ld (WOBFL),hl		; Put output buff in last page
	dec h			; move to previous page
	ld (WIBFL),hl		; put input buff in last but one page
	ret
;	ret z			
	
;
; ARH fn 00h Initialize drive
; 
	
DOSARH01:
	xor a			; Set working drive to 0
	ld (WDRV),a		
	jp DOSInit		; Go init
	
;
; ARH fn 05h Scratch directory entry
; Entry :
;	WFILE	= address of filename
;	WDRV	= drive number
;
DOSARH05:
	call sub_d3fah		; does nothing.....
	call DOSARH06		; Scan for the file
	jp c,ld3fdh		; not found : exit
	
	ld a,(ix+000h)		; get first character of filename
	and 080h		; Mask out all but top bit
	scf			; 
	jp nz,ld3fdh		; Already set : exit, file not found
	
	ld (ix+000h),DIRDeleted	; Mark file deleted
	jp DOSARH1F		; Re-save directory

;
; ARH fn 04h Create a directory entry
; Entry :
;	WFILE	= address of filename
;	WDRV	= drive number
; 	WEXEC	= exec address
;	HL	= load address
; 	BC	= size of file
;	D	= track
;	E	= sector
; Exit :
;	Carry set if filename already exists
;

DOSARH04:
	call sub_d3fah		; does nothing.....
	call DOSARH06		; Scan directory
	ccf			
	ret c			; Return immediatly if file found
	
	ld (ix+DIRFlen+1),b	; fill in file length
	ld (ix+DIRFlen),c
	ld (ix+DIRSector),e	; fill in start sector
	ld (ix+DIRTrack),d	; fill in start track
	ld (ix+DIRLoadAddr+1),h	; fill in load address
	ld (ix+DIRLoadAddr),l
	ld hl,(WEXEC)		; get exec address
	ld (ix+DIRExecAddr+1),h	; fill in exec address
	ld (ix+DIRExecAddr),l	
	ld hl,(WFILE)		; Get address of filename
	push ix			; de=ix
	pop de			
	ld bc,DOSMaxFNLen	; Filename length
	ldir			; copy it
	jp DOSARH1F		; Save it to disk
	
;
; ARH fn 0Ah Open directory
;
	
DOSARH0A:
	call sub_d3f7h		; Another dos nothing.....
	ld hl,DOSDirBuffEnd	; End of dir buffer, forces read of next sector
	ld (WDIRP),hl		; Set dir pointer
	ld a,DIRFirstSec	; Set current dir sector = first
	ld (WDIRS),a		
	ld a,(WDRV)		; Set dir drive = current drive
	ld (WDDRV),a		
	ret			

;
; ARH fn 0Bh Open input file.
; Entry :
;	WFILE	= address of filename
;	WDRV	= drive number
; Exit :
;  Carry set on error else file pointer at BOF.
;

DOSARH0B:
	call DOSARH06		; Scan directory for file
	ret c			; Not found : return error.
	
	ld hl,0ffffh		; Initialize input buffer pointer
	ld (WIBP),hl		
	call GetFileDetails	; Get file details from dir entry at ix 
	ld ix,WFPIN		; Point to input file pointer

ld936h:
	ld (ix+FPST),d		; Set start track
	ld (ix+FPSS),e		; Set start sector
	ld (ix+FPLN),c		; Set initial file length
	ld (ix+FPLN+1),b	
	ld (ix+FPDN),a		; Set drive number of file
	ld (ix+FPTK),d		; Set current track of file
	ld (ix+FPSE),e		; Set current sector of file
	or a			; Set flags
	ret			 

; 
; ARH fun 0Ch Open output file, file must already exist.
; Entry :
;	WFILE	= address of filename
;	WDRV	= drive number
; Exit :
; 	Carry set on error else file opened and pointers setup.
;

DOSARH0C:	
	call DOSARH06		; Scan directory for file
	ret c			; Not found : return error.

	ld a,(ix+000h)		; Get first char of filename and check b7 not set
	and 080h		; Reached end of dir / deleted ?
	scf			
	ret nz			; Yep : error return
	
	call GetFileDetails	; Get file details in regs
	ld ix,WFPOT		; Get output file pointer buffer in ix 
	rst 10h			; RCAL
	defb	(ld936h-$)-1	; offset, set filepointer 
	
	dec bc			; File len -1  
	call FindSectorN	; Find last sector in file
	
	ld (ix+FPTK),d		; Set filepointer to EOF sector
	ld (ix+FPSE),e		
	ld hl,(WOBFL)		; Get pointer to output buffer
	ld bc,00001h		; 
	ld a,(ix+FPDN)		; Get drive number
	call DOSARH14		; Read sector into buffer
	
	ld bc,DOSSecSize	; Scan the whole sector
	ld a,DOSEoFMarker	; for the EOF marker
	cpir			; scan for it
	dec hl			; compensate for over loop
	jr z,ld988h		; found it skip
	
	ld hl,(WOBFL)		; Get file buffer pointer
	inc h			; Move to last byte of buffer
	dec hl			
	ld (hl),DOSEoFMarker	; Insert EOF marker  
ld988h:
	ld (WOBP),hl		; Set Output pointer 
	ld hl,(WFILE)		; Get address of opened filename
	ld de,WONME		; Put it in output filename
	ld bc,DOSMaxFNLen	
	ldir		
	or a			; Set flags & return
	ret			
	
;
; ARH fn 0Dh Prepare new output file
; Entry :
;	WFILE	= address of filename
;	WDRV	= drive number
; Exit :
	
DOSARH0D:
	call sub_d3fah		; does nothing.....
	call DOSARH06		; Scan directory for file
	ccf			
	jp c,ld3fdh		; File found exit : error

	call sub_dd31h		;d9a2	cd 31 dd 	. 1 . 
	ld bc,(WSIZE)		; Get max no of sectors to allocate 
ld9a9h:
	push bc			;d9a9	c5 	. 
	call sub_dccch		;d9aa	cd cc dc 	. . . 
	pop bc			;d9ad	c1 	. 
	jr nc,ld9b8h		;d9ae	30 08 	0 . 
	
	call DecBCToZero	;d9b0	cd 2d de 	. - . 
	jr nz,ld9a9h		;d9b3	20 f4 	  . 
	jp ErrorNoSpace		; Error : exit
	
ld9b8h:
	ld hl,00000h		; Set exec address to 0
	ld (WEXEC),hl		
	call DOSARH04		; Create dir entry
	ld hl,(WOBFL)		; Get pointer to output buffer
	rst 10h			; RCAL 
	defb (ld988h-$)-1
	
	call DOSARH06		; Scan directory
	call GetFileDetails	; Get file details
	ld ix,WFPOT
	jp ld936h		; set them and return to caller

;
; ARH fn 015h, Read directory
; Exit :
;	ix = pointer to next dir entry
;	carry flag set at end of dir (and (ix)=0ffh).
;	
DOSARH15:
	ld hl,(WDIRP)		; Get directory pointer
	ld de,DIREntryLen	; Skip to next entry
	add hl,de			
	ld (WDIRP),hl		; Update dir entry pointer
	ld a,h			; 
	cp 00fh			; Past end of dir sector ?
	jr c,lda01h		; Nope skip 
	
	ld a,(WDIRS)		; Get next directory sector no
	cp DOSSecPerTrk+1	; Past end of dir ?
	jp z,ErrorDirEnd		; Yep : 
	
	ld e,a			; Get sector number for read
	inc a			; Update next dir sector
	ld (WDIRS),a		
	ld a,(WDDRV)		; Get drive
	ld hl,DOSDirBuff	; Dir buffer
	push hl			; Move to ix 
	pop ix		
	ld bc,00001h		; Read 1 sector
	ld d,b			; Track 0
	call DOSARH14		; Read the sector
	jr lda05h		
	
lda01h:
	ld ix,(WDIRP)		; Get dir pointer 
lda05h:
	ld (WDIRP),ix		; Update dir pointer
	ld a,(ix+000h)		; 
	cp DIRDeleted		; Deleted entry ?
	jr z,DOSARH15		; Yep : keep going
	ccf			; Set flags & return
	ret			
;
; ARH fn 022h, Read sequential
; Entry :
;	HL = address of buffer
;	BC = byte count
; Exit :
; 	Carry set on EOF
;	
DOSARH16:
	ld a,(WFPIN)		; Get start track ?
	cp DOSInvalid		; Invalid ?
	jp z,ErrorNotOpen	; Yep exit
	
	push hl			;da1a	e5 	. 
	push bc			;da1b	c5 	. 
lda1ch:
	ld (hl),000h		;da1c	36 00 	6 . 
	inc hl			;da1e	23 	# 
	call DecBCToZero		;da1f	cd 2d de 	. - . 
	jr nz,lda1ch		;da22	20 f8 	  . 

	pop bc			;da24	c1 	. 
lda25h:
	rst 10h			; RCAL 
	inc (hl)			;da26	34 	4 
	ld hl,(WIBP)		;da27	2a 38 0d 	* 8 . 
	ld a,(hl)			;da2a	7e 	~ 
	cp 00dh		;da2b	fe 0d 	. . 
	jr nz,lda35h		;da2d	20 06 	  . 

	inc hl			;da2f	23 	# 
	ld (WIBP),hl		;da30	22 38 0d 	" 8 . 
	jr lda25h		;da33	18 f0 	. . 

lda35h:
	cp 0ffh		;da35	fe ff 	. . 
	jr nz,lda3ch		;da37	20 03 	  . 

	pop hl			;da39	e1 	. 
	scf			;da3a	37 	7 
	ret			;da3b	c9 	. 
lda3ch:
	rst 10h			; RCAL 
	defb 	(lda5bh-$)-1
	
	ld de,(WIBP)		;da3e	ed 5b 38 0d 	. [ 8 . 
	ld a,(de)			;da42	1a 	. 
	pop hl			;da43	e1 	. 
	ld (hl),a			;da44	77 	w 
	inc hl			;da45	23 	# 
	push hl			;da46	e5 	. 
	ld hl,(WIBP)		;da47	2a 38 0d 	* 8 . 
	inc hl			;da4a	23 	# 
	ld (WIBP),hl		;da4b	22 38 0d 	" 8 . 
	cp 00dh		;da4e	fe 0d 	. . 
	jr z,lda58h		;da50	28 06 	( . 
	
	call DecBCToZero	;da52	cd 2d de 	. - . 
	jr nz,lda3ch		;da55	20 e5 	  . 

	dec a			;da57	3d 	= 
lda58h:
	pop hl			;da58	e1 	. 
	or a			;da59	b7 	. 
	ret			;da5a	c9 	. 
	
lda5bh:
	ld hl,(WIBFL)		;da5b	2a 4a 0d 	* J . 
	ld de,000ffh		;da5e	11 ff 00 	. . . 
	add hl,de			;da61	19 	. 
	ld de,(WIBP)		;da62	ed 5b 38 0d 	. [ 8 . 
	or a			;da66	b7 	. 
	sbc hl,de		;da67	ed 52 	. R 
	ret nc			;da69	d0 	. 
	push ix		;da6a	dd e5 	. . 
	push bc			;da6c	c5 	. 
	
	ld ix,WFPIN		; Point at input filepointer
	ld a,(ix+FPDN)		; Get drive number
	ld bc,00001h		; 1 byte ?
	ld d,(ix+FPTK)		; Get track
	ld e,(ix+FPSE)		; Get sector
	ld hl,(WIBFL)		; Get input buffer location
	ld (WIBP),hl		; And set input buffer to it
	
	rst 10h			; RCAL 
	defb 	(DOSARH14-$)-1

	ld hl,WFPIN		; point to input file pointer
	call ldddfh
	pop bc
	pop ix
	ret			

;
; ARH fn 017h, Read random
; Entry : 
;	BC = Sector offset
; 	HL = Buffer address
; Exit :
; 	Sets filepointer then calls ARH fn 016h, read sequential.
;

DOSARH17:	
	rst 10h			; RCAL 
	defb (lda97h-$)-1	; set filepointer
	
	ld bc,DOSSecSize	; Read one sector
	jp DOSARH16		; Go read sequential
	
lda97h:	
	ld ix,WFPIN		; Get input file pointer
	call sub_de08h		; Seek to sector
	ld de,-1		; buffer pointer -1
	ld (WIBP),de	
	ret
	
;
; ARH fn 018h, Read whole file.
; Entry :
;	WFILE = pointer to filename
;	WDRV  = drive number
; Exit :
; 	File loaded at address specified in dir, carry on error.
;
	
DOSARH18:
	call DOSARH06		; Scan directory
	ret c			; Error : exit
 
	call GetFileDetails	; get details of file from dir entry
				; bc=len, d=trk, e=sec, a=drv, hl=buff

;
; ARH fn 014h, Direct read
; Entry :
; 	HL = buffer
;	BC = Sector count
; 	DE = start trk, sector
;	A  = drive
;
DOSARH14:
	push bc			; Save details
	push de			
	push hl			
	ld (DWDRV),a		; set work drive		
ldab3h:
	push bc			;
	push de			;
	ld a,(DWDRV)		; Get drive
	call DOSRSCT		; Read a sector
	jp nz,CallUSDIO		; Error : 
	
	pop de			; restore track & sector
	ld bc,00001h		; pretend file is 1 sec long
	call FindSectorN	; doing this updates track & sec if needed
	
	pop bc			; Restore length
	call DecBCToZero	; Check for end of file
	jr nz,ldab3h		; Do next
	
	pop hl			; Restore & return
	pop de			
	pop bc			
	or a			
	ret			

;
; ARH fn 019h, Read directory header
; Entry :
;	WDRV = drive to process
; Exit :
;	ix = ptr to dir header
;	carry on error
	
DOSARH19:
	ld a,DIRHeadFlag	; Flag reading header
	ld (WDIRS),a		
	rst 10h			; RCAL
	defb (ldad9h-$)-1
	
	jr DOSARH14		; Read the sector
	
ldad9h:
	ld bc,00001h		; Sector count, 1 sector
	ld de,00002h		; Track 0, sector 2
	ld hl,DOSDirBuff	; Buffer
	push hl			; Point ix at it
	pop ix		
	ld a,(WDRV)		; Get drive id 
	ret			
	
;
; ARH fn 023h, Write directory header
; Entry :
;	WDRV = drive to process
; Exit :
	
DOSARH23:
	ld a,(WDIRS)		; Get dir pointer
	cp DIRHeadFlag		; Is header in buffer ?
	jp nz,ErrorDirEnd	; Nope : Error
	
	rst 10h			; RCAL
	defb (ldad9h-$)-1
	
	jp DOSARH1E		; Write sector to disk
	
;
; ARH fn 020h, Save sequential
; Entry : 
; 	HL = buffer address
;	BC = length
; Exit :
; 
	
DOSARH20:
	rst 10h			; RCAL 
	defb 	(ldafeh-$)-1
	
	ld bc,00001h		
	ld hl,0dea3h
ldafeh:	
	ld a,(WFPOT)		; Get start sector
	cp DOSInvalid		; File closed ?
	jp z,ErrorNotOpen	; nope : return error
	
	ld a,(WFPOTDN)		; Get drive number
	ld (WDRV),a		; Set current drive

sub_db0ch:
	push hl			; Save buffer ptr
ldb0dh:
	ld hl,(WOBFL)		; Point to base of output buffer
	ld de,DOSSecSize	; Get sector size
	add hl,de		; add to output pointer
	ex de,hl		; swap
	ld hl,(WOBP)		; Get output buffer pointer 
	or a			; Set flags
	sbc hl,de		; Beyond end of buffer ?
	jr c,ldb24h		; no : skip on
	
	push bc			; Save length
	rst 10h			; RCAL 
	defb 	(sub_db38h-$)-1	
	
	call c,sub_db74h
	pop bc			; restore length
ldb24h:
	pop hl			; restore buffer ptr 
	ld a,(hl)		; get a byte from buffer
	inc hl			; increment pointer
	push hl			; save buffer pointer
	ld hl,(WOBP)		; get output buffer pointer
	ld (hl),a		; put byte in buffer
	inc hl			; increment pointer
	ld (WOBP),hl		; save pointer
	call DecBCToZero	; decrement byte count
	jr nz,ldb0dh		; more bytes : loop again
	
	pop hl			; restore buffer pointer
	or a			; set flags
	ret			

sub_db38h:
	ld hl,(WOBFL)		; Get base of output buffer
	push hl			; Save it
	ld de,(WOBP)		; Get Output buffer ptr
	or a			; Set flags
	sbc hl,de		; 
	pop hl			; Restore output buffer base
	ret z			; 
	
	ld (WOBP),hl		;db45	22 41 0d 	" A . 
	ld bc,00001h		;db48	01 01 00 	. . . 
	
	ld ix,WFPOT		; Point ix at output filepointer
	ld a,(ix+FPDN)		; Get drive number
	ld d,(ix+FPTK)		; Get track
	ld e,(ix+FPSE)		; Get sector
	call DOSARH1E		; Write the sector to disk
	call FindSectorN	; 
	
	ld (ix+FPTK),d		; Update track
	ld (ix+FPSE),e		; Update sector
	push ix			; Save filepointer pointer
	call sub_dcb3h		;
	pop ix			; Restore filepointer pointer
	ld bc,(WFPOTLN)		; Get length of file
	or a			; 
	sbc hl,bc		;
	ccf			;
	ret			
	
sub_db74h:
	inc bc			;db74	03 	. 
	ld (WFPOTLN),bc		;db75	ed 43 45 0d 	. C E . 
	push bc			;db79	c5 	. 
	call sub_dd31h		;db7a	cd 31 dd 	. 1 . 
	
	ld ix,WFPOT		;db7d	dd 21 43 0d 	. ! C . 
	ld bc,00001h		;db81	01 01 00 	. . . 
	ld d,(ix+005h)		;db84	dd 56 05 	. V . 
	ld e,(ix+006h)		;db87	dd 5e 06 	. ^ . 
	call sub_dd4eh		;db8a	cd 4e dd 	. N . 
	
	ld hl,WONME		;db8d	21 30 0d 	! 0 . 
	ld (WFILE),hl		;db90	22 21 0d 	" ! . 
	call sub_d3fah		; does nothing.....
	call DOSARH06		;db96	cd b1 dd 	. . . 
	jp c,ErrorNotOpen		;db99	da 8c de 	. . . 
	
	pop bc			;db9c	c1 	. 
	ld (ix+00eh),c		;db9d	dd 71 0e 	. q . 
	ld (ix+00fh),b		;dba0	dd 70 0f 	. p . 
	jr DOSARH1F		;dba3	18 43 	. C 

DOSARH21:
	rst 10h			; RCAL 
	defb 	(sub_dbb4h-$)-1
	ld bc,000ffh		;dba7	01 ff 00 	. . . 
	call DOSARH20		;dbaa	cd f6 da 	. . . 
sub_dbadh:
	call sub_db38h		;dbad	cd 38 db 	. 8 . 
	call c,sub_db74h		;dbb0	dc 74 db 	. t . 
	ret			;dbb3	c9 	. 

sub_dbb4h:
	ld ix,WFPOT		;dbb4	dd 21 43 0d 	. ! C . 
	call sub_de08h		;dbb8	cd 08 de 	. . . 
	ld de,(WOBFL)		;dbbb	ed 5b 4c 0d 	. [ L . 
	ld (WOBP),de		;dbbf	ed 53 41 0d 	. S A . 
	ret			;dbc3	c9 	. 


;
; ARH fn 01Eh, Direct save
; Entry :
; 	HL = buffer
;	BC = Sector count
; 	DE = start trk, sector
;	A  = drive
;

DOSARH1E:
	push bc			; Save regs
	push de			
	push hl			
	ld (DWDRV),a		; Set working drive
ldbcah:
	push bc			; Save again
	push de			
	ld a,(DWDRV)		; get working drive
	call DOSWSCT		; Write the sector
	or a			; 
	jp nz,CallUSDIO		; Error, try user routine
	
	pop de			; restore track & sector
	ld bc,00001h		; pretend file is 1 sec long
	call FindSectorN	; doing this updates track & sec if needed
	
	pop bc			; Restore length
	call DecBCToZero	; Check for end of file
	jr nz,ldbcah		; Don next if not
	
	pop hl			; Restore set flags and return
	pop de			
	pop bc			
	or a			
	ret			

;
; ARH fn 01fh, Save directory.
; 

DOSARH1F:
	push bc			; Save regs
	push de			
	push hl			

	ld a,(WDIRS)		; Get next directory sector no
	ld e,a			; Get sector number for write
	dec e			; update current sector

	ld a,(WDDRV)		; Get drive
	ld bc,00001h		; Write 1 sector
	ld d,b			; Track 0
	ld hl,DOSDirBuff	; Dir buffer
	
	call DOSARH1E		; Write the sec
	pop hl			; Restore regs
	pop de			
	pop bc			
	jp ld3fdh		; do nothing sub, just returns
	
;
; ARH fn 022h, Save whole file.
; Entry :
;	WFILE = pointer to filename.
;	WDRV  = drive no
;	HL    = beginning of buffer to write
;	DE    = end byte of the buffer +1
;	BC    = exec address
;
	
DOSARH22:
	ld (WEXEC),bc		; Set exec address
	push de			; Save start and end addresses
	push hl			
	call DOSARH06		; Scan directory
	jr c,ldc2fh		; Not found : skip
	
	rst 28h			; Display string
	defb	'Delete?',000h
	
	rst 18h			; SCAL
	defb	BLINK		; Wait for key with cursor
	
	cp 'Y'			; Did user say yes ?
	jr z,ldc22h		; Yep : skip on
	
	rst 18h			; SCAL
	defb	CRLF		; do eol
	jp DoDOSWarm		; exit : warmstart dos

ldc22h:
	rst 30h			; display the Y
	rst 18h			; SCAL
	defb	CRLF		; do eol
	
	call DOSARH05		; Delete the file
	jr nc,ldc2fh		; ok : skip
  
	ld a,DOSErr05		; Error code
	jp ErrorCodeExit	; display code and exit

ldc2fh:
	pop hl			; Restore start and end addresses 
	pop de			
	push hl			; Save start addr
	ex de,hl		; swap start and end
	scf			; set carry
	sbc hl,de		; Work out length in bytes
	
	ld bc,00000h		; Zero sector count
	ld de,DOSSecSize	; Sector len
ldc3ch:
	sbc hl,de		; Decrement byte count
	inc bc			; Increment sector count
	jr nc,ldc3ch		; Keep going until all bytes accounted for
	call sub_d3fah		; does nothing.....
	
	push bc			; Save sector count
	call sub_dcc7h		;dc45	cd c7 dc 	. . . 
	pop bc			; Restore sector count
	jp c,ErrorNoSpace	; Error : exit
	
	pop hl			
	push bc			
	push de			 
	push hl			
	call DOSARH04		; Create dir entry
	
	pop hl			
	pop de			
	pop bc			
	ld a,(WDRV)		; Get drive
	jp DOSARH1E		; Directory save
	
;
; ARH fn 028h, Close input file
;	
DOSARH28:
	ld a,DOSInvalid		; Invalid flag
	ld (WFPIN),a		; Set input filepointer invalid
ldc61h:
	ret	

;
; ARH fn 029h, Close output file.
;
DOSARH29:
	ld hl,WFPOT		; get pointer to file pointer
	ld a,(hl)		; get start track
	cp DOSInvalid		; Invalid
	ret z			; Yep : return
	
	ld hl,ldea4h		; EOF marker in DOS ROM
	ld bc,00001h		; Copy 1 byte
	call sub_db0ch		; Put byte in output buffer
	call sub_db38h		;
	
	ld ix,WFPOT		; get pointer to file pointer
	rst 10h			; RCAL 
	defb 	(sub_dcb3h-$)-1
	
	push hl			; 
	ld hl,WONME		; Get pointer to current filename
	ld (WFILE),hl		; Set it
	ld a,(WFPOTDN)		; Get file drive number
	ld (WDRV),a		; make it current drive 
	call sub_d3fah		; does nothing.....
	
	call DOSARH06		; Scan directory
	jp c,ErrorNotOpen	; error : file not open
	
	pop bc			; Restore length
	ld (ix+DIRFlen),c	; Set file length
	ld (ix+DIRFlen+1),b	
	ld hl,WFPOT		; get pointer to output filepointer
	ld (hl),DOSInvalid	; Invalidate file
	jp DOSARH1F		; Save directory


ldca0h:	
	ld b,(ix+FPST)		; Get start track
	ld hl,00000h		; Sero sector count
	ld de,DOSSecPerTrk	; Sectors / track 
ldca9h:
	add hl,de		; Add a tracks worth of sectors to count
	djnz ldca9h		; Keep going whilst more tracks
	
	ld c,(ix+FPSS)		; Get start sector
	ld b,000h		; Convert to 16 bit 
	add hl,bc		; Add to total
	ret			 

sub_dcb3h:
	rst 10h			; RCAL 
	defb	(ldca0h-$)-1	; Get sector count of file in HL
	
	push hl			; save sector count
	inc ix			; ix=ix+6
	inc ix			
	inc ix			
	inc ix			
	inc ix			
	rst 10h			; RCAL 
	defb	(ldca0h-$)-1	; Get sector count of file in HL
	
	pop de			; Get sector count of first file
	or a			; Set flags
	sbc hl,de		; Find the difference
	ret			

sub_dcc7h:
	push bc			; Save BC
	call sub_dd31h		;dcc8	cd 31 dd 	. 1 . 
	pop bc			;dccb	c1 	. 

sub_dccch:
	ld hl,00d50h		;dccc	21 50 0d 	! P . 
	ld a,080h		;dccf	3e 80 	> . 
	push hl			;dcd1	e5 	. 
	push af			;dcd2	f5 	. 
ldcd3h:
	ld de,00000h		;dcd3	11 00 00 	. . . 
ldcd6h:
	push af			;dcd6	f5 	. 
	and (hl)			;dcd7	a6 	. 
	jr z,ldcfbh		;dcd8	28 21 	( ! 
	pop af			;dcda	f1 	. 
	rst 10h			; RCAL 
	defb (ldce3h-$)-1
	
	pop de			;dcdd	d1 	. 
	pop de			;dcde	d1 	. 
	push hl			;dcdf	e5 	. 
	push af			;dce0	f5 	. 
	jr ldcd3h		; loop again
ldce3h:	
	srl a			;dce3	cb 3f 	. ? 
	ret nc			;dce5	d0 	. 
	inc hl			;dce6	23 	# 
	push hl			;dce7	e5 	. 
	push de			;dce8	d5 	. 
	ld de,DWDRV		;dce9	11 e9 0d 	. . . 
	or a			;dcec	b7 	. 
	sbc hl,de		;dced	ed 52 	. R 
	pop de			;dcef	d1 	. 
	pop hl			;dcf0	e1 	. 
	jr nc,ldcf6h		;dcf1	30 03 	0 . 

	ld a,080h		;dcf3	3e 80 	> . 
	ret			;dcf5	c9 	. 

ldcf6h:
	pop hl			;dcf6	e1 	. 
	pop de			;dcf7	d1 	. 
	pop af			;dcf8	f1 	. 
	scf			;dcf9	37 	7 
	ret			;dcfa	c9 	. 

ldcfbh:
	pop af			;dcfb	f1 	. 
	inc de			;dcfc	13 	. 
	push hl			;dcfd	e5 	. 
	ld h,b			;dcfe	60 	` 
	ld l,c			;dcff	69 	i 
	or a			;dd00	b7 	. 
	sbc hl,de		;dd01	ed 52 	. R 
	pop hl			;dd03	e1 	. 
	jr nz,ldd2dh		;dd04	20 27 	  ' 
	
	pop af			;dd06	f1 	. 
	pop hl			;dd07	e1 	. 
	ld de,00d50h		;dd08	11 50 0d 	. P . 
	or a			;dd0b	b7 	. 
	sbc hl,de		;dd0c	ed 52 	. R 
	ld b,008h		;dd0e	06 08 	. . 
	ld de,00000h		;dd10	11 00 00 	. . . 
	ex de,hl			;dd13	eb 	. 
ldd14h:
	add hl,de			;dd14	19 	. 
	djnz ldd14h		;dd15	10 fd 	. . 
	
	dec hl			;dd17	2b 	+ 
ldd18h:
	inc hl			;dd18	23 	# 
	sla a		;dd19	cb 27 	. ' 
	jr nc,ldd18h		;dd1b	30 fb 	0 . 
	ld bc,00010h		;dd1d	01 10 00 	. . . 
	ld d,000h		;dd20	16 00 	. . 
ldd22h:
	inc d			;dd22	14 	. 
	or a			;dd23	b7 	. 
	sbc hl,bc		;dd24	ed 42 	. B 
	jr nc,ldd22h		;dd26	30 fa 	0 . 
	
	add hl,bc			;dd28	09 	. 
	ld e,l			;dd29	5d 	] 
	inc e			;dd2a	1c 	. 
	or a			;dd2b	b7 	. 
	ret			;dd2c	c9 	. 

ldd2dh:
	rst 10h			; RCAL 
	defb (ldce3h-$)-1
	jr ldcd6h		;dd2f	18 a5 	. . 


sub_dd31h:
	ld hl,00d50h		; Zero buffer at 0d50h
	ld (hl),000h		
	ld de,00d51h		;
	ld bc,00098h		; 098h bytes
	ldir		

	ld (hl),0ffh		; ff terminator
	call DOSARH0A		; Open directory 
ldd43h:
	call DOSARH15		; Read directory
	ret c			; error : return 

	call sub_de46h		; Get file len (BC), trk & sec (DE)
	rst 10h			; RCAL
	defb 	(sub_dd4eh-$)-1
	jr ldd43h		; Do next

sub_dd4eh:
	push bc			; Save file len
	push de			; Save trk &  sec
	ld b,d			; get track count into b
	push de			; Save trk & sec
	ld hl,-DOSSecPerTrk 	; -010 sectors 
	ld de,DOSSecPerTrk	; add sectors 
ldd58h:
	add hl,de		; Add sectors / track to running sector count
	djnz ldd58h		; loop while more tracks
	
	pop de			; Restore trk & sec
	ld b,000h		; Get start sector in bc
	ld c,e
	add hl,bc		; Add it to total
	dec hl			; make zero based
	ld c,008h		; BC=00008h (B is still 0 from above!)
	ld d,b			; DE=0
	ld e,b			
	or a			;dd65	b7 	. 
ldd66h:
	sbc hl,bc		;dd66	ed 42 	. B 
	inc de			;dd68	13 	. 
	jr nc,ldd66h		;dd69	30 fb 	0 .
 
	add hl,bc			;dd6b	09 	. 
	ld b,l			;dd6c	45 	E 
	dec de			;dd6d	1b 	. 
	ld hl,00d50h		;dd6e	21 50 0d 	! P . 
	add hl,de			;dd71	19 	. 
	xor a			;dd72	af 	. 
	inc b			;dd73	04 	. 
	scf			;dd74	37 	7 
ldd75h:
	rra			;dd75	1f 	. 
	djnz ldd75h		;dd76	10 fd 	. . 
	
	push af			;dd78	f5 	. 
	and (hl)			;dd79	a6 	. 
	jp nz,ErrorNoSpace		;dd7a	c2 90 de 	. . . 
	
	pop af			;dd7d	f1 	. 
	or (hl)			;dd7e	b6 	. 
	ld (hl),a			;dd7f	77 	w 
	pop de			;dd80	d1 	. 
	ld bc,00001h		;dd81	01 01 00 	. . . 
	rst 10h			; RCAL 
	defb	(FindSectorN-$)-1
	
	pop bc			;dd86	c1 	. 
	call DecBCToZero		;dd87	cd 2d de 	. - . 
	jr nz,sub_dd4eh		;dd8a	20 c2 	  . 
	ret	

DOSARH03:
	call DOSARH18		;dd8d	cd a6 da 	. . . 
	ret c			;dd90	d8 	. 
	ld hl,00c0bh		;dd91	21 0b 0c 	! . . 
	ld a,001h		;dd94	3e 01 	> . 
	ld b,015h		;dd96	06 15 	. . 
ldd98h:
	ld (hl),a			;dd98	77 	w 
	xor a			;dd99	af 	. 
	inc hl			;dd9a	23 	# 
	djnz ldd98h		;dd9b	10 fb 	. . 
	ld sp,01000h		;dd9d	31 00 10 	1 . . 
	ld hl,(WEXEC)		;dda0	2a 23 0d 	* # . 
	ld (00c0ch),hl		;dda3	22 0c 0c 	" . . 
	ld a,0ffh		;dda6	3e ff 	> . 
	ld (00c26h),a		;dda8	32 26 0c 	2 & . 
	ld a,045h		;ddab	3e 45 	> E 
	ld (00c2bh),a		;ddad	32 2b 0c 	2 + . 
	jp (hl)			;ddb0	e9 	. 
	
;
; ARH fn 006h, Scan directory
; Entry :
;	WFILE = filname to scan for
;	WDRV  = drive to scan on
; Exit :
;	Carry set on error (file not found or disk error).
;	IX    = pointer to the directory entry of the file.
; 
	
DOSARH06:
	exx			; Use alternate register set
	call DOSARH0A		; Open directory
lddb5h:
	call DOSARH15		; Read next directory entry
	jr c,lddceh		; Error : exit
	
	ld hl,(WFILE)		; Get address of name to find
	ld de,(WDIRP)		; Get address of found directory entry
	ld b,DOSMaxFNLen	; characters to scan
lddc3h:
	ld a,(de)		; load a character from the dir entry
	sub (hl)		; work out the difference
	and 07fh		; force it 7 bit
	jr nz,lddb5h		; Not the same, abort and try next dir entry
	
	inc de			; Move to next char to check
	inc hl			
	djnz lddc3h		; If more chars remaining check the next
	or a			; Found, set flags and return

lddceh:
	exx			; Restore and return
	ret			

; Calculates track and sector of sector offset in file.
; Entry
; 	BC = Sector offset to find
; 	D  = start track
; 	E  = start sector
; Exit
;	BC = 0
;	D  = track of last sector
; 	E  = last sector
FindSectorN:
	ld a,b			; Test file len == 0
	or c			
	ret z			; yep return
	
	dec bc			; Decrement sector count 
	inc e			; increment current sector no
	ld a,e			; Past end of track ?
	cp DOSSecPerTrk+1	 
	jr nz,FindSectorN	; Nope : loop again
	
	ld e,001h		; Reset back to first sector
	inc d			; Increment track
	jr FindSectorN		; Loop again
	
ldddfh:
	push hl			;dddf	e5 	. 
	push hl			;dde0	e5 	. 
	pop ix		;dde1	dd e1 	. . 
	push ix		;dde3	dd e5 	. . 
	call sub_dcb3h		;dde5	cd b3 dc 	. . . 
	pop ix		;dde8	dd e1 	. . 
	ld c,(ix+002h)		;ddea	dd 4e 02 	. N . 
	ld b,(ix+003h)		;dded	dd 46 03 	. F . 
	or a			;ddf0	b7 	. 
	sbc hl,bc		;ddf1	ed 42 	. B 
	jp nc,ErrorNoSpace		;ddf3	d2 90 de 	. . . 
	pop hl			;ddf6	e1 	. 
	ld de,00006h		;ddf7	11 06 00 	. . . 
	add hl,de			;ddfa	19 	. 
	ld a,(hl)			;ddfb	7e 	~ 
	inc a			;ddfc	3c 	< 
	cp 011h		;ddfd	fe 11 	. . 
	jr nz,lde06h		;ddff	20 05 	  . 
	dec hl			;de01	2b 	+ 
	inc (hl)			;de02	34 	4 
	inc hl			;de03	23 	# 
	ld a,001h		;de04	3e 01 	> . 
lde06h:
	ld (hl),a			;de06	77 	w 
	ret			;de07	c9 	. 

;
; Entry:
; 	BC = Sector offset
;	IX = filepointer
;
sub_de08h:
	ld a,(ix+FPST)		; get start track
	cp DOSInvalid		; is it invalid ?		
	jp z,ErrorNotOpen	; yep : return error
	
	push hl			; Save HL
	ld d,a			; start track into d
	ld e,(ix+FPSS)		; get start sector
	ld l,(ix+FPLN)		; get length of file in HL
	ld h,(ix+FPLN+1)	
	or a			; 
	sbc hl,bc		; Subtract requested sector from total sectors -ve if error
	pop hl			; Restore HL
	ld a,DOSErrPastEOF	; Error code to return if invalid
	jp c,ErrorCodeExit	; Error : past end of file
	
	rst 10h			; RCAL 
	defb	(FindSectorN-$)-1
	
	ld (ix+FPTK),d		; Set track
	ld (ix+FPSE),e		; Set sector
	ret			

;
; returns with z flag set if BC is zero or 1 
; IF BC<>0 THEN BC=BC-1
; Corrupts a
;
DecBCToZero:
	ld a,b			; BC=0 ?
	or c			
	ret z			; Yep return
	
	dec bc			; Decrement BC
	ld a,b			; flag zero
	or c			
	ret			

; get file details from dir header
GetFileDetails:
	ld h,(ix+DIRExecAddr+1)	; Fill in exec address
	ld l,(ix+DIRExecAddr)	
	ld (WEXEC),hl		
	ld a,(WDRV)		; get current drive
	ld h,(ix+DIRLoadAddr+1)	; get load address in HL
	ld l,(ix+DIRLoadAddr)		

sub_de46h:
	ld b,(ix+DIRFlen+1)	; Get file len in BC
	ld c,(ix+DIRFlen)		
	ld e,(ix+DIRSector)	; Get sector in E
	ld d,(ix+DIRTrack)	; Get track in D
	ret			

CallUSDIO:
	pop de			;
	pop bc			;
	push af			; 
	ld a,(DWDRV)		; Get drive
	ld c,a			;
	pop af			;
	ld sp,SYSSTACK		; Set NAS-SYS stack
	jp USDIO		; User disk IO routine
	
DisplayErrExit:
	push af			; Save error code
	rst 28h			; Display string
	defb	00dh,'Err.',000h
	pop af			; Restore error code 
	
	rst 18h			; SCAL
	defb	B2HEX		; Display error code in A as hex
	
	rst 28h			; Display string 
	defb	' @ ',000h
lde71h:
	push bc			;
	rst 18h			; SCAL
	defb	TBCD3		; output HL as hex
	
	pop bc			; 
	ld a,d			; Get track into A
	rst 18h			; SCAL
	defb	B2HEX		; Display track in A as hex
	
	ld a,'/'		; Char to display
	rst 30h			; Display character in A
 
	push af			; Save seperator
	ld a,e			; Get sector into a
	rst 18h			; SCAL
	defb	B2HEX		; Display sector in A as hex
	pop af			; Restore seperator
	
	rst 30h			; Display character in A

	ld a,c			; Get drive no in a
	rst 18h			; SCAL
	defb	B2HEX		; Display drive no in A as hex
	
	rst 18h			; SCAL 	
	defb	CRLF		; display EOL
	jr InvalidExit		; Make output FP sector invalid
	
ErrorDirEnd:
	ld a,DOSErrDirEnd	; End of directory
	jr ErrorCodeExit	

ErrorNotOpen:
	ld a,DOSErrNotOpen	; File not open
	jr ErrorCodeExit	
	
ErrorNoSpace:
	ld a,DOSErrNoSpace	; No space to extend / save file

ErrorCodeExit:
	call ld3fdh		; do nothing sub, just returns 
	rst 18h			; SCAL 	
	defb	B2HEX		; Output A as hex
	
	rst 18h			; SCAL 	
	defb 	SPACE		; output a space
	
	rst 18h			; SCAL 	
	defb	ERRM		; Output 'Error'

InvalidExit:
	ld hl,WFPOT		; Get output file pointer
	ld (hl),0ffh		; Make start track invalid
	rst 18h			; SCAL
	defb	MRET		; return to NAS-SYS
	
	nop			;dea2	00 	. 
	dec c			;dea3	0d 	. 
ldea4h:
	defb	DOSInvalid	

;
; Basic routine handler
;
DoDOSBRH:
	ld (DSAVESP),sp		; Save stack pointer 
	call 0e98bh		; Call basic, get USR arg
	ld a,d			; Get function no ?
	or a			;  
	jp nz,RetBasicErr	; error : return to basic
	
	ld hl,BRHTable		; Point to the BRH table
	call ScanTable		; scan it for the required function 
	jr c,RetBasicErr	; error : return to basic
	
	ex de,hl		; Get function handler address into HL 
	jp (hl)			; Call the function
	
ldebbh:	
	pop de			; 
	ld (WFILE),de		; fill in filename address 
	call sub_df78h		;dec0	cd 78 df 	. x . 
	
	ld de,(WFILE)		; Retrieve filename address
	push de			; save it
	jr c,RetBasicErr	; error : return to basic 
	
	ld (WFILE),hl		; set filename address
	ld a,c			; 
	cp 008h			; 
	ret z			;
	jr RetBasicErr		;  error : return to basic
	
;
; BRH fn 000h, Init disk.
; Entry :
;	WDRV = disk to init.
;
	
DOSBRH01:
	call DOSARH01		; Initialise the disk
	ld b,a			; return status
	jr RetBasicOK		; return ok		

;
; BRH fn 003h, Chain to new program
; Entry :
;	WDRV = disk that the file is on.
;	filename in string call with :
;	a=usr(3),fn$
;
DOSBRH03:	
	rst 10h			; RCAL 
	defb	(ldebbh-$)-1
	
	ld sp,SYSSTACK		; Setup system stack
	call DOSARH03		; call ARH, to chain the program
	ld sp,(DSAVESP)		; restore basic stack
	jr RetBasicOKb01	; set b=1, return to basic

;
; BRH fn 005h, Scratch (delete) a file from disk.
; Entry :
;	WDRV = disk that the file is on.
;	filename in string call with :
;	a=usr(3),fn$
; Exit :
;	a nonzero on error.
;
	
DOSBRH05:
	rst 10h			; RCAL 
	defb	(ldebbh-$)-1
	
	call	DOSARH05	; Call ARH to delete file
	jr RetBasicCarry	; return 1=carry, 0=no carry

;
; BRH fn 00Bh, Open input file
; Entry :
;	WDRV = disk that the file is on.
;	filename in string call with :
;	a=usr(3),fn$
; Exit :
;	a nonzero on error.
;

DOSBRH0B:
	rst 10h			; RCAL 
	defb	(ldebbh-$)-1
	
	call	DOSARH0B	; Call ARH to open input filer
	jr RetBasicCarry	; return 1=carry, 0=no carry
	
;
; BRH fn 00Bh, Open output file
; Entry :
;	WDRV = disk that the file is on.
;	filename in string call with :
;	a=usr(3),fn$
; Exit :
;	a nonzero on error.
;
DOSBRH0C:
	rst 10h			; RCAL 
	defb	(ldebbh-$)-1
	call DOSARH0C		; Call ARH to open output file
	jr RetBasicCarry	; return 1=carry, 0=no carry

;
; BRH fn 00Bh, Prepare output file
; Entry :
;	WDRV  = disk that the file is on.
;	WSIZE = number of sectors to create in the file
;	filename in string call with :
;	a=usr(3),fn$
; Exit :
;	a nonzero on error.
;

DOSBRH0D:	
	rst 10h			; RCAL 
	defb	(ldebbh-$)-1
	call DOSARH0D		; Call ARH to create file
	jr RetBasicCarry	; return 1=carry, 0=no carry
	
	
DOSBRH16:
	rst 10h			; RCAL 
	defb	(sub_df78h-$)-1
	
	jr c,RetBasicOKb00	; Done ? yep : return to basic
	ld a,c			; 
	or a			;
	jr z,DOSBRH16		; Any more, get them
	
	push bc			; Save regs
	push hl			;
	call DOSARH16		; Call ARH to write the data
	pop hl			; Restore
	pop bc			;
	
	jr c,ldf18h		; 
	jr z,ldf18h		;
	jr DOSBRH16		; Loop again

ldf18h:
	ex af,af'		; use alt registers
ldf19h:
	rst 10h			; RCAL 
	defb	(sub_df78h-$)-1
	
	jr c,ldf25h		;df1b	38 08 	8 . 
	ld b,c			; get count
ldf1eh:
	ld (hl),000h		; zero byte at hl
	inc hl			; increment it
	djnz ldf1eh		; loop if more
	jr ldf19h		; Keep going
	
ldf25h:
	ex af,af'		; Restore regs
	jr RetBasicCarry	; return 1=carry, 0=no carry
	
	
;
; BRH fn 017h, Read random
; Entry :
;	WSECT = Relative sector to read.
;	filename in string call with :
;	a=usr(3),fn$
; Exit :
;	a nonzero on error.
;
	
DOSBRH17:
	ld bc,(WSECT)		; Get rel sector number
	call lda97h		; Set filepointer to requested sector
	jr DOSBRH16		; Go read sequential
	
RetBasicErr:
	ld sp,(DSAVESP)		; Restore basic stack
	jp 0e9a0h		; Return to basic
	
RetBasicCarry:
	jr c,RetBasicOKb01	;df38	38 06 	8 . 
RetBasicOKb00:
	ld b,000h		;df3a	06 00 	. . 
RetBasicOK:
	xor a			;df3c	af 	. 
	jp 0f0f2h		;df3d	c3 f2 f0 	. . . 
RetBasicOKb01:
	ld b,001h		;df40	06 01 	. . 
	jr RetBasicOK		;df42	18 f8 	. . 


;
; BRH fn 028h, Close input file
; Entry :
; Exit :
;

DOSBRH28:
	call DOSARH28		; Call ARH to close the file
	jr RetBasicOKb00	; return to basic
	
;
; BRH fn 020h, Write sequential.
; Entry :
;	a=usr(x),fn$
; Exit :
;	a nonzero on error.
;
	
DOSBRH20:
	rst 10h			; RCAL 
	defb 	(sub_df78h-$)-1
	jr c,ldf54h		; 
	ld a,c			;df4d	79 	y 
	or a			;df4e	b7 	. 
	call nz,ldafeh		;df4f	c4 fe da 	. . . 
	jr DOSBRH20		;df52	18 f5 	. . 
ldf54h:
	call 0daf8h		;df54	cd f8 da 	. . . 
	jr RetBasicOKb00		;df57	18 e1 	. . 
DOSBRH21:
	ld bc,(WSECT)		;df59	ed 4b 25 0d 	. K % . 
	call sub_dbb4h		;df5d	cd b4 db 	. . . 
ldf60h:
	rst 10h			; RCAL 
	ld d,038h		;df61	16 38 	. 8 
	rlca			;df63	07 	. 
	ld a,c			;df64	79 	y 
	or a			;df65	b7 	. 
	call nz,ldafeh		;df66	c4 fe da 	. . . 
	jr ldf60h		;df69	18 f5 	. . 
	call 0daf8h		;df6b	cd f8 da 	. . . 
	call sub_dbadh		;df6e	cd ad db 	. . . 
	jr RetBasicOKb00		;df71	18 c7 	. . 

DOSBRH29:
	call DOSARH29		;df73	cd 62 dc 	. b . 
	jr RetBasicOKb00		;df76	18 c2 	. . 


sub_df78h:
	pop ix			; Restore regs
	pop iy			;
	pop hl			; HL= basic input pointer ?
	ld a,(hl)		; get byte from basic
	cp ','			; Comma ?
	jr nz,ldfd4h		; nope : exit
	
	inc hl			; move to next
	ld b,(hl)		; get next byte
	ld c,000h		; clear c incase single char var name
	inc hl			; move to next
	ld a,(hl)		; get next byte
	cp '$'			; dollar sign ?
	jr z,ldf9dh		; yep skip on, single char name e.g. a$
	ld c,a			; 
ldf8dh:
	inc hl			; move to next
	ld a,(hl)		; get next byte
	cp '$'			; dollar sign ?
	jr z,ldf9dh		; yep skip on, 2 char var name e.g. ab$

	; since only the first two characters of a var name are significant
	; keep looping until we hit an invalid char or a $
	cp '0'			; Zero ?
	jr c,RetBasicErr	; Below, error
	cp '['			; Open square ?
	jr nc,RetBasicErr	; Above, error
	jr ldf8dh		; loop again
	
	; at this point bc will contain the two ascii characters of the
	; var name (or c=0 if only single character name).
ldf9dh:
	set 7,c			; set msb of c
	ld a,' '		; skip any spaces
ldfa1h:
	inc hl			; move to next
	cp (hl)			; is byte at hl space
	jr z,ldfa1h		; yep : skip it
	
	push hl			; Save regs
	push iy			
	push ix			

	ld hl,(PROGND)		; get pointer to end of program
ldfadh:
	ld de,(VAREND)		; get pointer to end of vars
	call CPDEHL		; compare DE & HL
	jp nc,RetBasicErr	; Error : exit (no vars)
	push hl			; 
 
	ld a,(hl)		; Get a byte from var definition 
	cp c			; matches our var ?
	jr nz,ldfcdh		; nope : skip to next
	
	inc hl			; Get next byte of name
	ld a,(hl)		; 
	cp b			; Is it the one we are looking for ?
	jr nz,ldfcdh		; nope : skip to next
	
	inc hl			 
	ld c,(hl)		; Byte count into BC ?
	ld b,000h		 
	inc hl			
	inc hl			
	ld e,(hl)		; Location into DE ?
	inc hl			
ldfc9h:
	ld d,(hl)		;dfc9	56 	V 
	ex de,hl		;dfca	eb 	. 
	pop de			;dfcb	d1 	. 
	ret			;dfcc	c9 	. 
ldfcdh:
	pop hl			;dfcd	e1 	. 
	ld de,BASVarSize	;dfce	11 06 00 	. . . 
	add hl,de			;dfd1	19 	. 
ldfd2h:
	jr ldfadh		;dfd2	18 d9 	. . 
ldfd4h:
	push hl			; Resave regs for basic
	push iy			
	push ix			
	scf			; flag error
	ret			; return
;
; BRH table, same format as ARH table
;
BRHTable:
	defb 001h		
	defw DOSBRH01		
	
	defb 003h		
	defw DOSBRH03		
	
	defb 005h		
	defw DOSBRH05		
	
	defb 00bh		
	defw DOSBRH0B		
	
	defb 00ch		
	defw DOSBRH0C		
	
	defb 00dh		
	defw DOSBRH0D		
	
	defb 016h		
	defw DOSBRH16		
	
	defb 017h		
	defw DOSBRH17		
	
	defb 020h		
	defw DOSBRH20		
	
	defb 021h		
	defw DOSBRH21		
	
	defb 028h		
	defw DOSBRH28		
	
	defb 029h		
	defw DOSBRH29		
	
	defb 000h		; Table terminator
	