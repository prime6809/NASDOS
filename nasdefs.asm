; NAS-SYS 3 routines, rst 18h followed by the appropreate byte
MRET:		equ	05bh		; End program and return to NAS-SYS
SCALJ:		equ	05ch		; Exec another routine who's number is at 0C0Ah
TDEL:		equ	05dh		; time delay (1s at 4MHz), on exit A=0, B=0
FFLP:		equ	05eh		; flip 1 or more bits on port 0 then back again, A=bits
MFLP:		equ	05fh		; flip tape led on or off
ARGS:		equ	060h		; collect args from command line HL=arg1, DE=arg2, BC=arg3 
NASIN:		equ	062h		; scan for input, if so CF=1, and A=char, does not wait
INLIN:		equ	063h		; obtain a line of input, DE=addreess.
NUM:		equ	064h		; Examine a line and convert to HEX
TBCD3:		equ	066h		; Output HL as hex C=C+H+L
TBCD2:		equ	067h		; Output A as hex C=C+A
B2HEX:		equ	068h		; Output A as hex
SPACE:		equ	069h		; Output a space, a set to space.
CRLF:		equ	06ah		; Output CR, LF A=CR
ERRM:		equ	06bh		; Error message output "Error"+CR, A=CR
TX1:		equ	06ch		; Output HL & DE as hex C=C+H+L+D+E
SSOUT:		equ	06dh		; Output string to serial port HL=ptr, B=len
SRLX:		equ	06fh		; Output A to the serial port
RLIN:		equ	079h		; Eamine input line at DE & convert to bin in ARGs
B1HEX:		equ	07ah		; output the LSN of A as hex
BLINK:		equ	07bh		; Get a character in A, while waiting blink cursor.
CPOS:		equ	07ch		; Gets addr of first character in row pointed to by HL
SP2:		equ	07eh		; output 2 spaces
SCALI:		equ	07fh		; Exec the routine who's code is in E

; NAS-SYS 3 0C00h page vars
PORT0:		equ	0c00h		; Copy of port 0 output status
KMAP:		equ	0c01h		; Keyboard map, 9 bytes
ARGC:		equ	0c0ah		; Routine number passed to SCALJ
ARGN:		equ	0c0bh		; Number of values in input line
ARG1:		equ	0c0ch		; Argument #1
ARG2:		equ	0c0eh		; Argument #2
ARG3:		equ	0c10h		; Argument #3
ARG4:		equ	0c12h		; Argument #4
ARG5:		equ	0c14h		; Argument #5
ARG6:		equ	0c16h		; Argument #6
ARG7:		equ	0c18h		; Argument #7
ARG8:		equ	0c1ah		; Argument #8
ARG9:		equ	0c1ch		; Argument #9
ARG10:		equ	0c1eh		; Argument #10
ARG69:		equ	ARG6
ARGLEN:		equ	(ARG10-ARG1)+2	; Length of arguments

NUMN:		equ	0c20h		; Number of arguments examined by routine NUM
NUMV:		equ	0c21h		; Number returned by NUM
BRKADR:		equ	0c23h		; Breakpoint address
BRKVAL:		equ	0c25h		; Saved value from breakpoint address
CONFLG:		equ	0c26h		; Normally 0 but set to -1 for E command
KOPT:		equ	0c27h		; Keyboard option see K command
XOPT:		equ	0c28h		; X option see X command.
CURSOR:		equ	0c29h		; Position of cursor
ARGX:		equ	0c2bh		; last command entered
KCNT:		equ	0c2ch		; Keyboard repeat counter
KLONG:		equ	0c2eh		; Keyboard initial delay before repeat
KSHORT:		equ	0c30h		; Leyboard repeat speed between repeats
KBLINK:		equ	0c32h		; Cursor blink speed.
MONSTK:		equ	0c34h		; Monitor (NAS-SYS) stack area
RBC:		equ	0c61h		; Register BC save
RDE:		equ	0c63h		; Register DE save
RHL:		equ	0c65h		; Register HL save
RAF:		equ	0c67h		; Register AF save
RPC:		equ	0c69h		; Register PC save
RSP:		equ	0c6bh		; Register SP save
KTABL:		equ	0c6dh		; Length of keyboard table
KTAB:		equ	0c6fh		; Address of last byte in keyboard table
STAB:		equ	0c71h		; Start of routine addresses for routine 00
SOUT:		equ	0c73h		; Start of table of output routines
SIN:		equ	0c75h		; Start of table of input routines
SUOUT:		equ	0c77h		; Jump to user output routine
SUIN:		equ	0c7ah		; Jump to user input routine
SNMI:		equ	0c7dh		; Address of breakpoint / NMI routine.

SYSSTACK:	equ	01000h		; System stack

; Floppy controller WD179x or WD279x
CmdStatReg:	equ	0xe0
TrackReg:	equ	0xe1
SectorReg:	equ	0xe2
DataReg:	equ	0xe3

CTRLReg:	equ	0xe4
INTReg:		equ	0xe5

DOSErrBadFn:	equ	0x00		; Bad ARH / BRH function
DOSErrDirEnd:	equ	0x01		; End of directory reached, need to ]C
DOSErrNotOpen:	equ	0x02		; File being accessed is not open
DOSErrPastEOF:	equ	0x03		; Specified rel sector is past EOF.
DOSErrNoSpace:	equ	0x04		; No spece to extend / save file.
 
DOSErr05:	equ	0x05
DOSErr1F:	equ	0x1f

DOSErrTimeOut:	equ	0xfd		; Time out talking to FDC
DOSErrInvDat:	equ	0xfe		; Invalid data
DOSErrInvFmt:	equ	0xff		; Invalid format

; FDC commands
WDCmdRestore:	equ	0x00		; Restore to track 0
WDCmdSeek:	equ	0x10		; Seek to track command
WDCmdStep:	equ	0x20		; Step heads
WDCmdStepIn:	equ	0x40		; Step heads in
WDCmdStepOut:	equ	0x60		; Step heads out
WDCmdReadSec:	equ	0x80		; Read sector command
WDCmdWriteSec:	equ	0xA0		; Write sector command
WDCmdReadAddr:	equ	0xC0		; Read address mark
WDCmdForceInt:	equ	0xD0		; Force inturrupt
WDCmdReadTrack:	equ	0xE0		; Read a track
WDCmdWriteTrack: equ	0xF0		; Write (format) track	

; FDC command options
WDCStep6ms:	equ	0x00		; Type 1 step rate 0, 6ms
WDCStep12ms:	equ	0x01		; Type 1 step rate 1, 12ms 
WDCStep20ms:	equ	0x02		; Type 1 step rate 2, 20ms
WDCStep30ms:	equ	0x03		; Type 1 step rate 3, 30ms
WDCVerifyTrk:	equ	0x04		; Type I verify track
WDCHeadLoad:	equ	0x08		; Type 1 head load
WDCTrkUpdate:	equ	0x10		; Type 1 track update (steps only)
WDCSideComp:	equ	0x02		; Type II side compare (x791, x793)
WDCSetSSO:	equ	0x02		; Type II update SSO  (x795, x797)
WDCDelay5ms:	equ	0x04		; Type II / III command delay
WDCSide1:	equ	0x08		; Type II Compare for side 1 (x791, x793)
WDCMultiple:	equ	0x10		; Type II multiple sectors

; Define this to WDCStepXms, Where X= 6,12,20,30 as defined above to globally set step rate.
; Early versions used 30ms, later versions used 20ms or even 12ms,
; with suitable drives this can be set to 6ms. e.g. TEAC FD55-F or most 3.5" drives.
WDCStepRate:	equ	WDCStep20ms	

; FDC Status bits
; Type I
WDSTNotReady:	equ	0x80		; Drive not ready
WDSTWriteProt:	equ	0x40		; Drive write protected 
WDSTHeadLoad:	equ	0x20		; Head is loaded
WDSTSeekErr:	equ	0x10		; Error seeking to track
WDSTCRCErr:	equ	0x08		; CRC error
WDSTTrk0:	equ	0x04		; Heads are at track 0
WDSTIndex:	equ	0x02		; Index pulse from drive
WDSTBusy:	equ	0x01		; Controller is busy


WDSTRNF:	equ	0x10		; Record not found 
WDSTRecordType:	equ	0x20		; Record type
WDSTLostData:	equ	0x04		; Lost data
WDSTDRQ:	equ	0x02		; Copy of DRQ line



; Control reg E4, read
CTLBDS0:	equ	0x01		; Drive select 0
CTLBDS1:	equ	0x02		; Drive select 1
CTLBDS2:	equ	0x04		; Drive select 2
CTLBDS3:	equ	0x08		; Drive select 3
CTLBSide:	equ	0x10		; Side select (if 1793 / 2793) & LK2 A-C
CTRLBSpare1:	equ	0x20		; Spare 1 connected to LK3
CTRLBDDEN:	equ	0x40		; Double desnity enable DD=1, SD=0
CTRLBSpare2:	equ	0x80		; Spare 2 connected to LK4

MaskDrives:	equ	CTLBDS0 + CTLBDS1 + CTLBDS2 + CTLBDS3
MaskDriveSide:	equ	MaskDrives + CTLBSide

; Control reg E4, write
; Bits 0..3 DS0 to DS3 will be as last write
; Bit 4 Side select will be valid only if LK2 connects A & C
; Bit 6 will be as last written
; Bit 7 connected to LK4

CTRLBMotor:	equ	0x20		; Low stops motor, high starts motor, times out after 10s

; Control reg E5, read only
; Bits 2..6 unused.
CTRLBIntRQ:	equ	0x01		; Int rq from FDC
CTRLBNotReady:	equ	0x02		; For this to be low motor must be on and in the case of 8" drives LK1 
					; must be present and READY i/p must be true.
CTRLBDRQ:	equ	0x80		; DRQ from FDC.								

; DOS constants
DOSSecPerTrk:	equ	010h		; Sectors per track
DOSTrkPerDsk:	equ	04Dh		; Tracks per disk : 77 (was 050h(80) in hacked bios in MAME)
DOSSides:	equ	001h		; Sides per disk
DOSSecSize:	equ	00100h		; sector size in bytes

DOSDataSecs:	equ	(DOSTrkPerDsk-1) * DOSSecPerTrk

DOSFormatBuf:	equ	01000h		; Where format buffer is in memory
DOSRawSecBuf:	equ	DOSFormatBuf + DOSSecPerTrk	; buffer for raw sector data for format

DOSMaxFNLen:	equ	008h		; Max filename length
DOSSeperator:	equ	':' 		; Dos seperator character ':'

DOSUserAddr:	equ	01000h		; Address userboot loaded at

DOSMaxPrep:	equ	00030h		; default max sectors to allocate with prep

DOSEoFMarker:	equ	0ffh		; End of file marker

DOSInvalid:	equ	0ffh		; Invalid / closed flag

;Dos workspace
USRMCP:		equ	0x0d00		; user master control program
WFDC:		equ	0x0d03		; Jump to cold start DOS
CMDCHR:		equ	0x0d06		; Command character normally ']'
WCMTAB:		equ	0x0d07		; Start of command table
WFILENAME:	equ	0x0d09		; Filename being accessed ?

WKSPACE:	equ 	0x0d18		; Workspace (8 bytes).
PHYSDRV:	equ	WKSPACE+0	; Current physical drive
WDCMD:		equ	WKSPACE+1	; Currently executing command

WDRV:		equ	0x0d20		; Drive number to be accessed
WFILE:		equ	0x0d21		; Filename address
WEXEC:		equ	0x0d23		; Exec address
WSECT:		equ	0x0d25		; Random sector number for BRH
WSIZE:		equ	0x0d27		; Max number of sectors to allocate during #prep
USDIO:		equ	0x0d29		; User disk I/O error routine
WDIRP:		equ	0x0d2C		; Directory pointer
WDIRS:		equ	0x0d2e		; Next dir sector to be read
WDDRV:		equ	0x0d2f		; Last dir drive accessed
WONME:		equ	0x0d30		; Current output filename (if open)

; Offsets within WFPIN and WFPOT
FPST:		equ	000h		; Start track
FPSS:		equ	001h		; Start sector
FPLN:		equ	002h		; Length of file (in sectors) at open
FPDN:		equ	004h		; Drive number where file located
FPTK:		equ	005h		; Current track pointer within file
FPSE:		equ	006h		; Current sector pointer within file

WIBP:		equ	0x0d38		; Input file pointer within buffer
WFPIN:		equ	0x0d3A		; Input file pointers
WFPINST:	equ	WFPIN+FPST 	; Start track
WFPINSS:	equ	WFPIN+FPSS 	; Start sector
WFPINLN:	equ	WFPIN+FPLN 	; Length of file (in sectors) at open
WFPINDN:	equ	WFPIN+FPDN 	; Drive number where file located
WFPINTK:	equ	WFPIN+FPTK 	; Current track pointer within file
WFPINSE:	equ	WFPIN+FPSE 	; Current sector pointer within file

WOBP:		equ	0x0d41		; Output buffer pointer within buffer
WFPOT:		equ	0x0d43		; Output file pointers
WFPOTST:	equ	WFPOT+FPST 	; Start track
WFPOTSS:	equ	WFPOT+FPSS 	; Start sector
WFPOTLN:	equ	WFPOT+FPLN 	; Length of file (in sectors) at open
WFPOTDN:	equ	WFPOT+FPDN 	; Drive number where file located
WFPOTTK:	equ	WFPOT+FPTK 	; Current track pointer within file
WFPOTSE:	equ	WFPOT+FPSE 	; Current sector pointer within file

WIBFL:		equ	0x0d4a		; Input buffer location
WOBFL:		equ	0x0d4c		; Output buffer location
WDTAB:		equ	0x0d4e		; Address of ARH table

DWDRV:		equ	0x0de9		; working drive whilst reading file
DSAVESP:	equ	0x0dea		; Saved SP

DOSDirBuff:	equ	00e00h		; DOS Directory buffer
DOSDirBuffEnd:	equ	DOSDirBuff + DOSSecSize 

; Opcodes used to fill in vectors etc

OpCodeJP:	equ	0c3h

; ARH  Codes 

AINITD:		equ	001h		; Initialize drive
AFORM:		equ	002h		; Format a disk
ACHAIN:		equ	003h		; Chain another program
ACRATE:		equ	004h		; Create a directory entry
ASCRAT:		equ	005h		; Scratch a directory entry
ASCAND:		equ	006h		; Scan directory
AOPEND:		equ	00ah		; Open a directory
AOPENI:		equ	00bh		; Open input file
AOPENO:		equ	00ch		; Open output file
APREPO:		equ	00dh		; Prepare new output file
AREAD:		equ	014h		; Direct read
AREADD:		equ	015h		; Read directory
AREADS:		equ	016h		; Read sequential
AREADR:		equ	017h		; Read random
AREADF:		equ	018h		; Read whole file
AREADH:		equ	019h		; Read directory header
ASAVE:		equ	01eh		; Direct save
ASAVED:		equ	01fh		; Directory save
ASAVES:		equ	020h		; Save sequential
ASAVER:		equ	021h		; Save random
ASAVEF:		equ	022h		; Save whole file
ASAVEH:		equ	023h		; Save directory header
ACLSEI:		equ	028h		; Close input file
ACLSEO:		equ	029h		; Close output file

; BRH  Codes, these are a subset of the ARH codes

BINITD:		equ	AINITD		; Initialize drive
BCHAIN:		equ	ACHAIN		; Chain another program
BSCRAT:		equ	ASCRAT		; Scratch a directory entry
BOPENI:		equ	AOPENI		; Open input file
BOPENO:		equ	AOPENO		; Open output file
BPREPO:		equ	APREPO		; Prepare new output file
BREADS:		equ	AREADS		; Read sequential
BREADR:		equ	AREADR		; Read random
BSAVES:		equ	ASAVES		; Save sequential
BSAVER:		equ	ASAVER		; Save random
BCLSEI:		equ	ACLSEI		; Close input file
BCLSEO:		equ	ACLSEO		; Close output file

; DIR entry offsets

DIRFilename:	equ	000h		; Filename
DIRExecAddr:	equ	008h		; Exec addresss
DIRLoadAddr:	equ	00Ah		; Load address
DIRTrack:	equ	00Ch		; Start track
DIRSector:	equ	00Dh		; Start sector
DIRFlen:	equ	00Eh		; File length
DIREntryLen:	equ	DIRFlen+2 	; Length of each dir entry

DIRFirstSec:	equ	003h		; First sector of dir
DIRSize:	equ	(DOSSecPerTrk - DIRFirstSec)+1
DIRCleanAddr:	equ	01000h		; Load address when cleaning dir
DIRNewAddr:	equ	01e00h		; Address where new dir built up
DIREntryCount:	equ	(DIRSize * DOSSecSize) / DIREntryLen

DIRDeleted:	equ	0feh		; Deleted file marker

DIRHeadFlag:	equ	0eeh		; Flag to read / write dir header

; 8K ROM Basic defs

BASBaseAddr:	equ	01000h		; Base address of basic
BASTXT:  	EQU     105EH		; Pointer to start of program
PROGND:  	EQU     10D6H		; End of program
VAREND:  	EQU     10D8H		; End of variables
ARREND:  	EQU     10DAH		; End of arrays

BASVarSize:	equ	006h		; Size of basic variable definition

TokREM:		equ	08eh		; REM basic token

; basic calls
CPDEHL:		equ	0e68ah		; Compare DE & HL

; NAS-PEN defs
PENBaseAddr:	equ	01000h		; Base address of NAS-PEN
PENEndText:	equ	0101ah		; End of text pointer
PENCold:	equ	0b800h		; NAS-PEN Cold start
PENWarm:	equ	0b806h		; NAS-PEN Warm start

; Zeap defs
ZEPBaseAddrPtr:	equ	00f00h		; Base address pointer to Zeap source file

