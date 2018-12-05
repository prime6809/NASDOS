all: reasm

disasm:
	z80dasm.exe -v nasdos.rom --origin=0xd000 --sym-output=dossym.asm --sym-input=nasdefs-in.asm --block-def=blocks.txt -l -t -o nasdos_dis.asm

reasm:
	z80asm -vv -o nasdos.rom nasdefs.asm nasdos.asm -l 2> nasdos.lst
  

clean:
	rm -f *.lst 
	rm -f *.rom