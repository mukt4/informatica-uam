all: pract1a.exe pract1b.exe pract1c.exe

pract1a.exe: pract1a.obj
	tlink /v pract1a.obj
	
pract1a.obj: pract1a.asm
	tasm /zi pract1a.asm
	
pract1b.exe: pract1b.obj
	tlink /v pract1b.obj
	
pract1b.obj: pract1b.asm
	tasm /zi pract1b.asm
	
pract1c.exe: pract1c.obj
	tlink /v pract1c.obj
	
pract1c.obj: pract1c.asm
	tasm /zi pract1c.asm
	
clean: delPract1a delPract1b delPract1c

delPract1a: 
	del pract1a.obj
	del pract1a.map 
	del pract1a.exe

delPract1b: 
	del pract1b.obj 
	del pract1b.map 
	del pract1b.exe

delPract1c: 
	del pract1c.obj 
	del pract1c.map 
	del pract1c.exe
