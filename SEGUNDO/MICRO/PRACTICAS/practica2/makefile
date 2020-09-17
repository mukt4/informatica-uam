all: pract2a.exe pract2b.exe pract2c.exe

pract2a.exe: pract2a.obj
	tlink /v pract2a.obj
	
pract2a.obj: pract2a.asm
	tasm /zi pract2a.asm
	
pract2b.exe: pract2b.obj
	tlink /v pract2b.obj
	
pract2b.obj: pract2b.asm
	tasm /zi pract2b.asm
	
pract2c.exe: pract2c.obj
	tlink /v pract2c.obj
	
pract2c.obj: pract2c.asm
	tasm /zi pract2c.asm
	
clean: delPract2a delPract2b delPract2c

delPract2a: 
	del pract2a.obj
	del pract2a.map 
	del pract2a.exe

delPract2b: 
	del pract2b.obj 
	del pract2b.map 
	del pract2b.exe

delPract2c: 
	del pract2c.obj 
	del pract2c.map 
	del pract2c.exe
