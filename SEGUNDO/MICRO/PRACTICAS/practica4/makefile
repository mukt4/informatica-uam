all: p4a.com p4b.exe
	
p4a.com: p4a.obj
	tlink /t p4a.obj
	
p4a.obj: p4a.asm
	tasm /zi p4a.asm
	
p4b.exe: p4b.obj
	tlink /v p4b.obj
	
p4b.obj: p4b.asm
	tasm /zi p4b.asm
	
clean: delp4a delp4b

delp4a: 
	del p4a.obj
	del p4a.map 
	del p4a.com

delp4b: 
	del p4b.obj 
	del p4b.map 
	del p4b.exe
