CC = gcc
CFLAGS = -Wall -ansi -pedantic -g

all: ejecutables order

%.o :	%.c
	@echo -n compilando objeto \'$<\'...
	@$(CC) $(CFLAGS) $< -c
	@echo [OK]
	
% :	%.o
	@echo -n compilando ejecutable \'$@\'...
	@$(CC) $(CFLAGS) $^ -o $@
	@echo [OK]

.PHONY: clean dist runtests order

clean:
	rm -f *.o 
	rm -R tests automatas
	
dist:
	tar -zcvf AUTLEN_P1_G10.tgz *.h *.c *.pdf

tests: testAlfabeto testEstado testPalabra testAfnd

ejecutables: main

runtests:
	@echo "#Alfabeto test---->"
	tests/./testAlfabeto
	
	@echo "#Estado test---->"
	tests/./testEstado
	
	@echo "#Palabra test---->"
	tests/./testPalabra
	
	@echo "#Afnd test---->"
	tests/./testAfnd
	
order:
	mkdir tests automatas
	mv testAlfabeto testEstado testPalabra testAfnd tests
	mv main testAFNDGrupo10 testAFNDGrupo11 testAFNDFallosGrupo10 automatas
	
main: estado.o alfabeto.o palabra.o afnd.o main.o

testAlfabeto: alfabeto.o testAlfabeto.o

testEstado: estado.o testEstado.o

testPalabra: palabra.o testPalabra.o

testAfnd: afnd.o testAfnd.o alfabeto.o palabra.o estado.o

testAFNDGrupo10: estado.o alfabeto.o palabra.o afnd.o testAFNDGrupo10.o

testAFNDGrupo11: estado.o alfabeto.o palabra.o afnd.o testAFNDGrupo11.o

testAFNDFallosGrupo10: estado.o alfabeto.o palabra.o afnd.o testAFNDFallosGrupo10.o