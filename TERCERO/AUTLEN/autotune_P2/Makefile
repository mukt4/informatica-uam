CC = gcc
CFLAGS = -Wall -ansi -pedantic -g

all: tests ejecutables order

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

tests: testPalabra testAfnd

ejecutables: main testsClase

runtests:
	@echo "#Palabra test---->"
	tests/./testPalabra
	
	@echo "#Afnd test---->"
	tests/./testAfnd
	
order:
	mkdir tests automatas
	mv testPalabra testAfnd tests
	mv main testsClase automatas
	
main: estado.o alfabeto.o palabra.o afnd.o main.o

testsClase: estado.o alfabeto.o palabra.o afnd.o testsClase.o

testPalabra: palabra.o testPalabra.o

testAfnd: afnd.o testAfnd.o alfabeto.o palabra.o estado.o

