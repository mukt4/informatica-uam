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
	rm -f *.o *.dot
	rm -R tests automatas

dist:
	tar -zcvf AUTLEN_P1_G10.tgz *.h *.c *.pdf

tests: testPalabra testAfnd

ejecutables: main pr_er_enunciado

order:
	mkdir automatas
	mv main pr_er_enunciado automatas

main: estado.o alfabeto.o palabra.o afnd.o main.o

pr_er_enunciado: estado.o alfabeto.o palabra.o afnd.o pr_er_enunciado.o
