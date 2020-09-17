########################################################
CC=gcc
CFLAGS= -g -Wall -pedantic -ansi
EJS = p4_e1 p4_e2 p4_e2_bal p4_e2_test p4_e3 p4_e3_list p4_e4
########################################################
## list.o se necesita si se tiene implementado el opcional en P4_E3
OBJECTSP4_E1 = p4_e1.o tree.o functions.o node.o list.o
OBJECTSP4_E2 = p4_e2.o tree.o list.o functions.o node_p4.o
OBJECTSP4_E2_TEST = p4_e2_test.o tree.o functions.o node_p4.o list.o
OBJECTSP4_E3 = p4_e3.o tree.o functions.o node_p4.o list.o
OBJECTSP4_E3_LIST = p4_e3_list.o tree.o functions.o node_p4.o list.o
OBJECTSP4_E4 = p4_e4.o tree.o functions.o node.o list.o
########################################################

all: $(EJS) clear

p4_e1: $(OBJECTSP4_E1)
	$(CC) $(CFLAGS) -o p4_e1 $(OBJECTSP4_E1)
	
p4_e2: $(OBJECTSP4_E2)
	$(CC) $(CFLAGS) -o p4_e2 $(OBJECTSP4_E2)

p4_e2_bal: $(OBJECTSP4_E2)
	$(CC) $(CFLAGS) -o p4_e2_bal $(OBJECTSP4_E2)

p4_e2_test: $(OBJECTSP4_E2_TEST)
	$(CC) $(CFLAGS) -o p4_e2_test $(OBJECTSP4_E2_TEST)
	
p4_e3: $(OBJECTSP4_E3)
	$(CC) $(CFLAGS) -o p4_e3 $(OBJECTSP4_E3)
	
p4_e3_list: $(OBJECTSP4_E3_LIST)
	$(CC) $(CFLAGS) -o p4_e3_list $(OBJECTSP4_E3_LIST)

p4_e4: $(OBJECTSP4_E4)
	$(CC) $(CFLAGS) -o p4_e4 $(OBJECTSP4_E4)

p4_e1.o:p4_e1.c tree.h functions.h
	$(CC) $(CFLAGS) -c p4_e1.c

p4_e2.o: p4_e2.c tree.h functions.h
	$(CC) $(CFLAGS) -c p4_e2.c
	
p4_e2_test.o: p4_e2_test.c tree.h functions.h tree.h
	$(CC) $(CFLAGS) -c p4_e2_test.c

p4_e3.o: p4_e3.c tree.h functions.h
	$(CC) $(CFLAGS) -c p4_e3.c

p4_e3_list.o: p4_e3_list.c tree.h functions.h list.h
	$(CC) $(CFLAGS) -c p4_e3_list.c

p4_e4.o: p4_e4.c tree.h functions.h
	$(CC) $(CFLAGS) -c p4_e4.c

functions.o: functions.c functions.h
	$(CC) $(CFLAGS) -c functions.c

list.o: list.c list.h
	$(CC) $(CFLAGS) -c list.c

tree.o: tree.c tree.h
	$(CC) $(CFLAGS) -c tree.c

node_p4.o: node_p4.c node.h
	$(CC) $(CFLAGS) -c node_p4.c

clear:
	rm -rf *.o 

clean:
	rm -rf *.o $(EJS)

run:
	@echo ">>>>>>Running p4_e1"
	./p4_e1 numeros.txt
	@echo ">>>>>>Running p4_e2"
	./p4_e2 dict10.dat
	@echo ">>>>>>Running p4_e2_bal"
	./p4_e2_bal dict10.dat
	@echo ">>>>>>Running p4_e2_test"
	./p4_e2_test nodos.txt
	@echo ">>>>>>Running p4_e3"
	./p4_e3 numeros.txt
	@echo ">>>>>>Running p4_e3_list"
	./p4_e3_list numeros.txt
	@echo ">>>>>>Running p4_e4"
	./p4_e4 cadenas.txt

runv:
	@echo ">>>>>>Running p4_e1 with valgrind"
	valgrind --leak-check=full ./p4_e1 numeros.txt
	@echo ">>>>>>Running p4_e2 with valgrind"
	valgrind --leak-check=full ./p4_e2 dict10.dat
	@echo ">>>>>>Running p4_e2_bal with valgrind"
	valgrind --leak-check=full ./p4_e2_bal dict10.dat
	@echo ">>>>>>Running p4_e2_test with valgrind"
	valgrind --leak-check=full ./p4_e2_test nodos.txt
	@echo ">>>>>>Running p4_e3 with valgrind"
	valgrind --leak-check=full ./p4_e3 numeros.txt
	@echo ">>>>>>Running p4_e3_list with valgrind"
	valgrind --leak-check=full ./p4_e3_list numeros.txt
	@echo ">>>>>>Running p4_e24 with valgrind"
	valgrind --leak-check=full ./p4_e4 cadenas.txt