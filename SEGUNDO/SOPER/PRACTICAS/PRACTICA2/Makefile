CC = gcc
CFLAGS = -Wall -g -D_SVID_SOURCE

all: ejercicio2 ejercicio3 ejercicio4 ejercicio5 ejercicio6a ejercicio6b ejercicio7 ejercicio9

ejercicio2: ejercicio2.o
	$(CC) $(CFLAGS) -o $@ ejercicio2.o
	
ejercicio3: ejercicio3.o
	$(CC) $(CFLAGS) -o $@ ejercicio3.o
	
ejercicio4: ejercicio4.o
	$(CC) $(CFLAGS) -o $@ ejercicio4.o
	
ejercicio5: ejercicio5.o
	$(CC) $(CFLAGS) -o $@ ejercicio5.o

ejercicio6a: ejercicio6a.o
	$(CC) $(FLAGS6) -o $@ ejercicio6a.o

ejercicio6b: ejercicio6b.o
	$(CC) $(FLAGS6) -o $@ ejercicio6b.o

ejercicio7: ejercicio7.o semaforos.o
	$(CC) $(FLAGS6) -o $@ ejercicio7.o semaforos.o
	
ejercicio9: ejercicio9.o semaforos.o
	$(CC) $(CFLAGS) -o $@ ejercicio9.o semaforos.o

.PHONY: clean dist run runv

clean:
	rm -f *.o ejercicio2 ejercicio3 ejercicio4 ejercicio5 ejercicio6a ejercicio6b ejercicio7 ejercicio9
dist:
	tar -zcvf G2272_P02_2.tgz *.c *.odt Makefile
run:
	@echo "#Ejecutando ejercicio2---->"
	./ejercicio2
	@echo "#Ejecutando ejercicio3---->"
	./ejercicio3
	@echo "#Ejecutando ejercicio4---->"
	./ejercicio4
	@echo "#Ejecutando ejercicio5---->"
	./ejercicio5
	@echo "#Ejecutando ejercicio6a---->"
	./ejercicio6a
	@echo "#Ejecutando ejercicio6b---->"
	./ejercicio6b
	@echo "#Ejecutando ejercicio9---->"
	./ejercicio9
	@echo "#Ejecutando ejercicio9---->"
	./ejercicio99
runv:
	@echo "#Ejecutando ejercicio2 con valgrind---->"
	valgrind --leak-check=full ./ejercicio2
	@echo "#Ejecutando ejercicio3 con valgrind---->"
	valgrind --leak-check=full ./ejercicio3
	@echo "#Ejecutando ejercicio4 con valgrind---->"
	valgrind --leak-check=full ./ejercicio4
	@echo "#Ejecutando ejercicio5 con valgrind---->"
	valgrind --leak-check=full ./ejercicio5
	@echo "#Ejecutando ejercicio6a con valgrind---->"
	valgrind --leak-check=full ./ejercicio6a
	@echo "#Ejecutando ejercicio6b con valgrind---->"
	valgrind --leak-check=full ./ejercicio6b
	@echo "#Ejecutando ejercicio9 con valgrind---->"
	valgrind --leak-check=full ./ejercicio9
	@echo "#Ejecutando ejercicio9 con valgrind---->"
	valgrind --leak-check=full ./ejercicio99
	
ejercicio2.o: ejercicio2.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
ejercicio3.o: ejercicio3.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@

ejercicio4.o: ejercicio4.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
ejercicio5.o: ejercicio5.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
ejercicio6a.o: ejercicio6a.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
ejercicio6b.o: ejercicio6b.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
ejercicio7.o: ejercicio7.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
ejercicio9.o: ejercicio9.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
semaforos.o: semaforos.c semaforos.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
