CC = gcc
CFLAGS = -Wall -g -D_SVID_SOURCE
TFLAGS = -Wall -ansi -g -pthread

all: ejercicio2 ejercicio2_solved ejercicio3 ejercicio4 cadena_montaje

ejercicio2: ejercicio2.o 
	$(CC) $(CFLAGS) -o $@ ejercicio2.o 
	
ejercicio2_solved: ejercicio2_solved.o semaforos.o
	$(CC) $(CFLAGS) -o $@ ejercicio2_solved.o semaforos.o

ejercicio3: ejercicio3.o semaforos.o
	$(CC) $(CFLAGS) -o $@ ejercicio3.o semaforos.o

ejercicio4: ejercicio4.o
	$(CC) $(TFLAGS) -o $@ ejercicio4.o
	
cadena_montaje: cadena_montaje.o semaforos.o
	$(CC) $(CFLAGS) -o $@ cadena_montaje.o semaforos.o

.PHONY: clean dist run runv

clean:
	rm -f *.o ejercicio2 ejercicio2_solved ejercicio3 ejercicio4 cadena_montaje
dist:
	tar -zcvf G2272_P02_2.tgz *.c *.odt Makefile
run:
	@echo "#Ejecutando ejercicio2, ejercicio3, ejercicio4, cadena_montaje---->"
	./ejercicio2 ./ejercicio2_solved ./ejercicio3 ./ejercicio4 ./cadena_montaje
	
runv:
	@echo "#Ejecutando ejercicio2 con valgrind---->"
	valgrind --leak-check=full ./ejercicio2
	
	@echo "#Ejecutando ejercicio2_solved con valgrind---->"
	valgrind --leak-check=full ./ejercicio2_solved
	
	@echo "#Ejecutando ejercicio3 con valgrind---->"
	valgrind --leak-check=full ./ejercicio3
	
	@echo "#Ejecutando ejercicio4 con valgrind---->"
	valgrind --leak-check=full ./ejercicio4
	
	@echo "#Ejecutando cadena_montaje con valgrind---->"
	valgrind --leak-check=full ./cadena_montaje
	
ejercicio2.o: ejercicio2.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@

ejercicio2_solved.o: ejercicio2_solved.c
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
   
	$(CC) $(TFLAGS) -c $< -o $@
	
cadena_montaje.o: cadena_montaje.c
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
