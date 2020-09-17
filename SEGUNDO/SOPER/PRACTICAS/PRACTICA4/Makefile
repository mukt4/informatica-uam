CC = gcc
CFLAGS = -Wall -g -D_SVID_SOURCE -pthread

all: principal

principal: aleatorio.o semaforos.o seniales.o main.o  gestor.o monitor.o
	$(CC) $(CFLAGS) -o $@ aleatorio.o semaforos.o seniales.o main.o gestor.o monitor.o

.PHONY: clean dist run runv

clean:
	rm -f *.o principal
dist:
	tar -zcvf G2272_P02_4.tgz *.c *.h *.txt Makefile
run:
	@echo "#Ejecutando programa principal---->"
	./principal
	
runv:
	@echo "#Ejecutando programa principal con valgrind---->"
	valgrind --leak-check=full ./ejercicio2
	
aleatorio.o: aleatorio.c aleatorio.h
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

seniales.o: seniales.c seniales.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@

main.o: main.c semaforos.h seniales.h aleatorio.h gestor.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
gestor.o: gestor.c gestor.h seniales.h 
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
monitor.o: monitor.c monitor.h gestor.h semaforos.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@


