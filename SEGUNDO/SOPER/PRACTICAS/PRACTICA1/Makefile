CC = gcc
CFLAGS = -Wall -ansi -g

all: ejercicio4a ejercicio4b ejercicio5a ejercicio5b ejercicio6 ejercicio8 ejercicio9 ejercicio12a ejercicio12b ejercicio13

ejercicio4a: ejercicio4a.o
	$(CC) $(CFLAGS) -o $@ ejercicio4a.o
	
ejercicio4b: ejercicio4b.o
	$(CC) $(CFLAGS) -o $@ ejercicio4b.o
	
ejercicio5a: ejercicio5a.o
	$(CC) $(CFLAGS) -o $@ ejercicio5a.o
	
ejercicio5b: ejercicio5b.o
	$(CC) $(CFLAGS) -o $@ ejercicio5b.o
	
ejercicio6: ejercicio6.o
	$(CC) $(CFLAGS) -o $@ ejercicio6.o
	
ejercicio8: ejercicio8.o
	$(CC) $(CFLAGS) -o $@ ejercicio8.o

ejercicio9: ejercicio9.o
	$(CC) $(CFLAGS) -o $@ ejercicio9.o
	
ejercicio12a: ejercicio12a.o
	$(CC) $(CFLAGS) -pthread -o $@ ejercicio12a.o

ejercicio12b: ejercicio12b.o
	$(CC) $(CFLAGS) -pthread -o $@ ejercicio12b.o
	
ejercicio13: ejercicio13.o
	$(CC) $(CFLAGS) -pthread -o $@ ejercicio13.o

.PHONY: clean dist run runv

clean:
	rm -f *.o ejercicio4a ejercicio4b ejercicio5a ejercicio5b ejercicio6 ejercicio8 ejercicio9 ejercicio12a ejercicio12b ejercicio13
dist:
	tar -zcvf G2272_P02_1.tgz *.c *.odt Makefile
run:
	@echo "#Ejecutando ejercicio4a---->"
	./ejercicio4a
	@echo "#Ejecutando ejercicio4b---->"
	./ejercicio4b
	@echo "#Ejecutando ejercicio5a---->"
	./ejercicio5a
	@echo "#Ejecutando ejercicio5b---->"
	./ejercicio5b
	@echo "#Ejecutando ejercicio6---->"
	./ejercicio6
	@echo "#Ejecutando ejercicio8---->"
	./ejercicio8 ls df du -lp
	@echo "#Ejecutando ejercicio9---->"
	./ejercicio9
	@echo "#Ejecutando ejercicio12a---->"
	./ejercicio12a 25
	@echo "#Ejecutando ejercicio12b---->"
	./ejercicio12b 25
	@echo "#Ejecutando ejercicio13---->"
	./ejercicio13
runv:
	@echo "#Ejecutando ejercicio4a con valgrind---->"
	valgrind --leak-check=full ./ejercicio4a
	@echo "#Ejecutando ejercicio4b con valgrind---->"
	valgrind --leak-check=full ./ejercicio4b
	@echo "#Ejecutando ejercicio5a con valgrind---->"
	valgrind --leak-check=full ./ejercicio5a
	@echo "#Ejecutando ejercicio5b con valgrind---->"
	valgrind --leak-check=full ./ejercicio5b
	@echo "#Ejecutando ejercicio6 con valgrind---->"
	valgrind --leak-check=full ./ejercicio6
	@echo "#Ejecutando ejercicio8 con valgrind---->"
	valgrind --leak-check=full ./ejercicio8 ls df du -lp
	@echo "#Ejecutando ejercicio9 con valgrind---->"
	valgrind --leak-check=full ./ejercicio9
	@echo "#Ejecutando ejercicio12a con valgrind---->"
	valgrind --leak-check=full ./ejercicio12a 25
	@echo "#Ejecutando ejercicio12b con valgrind---->"
	valgrind --leak-check=full ./ejercicio12b 25
	@echo "#Ejecutando ejercicio13 con valgrind---->"
	valgrind --leak-check=full ./ejercicio13

ejercicio4a.o: ejercicio4a.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
ejercicio4b.o: ejercicio4b.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@

ejercicio5a.o: ejercicio5a.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
ejercicio5b.o: ejercicio5b.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
ejercicio6.o: ejercicio6.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c $< -o $@
	
ejercicio8.o: ejercicio8.c
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
	
ejercicio12a.o: ejercicio12a.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -pthread -c $< -o $@
	
ejercicio12b.o: ejercicio12b.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -pthread -c $< -o $@
	
ejercicio13.o: ejercicio13.c
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -pthread -c $< -o $@