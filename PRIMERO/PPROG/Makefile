CC = gcc
CFLAGS = -Wall -ansi -pedantic -g
MOD = obj/game.o obj/game_loop.o obj/command.o obj/graphic_engine.o obj/screen.o obj/space.o obj/game_management.o obj/player.o obj/object.o obj/set.o obj/die.o obj/inventory.o obj/link.o obj/dialogue.o obj/game_rules.o obj/stats.o obj/enemy.o obj/menu.o obj/battle.o obj/music.o 
vpath %.c ./src
vpath %.h ./include

all: ocabas

doxygen:
	doxygen Doxyfile

set_test: obj/set.o obj/set_test.o
	$(CC) $(CFLAGS) -o $@ obj/set.o obj/set_test.o
	
die_test: obj/die.o obj/die_test.o
	$(CC) $(CFLAGS) -o $@ obj/die.o obj/die_test.o
	
inventory_test: obj/inventory_test.o obj/inventory.o obj/set.o
	$(CC) $(CFLAGS) -o $@ obj/inventory_test.o obj/inventory.o obj/set.o
	
link_test: obj/link.o obj/link_test.o
	$(CC) $(CFLAGS) -o $@ obj/link.o obj/link_test.o

space_test: obj/space_test.o obj/space.o 
	$(CC) $(CFLAGS) -o $@ obj/space_test.o obj/space.o obj/set.o
	
player_test: obj/player_test.o obj/player.o obj/inventory.o obj/set.o obj/stats.o
	$(CC) $(CFLAGS) -o $@ obj/player_test.o obj/player.o obj/inventory.o obj/set.o obj/stats.o
	
object_test: obj/object_test.o obj/object.o 
	$(CC) $(CFLAGS) -o $@ obj/object_test.o obj/object.o

command_test: obj/command_test.o obj/command.o 
	$(CC) $(CFLAGS) -o $@ obj/command_test.o obj/command.o
	
enemy_test: obj/enemy_test.o obj/enemy.o 
	$(CC) $(CFLAGS) -o $@ obj/enemy_test.o obj/enemy.o
	
battle_test: obj/battle_test.o obj/game.o obj/command.o obj/graphic_engine.o obj/screen.o obj/space.o obj/game_management.o obj/player.o obj/object.o obj/set.o obj/die.o obj/inventory.o obj/link.o obj/dialogue.o obj/game_rules.o obj/stats.o obj/enemy.o obj/menu.o obj/battle.o obj/music.o 
	$(CC) $(CFLAGS) -o $@ obj/battle_test.o obj/game.o obj/command.o obj/graphic_engine.o obj/screen.o obj/space.o obj/game_management.o obj/player.o obj/object.o obj/set.o obj/die.o obj/inventory.o obj/link.o obj/dialogue.o obj/game_rules.o obj/stats.o obj/enemy.o obj/menu.o obj/battle.o obj/music.o 
	
dialogue_test: obj/dialogue_test.o obj/dialogue.o 
	$(CC) $(CFLAGS) -o $@ obj/dialogue_test.o obj/dialogue.o
	
ocabas: $(MOD)
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
	
	$(CC) $(CFLAGS) -o $@ $(MOD)

.PHONY: clean dist tests runtests
clean:
	rm -f obj/*.o ocabas set_test die_test inventory_test link_test space_test player_test battle_test dialogue_test stats_test enemy_test command_test game_rules_test object_test
dist:
	tar -zcvf ocabas.tgz include/*.h src/*.c *.dat
tests: set_test die_test inventory_test link_test space_test player_test object_test command_test enemy_test battle_test dialogue_test
runtests:
	@echo "#Set test---->"
	./set_test
	@echo "#Die test---->"
	./die_test
	@echo "#Inventory test---->"
	./inventory_test
	@echo "#-Link test---->"
	./link_test
	@echo "#Space test---->"
	./space_test
	@echo "#Player test---->"
	./player_test
	@echo "#Object test---->"
	./object_test
	@echo "#Command test---->"
	./command_test
	@echo "#Enemy test---->"
	./enemy_test
	@echo "#Battle test---->"
	./battle_test
	@echo "#Dialogue test---->"
	./dialogue_test

obj/game.o: game.c game.h enemy.h game_management.h link.h command.h dialogue.h enemy.h battle.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/command.o: command.c command.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/command_test.o: command_test.c command.h command_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/graphic_engine.o: graphic_engine.c graphic_engine.h command.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/screen.o: screen.c screen.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/space.o: space.c space.h 
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/game_loop.o: game_loop.c graphic_engine.h menu.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/game_management.o: game_management.c game_management.h player.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/player.o: player.c player.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/object.o: object.c object.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/object_test.o: object_test.c object.h object_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/player_test.o: player_test.c player.h player_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/set.o: set.c set.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/set_test.o: set_test.c set.h set_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/die.o: die.c die.h game.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/die_test.o: die_test.c die.h die_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/inventory.o: inventory.c inventory.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/inventory_test.o: inventory_test.c inventory.h inventory_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/link_test.o: link_test.c link.h  link_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/link.o: link.c link.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/space_test.o: space_test.c space_test.h test.h space.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/dialogue.o: dialogue.c dialogue.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/dialogue_test.o: dialogue_test.c dialogue.h dialogue_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@	
	
obj/game_rules.o: game_rules.c game_rules.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/game_rules_test.o: game_rules_test.c game_rules.h game_rules_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/stats.o: stats.c stats.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/stats_test.o: stats_test.c stats.h stats_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/enemy.o: enemy.c enemy.h player.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/enemy_test.o: enemy_test.c enemy.h enemy_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

obj/menu.o: menu.c menu.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/battle.o: battle.c battle.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/battle_test.o: battle_test.c battle.h battle_test.h test.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@
	
obj/music.o: music.c music.h
	@echo "#---------------------------"
	@echo "# Generando $@"
	@echo "# Depende de $^"
	@echo "# Ha cambiado $<"
   
	$(CC) $(CFLAGS) -c -Iinclude $< -o $@

