/**
* @brief It loads all the information of the game
*
* This module loads all the information needed for 
* the proper working of the game
* @file game_management.c
* @author Guillermo Hoyo Bravo
* @version 1.0
* @date 12-3-2017
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "game_management.h"
#include "game.h"
#include "link.h"
#include "space.h"
#include "object.h"

STATUS game_reader_load_spaces(Game* game, char* filename){
  FILE* file = NULL;
  char line[WORD_SIZE] = "";
  char name[WORD_SIZE] = "";
  char description[SPACE_DESC] = "";
  char* toks = NULL;
  Id id = NO_ID, north = NO_ID, east = NO_ID, south = NO_ID, west = NO_ID,up=NO_ID,down=NO_ID;
  Space* space = NULL;
  STATUS status = OK;
  char gdesc[ROW][COLUMN+1];
  char iDesc[SPACE_DESC];
  BOOL ilumination;
  int i;
  
  if (!filename || !game) {
    return ERROR;
  }
  
  file = fopen(filename, "r");
  if (file == NULL) {
    return ERROR;
  }
  
  while (fgets(line, WORD_SIZE, file)) {
    if (strncmp("#s:", line, 3) == 0) {
      toks = strtok(line + 3, "%");
      id = atol(toks);
      toks = strtok(NULL, "%");
      strcpy(name, toks);
      toks = strtok(NULL, "%");
      north = atol(toks);
      toks = strtok(NULL, "%");
      east = atol(toks);
      toks = strtok(NULL, "%");
      south = atol(toks);
      toks = strtok(NULL, "%");
      west = atol(toks);
      toks = strtok(NULL, "%");
      up = atol(toks);
      toks = strtok(NULL, "%");
      down = atol(toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[0], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[1], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[2], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[3], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[4], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[5], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[6], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[7], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[8], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[9], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[10], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[11], toks);
      toks = strtok(NULL, "%");
      strcpy(gdesc[12], toks);
      toks = strtok(NULL, "%");
      strcpy(description, toks);
      toks = strtok(NULL, "%");
      strcpy(iDesc, toks);
      toks = strtok(NULL, "%");
      ilumination = atol(toks);
      
#ifdef DEBUG 
      printf("Leido: %ld|%s|%ld|%ld|%ld|%ld|%s|%s|%ld\n", id, name, north, east, south, west,description, iDesc, (BOOL)ilumination);
#endif
      space = space_create(id);
      if (space != NULL) {
	        space_set_name(space, name);
	        space_set_north(space, north);
	        space_set_east(space, east);
          space_set_south(space, south);
          space_set_west(space, west);
          space_set_up(space,up);
          space_set_down(space,down);
          space_set_iDesc(space, iDesc);
          space_set_description(space,description);
          for(i=0;i<ROW;i++){
            space_set_global_gdesc(space,i,gdesc[i]);
          }
          space_set_ilumination(space, ilumination);
          game_add_space(game, space);
      }
    }
  }
  
  if (ferror(file)) {
    status = ERROR;
  }
  fclose(file);
  
  return status;
}

STATUS game_reader_load_objects(Game* game, char* filename){
  FILE* file = NULL;
  char line[WORD_SIZE] = "";
  char name[WORD_SIZE] = "";
  char description[OBJECT_DESC] = "";
  char description2[OBJECT_DESC] = "";
  char* toks = NULL;
  int hp=0,ad=0,def=0,speed=0;
  Id id = NO_ID;
  Id spaceId =NO_ID;
  Object* object = NULL;
  STATUS status = OK;
  BOOL movible;
  BOOL movido;
  BOOL oculto;
  Id abre;
  BOOL ilumina;
  BOOL encendido;
  
  if (!filename)
    return ERROR;
  
  file = fopen(filename, "r");
  if (file == NULL)
    return ERROR;
  
  while (fgets(line, WORD_SIZE, file)) {
    if (strncmp("#o:", line, 3) == 0) {
      toks = strtok(line + 3, "|");
      id = atol(toks);
      toks = strtok(NULL, "|");
      strcpy(name, toks);
      toks = strtok(NULL, "|");
      spaceId = atol(toks);
      toks = strtok(NULL, "|");
      strcpy(description, toks);
      toks = strtok(NULL, "|");
      movible = atol(toks);
      toks = strtok(NULL, "|");
      movido = atol(toks);
      toks = strtok(NULL, "|");
      oculto = atol(toks);
      toks = strtok(NULL, "|");
      abre = atol(toks);
      toks = strtok(NULL, "|");
      ilumina = atol(toks);
      toks = strtok(NULL, "|");
      encendido = atol(toks);
      toks = strtok(NULL, "|");
      strcpy(description2, toks);
      hp = atol(toks);
      toks = strtok(NULL, "|");
      ad = atol(toks);
      toks = strtok(NULL, "|");
      def = atol(toks);
      toks = strtok(NULL, "|");
      speed = atol(toks);
      toks = strtok(NULL, "|");
      
#ifdef DEBUG 
      printf("Leido: %ld|%s|%ld|%s|%ld|%ld|%ld|%d|%ld|%ld\n", id, name, spaceId, description,(BOOL)movible, (BOOL)movido, (BOOL)oculto, (int)abre, (BOOL)ilumina, (BOOL)encendido);
#endif

      object = object_create(id);
      if (object != NULL) {
	        object_set_name(object,name);
	        object_set_description(object, description);
	        object_set_movible(object, movible);
	        object_set_movido(object, movido);
	        object_set_oculto(object, oculto);
	        object_set_abre(object, abre);
	        object_set_ilumina(object, ilumina);
	        object_set_encendido(object, encendido);
	        object_set_description2(object, description2);
	        object_set_hp(object,hp);
	        object_set_ad(object,ad);
	        object_set_def(object,def);
	        object_set_speed(object,speed);
      }
      space_add_object(game_get_space(game,spaceId),id);
      game_addObject(game,object);
    }
  }
  
  if (ferror(file)) {
    status = ERROR;
  }
  fclose(file);
  return status;
}

STATUS game_reader_load_links(Game* game, char* filename){
  FILE* file = NULL;
  char line[WORD_SIZE] = "";
  char name[WORD_SIZE] = "";
  char* toks = NULL;
  Id id = NO_ID;
  Id tied1 =NO_ID;
  Id tied2=NO_ID;
  BOOL stat=FALSE;
  Link* link = NULL;
  STATUS status=OK;
  
  if (!filename)
    return ERROR;
  
  file = fopen(filename, "r");
  if (file == NULL)
    return ERROR;
  
  while (fgets(line, WORD_SIZE, file)) {
    if (strncmp("#l:", line, 3) == 0) {
      toks = strtok(line + 3, "|");
      id = atol(toks);
      toks = strtok(NULL, "|");
      strcpy(name, toks);
      toks = strtok(NULL, "|");
      tied1 = atol(toks);
      toks = strtok(NULL, "|");
      tied2 = atol(toks);
      toks = strtok(NULL, "|");
      stat= atol(toks);
#ifdef DEBUG 
      printf("Leido: %ld|%s|%ld|%ld|%ld\n", id, name, tied1, tied2, (BOOL)stat);
#endif
      link = link_create(id);
      if (link != NULL) {
	        link_set_name(link,name);
	        link_set_tied(link,tied1,tied2);
	        link_set_status(link,stat);
      }
      game_addLink(game,link);
    }
  }
  
  if (ferror(file)) {
    status = ERROR;
  }
  fclose(file);
  return status;
}

STATUS game_reader_load_players(Game* game, char* filename){
  FILE* file = NULL;
  char line[WORD_SIZE] = "";
  char name[WORD_SIZE] = "";
  char* toks = NULL;
  Id id = NO_ID;
  Id location =NO_ID;
  Player *player=NULL;
  int hp = 0, ad = 0, def = 0, speed = 0;
  STATUS status=OK;
  
  if (!filename)
    return ERROR;
  
  file = fopen(filename, "r");
  if (file == NULL)
    return ERROR;
  
  while (fgets(line, WORD_SIZE, file)) {
    if (strncmp("#p:", line, 3) == 0) {
      toks = strtok(line + 3, "|");
      id = atol(toks);
      toks = strtok(NULL, "|");
      strcpy(name, toks);
      toks = strtok(NULL, "|");
      location = atol(toks);
      toks = strtok(NULL, "|");
      hp = atol(toks);
      toks = strtok(NULL, "|");
      ad = atol(toks);
      toks = strtok(NULL, "|");
      def = atol(toks);
      toks = strtok(NULL, "|");
      speed = atol(toks);
      
#ifdef DEBUG 
      printf("Leido: %ld|%s|%ld|%d|%d|%d|%d\n", id, name, location, hp, ad, def, speed);
#endif
      player = player_create(id);
      if (player != NULL) {
	        player_set_name(player,name);
	        player_set_space(player,location);
	        player_set_hp(player, hp);
	        player_set_ad(player, ad);
	        player_set_def(player, def);
	        player_set_speed(player, speed);
      }
      game_add_player(game, player);
      game_set_player_location(game, location);
    }
  }
  
  if (ferror(file)) {
    status = ERROR;
  }
  fclose(file);
  return status;
}

STATUS game_reader_load_enemies(Game* game, char* filename){
  FILE* file = NULL;
  char line[WORD_SIZE] = "";
  char name[WORD_SIZE] = "";
  char* toks = NULL;
  Id id = NO_ID;
  Id location =NO_ID;
  Enemy *enemy=NULL;
  int hp = 0, ad = 0, def = 0, speed = 0;
  STATUS status=OK;
  
  if (!filename)
    return ERROR;
  
  file = fopen(filename, "r");
  if (file == NULL)
    return ERROR;
  
  while (fgets(line, WORD_SIZE, file)) {
    if (strncmp("#e:", line, 3) == 0) {
      toks = strtok(line + 3, "|");
      id = atol(toks);
      toks = strtok(NULL, "|");
      strcpy(name, toks);
      toks = strtok(NULL, "|");
      location = atol(toks);
      toks = strtok(NULL, "|");
      hp = atol(toks);
      toks = strtok(NULL, "|");
      ad = atol(toks);
      toks = strtok(NULL, "|");
      def = atol(toks);
      toks = strtok(NULL, "|");
      speed = atol(toks);
      
#ifdef DEBUG 
      printf("Leido: %ld|%s|%ld|%d|%d|%d|%d\n", id, name, location, hp, ad, def, speed);
#endif
      enemy = enemy_create(id);
      if (enemy != NULL) {
	        enemy_set_name(enemy,name);
	        enemy_set_space(enemy,location);
	        enemy_set_hp(enemy, hp);
	        enemy_set_ad(enemy, ad);
	        enemy_set_def(enemy, def);
	        enemy_set_speed(enemy, speed);
      }
      game_add_enemy(game, enemy);
      game_set_enemy_location(game, location, id);
    }
  }
  
  if (ferror(file)) {
    status = ERROR;
  }
  fclose(file);
  return status;
}

STATUS game_management_save(Game * game, char * game_saved){
  FILE * file = NULL;
  Space *space=NULL;
  Set *set= NULL;
  Set *aux=NULL;
  Object * object = NULL;
  Player * player=NULL;
  Link * link = NULL;
  Enemy * enemy = NULL;
  int i;
  int j;
  int check=0;
  if (!game || !game_saved)
    return ERROR;
  
  file = fopen(game_saved, "w");
  set=game_get_objects_location(game);
  for(i=0;i<MAX_SPACES && game_get_space(game,game_get_space_id_at(game,i))!=NULL;i++){
    space=game_get_space(game,game_get_space_id_at(game,i));
    fprintf(file,"#s:%ld%%%s%%%ld%%%ld%%%ld%%%ld%%%ld%%%ld%%%s%%%s%%%s%%%s%%%s%%%s%%%s%%%s%%%s%%%s%%%s%%%s%%%s%%%s%%%s%%%d%%\n",(long)space_get_id(space),space_get_name(space),(long)space_get_north(space),(long)space_get_east(space),(long)space_get_south(space),(long)space_get_west(space),(long)space_get_up(space),(long)space_get_down(space),(char*)space_get_global_gdesc(space,0),(char*)space_get_global_gdesc(space,1),(char*)space_get_global_gdesc(space,2),(char*)space_get_global_gdesc(space,3),(char*)space_get_global_gdesc(space,4),(char*)space_get_global_gdesc(space,5),(char*)space_get_global_gdesc(space,6),(char*)space_get_global_gdesc(space,7),(char*)space_get_global_gdesc(space,8),(char*)space_get_global_gdesc(space,9),(char*)space_get_global_gdesc(space,10),(char*)space_get_global_gdesc(space,11),(char*)space_get_global_gdesc(space,12),(char*)space_get_description(space),(char*)space_get_iDesc(space),(int)space_get_ilumination(space));
  }
  
  for(i=0; i<MAX_OBJECTS && game_getObject(game, game_get_object_id_at(game,i)) != NULL; i++){
    object = game_getObject(game, game_get_object_id_at(game,i));
    for(j=0;j<set_getNumId(set);j++){
      space=game_get_space(game,set_get_id_at(set,j));
      aux=space_get_objects(space);
      if(set_checkId(aux,object_get_id(object))==OK){
        fprintf(file,"#o:%d|%s|%d|%s|%d|%d|%d|%d|%d|%d|%s|%d|%d|%d|%d|\n",(int)object_get_id(object), object_get_name(object), (int)space_get_id(space) ,object_get_description(object), object_get_movible(object), object_get_movido(object), object_get_oculto(object), (int)object_get_abre(object), object_get_ilumina(object), object_get_encendido(object),object_get_description2(object),object_get_hp(object),object_get_ad(object),object_get_def(object),object_get_speed(object)); 
        check++;
      }
    }
    if(check==0){
      fprintf(file,"#o:%d|%s|-1|%s|%d|%d|%d|%d|%d|%d|%s|%d|%d|%d|%d|\n", (int)object_get_id(object), object_get_name(object), object_get_description(object), object_get_movible(object), object_get_movido(object), object_get_oculto(object), (int)object_get_abre(object), object_get_ilumina(object), object_get_encendido(object),object_get_description2(object),object_get_hp(object),object_get_ad(object),object_get_def(object),object_get_speed(object));
    }
  }
  
  if( game_get_player(game) != NULL){
    player = game_get_player(game);
    fprintf(file,"#p:%d|%s|%d|%d|%d|%d|%d|\n", (int)player_get_id(player), player_get_name(player), (int)player_get_space(player), player_get_hp(player), player_get_ad(player), player_get_def(player), player_get_speed(player));
  }
  
  for (i=0; i<MAX_LINKS && game_get_link_id_at(game, i)!=NO_ID; i++){
    link = game_get_link(game, game_get_link_id_at(game, i));
    fprintf(file, "#l:%d|%s|%d|%d|%d|\n",(int)link_get_id(link), link_get_name(link), (int)link_get_tied1(link), (int)link_get_tied2(link), link_get_status(link));
  }
  
  for (i=0; i<MAX_ENEMY && game_get_enemy_at(game, i) != NO_ID ;i++){
    enemy = game_get_enemy(game, game_get_enemy_at(game,i));
    fprintf(file,"#e:%d|%s|%d|%d|%d|%d|%d|\n", (int)enemy_get_id(enemy), enemy_get_name(enemy), (int)enemy_get_space(enemy), enemy_get_hp(enemy), enemy_get_ad(enemy), enemy_get_def(enemy), enemy_get_speed(enemy));
  }
  fclose(file);
  return OK;
}

Game * game_management_load(Game *game, char *file){
  Player *player= NULL;
  int i=0;
  int j=0;
  Set *set=NULL;
  Set *aux=NULL;
  Object * object = NULL;
  Space *space=NULL;
  int check=0;
  if(!game || !file)
    return NULL;
  game_destroy(game);
  game=game_create();
  game_reader_load_spaces(game,file);
  game_reader_load_objects(game,file);
  game_reader_load_players(game,file);
  game_reader_load_links(game,file);
  game_reader_load_enemies(game, file);
  player=game_get_player(game);
  set=game_get_objects_location(game);
  for(i=0;i<MAX_OBJECTS && game_get_object_id_at(game,i)!=NO_ID;check=0,i++){
    object=game_getObject(game,game_get_object_id_at(game,i));
    for(j=0;j<set_getNumId(set);j++){
      space=game_get_space(game,set_get_id_at(set,j));
      aux=space_get_objects(space);
      if(set_checkId(aux,object_get_id(object))==OK){
        check++;
      }
    }
    if(check==0){
      player_add_object(player,object_get_id(object));
    }
  }
  
  return game;
}