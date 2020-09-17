/**
 * @brief It implements the game interface and all the associated callbacks
 * for each command
 *
 * @file game.c
 * @author Profesores PPROG, Guillermo Hoyo, Tomas Higuera & Alvaro Lopez
 * @version 2.0
 * @date 12-03-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include "game.h"
#include "game_management.h"
#include "link.h"
#include "command.h"
#include "dialogue.h"
#include "enemy.h"
#include "battle.h"
#include "menu.h"
#include "game_rules.h"
#include "music.h"

#define N_CALLBACK 12/*!< Number of commands */

/** The structure of Game will save information of the different characteristics of the game **/
struct _Game{
  Object * objects[MAX_OBJECTS + 1]; /**Array of the type Object* that contains the different information from the objects of the game*/
  Player * player; /**Type Player* that contains the information of the player of the game */
  Space* spaces[MAX_SPACES + 1]; /**Array of the type Space* that contains the different information from the spacess of the game*/
  Die * die; /**Type Die* that contains the information of the die of the game */
  T_Command *last_cmd; /**Type T_Command that contain the last comand used*/
  Link *links[MAX_LINKS + 1];/**Links of the game between spaces*/
  Dialogue *dialogue; /**Type dialogue that contains the phrase that the game will send*/
  Enemy * enemy[MAX_ENEMY + 1];/** Type enemy that contains the info about the enemy*/
  BOOL rule;/**Type bool that say if game_rules is ativated */
  BOOL save;/**Type bool that say if save is automatic is activated */
  BOOL music;/**Type bool that say if music is activated */
};

/**
   Define the function type for the callbacks
*/
typedef STATUS (*callback_fn)(Game* game);

/**
   List of callbacks for each command in the game
*/
STATUS game_callback_unknown(Game* game);
STATUS game_callback_quit(Game* game);
STATUS game_callback_pick(Game* game);
STATUS game_callback_drop(Game* game);
STATUS game_callback_throw(Game* game);
STATUS game_callback_inspect(Game* game);
STATUS game_callback_go(Game* game);
STATUS game_callback_turnon(Game* game);
STATUS game_callback_turnoff(Game* game);
STATUS game_callback_open(Game* game);
STATUS game_callback_save(Game * game);
STATUS game_callback_load(Game * game);

/*static callback_fn game_callback_fn_list[N_CALLBACK]={
  game_callback_unknown,
  game_callback_quit,
  game_callback_pick,
  game_callback_drop,
  game_callback_throw,
  game_callback_inspect,
  game_callback_go,
  game_callback_turnon,
  game_callback_turnoff,
  game_callback_open,
  game_callback_save,
  game_callback_load
};*/


/**
   Game interface implementation
 */

Game * game_create() {
  int i;
  Game *game;
  game=(Game*)malloc(sizeof(Game));
  if(!game)
    return NULL;
  for (i = 0; i < MAX_SPACES; i++) {
    game->spaces[i] = NULL;
  }
  game->player=NULL;
  game->last_cmd = command_ini();
  for(i=0;i<MAX_OBJECTS;i++){
    game->objects[i]=NULL;
  }
  for(i=0;i<MAX_LINKS;i++){
    game->links[i]=NULL;
  }
  game->die=die_create(0);
  game->dialogue=dialogue_create();
  for(i=0;i<MAX_ENEMY;i++){
    game->enemy[i]=NULL;
  }
  game->rule=FALSE;
  game->save=FALSE;
  game->music=FALSE;
  return game;
}

STATUS game_destroy(Game* game) {
    int i = 0;

    for (i = 0; (i < MAX_SPACES) && (game->spaces[i] != NULL); i++) {
      space_destroy(game->spaces[i]);
    }
    for (i=0; (i<MAX_OBJECTS) && (game->objects[i]!=NULL); i++){
      object_destroy(game->objects[i]);
    }
    for(i=0;i<MAX_LINKS && game->links[i]!=NULL;i++){
      link_destroy(game->links[i]);
    }
    player_destroy(game->player);
    die_destroy(game->die);
    command_destroy(game->last_cmd);
    dialogue_destroy(game->dialogue);
    for(i=0;i<MAX_ENEMY && (game->enemy[i]!=NULL);i++){
      enemy_destroy(game->enemy[i]);
    }
    free(game);
    return OK;
}


Die* game_get_die(Game* game){
    if (!game)
      return ERROR;

    return game->die;
}
Dialogue* game_get_dialogue(Game *game){
  if(!game)
    return ERROR;
  return game->dialogue;
}
Space* game_get_space(Game* game, Id id){
    int i = 0;

    if (id == NO_ID) {
      return NULL;
    }

    for (i = 0; i < MAX_SPACES && game->spaces[i] != NULL; i++) {
      if (id == space_get_id(game->spaces[i])){
      	return game->spaces[i];
      }
    }

    return NULL;
}

Id game_get_player_location(Game* game) {
    if(!game)
      return NO_ID;
    return player_get_space(game->player);
}

Set * game_get_enemy_locations(Game* game){
  int i = 0;
  Set * set = NULL;
  if(!game)
    return NULL;
    
  set=set_create();
  
  for (i = 0; i<MAX_ENEMY && game->enemy[i] != NULL; i++)
    set_add(set, enemy_get_space(game->enemy[i]));
    
  return set;
}

Set* game_get_objects_location(Game* game) {
    Set * set=set_create();
    int i=0;
    if(!game)
      return NULL;
    for(i=0;i<MAX_SPACES;i++){
      if(set_getNumId(space_get_objects(game->spaces[i]))>0){
        set_add(set,space_get_id(game->spaces[i]));
      }
    }
    return set;
}

STATUS game_update(Game* game, T_Command* cmd, int code) {
  STATUS status = ERROR;
  int input;
  Id space_id;
  char complement1[55],complement2[55];
  game->last_cmd = cmd;
  input=command_get_input(cmd);
  strcpy(complement1,command_get_complement1(cmd));
  strcpy(complement2,command_get_complement2(cmd));
    switch (input) {
        case UNKNOWN:
            status = game_callback_unknown(game);
            dialogue_unknown(game->dialogue, status, code);
            break;
        case QUIT:
            if (strcmp(complement1, "")==0) {
                status = game_callback_quit(game);
                dialogue_quit(game->dialogue, status, code);
            }
            break;
        case GO:
            status = game_callback_go(game);
            dialogue_go(game->dialogue, status, code, complement1);
            break;
        case PICK:
            status = game_callback_pick(game);
            dialogue_pick(game->dialogue, status, code, complement1);
            break;
        case DROP:
            status = game_callback_drop(game);
            dialogue_drop(game->dialogue, status, code, complement1);
            break;
        case INSPECT:
            status = game_callback_inspect(game);
            dialogue_inspect(game->dialogue, status, code, complement1);
            break;
        case TURNON:
            status =game_callback_turnon(game);
            dialogue_turnon(game->dialogue, status, code, complement1);
            break;
        case TURNOFF:
            status = game_callback_turnoff(game);
            dialogue_turnoff(game->dialogue, status, code, complement1);
            break;
        case OPEN:
            status =game_callback_open(game);
            dialogue_open(game->dialogue, status, code, complement1, complement2);
            break;
        case SAVE:
            if (strcmp(complement1, "")!=0) {
                status = game_callback_save(game);
                dialogue_save(game->dialogue, status, code);
            }
            break;
        case LOAD:
            if (strcmp(complement1, "")!=0) {
                status = game_callback_load(game);
                dialogue_load(game->dialogue, status, code);
            }
            break;
        case NO_CMD:
            break;
    }
    if(game->rule==TRUE){
      if(status==OK)
        gameRules_action(game->die,game);
    }
    if(game->save==TRUE){
      space_id=game_get_player_location(game);
      if(space_id==10 ||space_id==20 ||space_id==30 ||space_id==40){
        game_management_save(game,"autosave.txt");
      }
    }

  return status;
}

T_Command* game_get_last_command(Game* game){
  return game->last_cmd;
}

void game_print_data(Game* game) {
  Set * set=NULL;
  int i = 0;

  printf("\n\n-------------\n\n");

  printf("=> Spaces: \n");
  for (i = 0; i < MAX_SPACES && game->spaces[i] != NULL; i++) {
    space_print(game->spaces[i]);
  }
  set=game_get_objects_location(game);
  printf("=> Object location:");
  set_print(set);
  printf("\n=> Player location: %d\n", (int) game_get_player_location(game));
  printf("prompt:> ");
  set_destroy(set);
}

BOOL game_is_over(Game* game) {
  Player *player=NULL;
  player=game_get_player(game);
  if(player_get_space(player)==-2){
    return TRUE;
  }
  return FALSE;
}

BOOL game_finished(Game *game){
  if(game_get_player_location(game)==34){
    return TRUE;
  }
  return FALSE;
}

/**
   Callbacks implementation for each action
*/

STATUS game_callback_unknown(Game* game) {
  int code=0;
  dialogue_unknown(game->dialogue, OK,code);
  return OK;
}

STATUS game_callback_quit(Game* game) {
  int code=0;
  dialogue_quit(game->dialogue, OK,code);
  return OK;
}

STATUS game_callback_go(Game *game){
  char move[55];
  Id space_id = NO_ID;
  Link *linkAux=NULL;
  Space *spaceAux=NULL;
  int i, code=0;
  Set *set=NULL;
  
  strcpy(move,command_get_complement1(game_get_last_command(game)));
  
  if(!game){
    dialogue_go(game->dialogue, ERROR,code,move);
    return ERROR;
  }
  space_id = game_get_player_location(game);
  if(!space_id){
    dialogue_go(game->dialogue, ERROR,code, move);
    return ERROR;
  }
  spaceAux=game_get_space(game,space_id);
  strcpy(move,command_get_complement1(game_get_last_command(game)));
  if(strcmp(move,"north")==0 || strcmp(move,"n")==0){
    linkAux=game_get_link(game,space_get_north(spaceAux));
    if(linkAux != NULL){
      if(link_get_status(linkAux)==0){
          if(game_set_player_location(game,link_get_tied2(linkAux))==OK){
            for (i=0; i<MAX_ENEMY && game->enemy[i] != NULL;i++){
              if (player_get_space(game_get_player(game)) == enemy_get_space(game->enemy[i])){
                while ((player_get_space(game_get_player(game)) != -2) && (enemy_get_space(game->enemy[i])) != -2 && (enemy_get_space(game->enemy[i]) != -1)){
                  menu_battle(game, enemy_get_id(game->enemy[i]));
                  battle_on(game);
                }
                space_id = game_get_player_location(game);
                if(enemy_get_space(game->enemy[i])==-2){
                  set=space_get_objects(game_get_space(game,space_id));
                  for(i=0;i<set_getNumId(set);i++){
                    if(game_getObject(game,set_get_id_at(set,i))!=NULL){
                      if(object_get_oculto(game_getObject(game,set_get_id_at(set,i)))==1){
                        object_set_oculto(game_getObject(game,set_get_id_at(set,i)),0);
                      }
                    }
                  }
                }
                system("clear");
                code=-1;
                dialogue_go(game->dialogue, OK,code, move);
                return OK;
              }
            }
            code=-2;
            dialogue_go(game->dialogue, OK,code, move);
            return OK;
          }
      }
    }
  }
  else if(strcmp(move,"east")==0 || strcmp(move,"e")==0){
    linkAux=game_get_link(game,space_get_east(spaceAux));
    if(linkAux != NULL){
      if(link_get_status(linkAux)==0){
          if(game_set_player_location(game,link_get_tied2(linkAux))==OK){
            for (i=0; i<MAX_ENEMY && game->enemy[i] != NULL;i++){
              if (player_get_space(game_get_player(game)) == enemy_get_space(game->enemy[i])){
                while ((player_get_space(game_get_player(game)) != -2) && (enemy_get_space(game->enemy[i])) != -2 && (enemy_get_space(game->enemy[i]) != -1)){
                  menu_battle(game, enemy_get_id(game->enemy[i]));
                  battle_on(game);
                }
                space_id = game_get_player_location(game);
                if(enemy_get_space(game->enemy[i])==-2){
                  set=space_get_objects(game_get_space(game,space_id));
                  for(i=0;i<set_getNumId(set);i++){
                    if(game_getObject(game,set_get_id_at(set,i))!=NULL){
                      if(object_get_oculto(game_getObject(game,set_get_id_at(set,i)))==1){
                        object_set_oculto(game_getObject(game,set_get_id_at(set,i)),0);
                      }
                    }
                  }
                }
                system("clear");
                code=-7;
                dialogue_go(game->dialogue, OK,code, move);
                return OK;
              }
            }
            code=-8;
            dialogue_go(game->dialogue, OK,code, move);
            return OK;
          }
      }
    }
  }
  else if(strcmp(move,"west")==0 || strcmp(move,"w")==0){
    linkAux=game_get_link(game,space_get_west(spaceAux));
    if(linkAux != NULL){
      if(link_get_status(linkAux)==0){
          if(game_set_player_location(game,link_get_tied2(linkAux))==OK){
            for (i=0; i<MAX_ENEMY && game->enemy[i] != NULL;i++){
              if (player_get_space(game_get_player(game)) == enemy_get_space(game->enemy[i])){
                while ((player_get_space(game_get_player(game)) != -2) && (enemy_get_space(game->enemy[i])) != -2 && (enemy_get_space(game->enemy[i]) != -1)){
                  menu_battle(game, enemy_get_id(game->enemy[i]));
                  battle_on(game);
                }
                space_id = game_get_player_location(game);
                if(enemy_get_space(game->enemy[i])==-2){
                  set=space_get_objects(game_get_space(game,space_id));
                  for(i=0;i<set_getNumId(set);i++){
                    if(game_getObject(game,set_get_id_at(set,i))!=NULL){
                      if(object_get_oculto(game_getObject(game,set_get_id_at(set,i)))==1){
                        object_set_oculto(game_getObject(game,set_get_id_at(set,i)),0);
                      }
                    }
                  }
                }
                system("clear");
                code=-5;
                dialogue_go(game->dialogue, OK,code, move);
                return OK;
              }
            }
            code=-6;
            dialogue_go(game->dialogue, OK,code, move);
            return OK;
          }
      }
    }
  }
  else if(strcmp(move,"south")==0 || strcmp(move,"s")==0){
    linkAux=game_get_link(game,space_get_south(spaceAux));
    if(linkAux != NULL){
      if(link_get_status(linkAux)==0){
          if(game_set_player_location(game,link_get_tied2(linkAux))==OK){;
            for (i=0; i<MAX_ENEMY && game->enemy[i] != NULL;i++){
              if (player_get_space(game_get_player(game)) == enemy_get_space(game->enemy[i])){
                while ((player_get_space(game_get_player(game)) != -2) && (enemy_get_space(game->enemy[i])) != -2 && (enemy_get_space(game->enemy[i]) != -1)){
                  menu_battle(game, enemy_get_id(game->enemy[i]));
                  battle_on(game);
                }
                space_id = game_get_player_location(game);
                if(enemy_get_space(game->enemy[i])==-2){
                  set=space_get_objects(game_get_space(game,space_id));
                  for(i=0;i<set_getNumId(set);i++){
                    if(game_getObject(game,set_get_id_at(set,i))!=NULL){
                      if(object_get_oculto(game_getObject(game,set_get_id_at(set,i)))==1){
                        object_set_oculto(game_getObject(game,set_get_id_at(set,i)),0);
                      }
                    }
                  }
                }
                system("clear");
                return OK;
                code=-3;
                dialogue_go(game->dialogue, OK,code, move);
              }
            }
            code=-4;
            dialogue_go(game->dialogue, OK,code, move);
            return OK;
          }
      }
    }
  }
    
  else if(strcmp(move,"up")==0 || strcmp(move,"u")==0){
    linkAux=game_get_link(game,space_get_up(spaceAux));
    if(linkAux != NULL){
      if(link_get_status(linkAux)==0){
          if(game_set_player_location(game,link_get_tied2(linkAux))==OK){
            for (i=0; i<MAX_ENEMY && game->enemy[i] != NULL;i++){
              if (player_get_space(game_get_player(game)) == enemy_get_space(game->enemy[i])){
                while ((player_get_space(game_get_player(game)) != -2) && (enemy_get_space(game->enemy[i])) != -2 && (enemy_get_space(game->enemy[i]) != -1)){
                  menu_battle(game, enemy_get_id(game->enemy[i]));
                  battle_on(game);
                }
                space_id = game_get_player_location(game);
                if(enemy_get_space(game->enemy[i])==-2){
                  set=space_get_objects(game_get_space(game,space_id));
                  for(i=0;i<set_getNumId(set);i++){
                    if(game_getObject(game,set_get_id_at(set,i))!=NULL){
                      if(object_get_oculto(game_getObject(game,set_get_id_at(set,i)))==1){
                        object_set_oculto(game_getObject(game,set_get_id_at(set,i)),0);
                      }
                    }
                  }
                }
                system("clear");
                code=-11;
                dialogue_go(game->dialogue, OK,code, move);
                return OK;
              }
            }
            code=-12;
            dialogue_go(game->dialogue, OK,code, move);
            return OK;
          }
      }
    }  
  }
  else if(strcmp(move,"down")==0 || strcmp(move,"d")==0){
    linkAux=game_get_link(game,space_get_down(spaceAux));
    if(linkAux != NULL){
      if(link_get_status(linkAux)==0){
          if(game_set_player_location(game,link_get_tied2(linkAux))==OK){
            for (i=0; i<MAX_ENEMY && game->enemy[i] != NULL;i++){
              if (player_get_space(game_get_player(game)) == enemy_get_space(game->enemy[i])){
                while ((player_get_space(game_get_player(game)) != -2) && (enemy_get_space(game->enemy[i])) != -2 && (enemy_get_space(game->enemy[i]) != -1)){
                  menu_battle(game, enemy_get_id(game->enemy[i]));
                  battle_on(game);
                }
                space_id = game_get_player_location(game);
                if(enemy_get_space(game->enemy[i])==-2){
                  set=space_get_objects(game_get_space(game,space_id));
                  for(i=0;i<set_getNumId(set);i++){
                    if(game_getObject(game,set_get_id_at(set,i))!=NULL){
                      if(object_get_oculto(game_getObject(game,set_get_id_at(set,i)))==1){
                        object_set_oculto(game_getObject(game,set_get_id_at(set,i)),0);
                      }
                    }
                  }
                }
                system("clear");
                code=-9;
                dialogue_go(game->dialogue, OK,code, move);
                return OK;
              }
            }
            code=-10;
            dialogue_go(game->dialogue, OK,code, move);
            return OK;
          }
      }
    }  
  }
  return ERROR;
}

STATUS game_callback_pick(Game* game){
  Id current_id=NO_ID;
  char objectname[50];
  Id idObject=NO_ID;
  Set * set;
  Object *object=NULL;
  int i, code=0;
  Player * player = NULL;

  if (!game){
    dialogue_pick(game->dialogue, ERROR,code, objectname);
    return ERROR;
  }
  current_id=game_get_player_location(game);

  set = space_get_objects(game_get_space(game,current_id));
  if(space_get_ilumination(game_get_space(game,current_id))==TRUE){
    if(set_getNumId(set) == 0){
      command_set_description(game->last_cmd,"\0");
      dialogue_pick(game->dialogue, ERROR,code, objectname);
      return ERROR;
    }
    if(space_get_objects(game_get_space(game,current_id)) == NULL || inventory_get_numids(player_get_inventory(game_get_player(game))) >= inventory_get_max_objects(player_get_inventory(game_get_player(game)))){
      command_set_description(game->last_cmd,"\0");
      dialogue_pick(game->dialogue, ERROR,code, objectname);
      return ERROR;
    }
    strcpy(objectname, command_get_complement1(game_get_last_command(game)));
    for(i=0;i<set_getNumId(set);i++){
      if(strcmp(objectname, object_get_name(game_getObject(game,set_get_id_at(set,i))))==0)
        idObject=set_get_id_at(set,i);
    }
    if(space_haveObject(game_get_space(game,current_id),idObject)==OK){
      if(player_add_object(game->player, idObject)==ERROR){
        command_set_description(game->last_cmd,"\0");
        dialogue_pick(game->dialogue, ERROR,code, objectname);
        return ERROR;
      }
      if(space_deleteObject(game_get_space(game,current_id), idObject)==ERROR){
        command_set_description(game->last_cmd,"\0");
        dialogue_pick(game->dialogue, ERROR,code, objectname);
        return ERROR;
      }
    }
    else{
      command_set_description(game->last_cmd,"\0");
      dialogue_pick(game->dialogue, ERROR,code, objectname);
      return ERROR;
    }
    object=game_getObject(game,idObject);
    object_set_movido(object,1);
    if(object_get_hp(object) != 0 ||object_get_ad(object) != 0|| object_get_def(object) != 0|| object_get_speed(object) != 0){
      player = game_get_player(game);
      player_add_hp(player, object_get_hp(object));
      player_add_ad(player, object_get_ad(object));
      player_add_def(player, object_get_def(object));
      player_add_speed(player, object_get_speed(object));
    }
    code=-1;
    dialogue_pick(game->dialogue, OK,code, objectname);
    return OK;
  }
  else{
    dialogue_pick(game->dialogue, ERROR,code, objectname);
    return ERROR;
  }
}

STATUS game_callback_drop(Game* game){
  Id current_id=NO_ID;
  char  objectname[OBJECT_NAME];
  int i, code=0;
  Id id;
  char description[70];
  Player * player = NULL;
  Object * object = NULL;

  if(!game)
    return ERROR;
  current_id=game_get_player_location(game);
  if(space_get_ilumination(game_get_space(game,current_id))==TRUE){
    if(inventory_get_numids(player_get_inventory(game->player))== 0){
      command_set_description(game->last_cmd,"\0");
      dialogue_drop(game->dialogue, ERROR,code, objectname);
      return ERROR;
    }
    else{
        strcpy(objectname, command_get_complement1(game_get_last_command(game)));
        for(i=0; i<set_getNumId(inventory_get_ids(player_get_inventory(game_get_player(game)))); i++){
          if(strcmp(objectname, object_get_name(game_getObject(game,set_get_id_at((inventory_get_ids(player_get_inventory(game->player))), i)))) == 0){
            id = set_get_id_at((inventory_get_ids(player_get_inventory(game->player))),i);
            game_set_object_location(game, current_id, id);
              if (game_set_object_location(game, current_id, id) == OK){
                set_delete((inventory_get_ids(player_get_inventory(game->player))), id);
                object = game_getObject(game, id);
                 if(object_get_hp(object) != 0 ||object_get_ad(object) != 0|| object_get_def(object) != 0|| object_get_speed(object) != 0){
                  player = game_get_player(game);
                  player_subtract_hp(player, object_get_hp(object));
                  player_subtract_ad(player, object_get_ad(object));
                  player_subtract_def(player, object_get_def(object));
                  player_subtract_speed(player, object_get_speed(object));
                }
                command_set_description(game->last_cmd,description);
                code=-1;
                dialogue_drop(game->dialogue, OK,code, objectname);
                return OK;
              }
          }
        }
    }
    command_set_description(game->last_cmd,"\0");
    dialogue_drop(game->dialogue, ERROR,code, objectname);
    return ERROR;
  }
  else{
    command_set_description(game->last_cmd,"The room is dark, you cant drop any object");
    dialogue_drop(game->dialogue, ERROR,code, objectname);
    return ERROR;
  }
}

STATUS game_callback_throw(Game* game){
  if(!game)
    return ERROR;
  die_roll(game->die);
  return OK;
}

STATUS game_callback_inspect(Game* game){
  Id space_id = NO_ID;
  int i, code=0;
  Space * space = NULL;
  Set * set = NULL;
  Id object_id;
  char object_name[OBJECT_NAME+1];
  char object_description[OBJECT_DESC +50];
  char space_description[OBJECT_DESC +50];
  char description[70];

  space_id = game_get_player_location(game);

  space = game_get_space(game, space_id);

  set = space_get_objects(space);

  strcpy (object_name, command_get_complement1(game_get_last_command(game)));

  if ((strcmp("s",object_name)==0) || (strcmp("space",object_name)==0)){
    if(space_get_ilumination(space)==TRUE){
      strcpy(space_description,"Inspect: ");
      strcat(space_description, space_get_iDesc(space));
      command_set_description(game->last_cmd,space_description);
      code=-1;
      dialogue_inspect(game->dialogue, OK,code, object_name);
      return OK;
    }
    else{
      command_set_description(game->last_cmd,"You cant inspect a dark room");
      dialogue_inspect(game->dialogue, ERROR,code, object_name);
      return ERROR;
    }
  }
  
  else {
    if(space_get_ilumination(space)==TRUE){
      for(i=0; i< set_getNumId(set); i++){
        if(game_getObject(game,set_get_id_at(set,i))!=NULL){
          if (strcmp(object_name, object_get_name(game_getObject(game, set_get_id_at(set, i))))==0){
            object_id = set_get_id_at(set, i);
            if(object_get_movido(game_getObject(game,object_id))==FALSE){
              strcpy(object_description, object_get_description(game_getObject(game, object_id)));
              sprintf(description,"Inspect: %s",object_description);
              command_set_description(game->last_cmd,description);
              code=-1;
              dialogue_inspect(game->dialogue, OK,code, object_name);
              return OK;
            }
            else{
              strcpy(object_description, object_get_description2(game_getObject(game, object_id)));
              sprintf(description,"Inspect: %s",object_description);
              command_set_description(game->last_cmd,description);
              code=-1;
              dialogue_inspect(game->dialogue, OK,code, object_name);
              return OK;
            }
          }
        }
      }
      for(i=0;i<set_getNumId(inventory_get_ids(player_get_inventory(game_get_player(game))));i++){
        if(game_getObject(game, set_get_id_at(inventory_get_ids(player_get_inventory(game_get_player(game))), i))){
          if (strcmp(object_name, object_get_name(game_getObject(game, set_get_id_at(inventory_get_ids(player_get_inventory(game_get_player(game))), i))))==0){
            object_id = set_get_id_at(inventory_get_ids(player_get_inventory(game_get_player(game))), i);
            if(object_get_movido(game_getObject(game,object_id))==FALSE){
              strcpy(object_description, object_get_description(game_getObject(game, object_id)));
              sprintf(description,"Inspect: %s",object_description);
              command_set_description(game->last_cmd,description);
              code=-1;
              dialogue_inspect(game->dialogue, OK,code, object_name);
              return OK;
            }
            else{
              strcpy(object_description, object_get_description2(game_getObject(game, object_id)));
              sprintf(description,"Inspect: %s",object_description);
              command_set_description(game->last_cmd,description);
              code=-1;
              dialogue_inspect(game->dialogue, OK,code, object_name);
              return OK;
            }
          }
        }
      }
      command_set_description(game->last_cmd,"There is no object with that name");
      dialogue_inspect(game->dialogue, ERROR,code, object_name);
      return ERROR;
    }
    else{
      command_set_description(game->last_cmd,"You cant inspect a dark room");
      dialogue_inspect(game->dialogue, ERROR,code, object_name);
      return ERROR;
    }
  }
  command_set_description(game->last_cmd,"\0");
  dialogue_inspect(game->dialogue, ERROR,code, object_name);
  return ERROR;
}

STATUS game_callback_save(Game * game){
  char game_name[30] = "\0";
  int code=0;
  
  strcpy (game_name, command_get_complement1(game_get_last_command(game)));
  if (!game){
    dialogue_save(game->dialogue, ERROR,code);
    return ERROR;
  }
  game_management_save(game, game_name);
  command_set_description(game_get_last_command(game),"Guardando partida...");
  code=-1;
  dialogue_save(game->dialogue, OK,code);
  return OK;
}

STATUS game_callback_load(Game * game){
  char game_name[30] = "\0";
  int code=0;
  
  strcpy (game_name, command_get_complement1(game_get_last_command(game)));
  if (!game){
    dialogue_load(game->dialogue, ERROR,code);
    return ERROR;
  }  
  strcpy (game_name, command_get_complement1(game_get_last_command(game)));
  game_management_load(game, game_name);
  command_set_description(game_get_last_command(game),"Cargando partida...");
  code=-1;
  dialogue_load(game->dialogue, OK,code);
  return OK;
}


STATUS game_callback_turnon(Game* game){
  int i, code=0;
  char object_name[OBJECT_NAME+1];
  Id object_id=NO_ID;
  Space* space=NULL;
  
  space=game_get_space(game,game_get_player_location(game));
  strcpy (object_name, command_get_complement1(game_get_last_command(game)));
  
  if(space_get_ilumination(space)==FALSE){
    for(i=0;i<set_getNumId(inventory_get_ids(player_get_inventory(game_get_player(game))));i++){
      if(game_getObject(game, set_get_id_at(inventory_get_ids(player_get_inventory(game_get_player(game))), i))){
        if (strcmp(object_name, object_get_name(game_getObject(game, set_get_id_at(inventory_get_ids(player_get_inventory(game_get_player(game))), i))))==0){
          object_id = set_get_id_at(inventory_get_ids(player_get_inventory(game_get_player(game))), i);
          object_set_encendido(game_getObject(game,object_id),TRUE);
          if(object_get_ilumina(game_getObject(game,object_id))==TRUE){
            space_set_ilumination(space,TRUE);
            command_set_description(game->last_cmd,"Turning on the lights of the room...");
            code=-1;
            dialogue_turnon(game->dialogue, OK,code, object_name);
            return OK;
          }
          else{
            command_set_description(game->last_cmd,"You turned on an object that cant light up the room...");
            dialogue_turnon(game->dialogue, ERROR,code, object_name);
            return ERROR;
          }
        }
        else{
          command_set_description(game->last_cmd,"You dont have that object");
          dialogue_turnon(game->dialogue, ERROR,code, object_name);
        }
      }
    }
  }
  else{
    command_set_description(game->last_cmd,"The room is already iluminated, you dont need to turn on an object");
    dialogue_turnon(game->dialogue, ERROR,code, object_name);
    return ERROR;
  }
  dialogue_turnon(game->dialogue, ERROR,code, object_name);
  return ERROR;
}

STATUS game_callback_turnoff(Game* game){
  Player *player=NULL;
  Set *set=NULL;
  int i, code=0;
  char object_name[100]="";
  char description[100]="";
  
  strcpy (object_name, command_get_complement1(game_get_last_command(game)));
  if(!game){
    dialogue_turnoff(game->dialogue, ERROR,code, object_name);
    return ERROR;
  }
  player=game_get_player(game);
  player_get_inventory(player);
  for(i=0;i<set_getNumId(set);i++){
    if(strcmp(object_get_name(game_getObject(game,set_get_id_at(set,i))),object_name)==0){
      object_set_encendido(game_getObject(game,set_get_id_at(set,i)),TRUE);
      command_set_description(game->last_cmd,description);
      code= -1;
      dialogue_turnoff(game->dialogue, OK,code, object_name);
      return OK;
    }
  }
  command_set_description(game->last_cmd,"You dont have that object");
  dialogue_turnoff(game->dialogue, ERROR,code, object_name);
  return ERROR;
}

STATUS game_callback_open(Game* game){
  Player * player = NULL;
  Id space_id = NO_ID;
  char link_name_open[OBJECT_NAME] = "\0";
  char object_name_key[OBJECT_NAME] = "\0";
  Set * inventory = NULL;
  int i = 0, j = 0, code=0;
  Link * link = NULL;
  char description[100]="";
  
  player = game_get_player(game);
  space_id = player_get_space(player);
  strcpy (link_name_open, command_get_complement1(game_get_last_command(game)));
  strcpy (object_name_key, command_get_complement2(game_get_last_command(game)));
  inventory = inventory_get_ids(player_get_inventory(player));
  
  for (i = 0; i < set_getNumId(inventory); i++){
    if (strcmp(object_name_key, object_get_name(game_getObject(game, set_get_id_at(inventory, i))))==0){
      for (j = 0; j<MAX_LINKS && game->links[j]!=NULL; j++){
        if(space_id == link_get_tied1(game_get_link(game, game_get_link_id_at(game, j))) && strcmp(link_name_open, link_get_name(game_get_link(game, game_get_link_id_at(game, j))))==0){
          link = game_get_link(game, game_get_link_id_at(game, j));
          if (link_get_status(link) == 1){
            link_set_status(link, 0);
            sprintf(description,"You,ve opened the %s correctly", link_name_open);
            command_set_description(game->last_cmd, description);
            code=-1;
            dialogue_open(game->dialogue, OK,code, link_name_open, object_name_key);
            return OK;
          }
          sprintf(description,"The %s was already opened -_-", link_name_open);
          command_set_description(game->last_cmd, description);
          code=-1;
          dialogue_open(game->dialogue, OK,code, link_name_open, object_name_key);
          return OK;
        }
      }
    }
  }
  dialogue_open(game->dialogue, ERROR,code, link_name_open, object_name_key);
  return ERROR;
}

STATUS game_create_from_file(Game* game,char* filename1, char *filename2,char *filename3,char *filename4,char *filename5){
  if(!game || !filename1 || !filename2 || !filename3 || !filename4 || !filename5)
    return ERROR;
  if (game_reader_load_spaces(game, filename1) == ERROR)
    return ERROR;
  if (game_reader_load_objects(game,filename2) == ERROR)
    return ERROR;
  if(game_reader_load_links(game,filename3)==ERROR)
    return ERROR;
  if(game_reader_load_players(game,filename4)==ERROR)
    return ERROR;
  if(game_reader_load_enemies(game,filename5)==ERROR)
    return ERROR;
  return OK;
}

Object * game_getObject(Game * game, Id idObject){
  int i;
  for(i=0;i<MAX_OBJECTS&&game->objects[i]!=NULL;i++){
    if(object_get_id(game->objects[i])==idObject)
      return game->objects[i];
  }
  return NULL;
}

Id  game_get_space_id_at(Game* game, int position){

    if (position < 0 || position >= MAX_SPACES) {
        return NO_ID;
    }

    return space_get_id(game->spaces[position]);
}

STATUS game_set_player_location(Game* game, Id id){

    if (id == NO_ID) {
        return ERROR;
    }
    player_set_space(game->player,id);

    return OK;
}

STATUS game_set_enemy_location(Game* game, Id id, Id enemy){
  int i = 0;
    if (id == NO_ID) {
        return ERROR;
    }
    
  for (i=0; i<MAX_ENEMY && game->enemy[i] != NULL; i++){
    if(enemy_get_id(game->enemy[i]) == enemy){
      enemy_set_space(game->enemy[i],id);
      return OK;
    }
  }  
    return ERROR;
}

STATUS game_set_object_location(Game* game, Id id, Id object){

    if (id == NO_ID) {
        return ERROR;
    }

    space_add_object(game_get_space(game,id),object);

    return OK;
}

Player * game_get_player(Game * game){

    if (!game)
      return NULL;

    return game->player;
}

Enemy * game_get_enemy(Game * game, Id enemy){
    int i;
    if (!game || enemy==NO_ID)
      return NULL;
      
    for (i=0; i<MAX_ENEMY && game->enemy[i] != NULL; i++){
      if(enemy_get_id(game->enemy[i]) == enemy){
        return game->enemy[i];
      }
    }
    return NULL;
}


Enemy * game_get_enemy_for_battle(Game * game){
  int i;
  if (!game)
    return NULL;

  for(i=0;i<MAX_ENEMY && game->enemy[i]!=NULL;i++){
    if(enemy_get_space(game->enemy[i])==game_get_player_location(game))
      return game->enemy[i];
  }
  return NULL;
}

STATUS game_add_space(Game* game, Space* space){

    int i = 0;

    if (space == NULL) {
        return ERROR;
    }

    while ( (i < MAX_SPACES) && (game->spaces[i] != NULL)){
        i++;
    }

    if (i >= MAX_SPACES) {
        return ERROR;
    }

    game->spaces[i] = space;

    return OK;
}

STATUS game_addObject(Game * game, Object * objects){
  int i;
  if(!game || !objects)
    return ERROR;
  for(i=0;i<MAX_OBJECTS && game->objects[i]!=NULL;i++);
  game->objects[i]=objects;
  if(!game->objects[i]){
    return ERROR;
  }

  return OK;
}

Link * game_get_link(Game *game, Id id){
  int i;

  if(!game || id==NO_ID)
    return NULL;
  for(i=0;i<MAX_LINKS && game->links[i]!=NULL;i++){
    if(link_get_id(game->links[i])==id)
      return game->links[i];
  }
  return NULL;
}

STATUS game_addLink(Game *game, Link *link){
  int i;
  if(!game || !link)
    return ERROR;
  for(i=0;i<MAX_LINKS && game->links[i]!=NULL;i++);
  game->links[i]=link;
  if(!game->links[i])
    return ERROR;
  return OK;
}

STATUS game_add_player(Game *game,Player *player){
  if(!game || !player)
    return ERROR;
  game->player=player;
  return OK;
}

STATUS game_add_enemy(Game *game,Enemy *enemy){
  int i;
  if(!game || !enemy)
    return ERROR;
  for (i = 0; i<MAX_ENEMY && game->enemy[i] != NULL; i++);
  game->enemy[i]=enemy;
  return OK;
}

Id game_get_object_id_at(Game* game, int position){
  if (position < 0 || position >= MAX_OBJECTS){
      return NO_ID;
  }
  
  return object_get_id(game->objects[position]);
}

Id game_get_link_id_at(Game* game, int position){
  if (position < 0 || position >= MAX_LINKS || game->links[position]==NULL){
      return NO_ID;
  }
  
  return link_get_id(game->links[position]);
}

Id game_get_enemy_at(Game *game, int position){
  if (position < 0 || position >= MAX_ENEMY || game->enemy[position]==NULL){
      return NO_ID;
  }
  
  return enemy_get_id(game->enemy[position]);
}

STATUS game_set_rule(Game *game,BOOL bol){
  if(!game)
    return ERROR;
  game->rule=bol;
  return OK;
}

STATUS game_set_music(Game *game, BOOL bol){
  if(!game)
    return ERROR;
  game->music=bol;
  return OK;
}

STATUS game_set_save(Game *game,BOOL bol){
  if(!game)
    return ERROR;
  game->save=bol;
  return OK;
}

BOOL game_get_save(Game *game){
  if(!game)
    return FALSE;
  return game->save;
}

BOOL game_get_music(Game *game){
  if(!game)
    return FALSE;
  return game->music;
}

BOOL game_get_rule(Game *game){
  if(!game)
    return FALSE;
  return game->rule;
}