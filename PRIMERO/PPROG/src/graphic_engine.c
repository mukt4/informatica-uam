/** 
 * @brief It defines a textual graphic engine
 * 
 * @file graphic_engine.c
 * @author Profesores PPROG,Tomás Higuera,Guillermo Hoyo & Alvaro Lopez
 * @version 2.0 
 * @date 12-3-2017
 * @copyright GNU Public License
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "graphic_engine.h"
#include "command.h"

/** The structure of Graphic engine contains the the information that is used to print the game*/
struct _Graphic_engine{
  Area *map, *descript, *feedback, *inspect;/**map->The information needed for painting the map/
  descript->The information needed for painting the description of the game/
  banner->The information needed for painting the title of the game
  help->The information needed for painting the help block
  feedback->The information needed for painting the last actions of the game*/
};

Graphic_engine *graphic_engine_create(){
  static Graphic_engine *ge = NULL;

  if (ge)
    return ge;
  
  screen_init();
  ge = (Graphic_engine *) malloc(sizeof(Graphic_engine));
  
  ge->map      = screen_area_init( 1, 1, 51, 15);
  ge->descript = screen_area_init(53, 1, 36, 15);
  ge->feedback = screen_area_init( 1,17, 88,  7);
  ge->inspect = screen_area_init(90,1,45,23);

  return ge;
}

void graphic_engine_destroy(Graphic_engine *ge){
  if (!ge)
    return;
  
  screen_area_destroy(ge->map);
  screen_area_destroy(ge->descript);
  screen_area_destroy(ge->feedback);
  screen_area_destroy(ge->inspect);
  
  screen_destroy();
  free(ge);
}

void graphic_engine_paint_game(Graphic_engine *ge, Game *game,STATUS status){
  Player *player=NULL;
  Space *space=NULL;
  Stats *stats=NULL;
  Inventory *inventory=NULL;
  Id location;
  int i;
  char *aux=NULL;
  char blanks[60]="                                                   \0";
  char description[300]="\0";
  Id north,south,east,west;
  Link *lEast=NULL;
  Link *lWest=NULL;
  Link *lNorth=NULL;
  Link *lSouth=NULL;
  Set *set=NULL;
  int j;
  Set *objects=NULL;
  Object *object=NULL;
  char copy[300];
  
  player=game_get_player(game);
  location=game_get_player_location(game);
  space=game_get_space(game,location);
  inventory=player_get_inventory(player);
  stats=player_get_stats(player);
  
  east=space_get_east(space);
  lEast=game_get_link(game,east);
  west=space_get_west(space);
  lWest=game_get_link(game,west);
  north=space_get_north(space);
  lNorth=game_get_link(game,north);
  south=space_get_south(space);
  lSouth=game_get_link(game,south);
  set=space_get_objects(space);
  
  /*Paint the map*/
  screen_area_clear(ge->map);
  if(lNorth!=NULL){
    if(link_get_status(lNorth)==0){
      strcpy(copy,blanks);
      copy[25]='^';
      screen_area_puts(ge->map,copy);
      memset( copy, 0,300 );
    }
    else{
      strcpy(copy,blanks);
      copy[25]='X';
      screen_area_puts(ge->map,copy);
      memset( copy, 0,300 );
    }
  }
  else{
    screen_area_puts(ge->map,blanks);
  }
  for(i=0;i<ROW;i++){/*24 nort y sur*/ /*23 hasta jugador y hasta objeto*/ /*6 objeto*/ /*7 este y oeste*/ /*49 de ancho sin contar laterales*/
    aux=space_get_global_gdesc(space,i);
    if(i==5){
      if(set_getNumId(set)>0){
        for(j=0;j<set_getNumId(set);j++){
          if(object_get_oculto(game_getObject(game,set_get_id_at(set,j)))==0){
            strcpy(copy,aux);
            copy[23+j*2]='O';
            screen_area_puts(ge->map,copy);
            memset( copy, 0,300 );
          }
          else{
            screen_area_puts(ge->map,aux);
          }
        }
      }
      else{
        screen_area_puts(ge->map,aux);
      }
    }
    else if(i==6){
      if(lEast!=NULL){
        if(lWest!=NULL){
          if(link_get_status(lEast)==0 && link_get_status(lWest)==0){
            strcpy(copy,aux);
            copy[50]='>';
            copy[0]='<';
            screen_area_puts(ge->map,copy);
            memset( copy, 0,300 );
          }
          else if(link_get_status(lEast)!=0 && link_get_status(lWest)==0){
            strcpy(copy,aux);
            copy[50]='X';
            copy[0]='<';
            screen_area_puts(ge->map,copy);
            memset( copy, 0,300 );
          }
          else if(link_get_status(lEast)==0 && link_get_status(lWest)!=0){
            strcpy(copy,aux);
            copy[50]='>';
            copy[0]='X';
            screen_area_puts(ge->map,copy);
            memset( copy, 0,300 );
          }
          else{
            screen_area_puts(ge->map,aux);
          }
        }
        else{
          if(link_get_status(lEast)==0){
            strcpy(copy,aux);
            copy[50]='>';
            screen_area_puts(ge->map,copy);
            memset( copy, 0,300 );
          }
          else{
            strcpy(copy,aux);
            copy[50]='X';
            screen_area_puts(ge->map,copy);
            memset( copy, 0,300 );
          }
        }
      }
      else if(lWest!=NULL){
        if(link_get_status(lWest)==0){
            strcpy(copy,aux);
            copy[0]='<';
            screen_area_puts(ge->map,copy);
            memset( copy, 0,300 );
        }
        else{
            strcpy(copy,aux);
            copy[0]='X';
            screen_area_puts(ge->map,copy);
            memset( copy, 0,300 );
        }
      }
    }
    else{
      screen_area_puts(ge->map,aux);
    }
  }
  if(lSouth!=NULL){
    if(link_get_status(lSouth)==0){
      strcpy(copy,blanks);
      copy[25]='v';
      screen_area_puts(ge->map,copy);
      memset( copy, 0,300 );
    }
    else{
      strcpy(copy,blanks);
      copy[25]='X';
      screen_area_puts(ge->map,copy);
      memset( copy, 0,300 );
    }
  }
  screen_area_puts(ge->map,blanks);
  
  /*Paint the description area*/
  screen_area_clear(ge->descript);
  screen_area_reset_cursor(ge->descript);
  sprintf(description,"   ->Inventory:");
  screen_area_puts(ge->descript,description);
  memset( description, 0,300 );
  objects=inventory_get_ids(inventory);
  for(i=0;i<set_getNumId(objects);i++){
    object=game_getObject(game,set_get_id_at(objects,i));
    sprintf(description,"   ");
    strcat(description,object_get_name(object));
    screen_area_puts(ge->descript,description);
    memset( description, 0,300 );
  }
  sprintf(description," ");
  screen_area_puts(ge->descript,description);
  sprintf(description,"   ->Stats:");
  screen_area_puts(ge->descript,description);
  memset( description, 0,300 );
  sprintf(description,"   HP:%d   AD:%d",stats_get_hp(stats),stats_get_ad(stats));
  screen_area_puts(ge->descript,description);
  memset( description, 0,300 );
  sprintf(description,"   DEF:%d   SPEED:%d",stats_get_def(stats),stats_get_speed(stats));
  screen_area_puts(ge->descript,description);
  memset( description, 0,300 );
  sprintf(description," ");
  screen_area_puts(ge->descript,description);
  memset( description, 0,300 );
  sprintf(description,"   ->Room description:");
  screen_area_puts(ge->descript,description);
  sprintf(description,"   %s",space_get_description(space));
  screen_area_puts(ge->descript,description);
  memset( description, 0,300 );
  sprintf(description," ");
  screen_area_puts(ge->descript,description);
  memset( description, 0,300 );
  sprintf(description,"   ->Objects:");
  screen_area_puts(ge->descript,description);
  memset( description, 0,300 );
  for(i=0;i<set_getNumId(set);i++){
    object=game_getObject(game,set_get_id_at(set,i));
    if(object_get_oculto(object)==0){
      sprintf(description,"   %s",object_get_name(object));
      screen_area_puts(ge->descript,description);
      memset( description, 0,300 );
    }
  }
  /* Paint the in the feedback area */
  if(strcmp("Invalid Command",(char *)dialogue_get(game_get_dialogue(game)))!=0){
    if(command_get_input(game_get_last_command(game))!=NO_CMD){
      screen_area_puts(ge->feedback,(char *)dialogue_get(game_get_dialogue(game)));
    }
  }
  screen_area_clear(ge->inspect);
  screen_area_puts(ge->inspect,command_get_description(game_get_last_command(game)));
  /* Dump to the terminal */
  screen_paint();
  printf("prompt:> ");
}
