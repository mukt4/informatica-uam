/**
* @brief It defines the type player
*
* This module include the functions that work with
* the type player.
* @file player.c
* @author Guillermo Hoyo Bravo
* @version 2.0
* @date 12-3-2017
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "player.h"
#include "game.h"

/** The structure of Player will save information of the different characteristics of the player**/
struct _Player{
    Id id; /**Number of ID of player*/
    char name[PLAYER_NAME + 1]; /**Type char that contains the name of the player*/
    Id space; /**Number of ID of the space were the player is located*/
    Inventory * inventory; /**Number of ID of the object the player carries*/
    Stats * stats;
};

Player * player_create(Id id){
    Player* player=NULL;
    
    if(id==NO_ID)
        return NULL;
    player=(Player*)malloc(sizeof (Player));
    if(!player)
        return NULL;
    player->id=id;
    player->name[0]='\0';
    player->space=NO_ID;
    player->inventory=inventory_create(MAX_OBJECTS);
    player->stats = stats_ini();
    return player;
}

STATUS player_destroy(Player * player){
    if(!player)
        return ERROR;
    inventory_destroy(player->inventory);
    stats_destroy(player->stats);
    free(player);
    return OK;
}
Id player_get_id(Player * player){
    if(!player)
        return NO_ID;
    return player->id;
}

STATUS player_set_name(Player * player, char* name){
    if(!player || !name)
        return ERROR;
    if (!strcpy(player->name, name)) {
        return ERROR;
    }
    return OK;
}
const char* player_get_name(Player * player){
    if(!player)
        return NULL;
    return player->name;
}

STATUS player_set_space(Player * player,Id space){
    if(!player || space==NO_ID)
        return ERROR;
    player->space=space;
    return OK;
}

Id player_get_space(Player * player){
    if(!player)
        return NO_ID;
    return player->space;
}

STATUS player_print(Player * player){
    if (!player) {
        return ERROR;
    }
    fprintf(stdout, "--> Player (Id: %ld; Name: %s; Space: %ld; )\n", player->id, player->name, player->space);
    inventory_print(player->inventory);
    return OK;
}

BOOL player_inventory_have_object(Player *player, Id object){
    if(!player){
        return FALSE;
    }
    if(set_checkId(inventory_get_ids(player->inventory),object)==OK)
        return TRUE;
    return FALSE;
}

Inventory* player_get_inventory(Player * player){
    if(!player)
        return NULL;
    return player->inventory;
}

STATUS player_add_object(Player * player,Id object){
    if(!player)
        return ERROR;
    inventory_add_id(player->inventory, object);
    return OK;
}

STATUS player_delete_object(Player * player,Id object){
    if(!player)
        return ERROR;
    inventory_delete_id(player->inventory, object);
    return OK;
}

Id *player_get_inventory_ids(Player * player){
    Id *ids;
    if(!player)
        return NULL;
    ids=set_get_ids(inventory_get_ids(player->inventory));
    return ids;
}


Stats * player_get_stats(Player * player){
    if (!player)
        return NULL;
        
    return player->stats;
}

int player_get_hp(Player * player){
    Stats * s = NULL;
    if (!player)
        return -1;
        
    s = player_get_stats(player);
    return stats_get_hp(s);
}

int player_get_ad(Player * player){
    Stats * s = NULL;
    if (!player)
        return -1;
        
    s = player_get_stats(player);
    return stats_get_ad(s);
}

int player_get_def(Player * player){
    Stats * s = NULL;
    if (!player)
        return -1;
        
    s = player_get_stats(player);
    return stats_get_def(s);
}

int player_get_speed(Player * player){
    Stats * s = NULL;
    if (!player)
        return -1;
        
    s = player_get_stats(player);
    return stats_get_speed(s);
}

STATUS player_set_hp(Player * player, int hp){
    Stats * s = NULL;
    if (!player)
        return ERROR;
    
    s = player_get_stats(player);
    stats_set_hp(s, hp);
    return OK;
}

STATUS player_set_ad(Player * player, int ad){
    Stats * s = NULL;
    if (!player)
        return ERROR;
    
    s = player_get_stats(player);
    stats_set_ad(s, ad);
    return OK;
}

STATUS player_set_def(Player * player, int def){
    Stats * s = NULL;
    if (!player)
        return ERROR;
    
    s = player_get_stats(player);
    stats_set_def(s, def);
    return OK;
}

STATUS player_set_speed(Player * player, int speed){
    Stats * s = NULL;
    if (!player)
        return ERROR;
    
    s = player_get_stats(player);
    stats_set_speed(s, speed);
    return OK;
}

STATUS player_add_hp(Player * player, int hp){
    Stats * s = NULL;
    if (!player)
        return ERROR;
        
    s = player_get_stats(player);
    stats_add_hp(s, hp);
    
    return OK;
}

STATUS player_add_ad(Player * player, int ad){
    Stats * s = NULL;
    if (!player)
        return ERROR;
        
    s = player_get_stats(player);
    stats_add_ad(s, ad);
    
    return OK;
}

STATUS player_add_def(Player * player, int def){
    Stats * s = NULL;
    if (!player)
        return ERROR;
        
    s = player_get_stats(player);
    stats_add_def(s, def);
    
    return OK;
}

STATUS player_add_speed(Player * player, int speed){
    Stats * s = NULL;
    if (!player)
        return ERROR;
        
    s = player_get_stats(player);
    stats_add_speed(s, speed);
    
    return OK;
}

STATUS player_subtract_hp(Player * player, int hp){
    Stats * s = NULL;
    if (!player)
        return ERROR;
        
    s = player_get_stats(player);
    stats_subtract_hp(s, hp);
    
    return OK;
}

STATUS player_subtract_ad(Player * player, int ad){
    Stats * s = NULL;
    if (!player)
        return ERROR;
        
    s = player_get_stats(player);
    stats_subtract_ad(s, ad);
    
    return OK;
}

STATUS player_subtract_def(Player * player, int def){
    Stats * s = NULL;
    if (!player)
        return ERROR;
        
    s = player_get_stats(player);
    stats_subtract_def(s, def);
    
    return OK;
}

STATUS player_subtract_speed(Player * player, int speed){
    Stats * s = NULL;
    if (!player)
        return ERROR;
        
    s = player_get_stats(player);
    stats_subtract_speed(s, speed);
    
    return OK;
}



