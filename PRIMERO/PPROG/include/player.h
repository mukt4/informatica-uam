/**
* @brief It defines the type player
*
* This module include the functions that work with
* the type player.
* @file player.h
* @author Guillermo Hoyo Bravo
* @version 2.0
* @date 12-3-2017
*/

#ifndef PLAYER_H
#define PLAYER_H

#include "types.h"
#include "inventory.h"
#include "stats.h"

typedef struct _Player Player;

#define PLAYER_NAME 25 /*!< Max length of the player name */

/**
* @brief It create an object of the type Player
* @author Guillermo Hoyo Bravo
* 
* player_create(Id id) It allocates memory for an object of the type player
* and initialise his fields
* @param id The id of the player initialised
* @return Player* It returns the player initialised or it returns NULL if an 
* error ocurred
*/
Player* player_create(Id id);

/**
* @brief It destroy an object of the type Player
* @author Guillermo Hoyo Bravo
* 
* player_destroy(Player* player) It liberates the memory of an object of the
* type player
* @param object The player that is going to be destroyed
* @return STATUS It returns OK if the memory has been liberated successfuly or
* it returns ERROR if an error ocurred
*/
STATUS player_destroy(Player * player);

/**
* @brief It get the id of a Player
* @author Guillermo Hoyo Bravo
* 
* player_get_id(Player * player) It return the id of the field id in the type 
* player
* @param player An object of the type Player
* @return Id It returns the id of the player or it return NO_ID if an error 
* ocurred
*/
Id player_get_id(Player * player);

/**
* @brief It set the name of a player
* @author Guillermo Hoyo Bravo
* 
* player_set_name(Player * player,char * name) It sets the field name of the 
* type player
* @param player An object of the type player
* @param name The name that will have the player
* @return STATUS It returns OK if it sets the name correctly or ERROR if an 
* error ocurred
*/
STATUS player_set_name(Player * player, char* name);

/**
* @brief It get the name of the Player
* @author Guillermo Hoyo Bravo
* 
* player_get_name(Player * player) It return the name of the player 
* @param player An object of the type Player
* @return const char* It returns the name of the player or NULL if an error
* ocurred
*/
const char* player_get_name(Player * player);

/**
* @brief It print the information of an object of the type Player
* @author Guillermo Hoyo Bravo
* 
* player_print(Player * player) It prints the fields of a Player
* @param player An object of the type Player
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS player_print(Player * player);

/**
* @brief It adds an id to a type player
* @author Guillermo Hoyo Bravo
* 
* player_add_object(Player * player,Id object) It adds an id to a type player
* @param player An object of the type player
* @param object the id of the object that is goimg to be added
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_add_object(Player * player,Id object);

/**
* @brief It deletes an id from a type player
* @author Tomas Higuera
* 
* player_delete_object(Player * player,Id object) It deletes an id from a type player
* @param player An object of the type player
* @param object the id of the object that is goimg to be deleted
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_delete_object(Player * player,Id object);

/**
* @brief It set the id of the space where the player is
* @author Guillermo Hoyo Bravo
* 
* player_set_space(Player * player,Id space) It sets the field space of the 
* type player
* @param player An object of the type player
* @param space The id of the space
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_set_space(Player * player,Id space);

/**
* @brief It get the inventory from a type player
* @author Guillermo Hoyo Bravo
* 
* player_get_inventory(Player * player) It returns the inventory from a type Player
* @param player An object of the type Player
* @return Inventory * It returns the inventoryfrom the player
*/
Inventory* player_get_inventory(Player * player);

/**
* @brief It get the id of the space where the player is
* @author Guillermo Hoyo Bravo
* 
* player_get_space(Player * player) It return the id of the field space in the
* type player
* @param player An object of the type Player
* @return Id It returns the id of the space the player or it return NO_ID if an
* error ocurred
*/
Id player_get_space(Player * player);

/**
* @brief It tells if an specific id (from an object) is yn the inventory from the player
* @author Tomas Higuera viso
* 
* player_inventory_have_object(Player *player, Id object)
* @param player An object of the type Player
* @param object The Id of the object you want to check that it's int he inventory
* @return BOOL It returns a type BOOL that inform us if the 
* game has ended
*/
BOOL player_inventory_have_object(Player *player, Id object);

/**
* @brief It get the sets of ids from a type player
* @author Tomas Higuera viso
* 
* player_get_inventory_ids(Player * player) It returns the set of ids
* from the inventory of a type Player
* @param player An object of the type Player
* @return Id * It returns the sets of ids of the player's inventory
*/
Id *player_get_inventory_ids(Player * player);

/**
* @brief It get the stats from the player
* @author Tomas Higuera
* 
* player_get_stats(player * player) It returns the stats from the player
* @param player An object of the type player
* @return player * It returns the field stats
*/
Stats * player_get_stats(Player * player);

/**
* @brief It get the hp from the player
* @author Tomas Higuera
* 
* player_get_hp(player * player) It returns the hp from the player
* @param player An object of the type player
* @return player * It returns the field hp
*/
int player_get_hp(Player * player);

/**
* @brief It get the ad from the player
* @author Tomas Higuera
* 
* player_get_ad(player * player) It returns the ad from the player
* @param player An object of the type player
* @return player * It returns the field ad
*/
int player_get_ad(Player * player);

/**
* @brief It get the def from the player
* @author Tomas Higuera
* 
* player_get_def(player * player) It returns the def from the player
* @param player An object of the type player
* @return player * It returns the field def
*/
int player_get_def(Player * player);

/**
* @brief It get the speed from the player
* @author Tomas Higuera
* 
* player_get_speed(player * player) It returns the speed from the player
* @param player An object of the type player
* @return player * It returns the field speed
*/
int player_get_speed(Player * player);

/**
* @brief It sets the hp from the player
* @author Guillermo Hoyo
* 
* player_set_hp(player * player, int hp) It sets the hp from the player
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_set_hp(Player * player, int hp);

/**
* @brief It sets the ad from the player
* @author Guillermo Hoyo
* 
* player_set_ad(player * player, int ad) It sets the ad from the player
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_set_ad(Player * player, int ad);

/**
* @brief It sets the def from the player
* @author Guillermo Hoyo
* 
* player_set_def(player * player, int def) It sets the def from the player
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_set_def(Player * player, int def);

/**
* @brief It sets the speed from the player
* @author Guillermo Hoyo
* 
* player_set_speed(player * player, int speed) It sets the speed from the player
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_set_speed(Player * player, int speed);

/**
* @brief It adds the hp to the player->hp
* @author Guillermo Hoyo
* 
* player_set_hp(player * player, int hp) It adds the hp to the player->hp
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be add
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_add_hp(Player * player, int hp);

/**
* @brief It adds the ad to the player->ad
* @author Guillermo Hoyo
* 
* player_set_ad(player * player, int ad) It adds the ad to the player->ad
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be add
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/   
STATUS player_add_ad(Player * player, int ad);

/**
* @brief It adds the def to the player->def
* @author Guillermo Hoyo
* 
* player_set_def(player * player, int def) It adds the def to the player->def
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be add
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_add_def(Player * player, int def);

/**
* @brief It adds the speed to the player->speed
* @author Guillermo Hoyo
* 
* player_set_speed(player * player, int speed) It adds the speed to the player->speed
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be add
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_add_speed(Player * player, int speed);

/**
* @brief It subtracts the hp to the player->hp
* @author Guillermo Hoyo
* 
* player_set_hp(player * player, int hp) It subtracts the hp to the player->hp
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be subtract
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_subtract_hp(Player * player, int hp);

/**
* @brief It subtracts the ad to the player->ad
* @author Guillermo Hoyo
* 
* player_set_ad(player * player, int ad) It subtracts the ad to the player->ad
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be subtract
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_subtract_ad(Player * player, int ad);

/**
* @brief It subtracts the def to the player->def
* @author Guillermo Hoyo
* 
* player_set_def(player * player, int def) It subtracts the def to the player->def
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be subtract
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_subtract_def(Player * player, int def);

/**
* @brief It subtracts the speed to the player->speed
* @author Guillermo Hoyo
* 
* player_set_speed(player * player, int speed) It subtracts the speed to the player->speed
* @param player An object of the type player
* @param player An object of the type int that is the value of the field that is going to be subtract
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS player_subtract_speed(Player * player, int speed);

#endif