/** 
 * @brief It defines the game interface
 * for each command
 * 
 * @file game.h
 * @author Profesores PPROG, Guillermo Hoyo, Tomas Higuera & Alvaro Lopez
 * @version 1.0 
 * @date 12-3-2017
 * @copyright GNU Public License
 */

#ifndef GAME_H
#define GAME_H

#define MAX_OBJECTS 25
#define MAX_LINKS 500
#define MAX_PLAYERS 1
#define MAX_ENEMY 20

#include "types.h"
#include "command.h"
#include "space.h"
#include "object.h"
#include "player.h"
#include "die.h"
#include "link.h"
#include "enemy.h"
#include "dialogue.h"

typedef struct _Game Game;

/**
* @brief It creates an object of the type Game
* @author Tomás Higuera Viso
* 
* game_create(Game * game) It allocate memory for a type game and initialises it fields
* @param game An object of the type Game
* @return STATUS It returns OK if it initialised the game correctly or ERROR 
* if an error ocurred
*/
Game * game_create();

/**
* @brief It fills the game fields with the infomation of a file
* @author Alvaro Lopez
* 
* game_create_from_file(Game * game,char * filename) It fills the game fields
* depending on the file
* @param game An object of the type Game
* @param filename The name of the file
* @return STATUS It returns OK if it initialised the game correctly or ERROR 
* if an error ocurred
*/
STATUS game_create_from_file(Game* game,char* filename1, char* filename2, char* filename3, char *filename4, char *filename5);

/**
* @brief It update the actual game
* @author Guillermo Hoyo Bravo
* 
* game_update(Game * game, T_Command cmd, char* complement, int code) It update the game depending on
* the command introduced
* @param game An object of the type Game
* @param cmd The command introduced for the user
* @param code A int number with the code for dialogues
* @return STATUS It returns OK if the game was updated correctly or ERROR 
* if an error ocurred
*/
STATUS game_update(Game* game, T_Command* cmd, int code);

/**
* @brief It destroy a game
* @author Tomás Higuera Viso
* 
* game_destroy(Game * game) It liberates the memory that was allocated for 
* a type Game
* @param game An object of the type Game
* @return STATUS It returns OK if it destroyed the game correctly or ERROR 
* if an error ocurred
*/
STATUS game_destroy(Game* game);

/**
* @brief It recognises if the game  has ended
* @author Alvaro Lopez
* 
* game_is_over(Game * game) It is not implemented yet
* @param game An object of the type game
* @return BOOL It returns a type BOOL that inform us if the 
* game has ended
*/
BOOL   game_is_over(Game* game);

/**
* @brief It prints the screen of the game
* @author Guillermo Hoyo Bravo
* 
* game_print_screen(Game * game) It is not implemented yet
* @param game An object of the type game
* @return None
*/
void   game_print_screen(Game* game);

/**
* @brief It prints the data of the game
* @author Tomás Higuera Viso
* 
* game_print_data(Game * game) It prints the data of the fields of the type
* game
* @param game An object of the type  game
* @return None
*/
void   game_print_data(Game* game);

/**
* @brief It gets an object of the type space from game
* @author Alvaro Lopez
*
* game_get_space(Game * game,Id id) It looks for a space with one id in array 
* of objects of game
* @param game An object of the type game
* @param id The id of the space
* @return Space* It returns an object of the type game or NULL if there is not a space
* with that id in the game
*/
Space* game_get_space(Game* game, Id id);

/**
* @brief It gets the location of the player
* @author Guillermo Hoyo Bravo
*
* game_get_player_location(Game * game) It gets the id of the space where the player
* is located
* @param game An object of the type game
* @return Id It returns the id of the space where the player is located or it returns 
* NO_ID if there is an error or the player is not located
*/
Id game_get_player_location(Game* game);

/**
* @brief It gets the location of the penemy
* @author Guillermo Hoyo Bravo
*
* game_get_enemy_location(Game * game) It gets the id of the space where the enemy
* is located
* @param game An object of the type game
* @return Id It returns the set of the spaces where the enemies are located or it returns 
* NO_ID if there is an error or the enemies are not located
*/
Set * game_get_enemy_locations(Game* game);

/**
* @brief It gets the location of the object
* @author Tomás Higuera Viso
*
* game_get_object_location(Game * game) It gets a type set with all the objects locations
* @param game An object of the type game
* @return Set* It returns the all the ids of the all the objects locations
*/
Set*    game_get_objects_location(Game* game);

/**
* @brief It get the last command used on the game
* @author Alvaro Lopez
*
* game_get_last_command(Game * game) It gets the last command introduced for 
* the user
* @param game An object of the type game
* @return T_Command It returns the field last_cmd of game
*/
T_Command* game_get_last_command(Game* game);

/**
* @brief It gets an object of the type object from game
*  @author Tomás Higuera Viso
*
* game_getObject(Game * game, Id id) It looks for an object with one id in array 
* of objects of game
* @param game An object of the type game
* @param idObject The id of the object
* @return Object* It returns an object of the type game or NULL if there is not 
* the object with the id of idObject
*/
Object* game_getObject(Game * game, Id idObject);

/**
* @brief It get the id of a space of the game
* @author Guillermo Hoyo Bravo
*
* game_get_space_id_at(Game * game, int position) It look for a space in the 
* array of spaces of the game and return it id
* @param game An object of the type game
* @param position The position of the space in the array of spaces from game
* @return Id It returns the id of the space or NO_ID if and error ocurred
*/
Id     game_get_space_id_at(Game* game, int position);

/**
* @brief It set the player location
* @author Tomás Higuera Viso
* 
* game_set_player_location(Game * game,Id id) It set the position of the player
* in a determined space 
* @param game An object of the type game
* @param id The id of the space where the player is going to be located
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_set_player_location(Game* game, Id id);

/**
* @brief It set the object location
* @author Alvaro Lopez
*
* game_set_object_location(Game * game,Id id) It set the position of the object
* in a determined space 
* @param game An object of the type game
* @param id The id of the space where the object is going to be located
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_set_object_location(Game* game, Id id,Id object);

/**
* @brief It get the player of the game
* @author Guillermo Hoyo Bravo
*
* game_get_player(Game* game) It get the player fron the game 
* @param game An object of the type game
* @return Player It returns the player of the game
*/
Player* game_get_player(Game* game);

/**
* @brief It get the enemy on the same space that you are
* @author Guillermo Hoyo Bravo
*
* game_get_enemy_for_battle(Game* game) It get the enemy fron the game in the samme space as you
* @param game An object of the type game
* @return Enemy * It returns the enemy of the game
*/
Enemy* game_get_enemy_for_battle(Game* game);

/**
* @brief It get the die of the game
* @author Tomás Higuera Viso
* 
* game_get_player(Game* game) It get the die fron the game 
* @param game An object of the type game
* @return Die It returns the die of the game
*/
Die* game_get_die(Game* game);

/**
* @brief It add a space to the object game 
* @author Alvaro Lopez
*
* game_add_space(Game * game, Space * space) It add a space to the array of
* spaces in the object game
* @param game An object of the type game
* @param space The spaces that is going to be added
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_add_space(Game* game, Space* space);

/**
* @brief It add a object to the object game 
* @author Guillermo Hoyo Bravo
*
* game_addObject(Game * game, Object * object) It add a object to the object game
* @param game An object of the type game
* @param object The object that is going to be added
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_addObject(Game * game, Object * object);

/**
* @brief It gets the link with the id passed from a game
* @author Tomás Higuera
*
* game_get_link(Game *game, Id id) It returns a link with an specific id
* @param game An object of the type game
* @param id The id of the link serched
* @return Link An object of the type link
*/
Link * game_get_link(Game *game, Id id);

/**
* @brief It add a link to the object game 
* @author Guillermo Hoyo Bravo
*
* game_addLink(Game * game, Link *link) It add a link to the object game
* @param game An object of the type game
* @param link The link that is going to be added
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_addLink(Game *game, Link *link);

/**
* @brief It add a player to the object game 
* @author Tomas Higuera Viso
*
* game_add_player(Game * game, Player *player) It add a player to the object game
* @param game An object of the type game
* @param player The player that is going to be added
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_add_player(Game *game,Player *player);

/**
* @brief The action turnon
* @author Tomas Higuera Viso
*
* game_callback_turnon(Game * game) It turnon an object
* @param game An object of the type game
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_callback_turnon(Game* game);

/**
* @brief The action turnoff
* @author Tomas Higuera Viso
*
* game_callback_turnoff(Game * game) It turnoff an object
* @param game An object of the type game
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_callback_turnoff(Game* game);

/**
* @brief It get the id of an object of the game
* @author Guillermo Hoyo Bravo
*
* game_get_object_id_at(Game * game, int position) It look for an object in the 
* array of objects of the game and return it id
* @param game An object of the type game
* @param position The position of the object in the array of spaces from game
* @return Id It returns the id of the object or NO_ID if and error ocurred
*/
Id game_get_object_id_at(Game* game, int position);

/**
* @brief It get the id of an link of the game
* @author Guillermo Hoyo Bravo
*
* game_get_link_id_at(Game * game, int position) It look for an link in the 
* array of links of the game and return it id
* @param game An link of the type game
* @param position The position of the link in the array of spaces from game
* @return Id It returns the id of the link or NO_ID if and error ocurred
*/
Id game_get_link_id_at(Game* game, int position);

/**
* @brief It set the enemy location
* @author Tomás Higuera Viso
* 
* game_set_enemy_location(Game * game,Id id) It set the position of the enemy
* in a determined space 
* @param game An object of the type game
* @param id The id of the space where the enemy is going to be located
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_set_enemy_location(Game* game, Id id, Id enemy);

/**
* @brief It get the enemy of the game
* @author Guillermo Hoyo Bravo
*
* game_get_enemy(Game* game) It get the enemy fron the game 
* @param game An object of the type game
* @return enemy It returns the enemy of the game
*/
Enemy * game_get_enemy(Game * game, Id enemy);

/**
* @brief It gets the location of the enemy
* @author Guillermo Hoyo Bravo
*
* game_get_enemy_location(Game * game) It gets the id of the space where the enemy
* is located
* @param game An object of the type game
* @return Id It returns the id of the space where the enemy is located or it returns 
* NO_ID if there is an error or the enemy is not located
*/
Set * game_get_enemy_locations(Game* game);

/**
* @brief It get the enemy on the same space that you are
* @author Guillermo Hoyo Bravo
*
* game_get_enemy_for_battle(Game* game) It get the enemy fron the game in the samme space as you
* @param game An object of the type game
* @return Enemy * It returns the enemy of the game
*/
Enemy * game_get_enemy_for_battle(Game * game);

/**
* @brief It add a enemy to the object game 
* @author Alvaro Lopez
*
* game_add_enemy(Game * game, Enemy * enemy) It add a enemy to the array of
* enemys in the object game
* @param game An object of the type game
* @param enemy The enemys that is going to be added
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_add_enemy(Game *game,Enemy *enemy);

/**
* @brief It recognises if the game  has ended
* @author Pablo Gutierrez
* 
* game_finished(Game * game) It determine if the game is finished
* @param game An object of the type game
* @return BOOL It returns a type BOOL that inform us if the 
* game has ended
*/
BOOL game_finished(Game *game);

/**
* @brief It get the id of a enemy of the game
* @author Guillermo Hoyo Bravo
*
* game_get_enemy_id_at(Game * game, int position) It look for a enemy in the 
* array of enemys of the game and return it id
* @param game An object of the type game
* @param position The position of the enemy in the array of enemys from game
* @return Id It returns the id of the enemy or NO_ID if and error ocurred
*/
Id game_get_enemy_at(Game *game, int position);

/**
* @brief It get the dialogue the game
* @author Pablo Gutierrez
*
* game_get_dialogue(Game * game) It return the dialogue of the game
* @param game An object of the type game
* @return Dialogue* The dialogue of the game
*/
Dialogue* game_get_dialogue(Game *game);

/**
* @brief It set the parameter rule of game
* @author Pablo Gutierrez
*
* game_set_rule(Game * game,BOOL bol) It set de the parameter rule of game
* @param game An object of the type game
* @param bol The parameter that is going to be set
* @return STATUS Return OK if the action was successfull
*/
STATUS game_set_rule(Game *game,BOOL bol);

/**
* @brief It set the parameter music of game
* @author Pablo Gutierrez
*
* game_set_music(Game * game,BOOL bol) It set de the parameter music of game
* @param game An object of the type game
* @param bol The parameter that is going to be set
* @return STATUS Return OK if the action was successfull
*/
STATUS game_set_music(Game *game, BOOL bol);

/**
* @brief It set the parameter save of game
* @author Pablo Gutierrez
*
* game_set_save(Game * game,BOOL bol) It set de the parameter save of game
* @param game An object of the type game
* @param bol The parameter that is going to be set
* @return STATUS Return OK if the action was successfull
*/
STATUS game_set_save(Game *game,BOOL bol);

/**
* @brief It get the parameter save of game
* @author Pablo Gutierrez
*
* game_get_save(Game * game) It get de the parameter save of game
* @param game An object of the type game
* @return BOOL The parameter save
*/
BOOL game_get_save(Game *game);

/**
* @brief It get the parameter music of game
* @author Pablo Gutierrez
*
* game_get_music(Game * game) It get de the parameter music of game
* @param game An object of the type game
* @return BOOL The parameter music
*/
BOOL game_get_music(Game *game);

/**
* @brief It get the parameter rule of game
* @author Pablo Gutierrez
*
* game_get_rule(Game * game) It get de the parameter rule of game
* @param game An object of the type game
* @return BOOL The parameter rule
*/
BOOL game_get_rule(Game *game);

#endif

