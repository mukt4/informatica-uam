/**
* @brief It loads all the information of the game
*
* This module loads all the information needed for 
* the proper working of the game
* @file game_reader.h
* @author Guillermo Hoyo Bravo & ALvaro Lopez
* @version 1.0
* @date 12-3-2017
*/

#ifndef _GAME_READER_
#define _GAME_READER_

#include "game.h"
#include "types.h"
#include "space.h"
#include "object.h"
#include "link.h"

/**
* @brief It load al the spaces in the object game
* @author Guillermo Hoyo Bravo
*
* game_reader_load_spaces(Game * game, char * filename) It fills the spaces field in
* the object game
* @param game An object of the type game
* @param filename The name of the file that contents the spaces of the game
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_reader_load_spaces(Game* game, char* filename);

/**
* @brief It load al the objects in the object game
* @author Guillermo Hoyo Bravo
* 
* game_reader_load_objects(Game * game, char * filename) It fills the object field in
* the object game
* @param game An object of the type game
* @param filename The name of the file that contents the objects of the game
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_reader_load_objects(Game* game, char* filename);

/**
* @brief It load al the links in the object game
* @author Alvaro Lopez
* 
* game_reader_load_links(Game * game, char * filename) It fills the links field in
* the object game
* @param game An object of the type game
* @param filename The name of the file that contents the links of the game
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_reader_load_links(Game* game, char* filename);

/**
* @brief It load al the players in the object game
* @author Tomas Higuera Viso
* 
* game_reader_load_players(Game * game, char * filename) It fills the player field in
* the object game
* @param game An object of the type game
* @param filename The name of the file that contents the player of the game
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_reader_load_players(Game* game, char* filename);

/**
* @brief It load al the enemies in the object game
* @author Tomas Higuera Viso
* 
* game_reader_load_enemies(Game * game, char * filename) It fills the enemy field in
* the object game
* @param game An object of the type game
* @param filename The name of the file that contents the enemy of the game
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_reader_load_enemies(Game* game, char* filename);

/**
* @brief It saves al the information on the game
* @author Guillermo Hoyo Bravo
* 
* game_manamagement_save(Game * game, char * game_saved) saves all the info of the game in a file
* @param game An object of the type game
* @param game_saved The name of the file were the game is saved
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS game_management_save(Game * game, char * game_saved);

/**
* @brief It loads a previous game saved
* @author Alvaro Lopez
* 
* game_manamagement_load(Game * game, char * file) loads a game that was saved
* @param game An object of the type game
* @param file The name of the file from were the game is load
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
Game * game_management_load(Game *game, char *file);

#endif