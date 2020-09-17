/** 
 * @brief It defines the game rules
 * 
 * @file game_rules.h
 * @author Guillermo Hoyo
 * @version 2.0 
 * @date 05-5-2017
 * @copyright GNU Public License
 */

#ifndef GAME_RULES
#define GAME_RULES

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"
#include "game.h"

/**
* @brief It selects a random action from game rules
* @author Alvaro Lopez
* 
* gameRules_action(Die *die, Game *game) it selects one of the 13 possible actions
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action(Die *die, Game *game);


/**
* @brief Ít turns on the ilumination on a random space 
* @author Pablo Gutiérrez
* 
* gameRules_action1(Die *die, Game *game) It selects a random space and set on the ilumination on it
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action1(Die *die, Game *game);

/**
* @brief It turns off the ilumination on a random space 
* @author Pablo Gutiérrez
* 
* gameRules_action2(Die *die, Game *game) It selects a random space and set off the ilumination on it
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action2(Die *die, Game *game);
/**
* @brief It increases the attack dammage on a player of the game
* @author Pablo Gutiérrez
* 
* gameRules_action3(Die *die, Game *game) It increases the attack dammage(between 0 and 20 on a player of the game 
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/

STATUS gameRules_action3(Die *die, Game *game);

/**
* @brief It substracts the attack dammage on a player of the game
* @author Pablo Gutiérrez
* 
* gameRules_action4(Die *die, Game *game) It substracts the attack dammage(between 0 and 20 on a player of the game 
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/

STATUS gameRules_action4(Die *die, Game *game);


/**
* @brief It changes an object for a newone in a player of the game
* @author Pablo Gutiérrez
* 
* gameRules_action5(Die *die, Game *game)  It changes randoms objects for a newones in a player of the game
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action5(Die *die, Game *game);

/**
* @brief It lets an object to be moved
* @author Pablo Gutiérrez
* 
* gameRules_action6(Die *die, Game *game)  It lets an object of a player be moved if some conditions are correct
* @param game An object of the type game
* @param die An object of the type Die 
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action6(Die *die, Game *game);

/**
* @brief It denies an object the possibility to be moved
* @author Pablo Gutiérrez
* 
* gameRules_action7(Die *die, Game *game)  It denies an object of a player the possibility be moved if some conditions are correct
* @param game An object of the type game
* @param die An object of the type Die 
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action7(Die *die, Game *game);
/**
* @brief It increases the health points on a player of the game
* @author Pablo Gutiérrez
* 
* gameRules_action8(Die *die, Game *game) It increases the health points(between 0 and 20) on a player of the game 
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action8(Die *die, Game *game);

/**
* @brief It substracts the health points on a player of the game
* @author Pablo Gutiérrez
* 
* gameRules_action9(Die *die, Game *game) It substracts the health points(between 0 and 20) on a player of the game 
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action9(Die *die, Game *game);

/**
* @brief It increases the speed on a player of the game
* @author Pablo Gutiérrez
* 
* gameRules_action10(Die *die, Game *game) It increases the speed(between 0 and 20) on a player of the game 
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action10(Die *die, Game *game);

/**
* @brief It substracts the speed on a player of the game
* @author Pablo Gutiérrez
* 
* gameRules_action11(Die *die, Game *game) It substracts the speed(between 0 and 20) on a player of the game 
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action11(Die *die, Game *game);

/**
* @brief It increases the defense points on a player of the game
* @author Pablo Gutiérrez
* 
* gameRules_action12(Die *die, Game *game) It increases the defense points(between 0 and 20) on a player of the game 
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action12(Die *die, Game *game);

/**
* @brief It decreases the defense points on a player of the game
* @author Pablo Gutiérrez
* 
* gameRules_action10(Die *die, Game *game) It decreases the defense points(between 0 and 20) on a player of the game 
* @param game An object of the type game
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS gameRules_action13(Die *die, Game *game);


#endif