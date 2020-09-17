/** 
 * @brief It defines the menu
 * 
 * @file menu.h
 * @author Tomás Higuera
 * @version 2.0 
 * @date 05-5-2017
 * @copyright GNU Public License
 */

#ifndef MENU_H
#define MENU_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "menu.h"
#include "game.h"

/**
* @brief It prints the logo from the game
* @author Tomás Higuera Viso
* 
* menu_logo() It prints the logo from the game
* @param None
* @return void
*/
void menu_logo();

/**
* @brief It prints the initial menu from the game
* @author Tomás Higuera Viso
* 
* menu_ini() It prints the initial menu from the game
* @param None
* @return void
*/
void menu_ini();

/**
* @brief It prints a mesage when you finish the game
* @author Guillermo Hoyo Bravo
* 
* menu_end() It prints a mesage when you finish the game
* @param None
* @return void
*/

void menu_end();

/**
* @brief It prints the game over screen when you loose
* @author Tomás Higuera Viso
* 
* menu_game_over() It prints the game over screen when you loose
* @param None
* @return void
*/
void menu_game_over();

/**
* @brief It prints the options from the game
* @author Tomás Higuera Viso
* 
* menu_options() It prints the options from the game
* @param None
* @return void
*/
void menu_options();

/**
* @brief It prints the controls explanation from the game
* @author Tomás Higuera Viso
* 
* menu_controls() It prints the controls explanation from the game
* @param None
* @return void
*/
void menu_controls();

/**
* @brief It prints additional information from the game
* @author Tomás Higuera Viso
* 
* menu_additional_information() It prints additional information from the game
* @param None
* @return void
*/
void menu_additional_information();

/**
* @brief It prints the battles from the game
* @author Guillermo Hoyo Bravo
* 
* menu_battle() It prints the battles from the game
* @param game An object of the type Game
* @param enemy The id from the enemy you are going to battle with
* @return void
*/
void menu_battle(Game *game, Id enemy);

/**
* @brief It prints the load progress whrn charging
* @author Tomás Higuera Viso
* 
* menu_load() It prints load progress whrn charging
* @param None
* @return void
*/
void menu_load();

#endif