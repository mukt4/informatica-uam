/** 
 * @brief It defines a textual graphic engine
 * 
 * @file graphic_engine.h
 * @author Profesores PPROG,Tomás Higuera,Guillermo Hoyo & Alvaro Lopez
 * @version 2.0 
 * @date 12-3-2017
 * @copyright GNU Public License
 */

#ifndef __GRAPHIC_ENGINE__
#define __GRAPHIC_ENGINE__

#include "types.h"
#include "game.h"
#include "screen.h"
#include "link.h"

typedef struct _Graphic_engine Graphic_engine;

/**
* @brief It create an object of the type Graphic_engine
* @author Tomás Higuera Viso
* 
* graphic_engine_crete() It allocs memory for the type Graphic_engine 
* and initialise it fields
* @param None
* @return Graphic_engine* It returns the Graphic_engine if it has been created
* successfully
*/
Graphic_engine* graphic_engine_create();

/**
* @brief It destroys an object of the type Graphic_engine
* @author Guillermo Hoyo Bravo
*
* graphic_engine_destroy(Graphic_engine *ge) It libertaes the memory allocted for
* an object of the type Graphic_engine
* @param game An object of the type Graphic_engine
* @return None
*/
void graphic_engine_destroy(Graphic_engine *ge);

/**
* @brief It paints the map of the game
* @author Tomás Higuera Viso
*
* graphic_engine_paint_game(Graphic_engine *ge, Game *game) It paint the map of 
* the game depending on the fields of game
* @param ge An object of the type Graphic_engine
* @param game An object of the tye Game
* @return None
*/
void graphic_engine_paint_game(Graphic_engine *ge, Game *game,STATUS status);

/**
* @brief Not implemented yet
* @author Alvaro Lopez
*
* graphic_engine_write_command(Graphic_engine *ge, char *str) Not implemented yet
* @param ge An object of the type Graphic_engine
* @param str A pointer to char
* @return None
*/
void graphic_engine_write_command(Graphic_engine *ge, char *str);

#endif
