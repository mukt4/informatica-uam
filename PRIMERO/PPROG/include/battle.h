/** 
 * @brief It defines a battle
 * 
 * @file battle.h
 * @author Guillermo Hoyo
 * @version 2.0 
 * @date 05-5-2017
 * @copyright GNU Public License
 */

#ifndef BATTLE_H
#define BATTLE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "game.h"

typedef enum{
    ATTACK, DEFEND, DODGE, COUNTER, RUN /* A=0, D=1, DOG=2, C=3, R=4*/
} BATTLE;

/**
* @brief It simulates a battle
* @author Guillermo Hoyo Bravo
* 
* battle_on() It simulates a battle
* @param a structure type game
* @return STATUS
*/
STATUS battle_on(Game * game);

#endif