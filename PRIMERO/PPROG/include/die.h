/** 
 * @brief It defines the die type
 * 
 * This module include the functions that work with
 * the type die.
 * @file die.h
 * @author Alvaro Lopez
 * @version 1.0 
 * @date 12-03-2017
 * @copyright GNU Public License
 */

#ifndef DIE_H
#define DIE_H

#include "types.h"

typedef struct _Die Die;

/**
* @brief It creates an object of the type Die
* @author Alvaro Lopez
* 
* die_create(Id id) It allocate memory for a type Die and initialises it fields
* @param id The id that is going to be added
* @return Die* It returns the Die initialised or NULL if an error ocurred
*/
Die* die_create(Id id);

/**
* @brief It destroy an object of the type Die
* @author Alvaro Lopez
* 
* die_destroy(Die *die) It liberates the memory that was allocated for
* a type Die
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS die_destroy(Die* die);

/**
* @brief It simulates the die roll
* @author Alvaro Lopez
* 
* die_roll(Die* die) It generates a random number between 1 and 6
* to simulate the die roll
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS die_roll(Die* die);

/**
* @brief It prints the data of the Die
* @author Alvaro Lopez
* 
* die_print(Die* die) It prints the data of the fields of the type
* Die
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS die_print(Die* die);

/**
* @brief It get the last number of a Die
* @author Alvaro Lopez
* 
* die_get_last_num(Die* die) It return the field last_num of a type Die
* @param die An object of the type Die
* @return int It returns the last_num of a Die
*/
int die_get_last_num(Die * die);

/**
* @brief It get the last number of a Die
* @author Guillermo Hoyo
* 
* die_get_id(Die* die) It return the id of a type Die
* @param die An object of the type Die
* @return Id It returns the id of the Die
*/
Id die_get_id(Die * die);

/**
* @brief It simulates the die roll
* @author Tomas Higuera
* 
* die_roll(Die* die) It generates a random number between 1 and 20
* to simulate the die roll
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS die_roll_action(Die* die);

/**
* @brief It simulates the die roll
* @author Tomas Higuera
* 
* die_roll(Die* die) It generates a random number between 1 and 25
* to simulate the die roll
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS die_action1(Die *die);

/**
* @brief It simulates the die roll
* @author Tomas Higuera
* 
* die_roll(Die* die) It generates a random number between 1 and 37
* to simulate the die roll
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS die_action2(Die *die);

/**
* @brief It simulates the die roll
* @author Tomas Higuera
* 
* die_roll(Die* die) It generates a random number between 1 and 10
* to simulate the die roll
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS die_action3(Die *die);

/**
* @brief It simulates the die roll
* @author Guillermo Hoyo
* 
* die_roll(Die* die) It generates a random number between 0 and 3
* to simulate the die roll
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS die_roll_battle(Die* die);

/**
* @brief It simulates the die roll
* @author Tomas Higuera
* 
* die_roll(Die* die) It generates a random number between 0 and 1
* to simulate the die roll
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS die_roll_dodge(Die* die);

/**
* @brief It simulates the die roll
* @author Tomas Higuera
* 
* die_roll(Die* die) It generates a random number between 0 and 2
* to simulate the die roll
* @param die An object of the type Die
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS die_roll_counter(Die* die);

#endif