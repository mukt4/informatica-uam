/** 
 * @brief It defines a set
 * 
 * @file set.h
 * @author Tomas Higuera
 * @version 2.0 
 * @date 12-3-2017
 * @copyright GNU Public License
 */
#ifndef SET_H
#define SET_H


#define MAX_IDS 100 /*!< Max number of ids */

#include "types.h"

typedef struct _Set Set;

/**
* @brief It creates an object of the type Set
*
* set_create() It allocate memory for a type Set and initialises it fields
* @param None
* @return Set* It returns the Set initialised or NULL if an error ocurred
*/
Set* set_create();

/**
* @brief It destroy a Set
*
* set_destroy(Set* set) It liberates the memory that was allocated for 
* a type Set
* @param set An object of the type Set
* @return STATUS It returns OK if it destroyed the set correctly or ERROR 
* if an error ocurred
*/
STATUS set_destroy(Set *set);

/**
* @brief It add a id to the object Set
*
* set_add(Set * set, Id id) It add a id to the array of
* id in the object Set and increases the field numId
* @param set An object of the type Set
* @param id The id that is going to be added
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS set_add(Set *set, Id id);

/**
* @brief It deletes a id of the object Set
*
* set_delete(Set * set, Id id) It deletes a id from the array of
* id in the object Set and dicrease the field numId
* @param set An object of the type Set
* @param id The id that is going to be deleted
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS set_delete(Set *set, Id id);

/**
* @brief It prints the data of the Set
*
* set_print(Set * set) It prints the data of the fields of the type
* Set
* @param set An object of the type Set
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS set_print(Set *set );

/**
* @brief It get the numId of a Set
*
* set_getNumId(Set * set) It return number of id's saved in the type Set
* @param set An object of the type Set
* @return int It return a type int
*/
int set_getNumId(Set * set);

/**
* @brief It checks if and id is in the array of ids of set
* @author Tomas Higuera
*
* set_checkId(Set * set,Id id) It checks if an id is in the object Set and returns a STATUS 
* @param set An object of the type Set
* @param id The id that is going to be checked
* @return STATUS It returns OK if the id is in the set and ERROR if its not or an error ocurred
*/
STATUS set_checkId(Set * set, Id id);

/**
* @brief It gets an Id from a specific position in the array of ids
* @author Tomas Higuera
*
* set_get_id_at(Set * set,int position) It return a Id in a position from the array of ids 
* @param set An object of the type Set
* @param position The position of the id that is going to be returned
* @return Id It the returns the id of a postion or NO_ID if there is not an id savedin that position
*/
Id set_get_id_at(Set * set, int position);

/**
* @brief It return the array of ids of the set
* @author Tomas Higuera
*
* set_get_ids(Set * set) It return the array of ids of the set
* @param set An object of the type Set
* @return Id* It return an array of ids or NULL if an error ocurred
*/
Id* set_get_ids(Set * set);

#endif