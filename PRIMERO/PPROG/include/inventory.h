/** 
 * @brief It defines the inventory type
 * 
 * @file inventory.h
 * @author Alvaro Lopez, Tomas Higuera & Guillermo Hoyo
 * @version 1.0 
 * @date 03-04-2017
 * @copyright GNU Public License
 */

#ifndef INVENTORY_H
#define INVENTORY_H

#include "set.h"
#include "types.h"

typedef struct _Inventory Inventory;

/**
* @brief It creates an object of the type Inventory
* @author Guillermo Hoyo Bravo
* 
* inventory_create(int max_objects) It allocate memory for a type Inventory and initialises it fields
* @param max_objects The value of the field max_objects from inventory
* @return Inventory* It returns the Inventory initialised or NULL if an error ocurred
*/
Inventory* inventory_create(int max_objects);

/**
* @brief It destroy an object of the type Inventory
* @author Guillermo Hoyo Bravo
* 
* inventory_destroy(Inventory * inventory) It liberates the memory that was allocated for
* a type Inventory
* @param inv The inventory that is going to be deleted
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS inventory_destroy(Inventory *inventory);

/**
* @brief It get the field ids from a type inventory
* @author Tomas Higuera Viso
* 
* inventory_get_ids(Inventory * inventory) It returns the field ids from a type inventory
* @param inv The inventory from the one you want to know the field ids
* @return Set * It return the ids field (type Set)
*/
Set * inventory_get_ids(Inventory * inv);

/**
* @brief It set the field ids from an inventory
* @author Alvaro Lopez
* 
* inventory_set_ids(Inventory * inv, Set * set) It set the field ids from an inventory
* @param inv The inventory that will get the field ids modified
* @param set The type Set *  that is going to be save in the field ids
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS inventory_set_ids(Inventory * inv, Set * set);
    
/**
* @brief It get the field max_objects from a type inventory
* @author Tomas Higuera Viso
* 
* inventory_get_max_objects(Inventory * inventory) It returns the field max_objects from a type inventory
* @param inv The inventory from the one you want to know the field max_objects
* @return int It returns the field max_objects
*/
int inventory_get_max_objects(Inventory * inv);

/**
* @brief It set the field max_objects from an inventory
* @author Alvaro Lopez
* 
* inventory_set_max_objects(Inventory * inv, int max_objects) It set the field max_objects from an inventory
* @param inv The inventory that will get the field max_objects modified
* @param int The type int that is going to be save in the field max_objects
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS inventory_set_max_objects(Inventory * inv, int max_objects);

/**
* @brief It prints the data of the Inventory
* @author Guillermo Hoyo
* 
* inventory_print(Inventory * inventory) It prints the data of the fields of the type Inventory
* @param inv An object of the type Inventory
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS inventory_print(Inventory * inv);

/**
* @brief It adds an object to the object Inventory
* @author Alvaro Lopez
*
* inventory_add_id(Inventory *inv, Id object) It adds an Id (object) to the pointer of
* the type Set of the inventory
* @param inv An object of the type Inventory
* @param object The id that is going to be added
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS inventory_add_id(Inventory *inv, Id object);

/**
* @brief It deletes an object from the object Inventory
* @author Guillermo Hoyo
*
* inventory_delete_id(Inventory *inv, Id object) It deletes an Id (object) from the pointer of
* the type Set of the inventory
* @param inv An object of the type Inventory
* @param object The id that is going to be deleted
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS inventory_delete_id(Inventory *inv, Id object);

/**
* @brief It get the number of ids from an inventory
* @author Guillermo Hoyo
* 
* inventory_get_numids(Inventory *inv) It return the field numids from an inventory
* @param inv An object of the type Inventory
* @return int It returns the number of ids that are saved in the inventory
*/
int inventory_get_numids(Inventory *inv);
#endif