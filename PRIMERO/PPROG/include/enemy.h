/** 
 * @brief It defines the enemy
 * 
 * @file enemy.h
 * @author Guillermo Hoyo
 * @version 2.0 
 * @date 05-5-2017
 * @copyright GNU Public License
 */

#ifndef ENEMY_H
#define ENEMY_H

#include "types.h"
#include "inventory.h"
#include "stats.h"

typedef struct _Enemy Enemy;

#define ENEMY_NAME 25 /*!< Max length of the  enemy name */

/**
* @brief It create an object of the type  enemy
* @author Guillermo Hoyo Bravo
* 
*  enemy_create(Id id) It allocates memory for an object of the type  enemy
* and initialise his fields
* @param id The id of the  enemy initialised
* @return  enemy* It returns the  enemy initialised or it returns NULL if an 
* error ocurred
*/
Enemy* enemy_create(Id id);

/**
* @brief It destroy an object of the type  enemy
* @author Guillermo Hoyo Bravo
* 
*  enemy_destroy( enemy*  enemy) It liberates the memory of an object of the
* type  enemy
* @param object The  enemy that is going to be destroyed
* @return STATUS It returns OK if the memory has been liberated successfuly or
* it returns ERROR if an error ocurred
*/
STATUS enemy_destroy(Enemy * enemy);

/**
* @brief It get the id of a  enemy
* @author Guillermo Hoyo Bravo
* 
*  enemy_get_id( enemy *  enemy) It return the id of the field id in the type 
*  enemy
* @param  enemy An object of the type  enemy
* @return Id It returns the id of the  enemy or it return NO_ID if an error 
* ocurred
*/
Id enemy_get_id(Enemy * enemy);

/**
* @brief It set the name of a  enemy
* @author Guillermo Hoyo Bravo
* 
*  enemy_set_name( enemy *  enemy,char * name) It sets the field name of the 
* type  enemy
* @param  enemy An object of the type  enemy
* @param name The name that will have the  enemy
* @return STATUS It returns OK if it sets the name correctly or ERROR if an 
* error ocurred
*/
STATUS enemy_set_name(Enemy * enemy, char* name);

/**
* @brief It get the name of the  enemy
* @author Guillermo Hoyo Bravo
* 
*  enemy_get_name( enemy *  enemy) It return the name of the  enemy 
* @param  enemy An object of the type  enemy
* @return const char* It returns the name of the  enemy or NULL if an error
* ocurred
*/
const char* enemy_get_name(Enemy *enemy);

/**
* @brief It print the information of an object of the type  enemy
* @author Guillermo Hoyo Bravo
* 
*  enemy_print( enemy *  enemy) It prints the fields of a  enemy
* @param  enemy An object of the type  enemy
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS enemy_print(Enemy * enemy);

/**
* @brief It adds an id to a type  enemy
* @author Guillermo Hoyo Bravo
* 
*  enemy_add_object( enemy *  enemy,Id object) It adds an id to a type  enemy
* @param  enemy An object of the type  enemy
* @param object the id of the object that is goimg to be added
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS enemy_add_object(Enemy * enemy,Id object);

/**
* @brief It deletes an id from a type  enemy
* @author Tomas Higuera
* 
*  enemy_delete_object( enemy *  enemy,Id object) It deletes an id from a type  enemy
* @param  enemy An object of the type  enemy
* @param object the id of the object that is goimg to be deleted
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS enemy_delete_object(Enemy * enemy,Id object);

/**
* @brief It set the id of the space where the  enemy is
* @author Guillermo Hoyo Bravo
* 
*  enemy_set_space( enemy *  enemy,Id space) It sets the field space of the 
* type  enemy
* @param  enemy An object of the type  enemy
* @param space The id of the space
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS enemy_set_space(Enemy * enemy,Id space);

/**
* @brief It get the inventory from a type  enemy
* @author Guillermo Hoyo Bravo
* 
*  enemy_get_inventory( enemy *  enemy) It returns the inventory from a type  enemy
* @param  enemy An object of the type  enemy
* @return Inventory * It returns the inventoryfrom the  enemy
*/
Inventory* enemy_get_inventory(Enemy * enemy);

/**
* @brief It get the id of the space where the  enemy is
* @author Guillermo Hoyo Bravo
* 
*  enemy_get_space( enemy *  enemy) It return the id of the field space in the
* type  enemy
* @param  enemy An object of the type  enemy
* @return Id It returns the id of the space the  enemy or it return NO_ID if an
* error ocurred
*/
Id enemy_get_space(Enemy * enemy);

/**
* @brief It tells if an specific id (from an object) is yn the inventory from the  enemy
* @author Tomas Higuera viso
* 
*  enemy_inventory_have_object( enemy * enemy, Id object)
* @param  enemy An object of the type  enemy
* @param object The Id of the object you want to check that it's int he inventory
* @return BOOL It returns a type BOOL that inform us if the 
* game has ended
*/
BOOL enemy_inventory_have_object(Enemy *enemy, Id object);

/**
* @brief It get the sets of ids from a type  enemy
* @author Tomas Higuera viso
* 
*  enemy_get_inventory_ids( enemy *  enemy) It returns the set of ids
* from the inventory of a type  enemy
* @param  enemy An object of the type  enemy
* @return Id * It returns the sets of ids of the  enemy's inventory
*/
Id *enemy_get_inventory_ids(Enemy * enemy);

/**
* @brief It get the field stats fron the enemy
* @author Guillermo Hoyo
* 
* enemy_get_stats(Enemy * enemy) It returns the stats from the enemy
* @param enemy An object of the type Enemy
* @return Stats * It returns the field Stats
*/
Stats * enemy_get_stats(Enemy * enemy);

/**
* @brief It get the hp from the enemy
* @author Guillermo Hoyo
* 
* enemy_get_hp(Enemy * enemy) It returns the hp from the enemy
* @param enemy An object of the type Enemy
* @return Stats * It returns the field hp
*/
int enemy_get_hp(Enemy * enemy);

/**
* @brief It get the ad from the enemy
* @author Guillermo Hoyo
* 
* enemy_get_ad(Enemy * enemy) It returns the ad from the enemy
* @param enemy An object of the type Enemy
* @return Stats * It returns the field ad
*/
int enemy_get_ad(Enemy * enemy);

/**
* @brief It get the def from the enemy
* @author Guillermo Hoyo
* 
* enemy_get_def(Enemy * enemy) It returns the def from the enemy
* @param enemy An object of the type Enemy
* @return Stats * It returns the field def
*/
int enemy_get_def(Enemy * enemy);

/**
* @brief It get the speed from the enemy
* @author Guillermo Hoyo
* 
* enemy_get_speed(Enemy * enemy) It returns the speed from the enemy
* @param enemy An object of the type Enemy
* @return Stats * It returns the field speed
*/
int enemy_get_speed(Enemy * enemy);

/**
* @brief It sets the hp from the enemy
* @author Guillermo Hoyo
* 
* enemy_set_hp(Enemy * enemy, int hp) It sets the hp from the enemy
* @param enemy An object of the type Enemy
* @param enemy An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS enemy_set_hp(Enemy * enemy, int hp);

/**
* @brief It sets the ad from the enemy
* @author Guillermo Hoyo
* 
* enemy_set_ad(Enemy * enemy, int ad) It sets the ad from the enemy
* @param enemy An object of the type Enemy
* @param enemy An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS enemy_set_ad(Enemy * enemy, int ad);

/**
* @brief It sets the def from the enemy
* @author Guillermo Hoyo
* 
* enemy_set_def(Enemy * enemy, int def) It sets the def from the enemy
* @param enemy An object of the type Enemy
* @param enemy An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS enemy_set_def(Enemy * enemy, int def);

/**
* @brief It sets the speed from the enemy
* @author Guillermo Hoyo
* 
* enemy_set_speed(Enemy * enemy, int speed) It sets the speed from the enemy
* @param enemy An object of the type Enemy
* @param enemy An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS enemy_set_speed(Enemy * enemy, int speed);

#endif