/** 
 * @brief It defines the stats
 * 
 * @file stats.h
 * @author Guillermo Hoyo
 * @version 2.0 
 * @date 05-5-2017
 * @copyright GNU Public License
 */

#ifndef STATS_H
#define STATS_H

#include "types.h"

typedef struct _Stats Stats;
/**
* @brief It create an object of the type Stats
* @author Alvaro Lopez
* stats_ini() It allocates memory for an object of the type stats
* and initialise his fields
* @param
* @return Stats* It returns the object initialised or it returns NULL if an 
* error ocurred
*/
Stats * stats_ini();

/**
* @brief It get the hp from the stats
* @author Alvaro Lopez
* 
* stats_get_hp(stats * stats) It returns the hp from the stats
* @param stats An object of the type stats
* @return Stats * It returns the field hp
*/
int stats_get_hp(Stats * stats);

/**
* @brief It get the ad from the stats
* @author Alvaro Lopez
* 
* stats_get_ad(stats * stats) It returns the ad from the stats
* @param stats An object of the type stats
* @return Stats * It returns the field ad
*/
int stats_get_ad(Stats * stats);

/**
* @brief It get the def from the stats
* @author Tomas Higuera
* 
* stats_get_def(stats * stats) It returns the def from the stats
* @param stats An object of the type stats
* @return Stats * It returns the field def
*/
int stats_get_def(Stats * stats);

/**
* @brief It get the speed from the stats
* @author Tomas Higuera
* 
* stats_get_speed(stats * stats) It returns the speed from the stats
* @param stats An object of the type stats
* @return Stats * It returns the field speed
*/
int stats_get_speed(Stats * stats);

/**
* @brief It sets the hp from the stats
* @author Tomas Higuera
* 
* stats_set_hp(stats * stats, int hp) It sets the hp from the stats
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_set_hp(Stats * stats, int hp);

/**
* @brief It sets the ad from the stats
* @author Guillermo Hoyo
* 
* stats_set_ad(stats * stats, int ad) It sets the ad from the stats
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_set_ad(Stats * stats, int ad);

/**
* @brief It sets the def from the stats
* @author Guillermo Hoyo
* 
* stats_set_def(stats * stats, int def) It sets the def from the stats
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_set_def(Stats * stats, int def);

/**
* @brief It sets the speed from the stats
* @author Guillermo Hoyo
* 
* stats_set_speed(stats * stats, int speed) It sets the speed from the stats
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_set_speed(Stats * stats, int speed);

/**
* @brief It adds the hp to the stats->hp
* @author Guillermo Hoyo
* 
* stats_set_hp(stats * stats, int hp) It adds the hp to the stats->hp
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be add
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_add_hp(Stats * stats, int hp);

/**
* @brief It adds the ad to the stats->ad
* @author Guillermo Hoyo
* 
* stats_set_ad(stats * stats, int ad) It adds the ad to the stats->ad
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be add
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_add_ad(Stats * stats, int ad);

/**
* @brief It adds the def to the stats->def
* @author Guillermo Hoyo
* 
* stats_set_def(stats * stats, int def) It adds the def to the stats->def
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be add
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_add_def(Stats * stats, int def);

/**
* @brief It adds the speed to the stats->speed
* @author Guillermo Hoyo
* 
* stats_set_speed(stats * stats, int speed) It adds the speed to the stats->speed
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be add
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_add_speed(Stats * stats, int speed);

/**
* @brief It subtracts the hp to the stats->hp
* @author Guillermo Hoyo
* 
* stats_set_hp(stats * stats, int hp) It subtracts the hp to the stats->hp
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be subtract
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_subtract_hp(Stats * stats, int hp);

/**
* @brief It subtracts the ad to the stats->ad
* @author Guillermo Hoyo
* 
* stats_set_ad(stats * stats, int ad) It subtracts the ad to the stats->ad
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be subtract
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_subtract_ad(Stats * stats, int ad);

/**
* @brief It subtracts the def to the stats->def
* @author Guillermo Hoyo
* 
* stats_set_def(stats * stats, int def) It subtracts the def to the stats->def
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be subtract
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_subtract_def(Stats * stats, int def);

/**
* @brief It subtracts the speed to the stats->speed
* @author Guillermo Hoyo
* 
* stats_set_speed(stats * stats, int speed) It subtracts the speed to the stats->speed
* @param stats An object of the type stats
* @param stats An object of the type int that is the value of the field that is going to be subtract
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS stats_subtract_speed(Stats * stats, int speed);

/**
* @brief It destroy a type stats
* @author Tomas Higuera
*
* stats_destroy(Stats * stats) It liberates the memory of a stats
* @param stats The stats that is going to be destroyed
* @return STATUS It returns OK if the memory has been liberated successfuly or
* it returns ERROR if an error ocurred
*/
void stats_destroy(Stats * stats);

#endif


