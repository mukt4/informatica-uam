/** 
 * @brief It defines the stats
 * 
 * @file stats.c
 * @author Guillermo Hoyo
 * @version 2.0 
 * @date 05-5-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stats.h"

struct _Stats{
    int hp;/**health points */
    int ad; /**attack damage*/
    int def; /**defense*/
    int speed; /**if you are faster, you attack first*/
};

Stats * stats_ini(){
    Stats * stats = NULL;
    stats = (Stats*)malloc(sizeof(Stats));
    if (!stats)
        return NULL;
    
    stats->hp = 0;
    stats->ad = 0;
    stats->def = 0;
    stats->speed = 0;
    
    return stats;
}

int stats_get_hp(Stats * stats){
    if (!stats)
        return -1;
        
    return stats->hp;
}

int stats_get_ad(Stats * stats){
    if (!stats)
        return -1;
        
    return stats->ad;
}

int stats_get_def(Stats * stats){
    if (!stats)
        return -1;
        
    return stats->def;
}

int stats_get_speed(Stats * stats){
    if (!stats)
        return -1;
        
    return stats->speed;
}

STATUS stats_set_hp(Stats * stats, int hp){
    if (!stats)
        return ERROR;
    
    stats->hp = hp;
    return OK;
}

STATUS stats_set_ad(Stats * stats, int ad){
    if (!stats)
        return ERROR;
    
    stats->ad = ad;
    return OK;
}

STATUS stats_set_def(Stats * stats, int def){
    if (!stats)
        return ERROR;
    
    stats->def = def;
    return OK;
}

STATUS stats_set_speed(Stats * stats, int speed){
    if (!stats)
        return ERROR;
    
    stats->speed = speed;
    return OK;
}




STATUS stats_add_hp(Stats * stats, int hp){
    if (!stats)
        return ERROR;
    
    stats->hp = stats->hp + hp;
    return OK;
}

STATUS stats_add_ad(Stats * stats, int ad){
    if (!stats)
        return ERROR;
    
    stats->ad = stats->ad + ad;
    return OK;
}

STATUS stats_add_def(Stats * stats, int def){
    if (!stats)
        return ERROR;
    
    stats->def = stats->def + def;
    return OK;
}

STATUS stats_add_speed(Stats * stats, int speed){
    if (!stats)
        return ERROR;
    
    stats->speed = stats->speed + speed;
    return OK;
}

STATUS stats_subtract_hp(Stats * stats, int hp){
    if (!stats)
        return ERROR;
    
    stats->hp = stats->hp - hp;
    return OK;
}

STATUS stats_subtract_ad(Stats * stats, int ad){
    if (!stats)
        return ERROR;
    
    stats->ad = stats->ad - ad;
    return OK;
}

STATUS stats_subtract_def(Stats * stats, int def){
    if (!stats)
        return ERROR;
    
    stats->def = stats->def - def;
    return OK;
}

STATUS stats_subtract_speed(Stats * stats, int speed){
    if (!stats)
        return ERROR;
    
    stats->speed = stats->speed - speed;
    return OK;
}

void stats_destroy(Stats * stats){
    if (!stats)
        return;
        
    stats->hp = -1;
    stats->ad = -1;
    stats->def = -1;
    stats->speed = -1;
    free(stats);
    return;
}