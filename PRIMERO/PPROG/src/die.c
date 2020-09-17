/** 
 * @brief It defines the die type
 * 
 * This module include the functions that work with
 * the type die.
 * @file die.c
 * @author Alvaro Lopez
 * @version 1.0 
 * @date 12-03-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "die.h"
#include "game.h"

/** The structure of Die will save information of the different characteristics of the die **/
struct _Die{
    Id id; /**Number of ID of die*/
    int last_num; /**Last number obtained when roll the die*/
};

Die* die_create(Id id){
    Die *die =NULL;
    
    die = (Die*)malloc(sizeof(Die));
    if (!die)
        return NULL;
    srand(time(NULL));    
    die->id=id;
    die->last_num = 0;    
    return die;
}

STATUS die_destroy(Die* die){
    if(!die)
        return ERROR;
    free(die);
    return OK;
}


STATUS die_roll(Die* die){
    int num;
    if(!die)
        return ERROR;
    num = 1+ rand()% 6;
    die->last_num=num;
    return OK;
}


STATUS die_print(Die* die){
    if(!die)
        return ERROR;
    fprintf (stdout, "-->Die (Id: %ld; Last number: %i )\n", die->id, die->last_num);
    return OK;
}

int die_get_last_num(Die* die){
    if(!die)
        return -1;
    
    return die->last_num;    
}

Id die_get_id(Die * die){
    if(!die)
        return NO_ID;
        
    return die->id;    
}

STATUS die_roll_action(Die* die){
    int num;
    if(!die)
        return ERROR;
    num = 1+ rand()% 20;
    die->last_num=num;
    return OK;
}

STATUS die_action1(Die *die){
    int num;
    if(!die)
        return ERROR;
    num = 1+ rand()% 25;
    die->last_num=num;
    return OK;
}

STATUS die_action2(Die *die){
    int num;
    if(!die)
        return ERROR;
    num = 1+ rand()% 20;
    die->last_num=num;
    return OK;
}

STATUS die_action3(Die *die){
    int num;
    if(!die)
        return ERROR;
    num = 1+ rand()% 10;
    die->last_num=num;
    return OK;
}

STATUS die_roll_battle(Die* die){
    int num;
    if(!die)
        return ERROR;
    num = 0+ rand()% 3;
    die->last_num=num;
    return OK;
}

STATUS die_roll_dodge(Die* die){ 
    int num;
    if(!die)
        return ERROR;
    num = 0+ rand()% 1;
    die->last_num=num;
    return OK;
}

STATUS die_roll_counter(Die* die){
    int num;
    if(!die)
        return ERROR;
    num = 0+ rand()% 2;
    die->last_num=num;
    return OK;
}