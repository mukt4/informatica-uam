#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "enemy.h"
#include "game.h"

struct _Enemy{
    Id id; /**Number of ID of player*/
    char name[ENEMY_NAME + 1]; /**Type char that contains the name of the player*/
    Id space; /**Number of ID of the space were the player is located*/
    /*Inventory * inventory; Number of ID of the object the player carries*/
    Stats * stats; /**Stats of the enemy*/
};

Enemy * enemy_create(Id id){
    Enemy* enemy=NULL;
    
    if(id==NO_ID)
        return NULL;
    enemy=(Enemy*)malloc(sizeof (Enemy));
    if(!enemy)
        return NULL;
    enemy->id=id;
    enemy->name[0]='\0';
    enemy->space=NO_ID;
    /*enemy->inventory=inventory_create(MAX_OBJECTS);*/
    enemy->stats = stats_ini();
    return enemy;
}

STATUS enemy_destroy(Enemy * enemy){
    if(!enemy)
        return ERROR;
    /*inventory_destroy(enemy->inventory);*/
    stats_destroy(enemy->stats);
    free(enemy);
    return OK;
}
Id enemy_get_id(Enemy * enemy){
    if(!enemy)
        return NO_ID;
    return enemy->id;
}

STATUS enemy_set_name(Enemy * enemy, char* name){
    if(!enemy || !name)
        return ERROR;
    if (!strcpy(enemy->name, name)) {
        return ERROR;
    }
    return OK;
}
const char* enemy_get_name(Enemy * enemy){
    if(!enemy)
        return NULL;
    return enemy->name;
}

STATUS enemy_set_space(Enemy * enemy,Id space){
    if(!enemy)
        return ERROR;
    enemy->space=space;
    return OK;
}

Id enemy_get_space(Enemy * enemy){
    if(!enemy)
        return NO_ID;
    return enemy->space;
}

STATUS enemy_print(Enemy * enemy){
    if (!enemy) {
        return ERROR;
    }
    fprintf(stdout, "--> Enemy (Id: %ld; Name: %s; Space: %ld; )\n", enemy->id, enemy->name, enemy->space);
    /*inventory_print(enemy->inventory);*/
    return OK;
}

/*BOOL enemy_inventory_have_object(Enemy *enemy, Id object){
    if(!enemy){
        return FALSE;
    }
    if(set_checkId(inventory_get_ids(enemy->inventory),object)==OK)
        return TRUE;
    return FALSE;
}*/

/*Inventory* enemy_get_inventory(Enemy * enemy){
    if(!enemy)
        return NULL;
    return enemy->inventory;
}*/

/*STATUS enemy_add_object(Enemy * enemy,Id object){
    if(!enemy)
        return ERROR;
    inventory_add_id(enemy->inventory, object);
    return OK;
}*/

/*STATUS enemy_delete_object(Enemy * enemy,Id object){
    if(!enemy)
        return ERROR;
    inventory_delete_id(enemy->inventory, object);
    return OK;
}*/

/*Id *enemy_get_inventory_ids(Enemy * enemy){
    Id *ids;
    if(!enemy)
        return NULL;
    ids=set_get_ids(inventory_get_ids(enemy->inventory));
    return ids;
}*/

Stats * enemy_get_stats(Enemy * enemy){
    if (!enemy)
        return NULL;
        
    return enemy->stats;
}

int enemy_get_hp(Enemy * enemy){
    Stats * s = NULL;
    if (!enemy)
        return -1;
        
    s = enemy_get_stats(enemy);
    return stats_get_hp(s);
}

int enemy_get_ad(Enemy * enemy){
    Stats * s = NULL;
    if (!enemy)
        return -1;
        
    s = enemy_get_stats(enemy);
    return stats_get_ad(s);
}

int enemy_get_def(Enemy * enemy){
    Stats * s = NULL;
    if (!enemy)
        return -1;
        
    s = enemy_get_stats(enemy);
    return stats_get_def(s);
}

int enemy_get_speed(Enemy * enemy){
    Stats * s = NULL;
    if (!enemy)
        return -1;
        
    s = enemy_get_stats(enemy);
    return stats_get_speed(s);
}

STATUS enemy_set_hp(Enemy * enemy, int hp){
    Stats * s = NULL;
    if (!enemy)
        return ERROR;
    
    s = enemy_get_stats(enemy);
    stats_set_hp(s, hp);
    return OK;
}

STATUS enemy_set_ad(Enemy * enemy, int ad){
    Stats * s = NULL;
    if (!enemy)
        return ERROR;
    
    s = enemy_get_stats(enemy);
    stats_set_ad(s, ad);
    return OK;
}
STATUS enemy_set_def(Enemy * enemy, int def){
    Stats * s = NULL;
    if (!enemy)
        return ERROR;
    
    s = enemy_get_stats(enemy);
    stats_set_def(s, def);
    return OK;
}

STATUS enemy_set_speed(Enemy * enemy, int speed){
    Stats * s = NULL;
    if (!enemy)
        return ERROR;
    
    s = enemy_get_stats(enemy);
    stats_set_speed(s, speed);
    return OK;
}


