/** 
 * @brief It defines the inventory type
 * 
 * @file inventory.c
 * @author Alvaro Lopez, Tomas Higuera & Guillermo Hoyo
 * @version 1.0 
 * @date 03-04-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>

#include "inventory.h"

struct _Inventory{
    Set* ids;/**Ids de los objetos */
    int max_objects; /**Número máximo de ids que pueden haber */
};

Inventory * inventory_create(int max_objects){
    Inventory* inv = NULL;
    
    inv=(Inventory*)malloc(sizeof(Inventory));
    if (!inv)
        return NULL;
        
    inv->ids = set_create();
    if (!inv->ids)
        return NULL;
        
    inv->max_objects = max_objects;    
    
    return inv;
}

STATUS inventory_destroy(Inventory* inv){
    if (!inv)
        return ERROR;
    
    set_destroy(inv->ids);
    
    free(inv);
    return OK;
}

Set * inventory_get_ids(Inventory * inv){
    if(!inv)
        return NULL;
        
    return inv->ids;    
}

STATUS inventory_set_ids(Inventory * inv, Set * set){
    if (!inv || !set)
        return ERROR;
        
    inv->ids = set;
    return OK;
}

STATUS inventory_add_id(Inventory *inv, Id object){
    if(!inv)
        return ERROR;
    set_add(inv->ids, object);
    return OK;
}

STATUS inventory_delete_id(Inventory *inv, Id object){
    if(!inv)
        return ERROR;
    set_delete(inv->ids, object);
    return OK;
}

int inventory_get_max_objects(Inventory * inv){
    if (!inv)
        return -1;
    
    return inv->max_objects;    
}

STATUS inventory_set_max_objects(Inventory * inv, int max_objects){
    if (!inv)
        return ERROR;
        
    inv->max_objects = max_objects;
    return OK;
}

STATUS inventory_print(Inventory * inv){
    if (!inv)
        return ERROR;
        
    fprintf (stdout, "-->Inventory objects ids:");
    set_print(inv->ids);
    fprintf (stdout, "-->Inventory max_objects %d", inv->max_objects);
    
    return OK;
}

int inventory_get_numids(Inventory *inv){
    int numids;
    
    if(!inv)
        return ERROR;
        
    numids=set_getNumId(inv->ids);
    return numids;
}

