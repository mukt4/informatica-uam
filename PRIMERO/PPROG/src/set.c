/** 
 * @brief It defines a set
 * 
 * @file set.c
 * @author Tomas Higuera
 * @version 2.0 
 * @date 12-3-2017
 * @copyright GNU Public License
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "set.h"

/** The struct set contains an array of ids*/
struct _Set{
    Id id[MAX_IDS];/**The array of object of the type Id*/
    int numId;/**The number of ids in the array*/
};

Set* set_create(){
    Set *set=NULL;
    int i = 0;
    set = (Set*)malloc(sizeof(Set));
    if(!set)
        return NULL;
        
    for (i=0; i<MAX_IDS; i++){
        set->id[i] = NO_ID;
    }
    set->numId=0;
    return set;    
}

STATUS set_destroy(Set *set){
    if(!set)
        return ERROR;
        
    free (set);
    return OK;
}

STATUS set_add(Set *set, Id id){
    int i;
    if(!set|| id<0)
        return ERROR;
    if((set->numId+1)==MAX_IDS)
        return ERROR;
    for(i=0;i<(set->numId);i++){
        if(set->id[i]==id)
            return OK;
    }
    set->id[set->numId]=id;
    set->numId=(set->numId) + 1;
    return OK;
}

STATUS set_delete(Set *set, Id id){
    int i;
    if(!set|| id<0)
        return ERROR;
        
    for (i=0; i<set->numId; i++){
        if(set->id[i] == id){
            for(;i<set->numId;i++)
                set->id[i] = set->id[i+1];
        }
    }
    set->numId--;
    
    return OK;
}

STATUS set_print(Set *set){
    int i;
    if (!set)
        return ERROR;
    
    for (i=0; i<(set->numId); i++){
        fprintf (stdout, "-->set (Id[%d]: %i) \n", i, (int) set->id[i]);
    }
    
    fprintf (stdout, "-->set (numId: %d) \n",set->numId);
    
    return OK;
}

int set_getNumId(Set * set){
    if(!set)
        return -1;
    
    return set->numId;
}

STATUS set_checkId(Set * set, Id id){
    int i;
    if(!set|| id<0)
        return ERROR;
    for(i=0;i<set_getNumId(set);i++){
        if(set->id[i]==id)
            return OK;
    }
    return ERROR;
}

Id set_get_id_at(Set * set, int position){
    if(!set)
        return NO_ID;
        
    return set->id[position];
}

Id* set_get_ids(Set * set){
    int i;
    Id *ids;
    if(!set)
        return NULL;
    ids=(Id *)malloc((set_getNumId(set))*sizeof(Id));
    if(!ids)
        return NULL;
    for(i=0;i<set_getNumId(set);i++)
        ids[i]=set_get_id_at(set,i);
    return ids;
}