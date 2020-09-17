/** 
 * @brief It defines the link type
 * 
 * @file link.c
 * @author Tomas Higuera
 * @version 1.0 
 * @date 03-04-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "link.h"

struct _Link{
    Id id; /**Id del link*/
    char name[LINK_NAME]; /**Nombre del link*/
    Id tied1;/**Id en la que te encuentras*/
    Id tied2; /**Id a la que vas*/
    BOOL status; /**Estado del link(Abierto o cerrado)*/
};
STATUS link_destroy(Link * link){
    
    if(!link)
        return ERROR;
    free(link);
    return OK;
    
}
Link * link_create(Id id){
    Link *link;
    if(id<0)
        return NULL;
    link=(Link*)malloc(sizeof(Link));
    if(!link)
        return NULL;
    link->id=id;
    link->name[0]='\0';
    link->tied1=NO_ID;
    link->tied2=NO_ID;
    link->status=0;
    return link;
}

STATUS link_set_id(Link *link,Id id){
    if(!link)
        return ERROR;
    if(id<0)
        return ERROR;
    link->id=id;
    return OK;
}

STATUS link_set_tied(Link *link,Id tied1, Id tied2){
    if(!link)
        return ERROR;
    if(tied1<0 || tied2<0)
        return ERROR;
    link->tied1=tied1;
    link->tied2=tied2;
    return OK;
}

STATUS link_set_name(Link *link,char *name){
    if(!link || !name)
        return ERROR;
    strcpy(link->name,name);
    return OK;
}

STATUS link_set_status(Link * link,BOOL status){
    if(!link)
        return ERROR;
    link->status=status;
    return OK;
}

Id link_get_id(Link * link){
    if(!link)
        return NO_ID;
    return link->id;
}

Id link_get_tied1(Link * link){
    if(!link)
        return NO_ID;
    return link->tied1;
}

Id link_get_tied2(Link *link){
    if(!link)
        return NO_ID;
    return link->tied2;
}

const char* link_get_name(Link * link){
    if(!link || !link->name)
        return NULL;
    return link->name;
}

BOOL link_get_status(Link *link){
    if(!link)
        return TRUE;
    return link->status;
}

STATUS link_print(Link *link){
    if(!link)
        return ERROR;
    fprintf(stdout,"Link --> ID: %d\n NAME: %s\n TIED1: %d\n TIED2: %d\n STATUS %d\n", (int)link->id, link->name, (int)link->tied1, (int)link->tied2, link->status);
    return OK;
}