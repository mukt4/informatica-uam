/** 
 * @brief It defines a space
 * 
 * @file space.c
 * @author Profesores PPROG & Alvaro Lopez
 * @version 2.0 
 * @date 12-03-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "space.h"

/** The structure of Space will save information of the different characteristics of the space **/
struct _Space {
    Id id; /**Number of ID of space*/
    char name[WORD_SIZE + 1]; /**Name of space*/
    Id north;/**ID of north space*/
    Id south;/**ID of south space*/
    Id east;/**ID of east space*/
    Id west;/**ID of west space*/
    Id up;/**ID of up space*/
    Id down;/**ID of down space*/
    Set * objects;/**Set of objects in the space*/
    char gdesc[ROW][COLUMN+1];/**Graphic description of the space*/
    char description[SPACE_DESC +1]; /**Type char that contains the description of the space*/
    char iDesc[SPACE_DESC +1];/**Type char that contains the */
    BOOL ilumination;/**Type bool that said if is illuminate */
};

Space* space_create(Id id) {
    int i, j;
    
    Space *newSpace = NULL;

    if (id == NO_ID)
        return NULL;

    newSpace = (Space *) malloc(sizeof (Space));

    if (newSpace == NULL) {
        return NULL;
    }
    newSpace->id = id;

    newSpace->name[0] = '\0';
    newSpace->objects=set_create();
    newSpace->north = NO_ID;
    newSpace->south = NO_ID;
    newSpace->east = NO_ID;
    newSpace->west = NO_ID;
    newSpace->up = NO_ID;
    newSpace->down = NO_ID;
    newSpace->description[0]='\0';
    newSpace->iDesc[0] = '\0';
    newSpace->ilumination = FALSE;
    
    for (i=0; i<ROW; i++){
        for (j=0; j<COLUMN; j++)
            newSpace->gdesc[i][j] = '\0';
    }
    return newSpace;
}

STATUS space_destroy(Space* space) {;
    if (!space) {
        return ERROR;
    }
    set_destroy(space->objects);
    free(space);

    return OK;
}

STATUS space_set_name(Space* space, char* name) {
    if (!space || !name) {
        return ERROR;
    }

    if (!strcpy(space->name, name)) {
        return ERROR;
    }

    return OK;
}

STATUS space_set_north(Space* space, Id id) {
    if (!space || id == NO_ID) {
        return ERROR;
    }
    space->north = id;
    return OK;
}

STATUS space_set_south(Space* space, Id id) {
    if (!space || id == NO_ID) {
        return ERROR;
    }
    space->south = id;
    return OK;
}

STATUS space_set_east(Space* space, Id id) {
    if (!space || id == NO_ID) {
        return ERROR;
    }
    space->east = id;
    return OK;
}

STATUS space_set_west(Space* space, Id id) {
    if (!space || id == NO_ID) {
        return ERROR;
    }
    space->west = id;
    return OK;
}

STATUS space_set_up(Space* space, Id id) {
    if (!space || id == NO_ID) {
        return ERROR;
    }
    space->up = id;
    return OK;
}

STATUS space_set_down(Space* space, Id id) {
    if (!space || id == NO_ID) {
        return ERROR;
    }
    space->down = id;
    return OK;
}

STATUS space_add_object(Space* space, Id id) {
    if (!space) {
        return ERROR;
    }
    set_add(space->objects,id);
    return OK;
}

const char * space_get_name(Space* space) {
    if (!space) {
        return NULL;
    }
    return space->name;
}

Id space_get_id(Space* space) {
    if (!space) {
        return NO_ID;
    }
    return space->id;
}

Id space_get_north(Space* space) {
    if (!space) {
        return NO_ID;
    }
    return space->north;
}

Id space_get_south(Space* space) {
    if (!space) {
        return NO_ID;
    }
    return space->south;
}

Id space_get_east(Space* space) {
    if (!space) {
        return NO_ID;
    }
    return space->east;
}

Id space_get_west(Space* space) {
    if (!space) {
        return NO_ID;
    }
    return space->west;
}

Id space_get_up(Space* space) {
    if (!space) {
        return NO_ID;
    }
    return space->up;
}

Id space_get_down(Space* space) {
    if (!space) {
        return NO_ID;
    }
    return space->down;
}

Set* space_get_objects(Space* space) {
    if (!space) {
        return NULL;
    }
    return space->objects;
}

STATUS space_haveObject(Space *space, Id id){
   if(!space){
       return ERROR;
   }
   
   return set_checkId(space->objects,id);

}

STATUS space_print(Space* space) {
    Id idaux = NO_ID;
    int i;
    
    if (!space) {
        return ERROR;
    }
    fprintf(stdout, "--> Space (Id: %ld; Name: %s)\n", space->id, space->name);
    
    idaux = space_get_north(space);
    if (NO_ID != idaux) {
        fprintf(stdout, "---> North link: %ld.\n", idaux);
    } else {
        fprintf(stdout, "---> No north link.\n");
    }
    
    idaux = space_get_south(space);
    if (NO_ID != idaux) {
        fprintf(stdout, "---> South link: %ld.\n", idaux);
    } else {
        fprintf(stdout, "---> No south link.\n");
    }
    
    idaux = space_get_east(space);
    if (NO_ID != idaux) {
        fprintf(stdout, "---> East link: %ld.\n", idaux);
    } else {
        fprintf(stdout, "---> No east link.\n");
    }
    
    idaux = space_get_west(space);
    if (NO_ID != idaux) {
        fprintf(stdout, "---> West link: %ld.\n", idaux);
    } else {
        fprintf(stdout, "---> No west link.\n");
    }
    
    idaux = space_get_up(space);
    if (NO_ID != idaux) {
        fprintf(stdout, "---> Up link: %ld.\n", idaux);
    } else {
        fprintf(stdout, "---> No up link.\n");
    }
    
    idaux = space_get_down(space);
    if (NO_ID != idaux) {
        fprintf(stdout, "---> Down link: %ld.\n", idaux);
    } else {
        fprintf(stdout, "---> No down link.\n");
    }
    
    if (set_getNumId(space->objects)==0) {
        fprintf(stdout, "---> No object in the space.\n");
    } else {
        fprintf(stdout,  "---> %d objects in the space.\n", set_getNumId((space->objects)));
    }
    
    fprintf(stdout, "---> Description of the space: %s\n", space->description);
    
    for(i=0;i<ROW;i++){
        fprintf(stdout,"%s",space->gdesc[i]);
    }
    
    if (space->ilumination == TRUE)
        fprintf (stdout, "The space is iluminated");
    else
        fprintf (stdout, "The space isnt iluminated");

    return OK;
}

STATUS space_deleteObject(Space *space, Id id){
    if(!space)
        return ERROR;
    if(space_haveObject(space,id)==OK)
        if(set_delete(space_get_objects(space),id)==OK)
            return OK;
    return ERROR;
}

/*char* space_get_gdesc1(Space* space){
    if(!space)
        return NULL;
 
    return space->gdesc[0];
   
}
char* space_get_gdesc2(Space* space){
    if(!space)
        return NULL;
 
    return space->gdesc[1];
   
}
char* space_get_gdesc3(Space* space){
    if(!space)
        return NULL;
 
    return space->gdesc[2];
   
}*/

char * space_get_global_gdesc(Space *space, int position){
    if(!space || position >=ROW)
        return NULL;
    return space->gdesc[position];
}

STATUS space_set_global_gdesc(Space *space,int position, char *gdesc){
    if(!space || !gdesc || position>=ROW)   
        return ERROR;
    strcpy(space->gdesc[position],gdesc);
    return OK;
}
/*STATUS space_set_gdesc1(Space *space, char *gdesc){
    if(!space || !gdesc)
        return ERROR;
    strcpy(space->gdesc[0],gdesc);
    return OK;
}
STATUS space_set_gdesc2(Space *space, char *gdesc){
    if(!space || !gdesc)
        return ERROR;
    strcpy(space->gdesc[1],gdesc);
    return OK;
}

STATUS space_set_gdesc3(Space *space, char *gdesc){
    if(!space || !gdesc)
        return ERROR;
    strcpy(space->gdesc[2],gdesc);
    return OK;
}*/

STATUS space_set_description(Space * space, char * description){
    if(!space || !description)
        return ERROR;
        
    if (!strcpy(space->description, description)) {
        return ERROR;
    }
    
    return OK;
}

const char * space_get_description(Space * space){
    if(!space)
        return NULL;
        
    return space->description;
}

BOOL space_get_ilumination(Space * s){
    if (!s)
        return FALSE;
        
    return s->ilumination;    
}

STATUS space_set_ilumination (Space * s, BOOL ilumination){
    if (!s)
        return ERROR;
        
    s->ilumination  = ilumination;
    return OK;
}

STATUS space_set_iDesc(Space * space, char * iDesc){
    if(!space || !iDesc)
        return ERROR;
        
    if (!strcpy(space->iDesc, iDesc)) {
        return ERROR;
    }
    
    return OK;
}

const char * space_get_iDesc(Space * space){
    if(!space)
        return NULL;
        
    return space->iDesc;
}