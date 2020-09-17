/**
* @brief It defines the type object
*
* This module include the functions that work with
* the type object.
* @file object.c
* @author Guillermo Hoyo Bravo
* @version 2.0
* @date 12-3-2017
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "object.h"

/** The structure of Object will save information of the different characteristics of the object **/
struct _Object{
    char name[OBJECT_NAME + 1]; /**Type char that contains the name of the object*/
    Id id;/**Number of ID of object*/
    char description[OBJECT_DESC +1]; /**Type char that contains the description of the object*/
    char description2[OBJECT_DESC +1];/**Type char that contains the description 2 of the object*/
    BOOL movible;/**Type bool that  */
    BOOL movido;/**Type bool that */
    BOOL oculto;/**Type bool that */
    Id abre;/**Type Id that */
    /*master key*/
    BOOL ilumina;/**Type bool that */
    BOOL encendido;/**Type bool that */
    int hp;
    int ad;
    int def;
    int speed;
};

Object* object_create(Id id){
    Object * object=NULL;
    if(id==NO_ID)
        return NULL;
    object=(Object *)malloc(sizeof(Object));
    if(!object)
        return NULL;
    object->id=id;
    memset(object->name, 0,OBJECT_NAME);
    memset(object->description, 0,OBJECT_DESC);
    memset(object->description2, 0,OBJECT_DESC);
    object->movible = FALSE;
    object->movido = FALSE;
    object->oculto = FALSE;
    object->abre = NO_ID;
    object->ilumina = FALSE;
    object->encendido = FALSE;
    object->hp = 0;
    object->ad = 0;
    object->def = 0;
    object->speed = 0;
    
    return object;
}

STATUS object_destroy(Object * object){
    if(!object)
        return ERROR;
    free(object);
    return OK;
}

Id object_get_id(Object * object){
    if(!object)
        return NO_ID;
    return object->id;
}

STATUS object_set_name(Object * object, char* name){
    if(!object || !name)
        return ERROR;
    if (!strcpy(object->name, name)) {
        return ERROR;
    }
    return OK;
}

const char* object_get_name(Object * object){
    if(!object)
        return NULL;
        
    return object->name;
}

STATUS object_print(Object * object){
    if (!object) {
        return ERROR;
    }

    fprintf(stdout, "--> Object (Id: %ld; \nName: %s; \nDescription: %s;)\n", object->id, object->name, object->description);
    if(object->movible==TRUE)
        fprintf(stdout,"Object is movable");
    else
        fprintf(stdout,"Object is not movable");
    
    if(object->movido==TRUE)
        fprintf(stdout,"Object is moved");
    else
        fprintf(stdout,"Object is not moved");
    
    if(object->oculto==TRUE)
        fprintf(stdout,"Object is hidden");
    else
        fprintf(stdout,"Object is not hidden");
        
    if(object->abre!= NO_ID)
        fprintf(stdout,"Object can open %ld link", object->abre);
    else
        fprintf(stdout,"Object can not open links");
        
    if(object->ilumina==TRUE)
        fprintf(stdout,"Object can illuminate");
    else
        fprintf(stdout,"Object can not illuminate");
    
    if(object->encendido==TRUE)
        fprintf(stdout,"Object is switched on");
    else
        fprintf(stdout,"Object is switched off");
    
    return OK;
}

STATUS object_set_description(Object * object, char * description){
    if(!object || !description)
        return ERROR;
        
    if (!strcpy(object->description, description)) {
        return ERROR;
    }
    
    return OK;
}

const char* object_get_description(Object * object){
    if(!object)
        return NULL;
        
    return object->description;
}

STATUS object_set_movible(Object * object, BOOL movible){
    
    if(!object)
        return ERROR;
    
    object->movible = movible;
    return OK;
}

BOOL object_get_movible(Object * object){
    if(!object)
        return FALSE;
    
    return object->movible;
} 

STATUS object_set_movido(Object * object, BOOL movido){
    
    if(!object)
        return ERROR;
    if(object->movible== FALSE)
        return ERROR;
    
    object->movido = movido;
    return OK;
}

BOOL object_get_movido(Object * object){
    if(!object)
        return FALSE;
    
    return object->movido;
} 

STATUS object_set_oculto(Object * object, BOOL oculto){
    if(!object)
        return ERROR;
    
    object->oculto = oculto;
    return OK;
}

BOOL object_get_oculto(Object * object){
    if(!object)
        return FALSE;
    
    return object->oculto;
} 

STATUS object_set_abre(Object * object, Id abre){
    
    if(!object)
        return ERROR;
    
    object->abre = abre;
    return OK;
}

Id object_get_abre(Object * object){
    if(!object)
        return FALSE;
    
    return object->abre;
} 

STATUS object_set_ilumina(Object * object, BOOL ilumina){
    
    if(!object)
        return ERROR;
    
    object->ilumina = ilumina;
    return OK;
}

BOOL object_get_ilumina(Object * object){
    if(!object)
        return FALSE;
    
    return object->ilumina;
} 

STATUS object_set_encendido(Object * object, BOOL encendido){
    
    if(!object)
        return ERROR;
    if(object->ilumina== FALSE)
        return ERROR;
    
    object->encendido = encendido;
    return OK;
}

BOOL object_get_encendido(Object * object){
    if(!object)
        return FALSE;
    
    return object->encendido;
} 

STATUS object_set_description2(Object * object, char * description){
    if(!object || !description)
        return ERROR;
        
    if (!strcpy(object->description2, description)) {
        return ERROR;
    }
    
    return OK;
}

const char* object_get_description2(Object * object){
    if(!object)
        return NULL;
        
    return object->description2;
}

int object_get_hp(Object * object){
    if (!object)
        return -1;
        
    return object->hp;
}

int object_get_ad(Object * object){
    if (!object)
        return -1;
        
    return object->ad;
}

int object_get_def(Object * object){
    if (!object)
        return -1;
        
    return object->def;
}

int object_get_speed(Object * object){
    if (!object)
        return -1;
        
    return object->speed;
}


STATUS object_set_hp(Object * object, int hp){
    if (!object)
        return ERROR;
    
    object->hp = hp;
    return OK;
}

STATUS object_set_ad(Object * object, int ad){
    if (!object)
        return ERROR;
    
    object->ad = ad;
    return OK;
}

STATUS object_set_def(Object * object, int def){
    if (!object)
        return ERROR;
    
    object->def = def;
    return OK;
}

STATUS object_set_speed(Object * object, int speed){
    if (!object)
        return ERROR;
    
    object->speed = speed;
    return OK;
}

