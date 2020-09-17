/**
* @brief It defines the type object
*
* This module include the functions that work with
* the type object.
* @file object.h
* @author Guillermo Hoyo Bravo
* @version 2.0
* @date 12-3-2017
*/

#ifndef OBJECT_H
#define OBJECT_H

#include "types.h"

typedef struct _Object Object;

#define OBJECT_NAME 25 /*!< Max length of the object name */
#define OBJECT_DESC 400
/**
* @brief It create an object of the type Object
* @author Guillermo Hoyo Bravo
* object_create(Id id) It allocates memory for an object of the type object
* and initialise his fields
* @param id The id of the object initialised
* @return Object* It returns the object initialised or it returns NULL if an 
* error ocurred
*/
Object* object_create(Id id);

/**
* @brief It destroy an object of the type Object
* @author Guillermo Hoyo Bravo
* object_destroy(Object * object) It liberates the memory of an object of the
* type object
* @param object The object that is going to be destroyed
* @return STATUS It returns OK if the memory has been liberated successfuly or
* it returns ERROR if an error ocurred
*/
STATUS object_destroy(Object * object);

/**
* @brief It get the id of an Object
* @author Guillermo Hoyo Bravo
* object_get_id(Object * object) It return the id of the field id in the type 
* object
* @param object An object of the type Object
* @return Id It returns the id of the object or it return NO_ID if an error 
* ocurred
*/
Id object_get_id(Object * object);

/**
* @brief It set the name of an object
* @author Guillermo Hoyo Bravo
* object_set_name(Object * object,char * name) It sets the field name of the 
* type object
* @param object An object of the type object
* @param name The name that will have the object
* @return STATUS It returns OK if it sets the name correctly or ERROR if an 
* error ocurred
*/
STATUS object_set_name(Object * object, char* name);

/**
* @brief It get the name of an Object
* @author Guillermo Hoyo Bravo
* object_get_name(Object * object) It return the char* of the field name in the 
* type object
* @param object An object of the type Object
* @return const char* It returns the name of the object or NULL if an error 
* ocurred
*/
const char* object_get_name(Object * object);

/**
* @brief It print the information of an object of the type object
* @author Guillermo Hoyo Bravo
* object_print(Object * object) It prints the fields of an Object
* @param object An object of the type Object
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS object_print(Object * object);

/**
* @brief It sets the description from a type object
* @author Guillermo Hoyo
* 
* object_set_description(Object * object, char * description) It sets the description
* from a type object
* @param object An object of the type Object
* @param char* The string that is going to be copy in the field description
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS object_set_description(Object * object, char * description);

/**
* @brief It get the description from a type object
* @author Guillermo Hoyo
* 
* object_get_description(Object * object) It returns description from an object
* @param object An object of the type Object
* @return char* It returns the field description from the object
*/
const char* object_get_description(Object * object);
    
/**
* @brief It sets the movible from a type object
* @author Guillermo Hoyo
* 
* object_set_movible(Object * object, BOOL movible) It sets the movible
* from a type object
* @param object An object of the type Object
* @param BOOL it sets the field movible to TRUE or FALSE
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS object_set_movible(Object * object, BOOL movible);

/**
* @brief It get the movible from a type object
* @author Guillermo Hoyo
* 
* object_get_movible(Object * object) It returns movible from an object
* @param object An object of the type Object
* @return BOOL It returns a type BOOL that inform us if the 
* game has ended
*/
BOOL object_get_movible(Object * object);

/**
* @brief It sets the movido from a type object
* @author Guillermo Hoyo
* 
* object_set_movido(Object * object, BOOL movido) It sets the movido
* from a type object
* @param object An object of the type Object
* @param BOOL it sets the field movido to TRUE or FALSE
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS object_set_movido(Object * object, BOOL movido);

/**
* @brief It get the movido from a type object
* @author Guillermo Hoyo
* 
* object_get_movido(Object * object) It returns movido from an object
* @param object An object of the type Object
* @return BOOL It returns a type BOOL that inform us if the 
* game has ended
*/
BOOL object_get_movido(Object * object);

/**
* @brief It sets the oculto from a type object
* @author Guillermo Hoyo
* 
* object_set_oculto(Object * object, BOOL oculto) It sets the oculto
* from a type object
* @param object An object of the type Object
* @param BOOL it sets the field oculto to TRUE or FALSE
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS object_set_oculto(Object * object, BOOL oculto);

/**
* @brief It get the oculto from a type object
* @author Guillermo Hoyo
* 
* object_get_oculto(Object * object) It returns oculto from an object
* @param object An object of the type Object
* @return BOOL It returns a type BOOL that inform us if the 
* game has ended
*/
BOOL object_get_oculto(Object * object);

/**
* @brief It sets the abre from a type object
* @author Guillermo Hoyo
* 
* object_set_abre(Object * object, Id abre) It sets the abre
* from a type object
* @param object An object of the type Object
* @param Id it is the id of the link that the object opens
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS object_set_abre(Object * object, Id abre);

/**
* @brief It get the abre from a type object
* @author Guillermo Hoyo
* 
* object_get_abre(Object * object) It returns abre from an object
* @param object An object of the type Object
* @return BOOL It returns a type BOOL that inform us if the 
* game has ended
*/
Id object_get_abre(Object * object);

/**
* @brief It sets the ilumina from a type object
* @author Guillermo Hoyo
* 
* object_set_ilumina(Object * object, BOOL ilumina) It sets the ilumina
* from a type object
* @param object An object of the type Object
* @param BOOL it sets the field ilumina to TRUE or FALSE
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS object_set_ilumina(Object * object, BOOL ilumina);

/**
* @brief It get the ilumina from a type object
* @author Guillermo Hoyo
* 
* object_get_ilumina(Object * object) It returns ilumina from an object
* @param object An object of the type Object
* @return BOOL It returns a type BOOL that inform us if the 
* game has ended
*/
BOOL object_get_ilumina(Object * object);

/**
* @brief It sets the encendido from a type object
* @author Guillermo Hoyo
* 
* object_set_encendido(Object * object, BOOL encendido) It sets the encendido
* from a type object
* @param object An object of the type Object
* @param BOOL it sets the field encendido to TRUE or FALSE
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS object_set_encendido(Object * object, BOOL encendido);

/**
* @brief It get the encendido from a type object
* @author Guillermo Hoyo
* 
* object_get_encendido(Object * object) It returns encendido from an object
* @param object An object of the type Object
* @return BOOL It returns a type BOOL that inform us if the 
* game has ended
*/
BOOL object_get_encendido(Object * encendido);

/**
* @brief It sets the description2 from a type object
* @author Guillermo Hoyo
* 
* object_set_description2(Object * object, char * description) It sets the description2
* from a type object
* @param object An object of the type Object
* @param char* The string that is going to be copy in the field description2
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS object_set_description2(Object * object, char * description);

/**
* @brief It get the description2 from a type object
* @author Guillermo Hoyo
* 
* object_get_description2(Object * object) It returns description2 from an object
* @param object An object of the type Object
* @return char* It returns the field description2 from the object
*/
const char* object_get_description2(Object * object);

/**
* @brief It get the hp from the object
* @author Guillermo Hoyo
* 
* object_get_hp(Object * object) It returns the hp from the object
* @param object An object of the type Object
* @return Stats * It returns the field hp
*/
int object_get_hp(Object * object);

/**
* @brief It get the ad from the object
* @author Guillermo Hoyo
* 
* object_get_ad(Object * object) It returns the ad from the object
* @param object An object of the type Object
* @return Stats * It returns the field ad
*/
int object_get_ad(Object * object);

/**
* @brief It get the def from the object
* @author Guillermo Hoyo
* 
* object_get_def(Object * object) It returns the def from the object
* @param object An object of the type Object
* @return Stats * It returns the field def
*/
int object_get_def(Object * object);

/**
* @brief It get the speed from the object
* @author Guillermo Hoyo
* 
* object_get_speed(Object * object) It returns the speed from the object
* @param object An object of the type Object
* @return Stats * It returns the field speed
*/
int object_get_speed(Object * object);

/**
* @brief It sets the hp from the object
* @author Guillermo Hoyo
* 
* object_set_hp(object * object, int hp) It sets the hp from the object
* @param object An object of the type object
* @param object An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS object_set_hp(Object * object, int hp);

/**
* @brief It sets the ad from the object
* @author Guillermo Hoyo
* 
* object_set_ad(object * object, int ad) It sets the ad from the object
* @param object An object of the type object
* @param object An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS object_set_ad(Object * object, int ad);

/**
* @brief It sets the def from the object
* @author Guillermo Hoyo
* 
* object_set_def(object * object, int def) It sets the def from the object
* @param object An object of the type object
* @param object An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS object_set_def(Object * object, int def);

/**
* @brief It sets the speed from the object
* @author Guillermo Hoyo
* 
* object_set_speed(object * object, int speed) It sets the speed from the object
* @param object An object of the type object
* @param object An object of the type int that is the value of the field that is going to be set
* @return STATUS It returns OK if it sets the space correctly or ERROR if an 
* error ocurred
*/
STATUS object_set_speed(Object * object, int speed);

#endif