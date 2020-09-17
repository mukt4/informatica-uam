/** 
 * @brief It defines a space
 * 
 * @file space.h
 * @author Profesores PPROG & Alvaro Lopez
 * @version 2.0 
 * @date 12-3-2017
 * @copyright GNU Public License
 */

#ifndef SPACE_H
#define SPACE_H

#include "types.h"
#include "set.h"

typedef struct _Space Space;

#define MAX_SPACES 100 /*!< Max number of spaces */
#define FIRST_SPACE 1 /*!< First space */
#define ROW 13 /*!< Max number of rows of gdesc*/
#define COLUMN 60 /*!< Max number of columns of gdesc*/
#define SPACE_DESC 1000

/**
* @brief It create an object of the type Space
* @author Profesores PPROG & Alvaro Lopez
*
* space_create(Id id) It allocates memory for an object of the type Space
* and initialise his fields
* @param id The id of the space initialised
* @return Space* It returns the space initialised or it returns NULL if an 
* error ocurred
*/
Space* space_create(Id id);

/**
* @brief It destroy an object of the type Space
* @author Alvaro Lopez
*
* space_destroy(Space * space) It liberates the memory of an object of the
* type Space
* @param space The space that is going to be destroyed
* @return STATUS It returns OK if the memory has been liberated successfuly or
* it returns ERROR if an error ocurred
*/
STATUS space_destroy(Space* space);

/**
* @brief It get the id of an Space
* @author Profesores PPROG & Alvaro Lopez
*
* space_get_id(Space * space) It return the id of the field id in the type 
* Space
* @param space An object of the type Space
* @return Id It returns the id of the space or it return NO_ID if an error 
* ocurred
*/
Id space_get_id(Space* space);

/**
* @brief It set the name of an Space
* @author Profesores PPROG
*
* space_set_name(Space* space,char * name) It sets the field name of the 
* type Space
* @param space An object of the type Space
* @param name The name that will have the Space
* @return STATUS It returns OK if it sets the name correctly or ERROR if an 
* error ocurred
*/
STATUS space_set_name(Space* space, char* name);


/**
* @brief It get the name of an Space
* @author Profesores PPROG
*
* space_get_name(Space * space) It return the char* of the field name in the 
* type space
* @param space An object of the type Space
* @return const char* It returns the name of the space or NULL if an error 
* ocurred
*/
const char* space_get_name(Space* space);

/**
* @brief It set the id of the space at the north
* @author Profesores PPROG
*
* space_set_north(Spaxe * space,Id id) It sets the field north of the 
* type Space
* @param space An object of the type Space
* @param id The id of the space at the north
* @return STATUS It returns OK if it sets the id correctly or ERROR if an 
* error ocurred
*/
STATUS space_set_north(Space* space, Id id);

/**
* @brief It get the id of the space at the north
* @author Profesores PPROG
*
* space_get_north(Space * space) It return the id of the field north in the type 
* Space
* @param space An object of the type Space
* @return Id It returns the id of the north space or it return NO_ID if an error
* ocurred
*/
Id space_get_north(Space* space);

/**
* @brief It set the id of the space at the south
* @author Profesores PPROG
*
* space_set_south(Space * space,Id id) It sets the field south of the 
* type Space
* @param space An object of the type Space
* @param id The id of the space at the south
* @return STATUS It returns OK if it sets the id correctly or ERROR if an 
* error ocurred
*/
STATUS space_set_south(Space* space, Id id);

/**
* @brief It get the id of the space at the south
* @author Profesores PPROG
*
* space_get_south(Space * space) It return the id of the field south in the type 
* Space
* @param space An object of the type Space
* @return Id It returns the id of the south space or it return NO_ID if an error
* ocurred
*/
Id space_get_south(Space* space);

/**
* @brief It set the id of the space at the east
* @author Profesores PPROG
*
* space_set_east(Space * space,Id id) It sets the field east of the 
* type Space
* @param space An object of the type Space
* @param id The id of the space at the east
* @return STATUS It returns OK if it sets the id correctly or ERROR if an 
* error ocurred
*/
STATUS space_set_east(Space* space, Id id);

/**
* @brief It get the id of the space at the east
* @author Profesores PPROG
*
* space_get_east(Space * space) It return the id of the field east in the type 
* Space
* @param space An object of the type Space
* @return Id It returns the id of the east space or it return NO_ID if an error 
* ocurred
*/
Id space_get_east(Space* space);

/**
* @brief It set the id of the space at the west
* @author Profesores PPROG
* 
* space_set_west(Space * space,Id id) It sets the field west of the 
* type Space
* @param space An object of the type Space
* @param id The id of the space at the west
* @return STATUS It returns OK if it sets the id correctly or ERROR if an 
* error ocurred
*/
STATUS space_set_west(Space* space, Id id);

/**
* @brief It get the id of the space at the west
* @author Profesores PPROG
*
* space_get_west(Space * space) It return the id of the field west in the type 
* Space
* @param space An object of the type Space
* @return Id It returns the id of the west space or it return NO_ID if an error 
* ocurred
*/
Id space_get_west(Space* space);

/**
* @brief It set the id of an object at the space
* @author Profesores PPROG
*
* object_set_object(Space * space,Id id) It sets the field west of the 
* type Space
* @param space An object of the type Space
* @param id The id of the object
* @return STATUS It returns OK if it sets the id correctly or ERROR if an 
* error ocurred
*/
STATUS space_add_object(Space* space, Id id);

/**
* @brief It get the id of the object located in the space
* @author Alvaro Lopez
*
* space_get_object(Space * space) It return the id of the field object in the type 
* Space
* @param space An object of the type Space
* @return Id It returns the id of the object or it return NO_ID if an error 
* ocurred
*/
Set* space_get_objects(Space* space);

/**
* @brief It print the information of an object of the type Space
* @author Alvaro Lopez
* 
* space_print(Space * space) It prints the fields of an Space
* @param space An object of the type Space
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS space_print(Space* space);

/**
* @brief It deletes an object from a space
* @author Alvaro Lopez & Profesores PPROG
* 
* space_deleteObject(Space *space, Id id) checks if the object is in the space, and it deletes it
* @param space An object of the type Space
* @param id The id of the object
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS space_deleteObject(Space *space, Id id);

/**
* @brief It checks if theres the object with the id in the space
* @author Alvaro Lopez
* 
* space_haveObject(Space *space, Id id) checks if the object is in the space
* @param space An object of the type Space
* @param id The id of the object
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS space_haveObject(Space *space, Id id);

/**
* @brief It get the first row of the field gdesc
* 
* space_get_gdesc1(Space* space) It returns the first row of gdesc
* @param space An object of the type Space
* @return char* It returns the first row of the array gdesc
*/
/*char* space_get_gdesc1(Space* space);*/

/**
* @brief It get the second row of the field gdesc
* @author Alvaro Lopez
* 
* space_get_gdesc2(Space* space) It returns the second row of gdesc
* @param space An object of the type Space
* @return char* It returns the second row of the array gdesc
*/
/*char* space_get_gdesc2(Space* space);*/

/**
* @brief It get the third row of the field gdesc
* @author Alvaro Lopez
* 
* space_get_gdesc3(Space* space) It returns the third row of gdesc
* @param space An object of the type Space
* @return char* It returns the third row of the array gdesc
*/
/*char* space_get_gdesc3(Space* space);*/

/**
* @brief It sets the first row of gdesc
* @author Alvaro Lopez
* 
* space_set_gdesc1(Space* space, char *gdesc) It sets the first row of the array gdesc
* @param space An object of the type Space
* @param char* The string that is going to be copy in gdesc[0]
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
/*STATUS space_set_gdesc1(Space* space, char *gdesc);*/

/**
* @brief It sets the second row of gdesc
* @author Alvaro Lopez
* 
* space_set_gdesc1(Space* space, char *gdesc) It sets the second row of the array gdesc
* @param space An object of the type Space
* @param char* The string that is going to be copy in gdesc[1]
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
/*STATUS space_set_gdesc2(Space* space, char *gdesc);*/

/**
* @brief It sets the third row of gdesc
* @author Alvaro Lopez
* 
* space_set_gdesc1(Space* space, char *gdesc) It sets the third row of the array gdesc
* @param space An object of the type Space
* @param char* The string that is going to be copy in gdesc[2]
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
/*STATUS space_set_gdesc3(Space* space, char *gdesc);*/


/**
* @brief It sets the description from a type space
* @author Guillermo Hoyo
* 
* space_set_description(Space * space, char * description) It sets the description
* from a type space
* @param space An object of the type Space
* @param char* The string that is going to be copy in the field description
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS space_set_description(Space * space, char * description);

/**
* @brief It get the description from a type space
* @author Guillermo Hoyo
* 
* space_get_description(Space * space) It returns description from a space
* @param space An object of the type Space
* @return char* It returns the field description from the space
*/
const char* space_get_description(Space * space);

/**
* @brief It get the id of the space at the up
* @author Profesores PPROG
*
* space_get_up(Space * space) It return the id of the field up in the type 
* Space
* @param space An object of the type Space
* @return Id It returns the id of the up space or it return NO_ID if an error 
* ocurred
*/
Id space_get_up(Space* space); 

/**
* @brief It get the id of the space at the down
* @author Profesores PPROG
*
* space_get_down(Space * space) It return the id of the field down in the type 
* Space
* @param space An object of the type Space
* @return Id It returns the id of the down space or it return NO_ID if an error 
* ocurred
*/
Id space_get_down(Space* space);

/**
* @brief It set the id of the space at the up
* @author Profesores PPROG
* 
* space_set_up(Space * space,Id id) It sets the field up of the 
* type Space
* @param space An object of the type Space
* @param id The id of the space at the up
* @return STATUS It returns OK if it sets the id correctly or ERROR if an 
* error ocurred
*/
STATUS space_set_up(Space* space, Id id);

/**
* @brief It set the id of the space at the down
* @author Profesores PPROG
* 
* space_set_down(Space * space,Id id) It sets the field down of the 
* type Space
* @param space An object of the type Space
* @param id The id of the space at the down
* @return STATUS It returns OK if it sets the id correctly or ERROR if an 
* error ocurred
*/
STATUS space_set_down(Space* space, Id id);

/**
* @brief It sets the iDesc from a type space
* @author Guillermo Hoyo
* 
* space_set_iDesc(Space * space, char * iDesc) It sets the iDesc
* from a type space
* @param space An object of the type Space
* @param char* The string that is going to be copy in the field iDesc
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS space_set_iDesc(Space * space, char * iDesc);

/**
* @brief It get the iDesc from a type space
* @author Guillermo Hoyo
* 
* space_get_iDesc(Space * space) It returns iDesc from a space
* @param space An object of the type Space
* @return char* It returns the field iDesc from the space
*/
const char * space_get_iDesc(Space * space);

/**
* @brief It get the id of the ilumination located in the space
* @author Alvaro Lopez
*
* space_get_ilumination(Space * space) It return the id of the field ilumination in the type 
* Space
* @param space An ilumination of the type Space
* @return BOOL, TRUE or FALSE
* ocurred
*/
BOOL space_get_ilumination(Space * s);

/**
* @brief It sets the ilumination from a type space
* @author Guillermo Hoyo
* 
* space_set_ilumination(Space * space, BOOL ilumination) It sets the ilumination
* from a type space
* @param space An object of the type Space
* @param char* The string that is going to be copy in the field ilumination
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS space_set_ilumination (Space * s, BOOL ilumination);

/**
* @brief It gets the gDesc from a type space
* @author Tomas Higuera
* 
* space_get_gobal_gdesc(Space * space, int position) It gets the gDesc
* from a type space
* @param space An object of the type Space
* @param position The position of the gDesc in the array gDesc of space
* @return char* It returns gdesc of the space
*/
char * space_get_global_gdesc(Space *space, int position);

/**
* @brief It sets the gDesc from a type space
* @author Tomas Higuera
* 
* space_set_gobal_gdesc(Space * space, int position,char *gdesc) It sets the gDesc
* from a type space
* @param space An object of the type Space
* @param position The position of the gDesc in the array gDesc of space
* @param gdesc The string that is going to be setted
* @return STATUS It returns OK if the information has been setted successfuly
* or ERROR if an error ocurred
*/
STATUS space_set_global_gdesc(Space *space,int position, char *gdesc);

#endif
