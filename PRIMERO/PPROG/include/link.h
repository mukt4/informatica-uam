/** 
 * @brief It defines the link type
 * 
 * @file link.h
 * @author Tomas Higuera
 * @version 1.0 
 * @date 03-04-2017
 * @copyright GNU Public License
 */

#ifndef LINK_H
#define LINK_H

#define LINK_NAME 50 /*!< Max number of characters of link's name */

#include "types.h"

typedef struct _Link Link;

/**
* @brief It destroy a link
* @author Tomas Higuera
*
* link_destroy(Link * link) It liberates the memory of a link
* @param link The link that is going to be destroyed
* @return STATUS It returns OK if the memory has been liberated successfuly or
* it returns ERROR if an error ocurred
*/
STATUS link_destroy(Link * link);

/**
* @brief It create a link
* @author Tomas Higuera
*
* link_create(Id id) It allocates memory for a link and initialise his fields
* @param id The id of the link initialised
* @return Link* It returns the link initialised or it returns NULL if an 
* error ocurred
*/
Link * link_create(Id id);

/**
* @brief It set the id of a Link
* @author Tomas Higuera
*
* link_set_id(Link* link,Id id) It sets the field id of the 
* type Link
* @param link A link
* @param id The id that will have the Link
* @return STATUS It returns OK if it sets the id correctly or ERROR if an 
* error ocurred
*/
STATUS link_set_id(Link *link,Id id);

/**
* @brief It set the ids of both sides of Link
* @author Tomas Higuera
*
* link_set_tied(Link* link,Id tied1, Id tied2) It sets the fields tied of the 
* type Link
* @param tied1 The id that will have the Link tied1, the side where you are
* @param tied2 The id that will have the Link tied2, the side where you go
* @return STATUS It returns OK if it sets the ids correctly or ERROR if an 
* error ocurred
*/
STATUS link_set_tied(Link *link,Id tied1, Id tied2);

/**
* @brief It set the name of a Link
* @author Tomas Higuera
*
* link_set_name(Link* link,char * name) It sets the field name of the 
* type Link
* @param link A link
* @param name The name that will have the Link
* @return STATUS It returns OK if it sets the name correctly or ERROR if an 
* error ocurred
*/
STATUS link_set_name(Link *link,char *name);

/**
* @brief It set the status of a Link
* @author Tomas Higuera
*
* link_set_status(Link* link,BOOL status) It sets the field status of the 
* type Link
* @param link A link
* @param status The BOOL value that will have the Link status
* @return STATUS It returns OK if it sets the status correctly or ERROR if an 
* error ocurred
*/
STATUS link_set_status(Link * link,BOOL status);

/**
* @brief It get the id of a Link
* @author Tomas Higuera
*
* link_get_id(Link * link) It return the id of the field id in the type 
* Link
* @param link A Link
* @return Id It returns the id of the link or it return NO_ID if an error 
* ocurred
*/
Id link_get_id(Link * link);

/**
* @brief It get the tied1 of a Link,the side where you are
* @author Tomas Higuera
*
* link_get_tied1(Link * link) It return the tied1 of the field id in the type 
* Link
* @param link A Link
* @return Id It returns the tied1 of the link or it return NO_ID if an error 
* ocurred
*/
Id link_get_tied1(Link * link);

/**
* @brief It get the tied1 of a Link,the side where you go
* @author Tomas Higuera
*
* link_get_tied2(Link * link) It return the tied2 of the field id in the type 
* Link
* @param link A Link
* @return Id It returns the tied2 of the link or it return NO_ID if an error 
* ocurred
*/
Id link_get_tied2(Link *link);

/**
* @brief It get the name of a Link
* @author Tomas Higuera
*
* link_get_name(Link * link) It return the char* of the field name in the 
* type link
* @param link A link
* @return const char* It returns the name of the link or NULL if an error 
* ocurred
*/
const char* link_get_name(Link * link);

/**
* @brief It get the status of a Link
* @author Tomas Higuera
*
* link_get_status(Link * link) It return the BOOL value of the field status in the 
* type link
* @param link A link
* @return BOOL It returns the status of the link or NULL if an error 
* ocurred
*/
BOOL link_get_status(Link *link);

/**
* @brief It print the information of a link
* @author Tomas Higuera
* 
* link_print(Space * space) It prints the fields of a Link
* @param link A link
* @return STATUS It returns OK if the information has been printed successfuly
* or ERROR if an error ocurred
*/
STATUS link_print(Link *link);

#endif