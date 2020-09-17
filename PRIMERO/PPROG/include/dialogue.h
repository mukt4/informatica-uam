/** 
 * @brief It implements the dialogues
 * 
 * @file dialogue.h
 * @author Alvaro Lopez
 * @version 1.0 
 * @date 5-05-2017 
 * @copyright GNU Public License
 */

#ifndef DIALOGUE_H
#define DIALOGUE_H

#include "types.h"

#define MAX_PHRASE 100

typedef struct _Dialogue Dialogue;

/**
* @brief It create a dialogue
* @author Alvaro Lopez
*
* dialogue_create() 
* @param 
* @return A dialogue type
*/
Dialogue *dialogue_create();

/**
* @brief It destroy a dialogue
* @author Alvaro Lopez
*
* dialogue_destroy(Dialogue *dialogue) 
* @param A dialogue type
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS dialogue_destroy(Dialogue *dialogue);

/**
* @brief It set a string in a dialogue
* @author Alvaro Lopez
*
* dialogue_set(Dialogue *dialogue, char * phrase) 
* @param A dialogue type
* @param A string char
* @return STATUS It returns a type STATUS that inform us if the 
* action has been done successfully
*/
STATUS dialogue_set(Dialogue *dialogue, char * phrase);

/**
* @brief It get a string of a dialogue
* @author Alvaro Lopez
*
* dialogue_get(Dialogue *dialogue) 
* @param A dialogue type
* @return A string char
*/
const char *dialogue_get(Dialogue *dialogue);

/**
* @brief It select the dialogue for quit command
* @author Alvaro Lopez
*
* dialogue_quit(Dialogue * dialogue, STATUS status, int code)
* @param A dialogue type for get it
* @param The STATUS of the command
* @param Int: Code for select the dialogue
* @return
*/
void dialogue_quit(Dialogue * dialogue, STATUS status, int code);

/**
* @brief It select the dialogue for go command
* @author Alvaro Lopez
*
* dialogue_go(Dialogue * dialogue, STATUS status, int code, char* complement)
* @param A dialogue type for get it
* @param The STATUS of the command
* @param Int: Code for select the dialogue
* @param: Char*: The complement of the command
* @return
*/
void dialogue_go(Dialogue * dialogue, STATUS status, int code, char* complement);

/**
* @brief It select the dialogue for pick command
* @author Alvaro Lopez
*
* dialogue_pick(Dialogue * dialogue, STATUS status, int code, char* complement)
* @param A dialogue type for get it
* @param The STATUS of the command
* @param Int: Code for select the dialogue
* @param: Char*: The complement of the command
* @return
*/
void dialogue_pick(Dialogue * dialogue, STATUS status, int code, char *complement);

/**
* @brief It select the dialogue for drop command
* @author Alvaro Lopez
*
* dialogue_drop(Dialogue * dialogue, STATUS status, int code, char* complement)
* @param A dialogue type for get it
* @param The STATUS of the command
* @param Int: Code for select the dialogue
* @param: Char*: The complement of the command
* @return
*/
void dialogue_drop(Dialogue * dialogue, STATUS status, int code, char *complement);

/**
* @brief It select the dialogue for drop command
* @author Alvaro Lopez
*
* dialogue_drop(Dialogue * dialogue, STATUS status, int code, char* complement)
* @param A dialogue type for get it
* @param The STATUS of the command
* @param Int: Code for select the dialogue
* @param: Char*: The complement of the command
* @return
*/
void dialogue_inspect(Dialogue * dialogue, STATUS status, int code, char *complement);

/**
* @brief It select the dialogue for turnoff command
* @author Alvaro Lopez
*
* dialogue_turnoff(Dialogue * dialogue, STATUS status, int code, char* complement)
* @param A dialogue type for get it
* @param The STATUS of the command
* @param Int: Code for select the dialogue
* @param: Char*: The complement of the command
* @return
*/
void dialogue_turnoff(Dialogue * dialogue, STATUS status, int code, char *complement);

/**
* @brief It select the dialogue for turnon command
* @author Alvaro Lopez
*
* dialogue_turnon(Dialogue * dialogue, STATUS status, int code, char* complement)
* @param A dialogue type for get it
* @param The STATUS of the command
* @param Int: Code for select the dialogue
* @param: Char*: The complement of the command
* @return
*/
void dialogue_turnon(Dialogue * dialogue, STATUS status, int code, char *complement);

/**
* @brief It select the dialogue for open command
* @author Alvaro Lopez
*
* dialogue_open(Dialogue * dialogue, STATUS status, int code, char* complement1, char*complement2)
* @param A dialogue type for get it
* @param The STATUS of the command
* @param Int: Code for select the dialogue
* @param Char*: The complement1 of the command
* @param Char*: The complement2 of the command
* @return
*/
void dialogue_open(Dialogue * dialogue, STATUS status, int code, char *complement1, char *complement2);

/**
* @brief It select the dialogue for save command
* @author Alvaro Lopez
*
* dialogue_save(Dialogue * dialogue, STATUS status, int code, char* complement)
* @param A dialogue type for get it
* @param The STATUS of the command
* @param Int: Code for select the dialogue
* @param: Char*: The complement of the command
* @return
*/
void dialogue_save(Dialogue * dialogue, STATUS status, int code);

/**
* @brief It select the dialogue for load command
* @author Alvaro Lopez
*
* dialogue_load(Dialogue * dialogue, STATUS status, int code, char* complement)
* @param A dialogue type for get it
* @param The STATUS of the command
* @param Int: Code for select the dialogue
* @param: Char*: The complement of the command
* @return
*/
void dialogue_load(Dialogue * dialogue, STATUS status, int code);

/**
* @brief It select the dialogue for unknown command
* @author Alvaro Lopez
*
* dialogue_unknown(Dialogue * dialogue, STATUS status, int code, char* complement)
* @param A dialogue type for get it
* @param The STATUS of the command
* @param Int: Code for select the dialogue
* @param: Char*: The complement of the command
* @return
*/
void dialogue_unknown(Dialogue * dialogue, STATUS status, int code);


#endif