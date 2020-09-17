/** 
 * @brief It implements the command interpreter
 * 
 * @file command.h
 * @author Profesores PPROG & Alvaro Lopez
 * @version 2.0 
 * @date 12-03-2017 
 * @copyright GNU Public License
 */

#ifndef COMMAND_H
#define COMMAND_H

#include "types.h"

typedef enum _Command {
  NO_CMD = -1, /*!< NO_CMD=-1 */
  UNKNOWN, /*!< UNKNOWN=0 */
  QUIT, /*!< QUIT=1 */
  PICK, /*!<  PICK=2 */
  DROP, /*!< DROP=3 */
  THROW, /*!< THROW=4  */
  INSPECT, /*!< INSPECT=5 */
  GO,  /*!< GO=6, GO */
  TURNON,
  TURNOFF,
  OPEN,
  SAVE,
  LOAD
}Command;

typedef struct _T_Command T_Command;

/**
* @brief It reads the command introduced from the keyboard
* @author Alvaro Lopez
* 
* get_user_input() It gets a string from the keyboard and recogniseif it is
* a command
* @param None
* @return T_Command It returns a T_Command depending on what the user introduced
* from the keyboard
*/
void get_user_input(T_Command* cmd);

/**
* @brief It return the complement1 of a command
* @author Tomas Higuera Viso
* 
* command_get_complement1 It returns a string of the complement1 or NULL if an error ocurred
* @param cmd Object of the type T_Command
* @return char* The string of the complemnet1 or NULL
*/
char * command_get_complement1(T_Command *cmd);

/**
* @brief It return the complement2 of a command
* @author Tomas Higuera Viso
* 
* command_get_complement2 It returns a string of the complement2 or NULL if an error ocurred
* @param cmd Object of the type T_Command
* @return char* The string of the complemnet2 or NULL
*/
char * command_get_complement2(T_Command *cmd);

/**
* @brief It return the input of a command
* @author Tomas Higuera Viso
* 
* command_get_input It returns a integer of the input or -1 if an error ocurred
* @param cmd Object of the type T_Command
* @return int The integer of the complemnet or -1
*/
int command_get_input(T_Command *cmd);

/**
* @brief It creates an object of the type T_Command
* @author Pablo Gutierrez
* 
* command_ini() It allocate memory for a type T_Command and initialises it fields
* @param nono
* @return T_Command* It returns the T_Command initialised or NULL if an error ocurred
*/
T_Command * command_ini();

/**
* @brief It destroy an object of the type T_Command
* @author Pablo Gutierrez
* 
* command_destroy It liberates the memory that was allocated for
* a type T_Command
* @param cmd An object of the type T_Command
* @return void
*/
void command_destroy(T_Command * cmd);

/**
* @brief It get the description of a command
* @author Pablo Gutierrez
* 
* command_get_description(T_Command *cmd) It return the description of a command
* @param cmd An object of the type T_Command
* @return char * A string with the description or NULL
*/
char * command_get_description(T_Command *cmd);

/**
* @brief It set the description of a command
* @author Pablo Gutierrez
* 
* command_set_description(T_Command *cmd,char *description) It set the description and return a type STATUS
* @param cmd An object of the type T_Command
* @param description  A string with the description
* @return STATUS It returns ERROR if an error ocurred or OK
*/
STATUS command_set_description(T_Command *cmd, char *description);

/**
* @brief It set the input of a command
* @author Pablo Gutierrez
* 
* command_set_input(T_Command *cmd,char *command) It set the input and return a type STATUS
* @param cmd An object of the type T_Command
* @param command  A string with the input
* @return STATUS It returns ERROR if an error ocurred or OK
*/
STATUS command_set_input(T_Command *cmd,char *command);

/**
* @brief It set the complement1 of a command
* @author Pablo Gutierrez
* 
* command_set_complement1(T_Command *cmd,char *complement) It set the complement1 and return a type STATUS
* @param cmd An object of the type T_Command
* @param command  A string with the complement
* @return STATUS It returns ERROR if an error ocurred or OK
*/
STATUS command_set_complement1(T_Command *cmd,char *complement);

/**
* @brief It set the complement2 of a command
* @author Pablo Gutierrez
* 
* command_set_complement2(T_Command *cmd,char *complement) It set the complement2 and return a type STATUS
* @param cmd An object of the type T_Command
* @param command  A string with the complement
* @return STATUS It returns ERROR if an error ocurred or OK
*/
STATUS command_set_complement2(T_Command *cmd,char *complement);

/**
* @brief It set the description_coment of a command
* @author Tomas HIguera
* 
* command_set_description_coment(T_Command *cmd,char *description_coment) It set the description_coment and return a type STATUS
* @param cmd An object of the type T_Command
* @param description_coment  A string with the description_coment
* @return STATUS It returns ERROR if an error ocurred or OK
*/
STATUS command_set_description_coment(T_Command *cmd, char *description_coment);

/**
* @brief It returns the description_coment of a command
* @author Tomas HIguera
* 
* command_get_description_coment(T_Command *cmd) It returns the description_coment
* @param cmd An object of the type T_Command
* @return char *  The description_coment
*/
char * command_get_description_coment(T_Command *cmd);

#endif
