/** 
 * @brief It implements the command interpreter
 * 
 * @file command.c
 * @author Profesores PPROG & Alvaro Lopez
 * @version 2.0 
 * @date 12-03-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "command.h"

#define CMD_LENGHT 30 /*!< Length of the string of the input */
#define N_CMD 13 /*!< Number of psosible commands */
#define CMD_COMPLEMENT 30 /*!< Length of the string of the complement */
#define MAX_INPUT 50 /*!< Length of the string of the full command */
#define MAX_DESCRIPTION 700 /*!< Length of the string of the description */
#define MAX_DESCRIPTION_COMENT 700

char * cmd_to_str[N_CMD] = {"No command", "Unknown", "Quit", "Pick", "Drop","Throw","Inspect","Go","TurnOn","TurnOff", "Open", "Save", "Load"};

struct _T_Command{
    Command cmd; /**The command*/
    char complement1[CMD_COMPLEMENT]; /**The complement1 of the command*/
    char complement2[CMD_COMPLEMENT]; /**The complement2 of the command*/
    char description[MAX_DESCRIPTION]; /**The description of the command that is going to be painted*/
    char description_coment[MAX_DESCRIPTION_COMENT]; /**The coment of the command*/
};

void get_user_input(T_Command *cmd){
    char input[CMD_LENGHT] = "";
    char complement[CMD_COMPLEMENT]="";
    char complement2[CMD_COMPLEMENT]="";
    char aux[CMD_COMPLEMENT]="";
    char command[MAX_INPUT]="";
    int i=0;
    int j=0;
    fgets(command, MAX_INPUT, stdin);
    if (strlen(command) > 0){
        while(command[i]!=' ' && command[i]!='\n' && command[i]!='\0'){
            input[i]=command[i];
            i++;
        }
        
        i++;
        while(command[i]!='\n' && command[i]!='\0' && command[i]!=' '){
            complement[j]=command[i];
            i++;
            j++;
        }
        i++;
        j=0;
        while(command[i]!='\n' && command[i]!='\0'  && command[i]!=' '){
            aux[j]=command[i];
            i++;
            j++;
        }
        i++;
        j=0;
        while(command[i]!='\n' && command[i]!='\0'  && command[i]!=' '){
            complement2[j]=command[i];
            i++;
            j++;
        }
        
        if (!strcmp(input, "q") || !strcmp(input, "quit")){
            cmd->cmd = QUIT;
            strcpy(cmd->complement1,"\0");
            strcpy(cmd->complement2,"\0");
        }else if (!strcmp(input, "p") || !strcmp(input, "pick")){
            cmd->cmd = PICK;
            strcpy(cmd->complement1,complement);
            strcpy(cmd->complement2, "\0");
        }else if (!strcmp(input, "d") || !strcmp(input, "drop")){
            cmd->cmd = DROP;
            strcpy(cmd->complement1,complement);
            strcpy(cmd->complement2, "\0");
        }else if (!strcmp(input, "t") || !strcmp(input,"throw")){
            cmd->cmd = THROW;
            strcpy(cmd->complement1,complement);
            strcpy(cmd->complement2, "\0");
        }else if (!strcmp(input, "i") || !strcmp(input,"inspect")){
            cmd->cmd = INSPECT;
            strcpy(cmd->complement1,complement);
            strcpy(cmd->complement2, "\0");
        }else if (!strcmp(input, "g") || !strcmp(input,"go")){
            cmd->cmd = GO;
            strcpy(cmd->complement1,complement);
            strcpy(cmd->complement2, "\0");
        }else if (!strcmp(input, "s") || !strcmp(input,"save")){
            cmd->cmd = SAVE;
            strcpy(cmd->complement1,complement);
            strcpy(cmd->complement2, "\0");
        }else if (!strcmp(input, "l") || !strcmp(input,"load")){
            cmd->cmd = LOAD;
            strcpy(cmd->complement1,complement);
            strcpy(cmd->complement2, "\0");
        }else if (!strcmp(input,"turnon")){
            cmd->cmd = TURNON;
            strcpy(cmd->complement1,complement);
            strcpy(cmd->complement2, "\0");
        }else if (!strcmp(input,"turnoff")){
            cmd->cmd = TURNOFF;
            strcpy(cmd->complement1,complement);
            strcpy(cmd->complement2, "\0");
        }else if(!strcmp(input,"o") || !strcmp(input, "open")){
            if(strcmp(aux, "with")!=0){
                cmd->cmd = UNKNOWN;
                strcpy(cmd->complement1, "\0");
                strcpy(cmd->complement2, "\0");
            }
            cmd->cmd = OPEN;
            strcpy(cmd->complement1, complement);
            strcpy(cmd->complement2, complement2);
        }else{
            cmd->cmd = UNKNOWN;
            strcpy(cmd->complement1,"\0");
            strcpy(cmd->complement2, "\0");
        }
    }
    return;
}

char * command_get_complement1(T_Command *cmd){
    if(!cmd)
        return NULL;
    return cmd->complement1;
}

char * command_get_complement2(T_Command *cmd){
    if(!cmd)
        return NULL;
    return cmd->complement2;
}

int command_get_input(T_Command *cmd){
    if(!cmd)
        return -1;
    return cmd->cmd;
}

T_Command * command_ini(){
    T_Command *cmd=NULL;
    cmd = (T_Command *)malloc(sizeof(T_Command));
    if(!cmd)
        return NULL;
    cmd->cmd=UNKNOWN;
    strcpy(cmd->complement1,"\0");
    strcpy(cmd->complement2,"\0");
    strcpy(cmd->description,"\0");
    strcpy(cmd->description_coment,"\0");
    return cmd;
}

void command_destroy(T_Command * cmd){
    if (!cmd)
        return;
    free(cmd);
}

char * command_get_description(T_Command *cmd){
    if(!cmd)
        return NULL;
  return cmd->description;
}

char * command_get_description_coment(T_Command *cmd){
    if(!cmd)
        return NULL;
  return cmd->description_coment;
}

STATUS command_set_description(T_Command *cmd, char *description){
    if(!cmd)
        return ERROR;
    strcpy(cmd->description,description);
    return OK;
}

STATUS command_set_description_coment(T_Command *cmd, char *description_coment){
    if(!cmd)
        return ERROR;
    strcpy(cmd->description_coment,description_coment);
    return OK;
}

STATUS command_set_complement1(T_Command *cmd,char *complement){
    if(!cmd || !complement)
        return ERROR;
    strcpy(cmd->complement1,complement);
    return OK;
}

STATUS command_set_complement2(T_Command *cmd,char *complement){
    if(!cmd || !complement)
        return ERROR;
    strcpy(cmd->complement2,complement);
    return OK;
}

STATUS command_set_input(T_Command *cmd,char *command){
    if(!cmd || !command)
        return ERROR;
    if (!strcmp(command, "q") || !strcmp(command, "quit")){
            cmd->cmd = QUIT;
            strcpy(cmd->complement1,"\0");
        }else if (!strcmp(command, "p") || !strcmp(command, "pick")){
            cmd->cmd = PICK;
        }else if (!strcmp(command, "d") || !strcmp(command, "drop")){
            cmd->cmd = DROP;
        }else if (!strcmp(command, "t") || !strcmp(command,"throw")){
            cmd->cmd = THROW;
        }else if (!strcmp(command, "i") || !strcmp(command,"inspect")){
            cmd->cmd = INSPECT;
        }else if (!strcmp(command, "g") || !strcmp(command,"go")){
            cmd->cmd = GO;
        }else if (!strcmp(command, "s") || !strcmp(command,"save")){
            cmd->cmd = SAVE;
        }else if (!strcmp(command, "l") || !strcmp(command,"load")){
            cmd->cmd = LOAD;
        }else if (!strcmp(command,"turnon")){
            cmd->cmd = TURNON;
        }else if (!strcmp(command,"turnoff")){
            cmd->cmd = TURNOFF;
        }else if (!strcmp(command, "o") || !strcmp(command,"open")){
            cmd->cmd = OPEN;
        }else{
            cmd->cmd = UNKNOWN;
            strcpy(cmd->complement1,"\0");
        }
        return OK;
}