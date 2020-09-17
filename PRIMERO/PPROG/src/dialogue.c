/** 
 * @brief It implements the dialogues
 * 
 * @file dialogue.c
 * @author Alvaro Lopez
 * @version 1.0 
 * @date 5-05-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "dialogue.h"

struct _Dialogue{
    char phrase[MAX_PHRASE];/**The string of the dialogue*/
};

Dialogue *dialogue_create() {
    Dialogue *dialogue = NULL;

    dialogue = (Dialogue *) malloc(sizeof (Dialogue));
    if (dialogue == NULL) {
        return NULL;
    }

    dialogue->phrase[0] = '\0';

    return dialogue;
}


STATUS dialogue_destroy(Dialogue *dialogue) {
    if (!dialogue) {
        return ERROR;
    }

    free(dialogue);
    dialogue = NULL;

    return OK;
}

STATUS dialogue_set(Dialogue *dialogue, char * phrase) {
    if (!dialogue || !phrase) {
        return ERROR;
    }

    if (!strcpy(dialogue->phrase, phrase)) {
        return ERROR;
    }

    return OK;
}

const char *dialogue_get(Dialogue *dialogue) {
    if (!dialogue) {
        return NULL;
    }
    return dialogue->phrase;
}

void dialogue_quit(Dialogue * dialogue, STATUS status, int code){
    char phrase[MAX_PHRASE];
    
    if (status == OK){
        if(code == 0){
            sprintf(phrase, "Game closed, thanks for playing");
            dialogue_set(dialogue, phrase); 
            return;
        }
    }else{
        return;
    }
}

void dialogue_go(Dialogue * dialogue, STATUS status, int code, char* complement){
    char phrase[MAX_PHRASE];
    char aux[MAX_PHRASE];
    if (status == OK){
        if(code == -1){
            sprintf(phrase, "You've gone north and you've ended the battle versus an enemy ");
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == -2){
            sprintf(phrase, "You've gone north");
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == -3){
            sprintf(phrase, "You've gone south and you've ended the battle versus an enemy");
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == -4){
            sprintf(phrase, "You've gone south");
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == -5){
            sprintf(phrase, "You've gone west and you've ended the battle versus an enemy");
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == -6){
            sprintf(phrase, "You've gone west");
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == -7){
            sprintf(phrase, "You've gone east and you've ended the a battle versus an enemy");
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == -8){
            sprintf(phrase, "You've gone east");
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == -9){
            sprintf(phrase, "You've gone down and you've ended the battle versus an enemy");
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == -10){
            sprintf(phrase, "You've gone down");
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == -11){
            sprintf(phrase, "You've gone up and you've ended the battle versus an enemy");
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == -12){
            sprintf(phrase, "You've gone up");
            dialogue_set(dialogue, phrase); 
            return;
        }
        
    }else{
        
        if(strcmp("e", complement)==0)
            strcpy(aux, "east");
        else if(strcmp("n", complement)==0)
            strcpy(aux, "north");
        else if(strcmp("w", complement)==0)
            strcpy(aux, "west");
        else if(strcmp("s", complement)==0)
            strcpy(aux, "south");
        else
            strcpy(aux, complement);
            
        if(code == 0){
            sprintf(phrase, "You can not go %s. Try another action.", aux);
            dialogue_set(dialogue, phrase); 
            return;
        }
        if(code == 1){
            sprintf(phrase, "You have done this before without success");
            dialogue_set(dialogue, phrase);
            return;
        }
        if(code> 1){
            sprintf(phrase, "You have done this before without success %i times", code);
        }
        return;
    }
    
}

void dialogue_pick(Dialogue * dialogue, STATUS status, int code, char *complement){
    char phrase[MAX_PHRASE];
    if (status == OK){
        if(code == -1){
            sprintf(phrase, "You've picked %s", complement);
            dialogue_set(dialogue, phrase); 
            return;
        }
    }else{
        if(code == 0){
            sprintf(phrase, "You can not pick %s. Try another action.", complement);
            dialogue_set(dialogue, phrase); 
            return;
        }
         if(code == 1){
            sprintf(phrase, "You have done this before without success");
            dialogue_set(dialogue, phrase);
            return;
        }
        if(code> 1){
            sprintf(phrase, "You have done this before without success %i times", code);
            dialogue_set(dialogue, phrase);
            return;
        }
        return;
    }
    
}

void dialogue_drop(Dialogue * dialogue, STATUS status, int code, char *complement){
    char phrase[MAX_PHRASE];
    
    if (status == OK){
        if(code == -1){
            sprintf(phrase, "You've dropped %s", complement);
            dialogue_set(dialogue, phrase); 
            return;
        }
    }else{
        if(code == 0){
            sprintf(phrase, "You can not drop %s. Try another action.", complement);
            dialogue_set(dialogue, phrase); 
            return;
        }
         if(code == 1){
            sprintf(phrase, "You have done this before without success");
            dialogue_set(dialogue, phrase);
            return;
        }
        if(code> 1){
            sprintf(phrase, "You have done this before without success %i times", code);
            dialogue_set(dialogue, phrase);
            return;
        }
        return;
    }
    
}

void dialogue_inspect(Dialogue * dialogue, STATUS status, int code, char *complement){
    char phrase[MAX_PHRASE];
    if (status == OK){
        if(code == -1){
            sprintf(phrase, "You've inspected %s", complement);
            dialogue_set(dialogue, phrase); 
            return;
        }
    }else{
        if(code == 0){
            sprintf(phrase, "You can not inspect %s. Try another action.", complement);
            dialogue_set(dialogue, phrase); 
            return;
        }
         if(code == 1){
            sprintf(phrase, "You have done this before without success");
            dialogue_set(dialogue, phrase);
            return;
        }
        if(code> 1){
            sprintf(phrase, "You have done this before without success %i times", code);
            dialogue_set(dialogue, phrase);
            return;
        }
        return;
    }
    
}

void dialogue_turnoff(Dialogue * dialogue, STATUS status, int code, char *complement){
    char phrase[MAX_PHRASE];
    if (status == OK){
        if(code == -1){
            sprintf(phrase, "You've turned off %s", complement);
            dialogue_set(dialogue, phrase); 
            return;
        }
    }else{
        if(code == 0){
            sprintf(phrase, "You can not turnoff %s. Try another action.", complement);
            dialogue_set(dialogue, phrase); 
            return;
        }
         if(code == 1){
            sprintf(phrase, "You have done this before without success");
            dialogue_set(dialogue, phrase);
            return;
        }
        if(code> 1){
            sprintf(phrase, "You have done this before without success %i times", code);
            dialogue_set(dialogue, phrase);
            return;
        }
        return;
    }
    
}

void dialogue_turnon(Dialogue * dialogue, STATUS status, int code, char *complement){
    char phrase[MAX_PHRASE];
    if (status == OK){
        if(code == -1){
            sprintf(phrase, "You've turned on %s", complement);
            dialogue_set(dialogue, phrase); 
            return;
        }
    }else{
        if(code == 0){
            sprintf(phrase, "You can not turnon %s. Try another action.", complement);
            dialogue_set(dialogue, phrase); 
            return;
        }
         if(code == 1){
            sprintf(phrase, "You have done this before without success");
            dialogue_set(dialogue, phrase);
            return;
        }
        if(code> 1){
            sprintf(phrase, "You have done this before without success %i times", code);
            dialogue_set(dialogue, phrase);
            return;
        }
        return;
    }
    
}

void dialogue_open(Dialogue * dialogue, STATUS status, int code, char *complement1, char* complement2){
    char phrase[MAX_PHRASE];
    if (status == OK){
        if(code == -1){
            sprintf(phrase, "You've opened %s", complement1);
            dialogue_set(dialogue, phrase); 
            return;
        }
    }else{
        if(code == 0){
            sprintf(phrase, "You can not open %s with %s. Try another action.", complement1, complement2);
            dialogue_set(dialogue, phrase); 
            return;
        }
         if(code == 1){
            sprintf(phrase, "You have done this before without success");
            dialogue_set(dialogue, phrase);
            return;
        }
        if(code> 1){
            sprintf(phrase, "You have done this before without success %i times", code);
            dialogue_set(dialogue, phrase);
            return;
        }
        return;
    }
    
}

void dialogue_save(Dialogue * dialogue, STATUS status, int code){
    char phrase[MAX_PHRASE];
    if (status == OK){
        if(code == -1){
            sprintf(phrase, "You've saved ");
            dialogue_set(dialogue, phrase); 
            return;
        }
    }else{
        if(code == 0){
            sprintf(phrase, "You can not save . Try another action.");
            dialogue_set(dialogue, phrase); 
            return;
        }
         if(code == 1){
            sprintf(phrase, "You have done this before without success");
            dialogue_set(dialogue, phrase);
            return;
        }
        if(code> 1){
            sprintf(phrase, "You have done this before without success %i times", code);
            dialogue_set(dialogue, phrase);
            return;
        }
        return;
    }
    
}

void dialogue_load(Dialogue * dialogue, STATUS status, int code){
    char phrase[MAX_PHRASE];
    if (status == OK){
        if(code == -1){
            sprintf(phrase, "You've loaded ");
            dialogue_set(dialogue, phrase); 
            return;
        }
    }else{
        if(code == 0){
            sprintf(phrase, "You can not load. Try another action.");
            dialogue_set(dialogue, phrase); 
            return;
        }
         if(code == 1){
            sprintf(phrase, "You have done this before without success");
            dialogue_set(dialogue, phrase);
            return;
        }
        if(code> 1){
            sprintf(phrase, "You have done this before without success %i times", code);
            dialogue_set(dialogue, phrase);
            return;
        }
        return;
    }
    
}

void dialogue_unknown(Dialogue * dialogue, STATUS status, int code){
    char phrase[MAX_PHRASE];
    
     if (status == OK){
        if(code == 0){
            sprintf(phrase, "Invalid command");
            dialogue_set(dialogue, phrase); 
            return;
        if(code == 1){
            sprintf(phrase, "You have done this before without success");
            return;
        }
        if(code > 1)
            sprintf(phrase, "You have done this before without success %i times", code);
            dialogue_set(dialogue, phrase);
            return;
        }
    }else{
        return;
    }
}