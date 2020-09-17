/** 
 * @brief It defines the game rules
 * 
 * @file game_rules.c
 * @author Tomas Higuera
 * @version 2.0 
 * @date 05-5-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "game_rules.h"
#include "die.h"
#include "space.h"
#include "link.h"

STATUS gameRules_action(Die *die, Game *game){
    int num;
    if(!die || !game)
        return ERROR;
    die_roll_action(die);
    num = die_get_last_num(die);
    
    switch(num){
        case 1:
            return gameRules_action1(die,game);
            break;
        case 2:
            return gameRules_action2(die,game);
            break;
        case 3:
            return gameRules_action3(die,game);
            break;
        case 4:
            return gameRules_action4(die,game);
            break;
        case 5:
            return gameRules_action5(die,game);
            break;
        case 6:
            return gameRules_action6(die,game);
            break;
        case 7:
            return gameRules_action7(die,game);
            break;
        case 8:
            return gameRules_action8(die,game);
            break;
        case 9:
            return gameRules_action9(die,game);
            break;
        case 10:
            return gameRules_action10(die,game);
            break;
        case 11:
            return gameRules_action11(die,game);
            break;
        case 12:
            return gameRules_action12(die,game);
            break;
        case 13:
            return gameRules_action13(die,game);
            break;
        default:
            return ERROR;
    }
}

STATUS gameRules_action1(Die *die, Game *game){
    int num;
    Space *space=NULL;
    if(!die || !game)
        return ERROR;
    die_action1(die);
    num = die_get_last_num(die);
    space=game_get_space(game,num);
    space_set_ilumination(space,TRUE);
    return OK;
}

STATUS gameRules_action2(Die *die, Game *game){
    Space *space=NULL;
    int num;
    if(!die || !game)
        return ERROR;
    die_action1(die);
    num = die_get_last_num(die);
    space=game_get_space(game,num);
    space_set_ilumination(space,FALSE);
    return OK;
}

STATUS gameRules_action3(Die *die, Game *game){
    Player *player=NULL;
    int num;
    player=game_get_player(game);
    if(!die || !game)
        return ERROR;
    die_action2(die);
    num=die_get_last_num(die);
    player_add_ad(player,num);
    return OK;
}

STATUS gameRules_action4(Die *die, Game *game){
    Player *player=NULL;
    int num;
    player=game_get_player(game);
    if(!die || !game)
        return ERROR;
    die_action2(die);
    num=die_get_last_num(die);
    player_subtract_ad(player,num);
    return OK;
}

STATUS gameRules_action5(Die *die, Game *game){
    Set *set=NULL;
    Space *space=NULL;
    Set *aux=NULL;
    Player *player=NULL;
    int i;
    int num;
    if(!die || !game)
        return ERROR;
    die_action3(die);
    num = die_get_last_num(die);
    set=game_get_objects_location(game);
    player=game_get_player(game);
    for(i=0;i<set_getNumId(set);i++){
        space=game_get_space(game,set_get_id_at(set,i));
        aux=space_get_objects(space);
        if(set_checkId(aux,num)==OK){
            space_deleteObject(space,num);
            player_add_object(player,num);
            return OK;
        }
    }
    return ERROR;
}

STATUS gameRules_action6(Die *die, Game *game){
    Set *set=NULL;
    Player *player=NULL;
    Object *object=NULL;
    int num;
    if(!die || !game)
        return ERROR;
    die_action3(die);
    num = die_get_last_num(die);
    player=game_get_player(game);
    set=inventory_get_ids(player_get_inventory(player));
    object=game_getObject(game,num);
    if(set_checkId(set,num)==OK){
        object_set_movido(object,TRUE);
    }
    return ERROR;
}

STATUS gameRules_action7(Die *die, Game *game){
    Set *set=NULL;
    Player *player=NULL;
    Object *object=NULL;
    int num;
    if(!die || !game)
        return ERROR;
    die_action3(die);
    num = die_get_last_num(die);
    player=game_get_player(game);
    set=inventory_get_ids(player_get_inventory(player));
    object=game_getObject(game,num);
    if(set_checkId(set,num)==OK){
        object_set_movido(object,FALSE);
    }
    return ERROR;
}

STATUS gameRules_action8(Die *die, Game *game){
    Player *player=NULL;
    int num;
    player=game_get_player(game);
    if(!die || !game)
        return ERROR;
    die_action2(die);
    num=die_get_last_num(die);
    player_add_hp(player,num);
    return OK;
}

STATUS gameRules_action9(Die *die, Game *game){
    Player *player=NULL;
    int num;
    player=game_get_player(game);
    if(!die || !game)
        return ERROR;
    die_action2(die);
    num=die_get_last_num(die);
    player_subtract_hp(player,num);
    return OK;
}

STATUS gameRules_action10(Die *die, Game *game){
    Player *player=NULL;
    int num;
    player=game_get_player(game);
    if(!die || !game)
        return ERROR;
    die_action2(die);
    num=die_get_last_num(die);
    player_add_speed(player,num);
    return OK;
}

STATUS gameRules_action11(Die *die, Game *game){
    Player *player=NULL;
    int num;
    player=game_get_player(game);
    if(!die || !game)
        return ERROR;
    die_action2(die);
    num=die_get_last_num(die);
    player_subtract_speed(player,num);
    return OK;
}

STATUS gameRules_action12(Die *die, Game *game){
    Player *player=NULL;
    int num;
    player=game_get_player(game);
    if(!die || !game)
        return ERROR;
    die_action2(die);
    num=die_get_last_num(die);
    player_add_def(player,num);
    return OK;
}

STATUS gameRules_action13(Die *die, Game *game){
    Player *player=NULL;
    int num;
    player=game_get_player(game);
    if(!die || !game)
        return ERROR;
    die_action2(die);
    num=die_get_last_num(die);
    player_subtract_def(player,num);
    return OK;
}
