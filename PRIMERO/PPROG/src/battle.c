/** 
 * @brief It defines a battle
 * 
 * @file battle.c
 * @author Guillermo Hoyo
 * @version 2.0 
 * @date 05-5-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#include "battle.h"
#include "player.h"
#include "enemy.h"
#include "stats.h"
#include "die.h"


STATUS battle_on(Game * game){
    Player * player = NULL;
    Enemy * enemy = NULL;
    int player_stats[4];
    int enemy_stats[4];
    char description_coment[100] = "\0";
    int accion;
    int enemy_accion;
    Die * die = NULL;
    
    if(!game)
        return ERROR;
    
    player = game_get_player(game);
    enemy = game_get_enemy_for_battle(game);
    die = game_get_die(game);
    
    player_stats[0] = player_get_hp(player);
    player_stats[1] = player_get_ad(player);
    player_stats[2] = player_get_def(player);
    player_stats[3] = player_get_speed(player);
    
    enemy_stats[0] = enemy_get_hp(enemy);
    enemy_stats[1] = enemy_get_ad(enemy);
    enemy_stats[2] = player_get_def(player);
    enemy_stats[3] = enemy_get_speed(enemy);
    
    /*sprintf(description,"Player Stats:\n\thp: %d\n\tad: %d\n\tspeed: %d", player_stats[0], player_stats[1], player_stats[2]);
    
    sprintf(aux,"\nEnemy Stats:\n\thp: %d\n\tad: %d\n\tspeed: %d", enemy_stats[0], enemy_stats[1], enemy_stats[2]);
    strcat(description,aux);
    
    sprintf(aux,"\nActions:\n\t 0 --> ATTACK \n\t 1 --> DEFEND \n\t 2 --> DODGE \n\t 3 --> COUNTER  \n\t 4--> RUN");
    strcat(description,aux);
    command_set_description(game_get_last_command(game),description);*/
    
    if (player_stats[0] <= 0){
        player_set_space(player, -2);
        return OK;
    }
    
    if(enemy_stats[0] <= 0){
        enemy_set_space(enemy, -2);
        return OK;
    }
    
    /*while(player_dead(player) == FALSE || enemy_dead(enemy) == FALSE){*/
    die_roll_battle(die);
    enemy_accion = die_get_last_num(die);
    fprintf(stdout, "Choose Action:");
    scanf("%d", &accion);
    fflush(stdout);
    
    /*ATTACK*/
    if (accion == 0 && enemy_accion==0){
        fprintf(stdout, "A & A");
        command_set_description_coment(game_get_last_command(game), description_coment);
        if (player_stats[3] >= enemy_stats[3]){
            fprintf(stdout, "You are faster BRO");
            enemy_set_hp(enemy, (enemy_stats[0] - player_stats[1]));
        }
        
        if (player_stats[3] < enemy_stats[3]){
            fprintf(stdout, "You are tortoise D:");
            player_set_hp(player, (player_stats[0] - enemy_stats[1]));
        }
        sleep(1);
        return OK;
    }
    if (accion == 0 && enemy_accion==1){
        fprintf(stdout, "A & D");
        sleep(1);
        enemy_set_hp(enemy, (enemy_stats[0]+(enemy_stats[2] - player_stats[1])));
        return OK;
    }
    if (accion == 0 && enemy_accion==2){
        die_roll_dodge(die);
        if (die_get_last_num(die)== 0){
            fprintf(stdout, "DODGE!");
            sleep(1);
            return OK;
        }
        fprintf(stdout, "FAIL DODGE!");
        enemy_set_hp(enemy, (enemy_stats[0] - player_stats[1]));
        sleep(1);
        return OK;
    }
        
    if (accion == 0 && enemy_accion==3){
        die_roll_counter(die);
        if (die_get_last_num(die)== 0){
            fprintf(stdout, "COUNTER!!!!");
            player_set_hp(player, (player_stats[0] - 2*(enemy_stats[1])));
            sleep(1);
            return OK;
        }
        fprintf(stdout, "FAIL COUNTER!");
        enemy_set_hp(enemy, (enemy_stats[0] - player_stats[1]));
        sleep(1);
        return OK;
    }
    
    /*DEFEND*/
    if (accion == 1 && enemy_accion==0){
        fprintf(stdout, "D & A");
        player_set_hp(player, (player_stats[0] +(player_stats[2] - enemy_stats[1])));
        sleep(1);
        return OK;
    }
        
    if (accion == 1 && enemy_accion==1){
        fprintf(stdout, "D & D... ok");
        sleep(1);
        return OK;
    }
    
    if (accion == 1 && enemy_accion==2){
        fprintf(stdout, "D & DOD... ok");
        sleep(1);
        return OK;
    }
        
    if (accion == 1 && enemy_accion==3){
        fprintf(stdout, "D & C");
        enemy_set_hp(enemy, (enemy_stats[0] -(enemy_stats[1] - player_stats[2])));
        sleep(1);
        return OK;
    }
    
    /*DODGE*/
    if (accion == 2 && enemy_accion==0){
        die_roll_dodge(die);
        if (die_get_last_num(die)== 0){
            fprintf(stdout, "DODGE!");
            sleep(1);
            return OK;
        }
        fprintf(stdout, "FAIL DODGE!");
        player_set_hp(player, (player_stats[0] - enemy_stats[1]));
        sleep(1);
        return OK;
    }
    
    if (accion == 2 && enemy_accion==1){
        fprintf(stdout, "DOD & D... ok");
        sleep(1);
        return OK;
    }
    
    if (accion == 2 && enemy_accion==2){
        fprintf(stdout, "DOD & DOD... ok");
        sleep(1);
        return OK;
    }
    
    if (accion == 2 && enemy_accion==3){
        fprintf(stdout, "DOD & C... ok");
        sleep(1);
        return OK;
    }
    
    
    /* COUNTER*/
    if (accion == 3 && enemy_accion==0){
        die_roll_counter(die);
        if (die_get_last_num(die) == 0){
            fprintf(stdout, "COUNTER!!!!");
            enemy_set_hp(enemy, (enemy_stats[0] - 2*(player_stats[1])));
            sleep(1);
            return OK;
        }
        fprintf(stdout, "FAIL!");
        player_set_hp(player, (player_stats[0] - enemy_stats[1]));
        sleep(1);
        return OK;
    }
    
    if (accion == 3 && enemy_accion==1){
        fprintf(stdout, "C & D");
        player_set_hp(player, (player_stats[0] -(player_stats[1] - enemy_stats[2])));
        sleep(1);
        return OK;
    }
    
    if (accion == 3 && enemy_accion==2){
        fprintf(stdout, "C & DOD... ok");
        sleep(1);
        return OK;
    }
    
    if (accion == 3 && enemy_accion==3 && enemy_stats[4] == 3){
        fprintf(stdout, "C & C... ok");
        sleep(1);
        return OK;
    }
    
    /*RUN*/
    if (accion == 4){
        if (player_stats[3] > enemy_stats[3]){
            fprintf(stdout, "You've scaped");
            enemy_set_space(enemy, -1);
            sleep(1);
            return OK;
        }
        else{
            fprintf(stdout, "You can't scape");
            player_set_hp(player, (player_stats[0] - enemy_stats[1]));
            sleep(1);
            return OK;
        }
    }
    
    return ERROR;
}

