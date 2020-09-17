/** 
 * @brief It defines the menu of the game
 * 
 * @file menu.c
 * @author Tomas Higuera
 * @version 2.0 
 * @date 05-5-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <string.h> 
#include <stdlib.h>
#include <dirent.h>
#include <time.h>
#include <unistd.h>

#include "menu.h"

/*fprintf(stdout,"\n\n\n\n\n");*/
void menu_logo(){
    system("clear");
    fprintf(stdout," _______  _______  __   __  _______                                                        \n");
    fprintf(stdout,"|       ||       ||  | |  ||       |                                                       \n");
    fprintf(stdout,"|    ___||   _   ||  |_|  ||   _   |                                                       \n");
    fprintf(stdout,"|   | __ |  | |  ||       ||  | |  |                                                       \n");
    fprintf(stdout,"|   ||  ||  |_|  ||_     _||  |_|  |                                                       \n");
    fprintf(stdout,"|   |_| ||       |  |   |  |       |                                                       \n");
    fprintf(stdout,"|_______||_______|  |___|  |_______|                                                       \n");
    fprintf(stdout," _______  ______   __   __  _______  __    _  _______  __   __  ______    _______  _______ \n");
    fprintf(stdout,"|   _   ||      | |  | |  ||       ||  |  | ||       ||  | |  ||    _ |  |       ||       |\n");
    fprintf(stdout,"|  |_|  ||  _    ||  |_|  ||    ___||   |_| ||_     _||  | |  ||   | ||  |    ___||  _____|\n");
    fprintf(stdout,"|       || | |   ||       ||   |___ |       |  |   |  |  |_|  ||   |_||_ |   |___ | |_____ \n");
    fprintf(stdout,"|       || |_|   ||       ||    ___||  _    |  |   |  |       ||    __  ||    ___||_____  |\n");
    fprintf(stdout,"|   _   ||       | |     | |   |___ | | |   |  |   |  |       ||   |  | ||   |___  _____| |\n");
    fprintf(stdout,"|__| |__||______|   |___|  |_______||_|  |__|  |___|  |_______||___|  |_||_______||_______|\n");
}

void menu_ini(){
    system("clear");
    fprintf(stdout,"                        MAIN MENU\n");
    fprintf(stdout,"\n");
    fprintf(stdout,"                    1-NEW GAME \n");
    fprintf(stdout,"                    2-LOAD GAME \n");
    fprintf(stdout,"                    3-OPTIONS \n");
    fprintf(stdout,"                    4-CONTROLS \n");
    fprintf(stdout,"                    5-ADDITIONAL INFORMATION \n");
    fprintf(stdout,"                    6-EXIT \n");
}

void menu_end(){
    system("clear");
    fprintf(stdout,"__      __    ____     __    __      ___       ___    _____      __      _              __      _     ____     ___       ___     __      __    ____     __    __  \n");
    fprintf(stdout,") \\    / (   / __ \\    ) )  ( (     (  (       )  )  (_   _)    /  \\    / )            /  \\    / )   / __ \\   (  (       )  )    ) \\    / (   / __ \\    ) )  ( ( \n"); 
    fprintf(stdout," \\ \\  / /   / /  \\ \\  ( (    ) )     \\  \\  _  /  /     | |     / /\\ \\  / /            / /\\ \\  / /   / /  \\ \\   \\  \\  _  /  /      \\ \\  / /   / /  \\ \\  ( (    ) ) \n");
    fprintf(stdout,"  \\ \\/ /   ( ()  () )  ) )  ( (       \\  \\/ \\/  /      | |     ) ) ) ) ) )            ) ) ) ) ) )  ( ()  () )   \\  \\/ \\/  /        \\ \\/ /   ( ()  () )  ) )  ( (  \n");
    fprintf(stdout,"   \\  /    ( ()  () ) ( (    ) )       )   _   (       | |    ( ( ( ( ( (            ( ( ( ( ( (   ( ()  () )    )   _   (          \\  /    ( ()  () ) ( (    ) ) \n");
    fprintf(stdout,"    )(      \\ \\__/ /   ) \\__/ (        \\  ( )  /      _| |__  / /  \\ \\/ /    __      / /  \\ \\/ /    \\ \\__/ /     \\  ( )  /           )(      \\ \\__/ /   ) \\__/ (  \n");
    fprintf(stdout,"   /__\\      \\____/    \\______/         \\_/ \\_/      /_____( (_/    \\__/    (  )    (_/    \\__/      \\____/       \\_/ \\_/           /__\\      \\____/    \\______/  \n");
    fprintf(stdout,"                                                                            /_/                                                         \n");                          
    fprintf(stdout,"   ____     ____        __      _       _____      __      _    ________     ____    __      __     ________   __    __    _____        \n");                          
    fprintf(stdout,"  / ___)   (    )      /  \\    / )     / ___/     /  \\    / )  (___  ___)   / __ \\   ) \\    / (    (___  ___) (  \\  /  )  / ___/      \n");                            
    fprintf(stdout," / /       / /\\ \\     / /\\ \\  / /     ( (__      / /\\ \\  / /       ) )     / /  \\ \\   \\ \\  / /         ) )     \\ (__) /  ( (__        \n");                            
    fprintf(stdout,"( (       ( (__) )    ) ) ) ) ) )      ) __)     ) ) ) ) ) )      ( (     ( ()  () )   \\ \\/ /         ( (       ) __ (    ) __)        \n");                           
    fprintf(stdout,"( (        )    (    ( ( ( ( ( (      ( (       ( ( ( ( ( (    __  ) )    ( ()  () )    \\  /           ) )     ( (  ) )  ( (             \n");                         
    fprintf(stdout," \\ \\___   /  /\\  \\   / /  \\ \\/ /       \\ \\___   / /  \\ \\/ /   ( (_/ /      \\ \\__/ /      )(           ( (       ) )( (    \\ \\___         \n");                         
    fprintf(stdout,"  \\____) /__(  )__\\ (_/    \\__/         \\____\\ (_/    \\__/     \\___/        \\____/      /__\\          /__\\     /_/  \\_\\    \\____\\        \n");                         
    fprintf(stdout,"                                                                                                                                             \n");                     
    fprintf(stdout,"    _____      ____     _____       ______      _____      __      _       _____      _____       _____       ___       ___    _____   ________   __    __       \n"); 
    fprintf(stdout,"   / ___ \\    / __ \\   (_   _)     (_  __ \\    / ___/     /  \\    / )     / ___/     / ___ \\     / ___ \\     (  (       )  )  (_   _) (___  ___) (  \\  /  ) \n");      
    fprintf(stdout,"  / /   \\_)  / /  \\ \\    | |         ) ) \\ \\  ( (__      / /\\ \\  / /     ( (__      / /   \\_)   / /   \\_)     \\  \\  _  /  /     | |       ) )     \\ (__) /      \n");  
    fprintf(stdout," ( (  ____  ( ()  () )   | |        ( (   ) )  ) __)     ) ) ) ) ) )      ) __)    ( (  ____   ( (  ____       \\  \\/ \\/  /      | |      ( (       ) __ (       \n");  
    fprintf(stdout," ( ( (__  ) ( ()  () )   | |   __    ) )  ) ) ( (       ( ( ( ( ( (      ( (       ( ( (__  )  ( ( (__  )       )   _   (       | |       ) )     ( (  ) )      \n");  
    fprintf(stdout,"  \\ \\__/ /   \\ \\__/ /  __| |___) )  / /__/ /   \\ \\___   / /  \\ \\/ /       \\ \\___    \\ \\__/ /    \\ \\__/ /        \\  ( )  /      _| |__    ( (       ) )( (       \n");  
    fprintf(stdout,"   \\____/     \\____/   \\________/  (______/     \\____\\ (_/    \\__/         \\____\\    \\____/      \\____/          \\_/ \\_/      /_____(    /__\\     /_/  \\_\\      \n");  
    fprintf(stdout,"                                                                                                                              \n");                                    
    fprintf(stdout,"__      __    ____     __    __   ______        _________   ______      _____    _____      __      _   ______      _____    \n");                                     
    fprintf(stdout,") \\    / (   / __ \\    ) )  ( (  (   __ \\      (_   _____) (   __ \\    (_   _)  / ___/     /  \\    / ) (_  __ \\    / ____\\     \n");                                   
    fprintf(stdout," \\ \\  / /   / /  \\ \\  ( (    ) )  ) (__) )       ) (___     ) (__) )     | |   ( (__      / /\\ \\  / /    ) ) \\ \\  ( (___      \n");                                    
    fprintf(stdout,"  \\ \\/ /   ( ()  () )  ) )  ( (  (    __/       (   ___)   (    __/      | |    ) __)     ) ) ) ) ) )   ( (   ) )  \\___ \\      \n");                                  
    fprintf(stdout,"   \\  /    ( ()  () ) ( (    ) )  ) \\ \\  _       ) (        ) \\ \\  _     | |   ( (       ( ( ( ( ( (     ) )  ) )      ) )    \n");                                    
    fprintf(stdout,"    )(      \\ \\__/ /   ) \\__/ (  ( ( \\ \\_))     (   )      ( ( \\ \\_))   _| |__  \\ \\___   / /  \\ \\/ /    / /__/ /   ___/ /     \n");                                    
    fprintf(stdout,"   /__\\      \\____/    \\______/   )_) \\__/       \\_/        )_) \\__/   /_____(   \\____\\ (_/    \\__/    (______/   /____/        \n");                                  
    fprintf(stdout,"                                                                                                                              \n");                                    
}

void menu_game_over(){
    system("clear");
    fprintf(stdout,"                                                                                                              \n");
    fprintf(stdout,"                uuuuuuu                                                                                       \n");
    fprintf(stdout,"             uu$$$$$$$$$$$uu                                                                                  \n");
    fprintf(stdout,"          uu$$$$$$$$$$$$$$$$$uu                                                                               \n");
    fprintf(stdout,"         u$$$$$$$$$$$$$$$$$$$$$u                                                                              \n");
    fprintf(stdout,"        u$$$$$$$$$$$$$$$$$$$$$$$u                                                                             \n");
    fprintf(stdout,"       u$$$$$$$$$$$$$$$$$$$$$$$$$u                                                                            \n");
    fprintf(stdout,"       u$$$$$$$$$$$$$$$$$$$$$$$$$u                                                                            \n");
    fprintf(stdout,"       u$$$$$$\"   \"$$$\"   \"$$$$$$u                                                                            \n");
    fprintf(stdout,"       \"$$$$\"      u$u       $$$$\"                    _/_/_/    _/_/    _/      _/  _/_/_/_/                  \n");
    fprintf(stdout,"        $$$u       u$u       u$$$                  _/        _/    _/  _/_/  _/_/  _/                         \n");
    fprintf(stdout,"        $$$u      u$$$u      u$$$                 _/  _/_/  _/_/_/_/  _/  _/  _/  _/_/_/                      \n");
    fprintf(stdout,"         \"$$$$uu$$$   $$$uu$$$$\"                _/    _/  _/    _/  _/      _/  _/                            \n");
    fprintf(stdout,"          \"$$$$$$$\"   \"$$$$$$$\"                 _/_/_/  _/    _/  _/      _/  _/_/_/_/                        \n");
    fprintf(stdout,"            u$$$$$$$u$$$$$$$u                                                                                 \n");
    fprintf(stdout,"             u$\"$\"$\"$\"$\"$\"$u                                                                                  \n");
    fprintf(stdout,"  uuu        $$u$ $ $ $ $u$$       uuu                     _/_/    _/      _/  _/_/_/_/  _/_/_/               \n");
    fprintf(stdout," u$$$$        $$$$$u$u$u$$$       u$$$$                  _/    _/  _/      _/  _/        _/    _/             \n");
    fprintf(stdout,"  $$$$$uu      \"$$$$$$$$$\"     uu$$$$$$                 _/    _/  _/      _/  _/_/_/    _/_/_/                \n");
    fprintf(stdout,"u$$$$$$$$$$$uu    \"\"\"\"\"    uuuu$$$$$$$$$$              _/    _/    _/  _/    _/        _/    _/               \n");
    fprintf(stdout,"$$$$\"\"\"$$$$$$$$$$uuu   uu$$$$$$$$$\"\"\"$$$\"               _/_/        _/      _/_/_/_/  _/    _/                \n");
    fprintf(stdout," \"\"\"      \"\"$$$$$$$$$$$uu \"\"$\"\"\"                                                                              \n");
    fprintf(stdout,"           uuuu \"\"$$$$$$$$$$uuu                                                                               \n");
    fprintf(stdout,"  u$$$uuu$$$$$$$$$uu \"\"$$$$$$$$$$$uuu$$$                                                                      \n");
    fprintf(stdout,"  $$$$$$$$$$\"\"\"\"           \"\"$$$$$$$$$$$\"                                                                     \n");
    fprintf(stdout,"   \"$$$$$\"                      \"\"$$$$\"\"                                                                      \n");
    fprintf(stdout,"    $$$\"                         $$$$\"                                                                        \n");
    fprintf(stdout,"                                                                                                              \n");   
    fprintf(stdout,"                                                                                                              \n");    
}

void menu_options(){
    system("clear");
    fprintf(stdout,"            OPTIONS\n");
    fprintf(stdout,"\n");
    fprintf(stdout,"        1-Music\n");
    fprintf(stdout,"        2-Difficulty\n");
    fprintf(stdout,"        3-Extra rules\n");
    fprintf(stdout,"        4-AutoSave\n");
    fprintf(stdout,"        5-Idioma\n");
    fprintf(stdout,"        6-Exit\n");
}

void menu_controls(){
    system("clear");
    fprintf(stdout,"            CONTROLS\n");
    fprintf(stdout,"\n");
    fprintf(stdout,"-Inspect: To inspect an object you have to write i \"name\"(name is the name of the object that you want to inspect)\n");
    fprintf(stdout,"          To inspect the space where you are placed you have to write i space\n");
    fprintf(stdout,"-Go:      To move your payer you have to write g \"cardinal point\"(the cardinal point can be north(n),south(s),east(e),west(w))\n");
    fprintf(stdout,"          You can also move up(u) and down(d)\n");
    fprintf(stdout,"-Turnon:  If you want to turnon an object you have to write turnon \"name\"(name is the name of the object that you want to turnon)\n");
    fprintf(stdout,"-Turnoff: If you want to turnon an object you have to write turnoff \"name\"(name is the name of the object that you want to turnoff)\n");
    fprintf(stdout,"-Save:    If you want to save the game you have to write save \"name\"(name is the name of the saved file)\n");
    fprintf(stdout,"-Quit:    If you want to quit the game you have to write quit\n");
    fprintf(stdout,"-Drop:    If you want to drop an object you have to write d \"name\"(name is name of the object that you want to drop)\n");
    fprintf(stdout,"-Pick:    If you want to pick an object you have to write d \"name\"(name is name of the object that you want to pick)\n");
    fprintf(stdout,"-Open:    If you want to open a door you have to write open \"door\" with \"name\"(door is the name of the door that you want to open and name is the name of the object that bis going to opend the door)\n");
    fprintf(stdout,"If you want to close the controlls window you have to write exit:");
}

void menu_additional_information(){
    system("clear");
    fprintf(stdout,"This game was created by Guillermo Hoyo Bravo, Alvaro Lopez de Pradena, Pablo Gutierrez y Tomás Higuera Viso\n");
    fprintf(stdout,"A lot of time have been spent on this game, enjoy it :)\n");
}

void menu_battle(Game *game, Id enemy){
    system("clear");
    fprintf(stdout,"                         BATTLE TIME!!!!                                                \n");
    fprintf(stdout,"                                                         _____                          \n");
    fprintf(stdout,"   ENEMY STATS:                                          |-_-|                          \n");
    fprintf(stdout,"     HP --> %d    DEF ---> %d             __________        |                            \n", enemy_get_hp(game_get_enemy(game, enemy)), enemy_get_def(game_get_enemy(game, enemy)));
    fprintf(stdout,"     AD --> %d    SPEED -> %d           /_-_-_-_-_|====----|-----                       \n", enemy_get_ad(game_get_enemy(game, enemy)), enemy_get_speed(game_get_enemy(game, enemy)));
    fprintf(stdout,"                                                           |                            \n");
    fprintf(stdout,"                                                           |                            \n");
    fprintf(stdout,"                                                          / \\                          \n");
    fprintf(stdout,"                                                         /   \\                         \n");
    fprintf(stdout,"         ·^^^^^·                                        /     \\                        \n");
    fprintf(stdout,"        <|.*_*.|>                                                                       \n");
    fprintf(stdout,"         ___|___           ____________                             0.- ATTACK          \n");
    fprintf(stdout,"    o----'-|-|-'-----|====/--_-_-_-__//                             1.- DEFEND          \n");
    fprintf(stdout,"        [/'|-|'\\]                                                   2.- DODGE           \n");
    fprintf(stdout,"        [--|-|--]                                                   3.- COUNTER ATTACK  \n");
    fprintf(stdout,"           |_|                                                      4.- RUN             \n");
    fprintf(stdout,"          /|\\                                                                          \n");
    fprintf(stdout,"         // \\\\             STATS:                                                     \n");
    fprintf(stdout,"        //   \\\\               HP --> %d    DEF ---> %d                                \n", player_get_hp(game_get_player(game)), player_get_def(game_get_player(game)));
    fprintf(stdout,"      _//     \\\\_             AD --> %d    SPEED -> %d                                \n", player_get_ad(game_get_player(game)), player_get_speed(game_get_player(game)));
    fprintf(stdout,"                                                                                        \n");
}

void menu_load(){
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"#····························");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"##···························");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n");
    fprintf(stdout,"###··························");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"####·························");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"#####························");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"######·······················");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"#######······················");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"########·····················");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"#########····················");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"##########···················");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"###########··················");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"#############················");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"##############···············");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"###############··············");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"################·············");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n"); 
    fprintf(stdout,"#####################········");
    fprintf(stdout,"\nLoading...");
    sleep(1);
    system("clear");
    fprintf(stdout, "\n\n\n\n\n");
    fprintf(stdout,"#############################");
    fprintf(stdout,"\nLoading...");
    sleep(1);
}
