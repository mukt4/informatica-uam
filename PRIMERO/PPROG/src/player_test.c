/** 
 * @brief It defines the player test
 * 
 * @file player_test.c
 * @author Guillermo Hoyo
 * @version 1.0 
 * @date 31-03-2017
 * @copyright GNU Public License
 */

#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 

#include "player.h"
#include "inventory.h"
#include "set.h"
#include "stats.h"
#include "player_test.h"
#include "test.h"

#define MAX_TESTS 28
/** 
 * @brief Funcion principal de pruebas para el modulo Player. 
 * 
 * Dos modos de ejecucion:
 *   1.-Si se ejecuta sin parametros se ejecutan todas las pruebas 
 *   2.-Si se ejecuta con un numero entre 1 y el numero de pruebas solo ejecuta 
 *      la prueba indicada  
 *  
 */
int main(int argc, char** argv) {

    int test = 0;
    int all = 1;

    if (argc < 2) {
        printf("Running all test for module Space:\n");
    } else {
        test = atoi(argv[1]);
        all = 0;
        printf("Running test %d:\t", test);
	if (test < 1 || test > MAX_TESTS) {
	  printf("Error: unknown test %d\t", test);
	  exit(EXIT_SUCCESS);
        }
    }
    
    if (all || test == 1) test1_player_create();
    if (all || test == 2) test2_player_create();
    if (all || test == 3) test1_player_set_name();
    if (all || test == 4) test2_player_set_name();
    if (all || test == 5) test3_player_set_name();
    if (all || test == 6) test1_player_set_space();
    if (all || test == 7) test2_player_set_space();
    if (all || test == 8) test3_player_set_space();
    if (all || test == 9) test1_player_inventory_have_object();
    if (all || test == 10) test2_player_inventory_have_object();
    if (all || test == 11) test3_player_inventory_have_object();
    if (all || test == 12) test4_player_inventory_have_object();
    if (all || test == 13) test1_player_add_object();
    if (all || test == 14) test2_player_add_object();
    if (all || test == 15) test3_player_add_object();
    if (all || test == 16) test1_player_delete_object();
    if (all || test == 17) test2_player_delete_object();
    if (all || test == 18) test1_player_get_id();
    if (all || test == 19) test2_player_get_id();
    if (all || test == 20) test1_player_get_name();
    if (all || test == 21) test2_player_get_name();
    if (all || test == 22) test1_player_get_space();
    if (all || test == 23) test2_player_get_space();
    if (all || test == 24) test3_player_get_space();
    if (all || test == 25) test1_player_get_inventory();
    if (all || test == 26) test2_player_get_inventory();
    if (all || test == 27) test1_player_get_inventory_ids();
    if (all || test == 28) test2_player_get_inventory_ids();
    
    PRINT_PASSED_PERCENTAGE;
    return 1;
}

void test1_player_create(){
    int result = player_create(8) != NULL;
    PRINT_TEST_RESULT(result);
}

void test2_player_create(){
    Player * p = player_create(8);
    PRINT_TEST_RESULT(player_get_id(p) == 8);
}

void test1_player_set_name(){
    Player * p = NULL;
    PRINT_TEST_RESULT(player_set_name(p, "klk") == ERROR);
}

void test2_player_set_name(){
    Player * p = player_create(8);
    PRINT_TEST_RESULT(player_set_name(p, NULL) == ERROR);
}

void test3_player_set_name(){
    Player * p = player_create(8);
    PRINT_TEST_RESULT(player_set_name(p, "klk") == OK);
}

void test1_player_set_space(){
    Player * p = NULL;
    PRINT_TEST_RESULT(player_set_space(p, 8) == ERROR);
}

void test2_player_set_space(){
    Player * p = player_create(8);
    PRINT_TEST_RESULT(player_set_space(p, NO_ID) == ERROR);
}

void test3_player_set_space(){
    Player * p = player_create(8);
    PRINT_TEST_RESULT(player_set_space(p, 8) == OK);
}

void test1_player_inventory_have_object(){
    Player * p = NULL;
    PRINT_TEST_RESULT(player_inventory_have_object(p, 8) == FALSE);
}

void test2_player_inventory_have_object(){
    Player * p = player_create(8);
    PRINT_TEST_RESULT(player_inventory_have_object(p, NO_ID) == FALSE);
}

void test3_player_inventory_have_object(){
    Player * p = player_create(8);
    PRINT_TEST_RESULT(player_inventory_have_object(p, 8) == FALSE);
}

void test4_player_inventory_have_object(){
    Player * p = player_create(8);
    player_add_object(p, 8);
    PRINT_TEST_RESULT(player_inventory_have_object(p, 8) == TRUE);
}

void test1_player_add_object(){
    Player * p = NULL;
    PRINT_TEST_RESULT(player_add_object(p, NO_ID) == ERROR);
}

void test2_player_add_object(){
    Player * p = NULL;
    PRINT_TEST_RESULT(player_add_object(p, 8) == ERROR);
}

void test3_player_add_object(){
    Player * p = player_create(8);
    PRINT_TEST_RESULT(player_add_object(p, 8) == OK);
}

void test1_player_delete_object(){
    Player * p = NULL;
    PRINT_TEST_RESULT(player_delete_object(p, 8) == ERROR);
}

void test2_player_delete_object(){
    Player * p = player_create(8);
    PRINT_TEST_RESULT(player_delete_object(p, 8) == OK);
}

void test1_player_get_id(){
    Player * p = NULL;
    PRINT_TEST_RESULT(player_get_id(p) == NO_ID);
}

void test2_player_get_id(){
    Player * p = player_create(8);
    PRINT_TEST_RESULT(player_get_id(p) == 8);
}

void test1_player_get_name(){
    Player * p = NULL;
    PRINT_TEST_RESULT(player_get_name(p) == NULL);
}

void test2_player_get_name(){
    Player * p = player_create(8);
    player_set_name(p, "KLK");
    PRINT_TEST_RESULT(player_get_name(p) != NULL);
}

void test1_player_get_space(){
    Player * p = player_create(8);
    player_set_space(p, 8);
    PRINT_TEST_RESULT(player_get_space(p) == 8);
}

void test2_player_get_space(){
    Player * p = player_create(8);
    player_set_space(p, NO_ID);
    PRINT_TEST_RESULT(player_get_space(p) == NO_ID);
}

void test3_player_get_space(){
    Player * p = NULL;
    PRINT_TEST_RESULT(player_get_space(p) == NO_ID);
}


void test1_player_get_inventory(){
    Player * p = NULL;
    PRINT_TEST_RESULT(player_get_inventory(p) == NULL);
}

void test2_player_get_inventory(){
    Player * p = player_create(8);
    PRINT_TEST_RESULT(player_get_inventory(p) != NULL);
}

void test1_player_get_inventory_ids(){
    Player * p = NULL;
    PRINT_TEST_RESULT(player_get_inventory_ids(p) == NULL);
}

void test2_player_get_inventory_ids(){
    Player * p = player_create(8);
    inventory_add_id(player_get_inventory(p), 8);
    PRINT_TEST_RESULT(player_get_inventory_ids(p) != NULL);
}