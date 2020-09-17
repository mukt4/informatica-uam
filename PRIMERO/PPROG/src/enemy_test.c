/** 
 * @brief It defines the enemy test
 * 
 * @file enemy_test.c
 * @author Alvaro Lopez
 * @version 1.0 
 * @date 05-05-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "enemy.h"
#include "enemy_test.h"
#include "test.h"

#define MAX_TESTS 14

/** 
 * @brief Funcion principal de pruebas para el modulo Dialogue. 
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
        printf("Running all test for module enemy:\n");
    } else {
        test = atoi(argv[1]);
        all = 0;
        printf("Running test %d:\t", test);
	if (test < 1 || test > MAX_TESTS) {
	  printf("Error: unknown test %d\t", test);
	  exit(EXIT_SUCCESS);
        }
    }
    
    if (all || test == 1) test1_enemy_create();
    if (all || test == 2) test2_enemy_create();
    if (all || test == 3) test1_enemy_destroy();
    if (all || test == 4) test2_enemy_destroy();
    if (all || test == 5) test1_enemy_get_id();
    if (all || test == 6) test2_enemy_get_id();
    if (all || test == 7) test1_enemy_set_name();
    if (all || test == 8) test2_enemy_set_name();
    if (all || test == 9) test1_enemy_get_name();
    if (all || test == 10) test2_enemy_get_name();
    if (all || test == 11) test1_enemy_set_space();
    if (all || test == 12) test2_enemy_set_space();
    if (all || test == 13) test1_enemy_get_space();
    if (all || test == 14) test2_enemy_get_space();

        
    PRINT_PASSED_PERCENTAGE;
    
    return 1;
    
}
    
void test1_enemy_create() {
	Id id = 5;
    int result = enemy_create(id) != NULL ;
    PRINT_TEST_RESULT(result);
}

void test2_enemy_create() {
	Id id = NO_ID;
    int result = enemy_create(id) == NULL ;
    PRINT_TEST_RESULT(result);
}

void test1_enemy_destroy() {
	Enemy *enemy = NULL;
    int result = enemy_destroy(enemy) != OK ;
    PRINT_TEST_RESULT(result);
}

void test2_enemy_destroy() {
	Enemy *enemy;
	enemy = enemy_create(5);
    PRINT_TEST_RESULT(enemy_destroy(enemy) != ERROR);
}

void test1_enemy_get_id() {
	Enemy *enemy = NULL;
	enemy = enemy_create(5);
    PRINT_TEST_RESULT(enemy_get_id(enemy) != NO_ID);
}

void test2_enemy_get_id() {
	Enemy *enemy = NULL;
    PRINT_TEST_RESULT(enemy_get_id(enemy) == NO_ID);
}

void test1_enemy_set_name() {
    Enemy *enemy = NULL;
    enemy = enemy_create(5);
    PRINT_TEST_RESULT(enemy_set_name(enemy, "test") == OK);
}

void test2_enemy_set_name() {
    Enemy *enemy = NULL;
    PRINT_TEST_RESULT(enemy_set_name(enemy, "test") == ERROR);
}

void test1_enemy_get_name() {
    Enemy *enemy = NULL;
    enemy = enemy_create(5);
    PRINT_TEST_RESULT(enemy_get_name(enemy) != NULL);
}

void test2_enemy_get_name() {
    Enemy *enemy = NULL;
    PRINT_TEST_RESULT(enemy_get_name(enemy) == NULL);
}

void test1_enemy_get_space() {
    Enemy *enemy = NULL;
    enemy = enemy_create(5);
    enemy_set_space(enemy, 8);
    PRINT_TEST_RESULT(enemy_get_space(enemy) != NO_ID);
}

void test2_enemy_get_space() {
    Enemy *enemy = NULL;
    PRINT_TEST_RESULT(enemy_get_space(enemy) == NO_ID);
}

void test1_enemy_set_space() {
    Enemy *enemy = NULL;
    enemy = enemy_create(5);
    PRINT_TEST_RESULT(enemy_set_space(enemy, 8) == OK);
}

void test2_enemy_set_space() {
    Enemy *enemy = NULL;
    enemy = enemy_create(5);
    PRINT_TEST_RESULT(enemy_set_space(enemy, NO_ID) == ERROR);
}



