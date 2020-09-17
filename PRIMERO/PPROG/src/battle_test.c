/** 
 * @brief It defines the battle test
 * 
 * @file battle_test.c
 * @author Alvaro Lopez
 * @version 1.0 
 * @date 05-05-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "battle.h"
#include "battle_test.h"
#include "test.h"

#define MAX_TESTS 2

/** 
 * @brief Funcion principal de pruebas para el modulo Battle. 
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
        printf("Running all test for module Battle:\n");
    } else {
        test = atoi(argv[1]);
        all = 0;
        printf("Running test %d:\t", test);
	if (test < 1 || test > MAX_TESTS) {
	  printf("Error: unknown test %d\t", test);
	  exit(EXIT_SUCCESS);
        }
    }
    
    if (all || test == 1) test1_battle_on();
    if (all || test == 2) test2_battle_on();
    
    PRINT_PASSED_PERCENTAGE;
    
    return 1;
}
void test1_battle_on(){
    Game *g;
    int result = battle_on(g)!=ERROR;
    PRINT_TEST_RESULT(result);
}
void test2_battle_on(){
    Game* g=NULL;
    PRINT_TEST_RESULT(battle_on(g) == ERROR);
}
