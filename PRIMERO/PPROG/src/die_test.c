/** 
 * @brief It defines the die test
 * 
 * @file die_test.c
 * @author Alvaro Lopez
 * @version 1.0 
 * @date 12-03-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "die.h"
#include "die_test.h"
#include "test.h"

#define MAX_TESTS 4

/** 
 * @brief Funcion principal de pruebas para el modulo Die. 
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
        printf("Running all test for module Die:\n");
    } else {
        test = atoi(argv[1]);
        all = 0;
        printf("Running test %d:\t", test);
	if (test < 1 || test > MAX_TESTS) {
	  printf("Error: unknown test %d\t", test);
	  exit(EXIT_SUCCESS);
        }
    }
    
    if (all || test == 1) test1_die_create();
    if (all || test == 2) test2_die_create();
    if (all || test == 3) test1_die_roll();
    if (all || test == 4) test2_die_roll();
    if (all || test == 5) test1_die_get_last_num();
    if (all || test == 6) test2_die_get_last_num();
    if (all || test == 7) test1_die_get_id();
    if (all || test == 8) test2_die_get_id();
    
    PRINT_PASSED_PERCENTAGE;
    
    return 1;
}

void test1_die_create(){
    int result = die_create(8)!=NULL;
    PRINT_TEST_RESULT(result);
}

void test2_die_create(){
    Die *d;
    d = die_create(8);
    PRINT_TEST_RESULT(die_get_id(d) == 8);
}

void test1_die_roll(){
    Die * d = NULL;
    PRINT_TEST_RESULT(die_roll(d) == ERROR);
}

void test2_die_roll(){
    Die * d = die_create(8);
    PRINT_TEST_RESULT(die_roll(d) == OK);
}    

void test1_die_get_last_num(){
    Die * d = NULL;
    PRINT_TEST_RESULT(die_get_last_num(d) == -1);
}

void test2_die_get_last_num(){
    Die * d = die_create(8);
    PRINT_TEST_RESULT(die_get_last_num(d) != -1);
}

void test1_die_get_id(){
    Die * d = NULL;
    PRINT_TEST_RESULT(die_get_id(d) == NO_ID);
}

void test2_die_get_id(){
    Die * d = die_create(8);
    PRINT_TEST_RESULT(die_get_id(d) == 8);
}