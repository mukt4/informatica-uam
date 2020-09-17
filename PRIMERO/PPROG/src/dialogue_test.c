/** 
 * @brief It defines the dialogue test
 * 
 * @file dialogue_test.c
 * @author Alvaro Lopez
 * @version 1.0 
 * @date 05-05-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "dialogue.h"
#include "dialogue_test.h"
#include "test.h"

#define MAX_TESTS 7

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
        printf("Running all test for module Dialogue:\n");
    } else {
        test = atoi(argv[1]);
        all = 0;
        printf("Running test %d:\t", test);
	if (test < 1 || test > MAX_TESTS) {
	  printf("Error: unknown test %d\t", test);
	  exit(EXIT_SUCCESS);
        }
    }
    
    if (all || test == 1) test1_dialogue_create();
    if (all || test == 2) test1_dialogue_destroy();
    if (all || test == 3) test2_dialogue_destroy();
    if (all || test == 4) test1_dialogue_set();
    if (all || test == 5) test2_dialogue_set();
    if (all || test == 6) test1_dialogue_get();
    if (all || test == 7) test2_dialogue_get();

    PRINT_PASSED_PERCENTAGE;
    
    return 1;
}

void test1_dialogue_create(){
    int result = dialogue_create()!=NULL;
    PRINT_TEST_RESULT(result);
}


void test1_dialogue_destroy() {
	Dialogue *d = NULL;
	d = dialogue_create(5);
    PRINT_TEST_RESULT(dialogue_destroy(d) != ERROR);
}

void test2_dialogue_destroy() {
	Dialogue *d = NULL;
    PRINT_TEST_RESULT(dialogue_destroy(d) == ERROR);
}

void test1_dialogue_set() {
    Dialogue *d = NULL;
    d = dialogue_create(5);
    PRINT_TEST_RESULT(dialogue_set(d, "test") == OK);
}

void test2_dialogue_set() {
    Dialogue *d = NULL;
    PRINT_TEST_RESULT(dialogue_set(d, "test") == ERROR);
}

void test3_dialogue_set() {
    Dialogue *d = NULL;
    d = dialogue_create(5);
    PRINT_TEST_RESULT(dialogue_set(d, NULL) == ERROR);
}

void test1_dialogue_get() {
    Dialogue *d = NULL;
    d = dialogue_create(5);
    PRINT_TEST_RESULT(dialogue_get(d) != NULL);
}

void test2_dialogue_get() {
    Dialogue *d = NULL;
    PRINT_TEST_RESULT(dialogue_get(d) == NULL);
}  
