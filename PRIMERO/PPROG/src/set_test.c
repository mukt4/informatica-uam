/** 
 * @brief It defines the set test
 * 
 * @file set_test.c
 * @author Tomas Higuera
 * @version 1.0 
 * @date 12-03-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "set.h"
#include "set_test.h"
#include "test.h"

#define MAX_TESTS 17
/** 
 * @brief Funcion principal de pruebas para el modulo Set. 
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
        printf("Running all test for module Set:\n");
    } else {
        test = atoi(argv[1]);
        all = 0;
        printf("Running test %d:\t", test);
	if (test < 1 || test > MAX_TESTS) {
	  printf("Error: unknown test %d\t", test);
	  exit(EXIT_SUCCESS);
        }
    }
    
    if (all || test == 1) test1_set_create();
    if (all || test == 2) test2_set_create();
    if (all || test == 3) test1_set_add();
    if (all || test == 4) test2_set_add();
    if (all || test == 5) test1_set_delete();
    if (all || test == 6) test2_set_delete();
    if (all || test == 7) test1_set_checkId();
    if (all || test == 8) test2_set_checkId();
    if (all || test == 9) test3_set_checkId();
    if (all || test == 10) test4_set_checkId();
    if (all || test == 11) test1_set_getNumId();
    if (all || test == 12) test2_set_getNumId();
    if (all || test == 13) test3_set_getNumId();
    if (all || test == 14) test1_get_id_at();
    if (all || test == 15) test2_get_id_at();
    if (all || test == 16) test1_set_get_ids();
    if (all || test == 17) test2_set_get_ids();
    
    
    PRINT_PASSED_PERCENTAGE;
    return 1;
}

void test1_set_create(){
    int result = set_create() != NULL;
    PRINT_TEST_RESULT(result);
}

void test2_set_create(){
    Set * s = set_create();
    PRINT_TEST_RESULT(set_getNumId(s) == 0);
}

void test1_set_add(){
    Set * s = NULL;
    PRINT_TEST_RESULT(set_add(s, 8) == ERROR);
}

void test2_set_add(){
    Set * s = set_create();
    PRINT_TEST_RESULT(set_add(s, 8) == OK);
}

void test1_set_delete(){
    Set * s = NULL;
    PRINT_TEST_RESULT(set_delete(s, 8) == ERROR);
}

void test2_set_delete(){
    Set * s = set_create();
    PRINT_TEST_RESULT(set_delete(s, 8) == OK);
}

void test1_set_checkId(){
    Set * s = NULL;
    PRINT_TEST_RESULT(set_checkId(s, 8) == ERROR);
}

void test2_set_checkId(){
    Set * s = set_create();
    PRINT_TEST_RESULT(set_checkId(s, NO_ID) == ERROR);
}

void test3_set_checkId(){
    Set * s = set_create();
    PRINT_TEST_RESULT(set_checkId(s, 8) == ERROR);
}

void test4_set_checkId(){
    Set * s = set_create();
    set_add(s, 8);
    PRINT_TEST_RESULT(set_checkId(s, 8) == OK);
}

void test1_set_getNumId(){
    Set * s = NULL;
    PRINT_TEST_RESULT(set_getNumId(s) == -1);
}

void test2_set_getNumId(){
    Set * s = set_create();
    PRINT_TEST_RESULT(set_getNumId(s) == 0);
}

void test3_set_getNumId(){
    Set * s = set_create();
    set_add(s, 8);
    PRINT_TEST_RESULT(set_getNumId(s) == 1);
}

void test1_get_id_at(){
    Set * s = NULL;
    PRINT_TEST_RESULT(set_get_id_at(s, 8) == NO_ID);
}

void test2_get_id_at(){
    Set * s = set_create();
    set_add(s, 8);
    PRINT_TEST_RESULT(set_get_id_at(s, 0) == 8);
}

void test1_set_get_ids(){
    Set * s = set_create();
    PRINT_TEST_RESULT(set_get_ids(s) != NULL);
}

void test2_set_get_ids(){
    Set * s = NULL;
    PRINT_TEST_RESULT(set_get_ids(s) == NULL);
}