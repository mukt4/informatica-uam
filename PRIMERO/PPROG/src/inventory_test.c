/** 
 * @brief It defines the die test
 * 
 * @file inventory_test.c
 * @author Guillermo Hoyo
 * @version 1.0 
 * @date 31-03-2017
 * @copyright GNU Public License
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "inventory.h"
#include "inventory_test.h"
#include "test.h"

#define MAX_TESTS 20

/** 
 * @brief Funcion principal de pruebas para el modulo inventory. 
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
        printf("Running all test for module Inventory:\n");
    } else {
        test = atoi(argv[1]);
        all = 0;
        printf("Running test %d:\t", test);
	if (test < 1 || test > MAX_TESTS) {
	  printf("Error: unknown test %d\t", test);
	  exit(EXIT_SUCCESS);
        }
    }
    
    if (all || test == 1) test1_inventory_create();
    if (all || test == 2) test2_inventory_create();
    if (all || test == 3) test1_inventory_set_ids();
    if (all || test == 4) test2_inventory_set_ids();
    if (all || test == 5) test3_inventory_set_ids();
    if (all || test == 6) test4_inventory_set_ids();
    if (all || test == 7) test1_inventory_set_max_objects();
    if (all || test == 8) test2_inventory_set_max_objects();
    if (all || test == 9) test1_inventory_add_id();
    if (all || test == 10) test2_inventory_add_id();
    if (all || test == 11) test1_inventory_delete_id();
    if (all || test == 12) test2_inventory_delete_id();
    if (all || test == 13) test1_inventory_get_ids();
    if (all || test == 14) test2_inventory_get_ids();
    if (all || test == 16) test1_inventory_get_max_objects();
    if (all || test == 17) test2_inventory_get_max_objects();
    if (all || test == 18) test1_inventory_get_numids();
    if (all || test == 19) test2_inventory_get_numids();
    if (all || test == 20) test3_inventory_get_numids();
    
    PRINT_PASSED_PERCENTAGE;
    
    return 1;
}

void test1_inventory_create(){
    int result = inventory_create(8)!=NULL;
    PRINT_TEST_RESULT(result);
}

void test2_inventory_create(){
    Inventory *i;
    i = inventory_create(8);
    PRINT_TEST_RESULT(inventory_get_max_objects(i) == 8);
}    

void test1_inventory_set_ids(){
    Inventory *i = NULL;
    Set * s = NULL;
    PRINT_TEST_RESULT(inventory_set_ids(i, s) == ERROR);
}

void test2_inventory_set_ids(){
    Inventory *i = inventory_create(8);
    Set * s = NULL;
    PRINT_TEST_RESULT(inventory_set_ids(i, s) == ERROR);
}

void test3_inventory_set_ids(){
    Inventory *i = NULL;
    Set * s = set_create();
    PRINT_TEST_RESULT(inventory_set_ids(i, s) == ERROR);
}

void test4_inventory_set_ids(){
    Inventory *i = inventory_create(8);
    Set * s = set_create();
    PRINT_TEST_RESULT(inventory_set_ids(i, s) == OK);
}

void test1_inventory_set_max_objects(){
    Inventory *i = NULL;
    PRINT_TEST_RESULT(inventory_set_max_objects(i, 8) == ERROR);
}

void test2_inventory_set_max_objects(){
    Inventory *i = inventory_create(8);
    PRINT_TEST_RESULT(inventory_set_max_objects(i, 8) == OK);
}

void test1_inventory_add_id(){
    Inventory *i = NULL;
    PRINT_TEST_RESULT(inventory_add_id(i, 8) == ERROR);
}

void test2_inventory_add_id(){
    Inventory *i = inventory_create(8);
    PRINT_TEST_RESULT(inventory_add_id(i, 8) == OK);
}

void test1_inventory_delete_id(){
    Inventory *i = NULL;
    PRINT_TEST_RESULT(inventory_add_id(i, 8) == ERROR);
}

void test2_inventory_delete_id(){
    Inventory *i = inventory_create(8);
    PRINT_TEST_RESULT(inventory_add_id(i, 8) == OK);
}

void test1_inventory_get_ids(){
    Inventory * i = NULL;
    PRINT_TEST_RESULT(inventory_get_ids(i) == NULL);
}

void test2_inventory_get_ids(){
    Inventory * i = inventory_create(8);
    Set * s = set_create();
    inventory_set_ids(i, s);
    PRINT_TEST_RESULT(inventory_get_ids(i) != NULL);
}

void test1_inventory_get_max_objects(){
    Inventory * i = NULL;
    PRINT_TEST_RESULT(inventory_get_max_objects(i) == -1);
}

void test2_inventory_get_max_objects(){
    Inventory * i = inventory_create(8);
    PRINT_TEST_RESULT(inventory_get_max_objects(i) == 8);
}

void test1_inventory_get_numids(){
    Inventory * i = NULL;
    PRINT_TEST_RESULT(inventory_get_numids(i) == ERROR);
}

void test2_inventory_get_numids(){
    Inventory * i = inventory_create(8);
    PRINT_TEST_RESULT(inventory_get_numids(i) == 0);
}

void test3_inventory_get_numids(){
    Inventory * i = inventory_create(8);
    inventory_add_id(i, 8);
    PRINT_TEST_RESULT(inventory_get_numids(i) == 1);
}