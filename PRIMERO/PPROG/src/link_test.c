/** 
 * @brief It defines the link test
 * 
 * @file link_test.c
 * @author Pablo Gutierrez
 * @version 1.0 
 * @date 03-04-2017
 * @copyright GNU Public License
 */

#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 

#include "link.h"
#include "link_test.h"
#include "test.h"

#define MAX_TESTS 24

/** 
 * @brief Funcion principal de pruebas para el modulo Link. 
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
        printf("Running all test for module Link:\n");
    } else {
        test = atoi(argv[1]);
        all = 0;
        printf("Running test %d:\t", test);
	if (test < 1 || test > MAX_TESTS) {
	  printf("Error: unknown test %d\t", test);
	  exit(EXIT_SUCCESS);
        }
    }
    
    if (all || test == 1) test1_link_create();
    if (all || test == 2) test2_link_create();
    if (all || test == 3) test1_link_set_name();
    if (all || test == 4) test2_link_set_name();
    if (all || test == 5) test3_link_set_name();
    if (all || test == 6) test1_link_set_id();
    if (all || test == 7) test2_link_set_id();
    if (all || test == 8) test3_link_set_id();
    if (all || test == 9) test1_link_set_tied();
    if (all || test == 10) test2_link_set_tied();
    if (all || test == 11) test3_link_set_tied();
    if (all || test == 12) test4_link_set_tied();
    if (all || test == 13) test1_link_set_status();
    if (all || test == 14) test2_link_set_status();
    if (all || test == 15) test1_link_get_name();
    if (all || test == 16) test2_link_get_name();
    if (all || test == 17) test1_link_get_id();
    if (all || test == 18) test2_link_get_id();
    if (all || test == 19) test1_link_get_tied1();
    if (all || test == 20) test2_link_get_tied1();
    if (all || test == 21) test1_link_get_tied2();
    if (all || test == 22) test2_link_get_tied2();
    if (all || test == 23) test1_link_get_status();
    if (all || test == 24) test2_link_get_status();

    PRINT_PASSED_PERCENTAGE;

    return 1;
}

void test1_link_create() {
  int result = link_create(-1)==NULL ;
  PRINT_TEST_RESULT(result);
}

void test2_link_create() {
    Link *l;
    l = link_create(4);
    PRINT_TEST_RESULT(link_get_id(l) == 4);
}

void test1_link_set_name() {
    Link *l;
    l = link_create(5);
    PRINT_TEST_RESULT(link_set_name(l, "hola") == OK);
}

void test2_link_set_name() {
    Link *l = NULL;
    PRINT_TEST_RESULT(link_set_name(l, "hola") == ERROR);
}

void test3_link_set_name() {
    Link *l;
    l = link_create(5);
    PRINT_TEST_RESULT(link_set_name(l, NULL) == ERROR);
}

void test1_link_set_id() {
    Link *l;
    l = link_create(5);
    PRINT_TEST_RESULT(link_set_id(l, 6) == OK);
}

void test2_link_set_id() {
    Link *l = NULL;
    PRINT_TEST_RESULT(link_set_id(l, 6) == ERROR);
}
void test3_link_set_id() {
    Link *l;
    l= link_create(5);
    PRINT_TEST_RESULT(link_set_id(l, -1) == ERROR);
}

void test1_link_set_tied() {
    Link *l;
    l = link_create(5);
    PRINT_TEST_RESULT(link_set_tied(l, 4, 3) == OK);
}

void test2_link_set_tied() {
    Link *l = NULL;
    PRINT_TEST_RESULT(link_set_tied(l, 4, 3) == ERROR);
}
void test3_link_set_tied() {
    Link *l;
    l = link_create(5);
    PRINT_TEST_RESULT(link_set_tied(l, -1, 3) == ERROR);
}
void test4_link_set_tied() {
    Link *l;
    l = link_create(5);
    PRINT_TEST_RESULT(link_set_tied(l, 4, -1) == ERROR);
}

void test1_link_set_status() {
    Link *l;
    l = link_create(5);
    PRINT_TEST_RESULT(link_set_status(l, TRUE) == OK);
}

void test2_link_set_status() {
    Link *l = NULL;
    PRINT_TEST_RESULT(link_set_status(l, TRUE) == ERROR);
}

void test1_link_get_name() {
    Link *l;
    l = link_create(1);
    link_set_name(l, "adios");
    PRINT_TEST_RESULT(strcmp(link_get_name(l), "adios") == 0);

}

void test2_link_get_name() {
    Link *l = NULL;
    PRINT_TEST_RESULT(link_get_name(l) == NULL);

}

void test1_link_get_id() {
    Link *l;
    l = link_create(1);
    PRINT_TEST_RESULT(link_get_id(l)== 1);
     
}

void test2_link_get_id() {
    Link *l= NULL;
    PRINT_TEST_RESULT(link_get_id(l) == NO_ID);
}

void test1_link_get_tied1() {
    Link *l;
    l = link_create(5);
    link_set_tied(l, 4, 5);
    PRINT_TEST_RESULT(link_get_tied1(l) == 4);
}

void test2_link_get_tied1() {
    Link *l = NULL;
    PRINT_TEST_RESULT(link_get_tied1(l) == NO_ID);
}

void test1_link_get_tied2() {
    Link *l;
    l = link_create(5);
    link_set_tied(l, 4, 5);
    PRINT_TEST_RESULT(link_get_tied2(l) == 5);
}

void test2_link_get_tied2() {
    Link *l = NULL;
    PRINT_TEST_RESULT(link_get_tied2(l) == NO_ID);
}

void test1_link_get_status() {
    Link *l;
    l = link_create(5);
    link_set_status(l,TRUE);
    PRINT_TEST_RESULT(link_get_status(l)==TRUE);
}

void test2_link_get_status() {
    Link *l = NULL;
    PRINT_TEST_RESULT(link_get_status(l)==FALSE);
}
