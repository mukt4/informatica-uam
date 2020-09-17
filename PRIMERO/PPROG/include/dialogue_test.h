/** 
 * @brief It declares the test for dialogue functions
 * 
 * @file dialogue_test.h
 * @author Alvaro Lopez
 * @version 1.0 
 * @date 05-05-2017
 * @copyright GNU Public License
 */

#ifndef DIALOGUE_TEST_H
#define DIALOGUE_TEST_H

/**
 * @test Prueba la función de create de dialogue .
 * @pre Se ejecuta correctamente
 * @post OK
 */
void test1_dialogue_create();


/**
 * @test Prueba la función de destroy de un dialogue .
 * @pre Se ejecuta correctamente
 * @post OK
 */
void test1_dialogue_destroy();

/**
 * @test Prueba la función de destroy de dialogue .
 * @pre Se ejecuta correctamente
 * @post ERROR
 */
void test2_dialogue_destroy();

/**
 * @test Prueba la función de set de dialogue .
 * @pre Se ejecuta correctamente
 * @post ERROR
 */
void test1_dialogue_set();

/**
 * @test Prueba la función de set de dialogue .
 * @pre Se ejecuta correctamente
 * @post Distinto de NULL
 */
void test2_dialogue_set();

/**
 * @test Prueba la función de get de dialogue .
 * @pre Se ejecuta correctamente
 * @post NULL
 */
void test1_dialogue_get();

/**
 * @test Prueba la función de get de dialogue .
 * @pre Se ejecuta correctamente
 * @post 
 */
void test2_dialogue_get();



#endif
