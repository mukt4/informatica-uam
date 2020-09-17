/** 
 * @brief It declares the test for die functions
 * 
 * @file die_test.h
 * @author Alvaro Lopez
 * @version 1.0 
 * @date 03-04-2017
 * @copyright GNU Public License
 */

#ifndef DIE_TEST_H
#define DIE_TEST_H

/**
 * @test Prueba la función de creación de un dado
 * @pre Un identificador como parámetro
 * @post Un puntero no nulo al espacio creado
 */
void test1_die_create();

/**
 * @test Prueba la función de creación de un dado
 * @pre Un identificador como parámetro
 * @post El identificador del dado es el intorducido
 */
void test2_die_create();

/**
 * @test Prueba la función de tirar el dado
 * @pre Creacion de un dado a NULL
 * @post ERROR
 */
void test1_die_roll();

/**
 * @test Prueba la función de tirar el dado
 * @pre Creacion de un dado con un identificador como parametro
 * @post OK
 */
void test2_die_roll(); 

/**
 * @test Prueba la función para obtener el ultimo resultado del dado
 * @pre Creacion de un dado a NULL
 * @post El int -1
 */
void test1_die_get_last_num();

/**
 * @test Prueba la función para obtener el ultimo resultado del dado
 * @pre Creacion de un dado con un identificador como parametro
 * @post Cualquier int menos el -1
 */
void test2_die_get_last_num();

/**
 * @test Prueba la función para obtener el identificador del dado
 * @pre Creacion de un dado a NULL
 * @post NO_ID
 */
void test1_die_get_id();

/**
 * @test Prueba la función para obtener el ultimo resultado del dado
 * @pre Creacion de un dado con un identificador como parametro
 * @post El identificador del dado
 */
void test2_die_get_id();

#endif