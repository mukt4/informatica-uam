/** 
 * @brief It declares the test for set functions
 * 
 * @file set_test.h
 * @author Tomas Higuera
 * @version 1.0 
 * @date 03-04-2017
 * @copyright GNU Public License
 */

#ifndef SET_TEST_H
#define SET_TEST_H

/**
 * @test Prueba la función de creación de un set
 * @pre Creacion de un set no nulo
 * @post Un puntero no nulo al set creado
 */
void test1_set_create();

/**
 * @test Prueba la función de creación de un set
 * @pre Creacion de un set no nulo
 * @post Un puntero no nulo al set creado
 */
void test2_set_create();

/**
 * @test Prueba la función de añadir un id
 * @pre Un puntero nulo al crear el set
 * @post La salida debe ser ERROR
 */
void test1_set_add();

/**
 * @test Prueba la función de añadir un id
 * @pre Creacion de un set no nulo
 * @pre Un identificador como parametro en la funcion  set_add
 * @post La salida debe ser OK
 */
void test2_set_add();

/**
 * @test Prueba la función de borrar un id
 * @pre Un puntero nulo al crear el set
 * @post La salida debe ser ERROR
 */
void test1_set_delete();

/**
 * @test Prueba la función de borrar un id
 * @pre Creacion de un set no nulo
 * @pre Un identificador como parametro en la funcion  set_delete
 * @post La salida debe ser OK
 */
void test2_set_delete();

/**
 * @test Prueba la función de comprobar si existe un id determinado en el set
 * @pre Un puntero nulo al crear el set
 * @pre Un identificador como parametro en la funcion set_checkId
 * @post La salida debe ser ERROR
 */
void test1_set_checkId();

/**
 * @test Prueba la función de comprobar si existe un id determinado en el set
 * @pre Creacion de un set no nulo
 * @pre NO_ID en la funcion set_checkId
 * @post La salida debe ser ERROR
 */
void test2_set_checkId();

/**
 * @test Prueba la función de comprobar si existe un id determinado en el set
 * @pre Creacion de un set no nulo
 * @pre Un identificador como parametro en la funcion set_checkId
 * @post La salida debe ser ERROR
 */
void test3_set_checkId();

/**
 * @test Prueba la función de comprobar si existe un id determinado en el set
 * @pre Creacion de un set no nulo
 * @pre Un identidicador como parametro en la funcion set_checkId
 * @pre Un identificador como parametro en la funcion set_checkId
 * @post La salida debe ser OK
 */
void test4_set_checkId();

/**
 * @test Prueba la función de comprobar si existe un id determinado en el set
 * @pre Un puntero nulo al crear el set
 * @post La salida debe ser -1
 */
void test1_set_getNumId();

/**
 * @test Prueba la función de comprobar si existe un id determinado en el set
 * @pre Creacion de un set no nulo
 * @pre Un identificador como parametro en la funcion set_getNumId
 * @post La salida debe ser 0
 */
void test2_set_getNumId();

/**
 * @test Prueba la función de comprobar si existe un id determinado en el set
 * @pre Creacion de un set no nulo
 * @pre Un identidicador como parametro en la funcion set_add
 * @post La salida debe ser 1
 */
void test3_set_getNumId();

/**
 * @test Prueba la función de devolver un id determinado del set
 * @pre Un puntero nulo al crear el set
 * @pre Un identidicador como parametro en la funcion set_get_id_at
 * @post La salida debe ser NO_ID
 */
void test1_get_id_at();

/**
 * @test Prueba la función de devolver un id determinado del set
 * @pre Creacion de un set no nulo
 * @pre Un identidicador como parametro en la funcion set_add
 * @pre Un identidicador como parametro en la funcion set_get_id_at
 * @post La salida debe ser 8
 */
void test2_get_id_at();

/**
 * @test Prueba la función de devolver el array de ids
 * @pre Un puntero nulo al crear el set
 * @post La salida debe ser distinto de NULL
 */
void test1_set_get_ids();

/**
 * @test Prueba la función de devolver el array de ids
 * @pre Un puntero nulo al crear el set
 * @post La salida debe ser NULL
 */
void test2_set_get_ids();

#endif