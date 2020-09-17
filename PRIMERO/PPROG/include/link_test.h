/** 
 * @brief It declares the test for link functions
 * 
 * @file link_test.h
 * @author Pablo Gutierrez
 * @version 1.0 
 * @date 03-04-2017
 * @copyright GNU Public License
 */

#ifndef LINK_TEST_H
#define LINK_TEST_H

/**
 * @test Prueba la función de creación de un link
 * @pre Un identificador -1 como parámetro
 * @post Un puntero nulo al link creado
 */
void test1_link_create();

/**
 * @test Prueba la función de creación de un link
 * @pre Un identificador como parámetro
 * @post El identificador del espacio es el introducido
 */
void test2_link_create();

/**
 * @test Prueba la función para establecer el nombre de un link
 * @pre Nombre que establecer al link
 * @post La salida debe ser OK
 */
void test1_link_set_name();

/**
 * @test Prueba la función para establecer el nombre de un link
 * @pre El link al que establecer el nombre es un puntero a NULL
 * @post La salida debe ser ERROR
 */
void test2_link_set_name();

/**
 * @test Prueba la función para establecer el nombre de un link
 * @pre El link es un puntero no NULL, pero el nombre a establecer es NULL
 * @post La salida debe ser ERROR
 */
void test3_link_set_name();

/**
 * @test Prueba la función para establecer el id de un link
 * @pre Id que establecer al link
 * @post La salida debe ser OK
 */
void test1_link_set_id();

/**
 * @test Prueba la función para establecer el id de un link
 * @pre El link al que establecer el id es un puntero a NULL
 * @post La salida debe ser ERROR
 */
void test2_link_set_id();

/**
 * @test Prueba la función para establecer el id de un link
 * @pre El link es un puntero no NULL, pero el id a establecer es NULL
 * @post La salida debe ser ERROR
 */
void test3_link_set_id();

/**
 * @test Prueba la función para establecer los tied de un link
 * @pre Ids que establecer al link como tied1 y tied2
 * @post La salida debe ser OK
 */
void test1_link_set_tied();

/**
 * @test Prueba la función para establecer los tied de un link
 * @pre El link al que establecer tied1 y tied2 es un puntero a NULL
 * @post La salida debe ser ERROR
 */
void test2_link_set_tied();

/**
 * @test Prueba la función para establecer los tied de un link
 * @pre Ids que establecer al link con tied1 negativo y tied2 positivo
 * @post La salida debe ser ERROR
 */
void test3_link_set_tied();

/**
 * @test Prueba la función para establecer los tied de un link
 * @pre Ids que establecer al link con tied2 negativo y tied1 positivo
 * @post La salida debe ser ERROR
 */
void test4_link_set_tied();

/**
 * @test Prueba la función para establecer el status de un link
 * @pre BOOL value que establecer al link
 * @post La salida debe ser OK
 */
void test1_link_set_status();

/**
 * @test Prueba la función para establecer el status de un link
 * @pre BOOL value que establecer al link que es un puntero a NULL
 * @post La salida debe ser ERROR
 */
void test2_link_set_status();

/**
 * @test Prueba la función para obtener el nombre de un link
 * @pre Char*,nombre que se establecera al link
 * @post La salida debe ser ERROR
 */
void test1_link_get_name();

/**
 * @test Prueba la función para obtener el nombre de un link
 * @pre Char * que se establecera a NULL
 * @post La salida debe ser ERROR
 */
void test2_link_get_name();

/**
 * @test Prueba la función para obtener el id de un link
 * @pre Id que se establecera positivo
 * @post La salida debe ser ERROR
 */
void test1_link_get_id();

/**
 * @test Prueba la función para obtener el id de un link
 * @pre Id que se establecera con valor NO_ID
 * @post La salida debe ser ERROR
 */
void test2_link_get_id();

/**
 * @test Prueba la función para obtener el id de la conexion de un link
 * @pre Id que se establecera en tied1 positivo
 * @post La salida debe ser ERROR
 */
void test1_link_get_tied1();

/**
 * @test Prueba la función para obtener el id de la conexion de un link
 * @pre Id que se establecera en tied1 igualado a NO_ID
 * @post La salida debe ser ERROR
 */
void test2_link_get_tied1();

/**
 * @test Prueba la función para obtener el id de la conexion de un link
 * @pre Id que se establecera en tied2 positivo
 * @post La salida debe ser ERROR
 */
void test1_link_get_tied2();

/**
 * @test Prueba la función para obtener el id de la conexion de un link
 * @pre Id que se establecera en tied2 igualado a NO_ID
 * @post La salida debe ser ERROR
 */
void test2_link_get_tied2();

/**
 * @test Prueba la función para obtener status de la conexion de un link
 * @pre BOOL con valor TRUE que seteara en link
 * @post La salida debe ser ERROR
 */
void test1_link_get_status();

/**
 * @test Prueba la función para obtener status de la conexion de un link
 * @pre BOOL con valor FALSE que seteara en link
 * @post La salida debe ser ERROR
 */
void test2_link_get_status();

#endif