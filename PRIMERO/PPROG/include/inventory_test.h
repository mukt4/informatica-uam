/** 
 * @brief It declares the test for inventory functions
 * 
 * @file inventory_test.h
 * @author Guillermo Hoyo
 * @version 1.0 
 * @date 03-04-2017
 * @copyright GNU Public License
 */

#ifndef INVENTORY_TEST_H
#define INVENTORY_TEST_H

/**
 * @test Prueba la función de creación de un inventario
 * @pre Un identificador como parámetro
 * @post Un puntero no nulo al inventario creado
 */
void test1_inventory_create();

/**
 * @test Prueba la función de creación de un inventario
 * @pre Un identificador como parámetro
 * @post El identificador del inventario es el introducido
 */
void test2_inventory_create();

/**
 * @test Prueba la función para establecer las ids guardadas en un inventario
 * @pre un puntero nulo al crear el inventario
 * @pre un puntero nulo al crear el set
 * @post La salida debe ser ERROR
 */
void test1_inventory_set_ids();

/**
 * @test Prueba la función para establecer las ids guardadas en un inventario
 * @pre Un identificador como parámetro al inventario
 * @pre un puntero nulo al crear el set
 * @post La salida debe ser ERROR
 */
void test2_inventory_set_ids();

/**
 * @test Prueba la función para establecer las ids guardadas en un inventario
 * @pre un puntero nulo al crear el inventario
 * @pre un puntero no nulo al crear el set
 * @post La salida debe ser ERROR
 */
void test3_inventory_set_ids();

/**
 * @test Prueba la función para establecer las ids guardadas en un inventario
 * @pre Un identificador como parámetro para el inventario
 * @pre un puntero no nulo al crear el set
 * @post La salida debe ser OK
 */
void test4_inventory_set_ids();

/**
 * @test Prueba la función de establecer el campo max_objects
 * @pre un puntero nulo al crear el inventario
 * @post La salida debe ser ERROR
 */
void test1_inventory_set_max_objects();

/**
 * @test Prueba la función de establecer el campo max_objects
 * @pre Un identificador como parámetro para el inventario
 * @pre Un identificador como parametro en la funcion inventory_set_max_objects
 * @post La salida debe ser OK
 */
void test2_inventory_set_max_objects();


/**
 * @test Prueba la función de añadir un id
 * @pre Un puntero nulo al crear el inventario
 * @pre Un identificador como parametro en la funcion inventory_add_id
 * @post La salida debe ser ERROR
 */
void test1_inventory_add_id();

/**
 * @test Prueba la función de añadir un id
 * @pre Un identificador como parámetro para el inventario
 * @pre Un identificador como parametro en la funcion inventory_add_id
 * @post La salida debe ser OK
 */
void test2_inventory_add_id();

/**
 * @test Prueba la función de borrar un id
 * @pre Un puntero nulo al crear el inventario
 * @pre Un identificador como parametro en la funcion inventory_delete_id
 * @post La salida debe ser ERROR
 */
void test1_inventory_delete_id();

/**
 * @test Prueba la función de borrar un id
 * @pre Un identificador como parámetro para el inventario
 * @pre Un identificador como parametro en la funcion inventory_delete_id
 * @post La salida debe ser OK
 */
void test2_inventory_delete_id();


/**
 * @test Prueba la función de obtener el conjunto de los ids
 * @pre Un puntero nulo al crear el inventario
 * @post La salida debe ser NULL
 */
void test1_inventory_get_ids();

/**
 * @test Prueba la función de obtener el conjunto de los ids
 * @pre Un identificador como parámetro para el inventario
 * @pre Puntero no nulo al crear el set
 * @post La salida debe ser != NULL
 */
void test2_inventory_get_ids();

/**
 * @test Prueba la función para obtener el campo max_objects
 * @pre Creacion de un inventory a NULL
 * @post El int -1
 */
void test1_inventory_get_max_objects();

/**
 * @test Prueba la función para obtener el campo max_objects
 * @pre Creacion de un inventory con un identificador como parametro 
 * @post El int 8 (el mismo identificador que al crear el espacio)
 */
void test2_inventory_get_max_objects();

/**
 * @test Prueba la función para obtener el campo numids
 * @pre Creacion de un inventory a NULL
 * @post ERROR
 */
void test1_inventory_get_numids();

/**
 * @test Prueba la función para obtener el campo max_objects
 * @pre Creacion de un inventory con un identificador como parametro 
 * @post El int 0
 */
void test2_inventory_get_numids();

/**
 * @test Prueba la función para obtener el campo max_objects
 * @pre Creacion de un inventory con un identificador como parametro 
 * @pre Adicion de un id al inventario con un identidicador cualquiera
 * @post El int 1 (solo se ha añadido un id al inventario)
 */
void test3_inventory_get_numids();
#endif