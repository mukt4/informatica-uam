/** 
 * @brief It declares the test for player functions
 * 
 * @file player_test.h
 * @author Guillermo Hoyo
 * @version 1.0 
 * @date 03-04-2017
 * @copyright GNU Public License
 */

#ifndef PLAYER_TEST_H
#define PLAYER_TEST_H

/**
 * @test Prueba la función de creación de un jugador
 * @pre Un identificador como parámetro
 * @post Un puntero no nulo al jugador creado
 */
void test1_player_create();

/**
 * @test Prueba la función de creación de un jugador
 * @pre Un identificador como parámetro
 * @post El identificador del jugador es el introducido
 */
void test2_player_create();

/**
 * @test Prueba la función para establecele un nombre al jugador
 * @pre un puntero nulo al crear el inventario
 * @pre una cadena de caracteres como parametro de la funcion player_set_name
 * @post La salida debe ser ERROR
 */
void test1_player_set_name();

/**
 * @test Prueba la función para establecele un nombre al jugador
 * @pre Un identificador como parámetro para el jugador
 * @pre una cadena de caracteres a NULL como parametro de la funcion player_set_name
 * @post La salida debe ser ERROR
 */
void test2_player_set_name();

/**
 * @test Prueba la función para establecele un nombre al jugador
 * @pre Un identificador como parámetro para el jugador
 * @pre una cadena de caracteres como parametro de la funcion player_set_name
 * @post La salida debe ser OK
 */
void test3_player_set_name();

/**
 * @test Prueba la función para establecele el espacio actual del jugador
 * @pre un puntero nulo al crear el inventario
 * @pre Un identificador como parámetro para la funcion player_set_space
 * @post La salida debe ser ERROR
 */
void test1_player_set_space();

/**
 * @test Prueba la función para establecele el espacio actual del jugador
 * @pre Un identificador como parámetro para el jugador
 * @pre El identificador NO_ID como parámetro para la funcion player_set_space
 * @post La salida debe ser ERROR
 */
void test2_player_set_space();

/**
 * @test Prueba la función para establecele el espacio actual del jugador
 * @pre Un identificador como parámetro para el jugador
 * @pre Un identificador como parámetro para la funcion player_set_space
 * @post La salida debe ser OK
 */
void test3_player_set_space();

/**
 * @test Prueba la función de comprobar si el inventario del jugador tiene una id detertminD
 * @pre Un puntero nulo al crear el inventario
 * @pre Un identificador como parámetro para la funcion player_inventory_have_object
 * @post La salida debe ser ERROR
 */
void test1_player_inventory_have_object();

/**
 * @test Prueba la función de comprobar si el inventario del jugador tiene una id detertminD
 * @pre Un identificador como parámetro para el jugador
 * @pre El identificador NO_ID como parámetro para la funcion player_inventory_have_object
 * @post La salida debe ser FALSE
 */
void test2_player_inventory_have_object();

/**
 * @test Prueba la función de comprobar si el inventario del jugador tiene una id detertminD
 * @pre Un identificador como parámetro para el jugador
 * @pre Un identificador como parámetro para la funcion player_inventory_have_object
 * @post La salida debe ser FALSE
 */
void test3_player_inventory_have_object();

/**
 * @test Prueba la función de comprobar si el inventario del jugador tiene una id detertminD
 * @pre Un identificador como parámetro para el jugador
 * @pre Un identificador como parámetro para la funcion player_add_object
 * @pre Un identificador como parámetro para la funcion player_inventory_have_object
 * @post La salida debe ser TRUE
 */
void test4_player_inventory_have_object();

/**
 * @test Prueba la función de añadir un objeto
 * @pre Un puntero nulo al crear el inventario
 * @pre El identificador NO_ID como parámetro para la funcion player_add_object
 * @post La salida debe ser ERROR
 */ 
void test1_player_add_object();

/**
 * @test Prueba la función de añadir un objeto
 * @pre Un puntero nulo al crear el inventario
 * @pre Un identificador como parámetro para la funcion player_add_object
 * @post La salida debe ser ERROR
 */
void test2_player_add_object();

/**
 * @test Prueba la función de añadir un objecto
 * @pre Un identificador como parámetro para el jugador
 * @pre El identificador como parámetro para la funcion player_add_object
 * @post La salida debe ser OK
 */
void test3_player_add_object();

/**
 * @test Prueba la función de eliminar un objecto
 * @pre Un identificador como parámetro para el jugador
 * @pre El identificador NO_ID como parámetro para la funcion player_delete_object
 * @post La salida debe ser ERROR
 */ 
void test1_player_delete_object();

/**
 * @test Prueba la función de eliminar un objecto
 * @pre Un identificador como parámetro para el jugador
 * @pre El identificador como parámetro para la funcion player_delete_object
 * @post La salida debe ser OK
 */ 
void test2_player_delete_object();

/**
 * @test Prueba la función de obtener la id del jugador
 * @pre Un puntero nulo al crear el inventario
 * @post La salida debe ser NO_ID
 */ 
void test1_player_get_id();

/**
 * @test Prueba la función de obtener la id del jugador
 * @pre Un puntero nulo al crear el inventario
 * @post La salida debe ser NO_ID
 */ 
void test2_player_get_id();
    
/**
 * @test Prueba la función de obtener el nombre del jugador
 * @pre Un char* como nombre del jugador
 * @post La salida debe ser OK
 */ 
void test1_player_get_name();

/**
 * @test Prueba la función de obtener el nombre del jugador
 * @pre Un char* como nombre del jugador
 * @post La salida debe ser OK
 */ 
void test2_player_get_name();

/**
 * @test Prueba la función de obtener el id del espacio en el que se encuentra el jugador
 * @pre Un identificador como espacio en el que se encuentra el jugador
 * @post La salida debe ser OK
 */     
void test1_player_get_space();

/**
 * @test Prueba la función de obtener el id del espacio en el que se encuentra el jugador
 * @pre Un identificador como espacio en el que se encuentra el jugador
 * @post La salida debe ser OK
 */   
void test2_player_get_space();

/**
 * @test Prueba la función de obtener el id del espacio en el que se encuentra el jugador
 * @pre Un identificador como espacio en el que se encuentra el jugador
 * @post La salida debe ser OK
 */   
void test3_player_get_space();

/**
 * @test Prueba la función de obtener el inventario del jugador
 * @pre El player sin inicializar
 * @post La salida debe ser OK
 */   
void test1_player_get_inventory();

/**
 * @test Prueba la función de obtener el inventario del jugador
 * @pre El player inicializado
 * @post La salida debe ser OK
 */   
void test2_player_get_inventory();
    
/**
 * @test Prueba la función de obtener las ids del inventario el jugador
 * @pre El inventario sin setear
 * @post La salida debe ser OK
 */       
void test1_player_get_inventory_ids();

/**
 * @test Prueba la función de obtener las ids del inventario el jugador
 * @pre El inventario seteado
 * @post La salida debe ser OK
 */       
void test2_player_get_inventory_ids();


#endif