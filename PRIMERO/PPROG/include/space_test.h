/** 
 * @brief It declares the test for space functions
 * 
 * @file space_test.h
 * @author Profesores Pprog
 * @version 2.0 
 * @date 03-04-2017
 * @copyright GNU Public License
 */
 
#ifndef SPACE_TEST_H
#define SPACE_TEST_H

/**
 * @test Prueba la función de creación de un espacio
 * @pre Un identificador como parámetro
 * @post Un puntero no nulo al espacio creado
 */
void test1_space_create();

/**
 * @test Prueba la función de creación de un espacio
 * @pre Un identificador como parámetro
 * @post El identificador del espacio es el introducido
 */
void test2_space_create();

/**
 * @test Prueba la función para establecer el nombre de un espacio
 * @pre Nombre que establecer al espacio
 * @post La salida debe ser OK
 */
void test1_space_set_name();

/**
 * @test Prueba la función para establecer el nombre de un espacio
 * @pre El espacio al que establecer el nombre es un puntero a NULL
 * @post La salida debe ser ERROR
 */
void test2_space_set_name();

/**
 * @test Prueba la función para establecer el nombre de un espacio
 * @pre El espacio es un puntero no NULL, pero el nombre a establecer es NULL
 * @post La salida debe ser ERROR
 */
void test3_space_set_name();

/**
 * @test Prueba la función para establecer el id del espacio al norte
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test1_space_set_north();

/**
 * @test Prueba la función para establecer el id del espacio al norte
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test2_space_set_north();

/**
 * @test Prueba la función para establecer el id del espacio al norte
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test3_space_set_north();

/**
 * @test Prueba la función para establecer el id del espacio al norte
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test4_space_set_north();

/**
 * @test Prueba la función para establecer el id del espacio al sur
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test1_space_set_south();

/**
 * @test Prueba la función para establecer el id del espacio al sur
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test2_space_set_south();

/**
 * @test Prueba la función para establecer el id del espacio al sur
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test3_space_set_south();

/**
 * @test Prueba la función para establecer el id del espacio al sur
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test4_space_set_south();

/**
 * @test Prueba la función para establecer el id del espacio al este
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test1_space_set_east();

/**
 * @test Prueba la función para establecer el id del espacio al este
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test2_space_set_east();

/**
 * @test Prueba la función para establecer el id del espacio al este
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test3_space_set_east();

/**
 * @test Prueba la función para establecer el id del espacio al este
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test4_space_set_east();

/**
 * @test Prueba la función para establecer el id del espacio al oeste
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test1_space_set_west();

/**
 * @test Prueba la función para establecer el id del espacio al oeste
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test2_space_set_west();

/**
 * @test Prueba la función para establecer el id del espacio al oeste
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test3_space_set_west();

/**
 * @test Prueba la función para establecer el id del espacio al oeste
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test4_space_set_west();

/**
 * @test Prueba la función para establecer el id del espacio
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test1_space_get_id();

/**
 * @test Prueba la función para establecer el id del espacio
 * @pre El id que establecer al espacio
 * @post La salida debe ser OK
 */
void test2_space_get_id();

/**
 * @test Prueba la función para añadir un objeto al espacio
 * @pre El id del espacio que se añadira
 * @post La salida debe ser OK
 */
void test1_space_add_object();

/**
 * @test Prueba la función para añadir un objeto al espacio
 * @pre El id del espacio que se añadira
 * @post La salida debe ser OK
 */
void test2_space_add_object();

/**
 * @test Prueba la función para obtener el nombre de un espacio
 * @pre El nombre del espacio
 * @post La salida debe ser OK
 */
void test1_space_get_name();

/**
 * @test Prueba la función para obtener el nombre de un espacio
 * @pre El nombre del espacio
 * @post La salida debe ser OK
 */
void test2_space_get_name();

/**
 * @test Prueba la función para obtener el id del espaco del norte
 * @pre El id del espacio del norte
 * @post La salida debe ser OK
 */
void test1_space_get_north();

/**
 * @test Prueba la función para obtener el id del espaco del norte
 * @pre El id del espacio del norte
 * @post La salida debe ser OK
 */
void test2_space_get_north();

/**
 * @test Prueba la función para obtener el id del espaco del sur
 * @pre El id del espacio del sur
 * @post La salida debe ser OK
 */
void test1_space_get_south();

/**
 * @test Prueba la función para obtener el id del espaco del sur
 * @pre El id del espacio del sur
 * @post La salida debe ser OK
 */
void test2_space_get_south();

/**
 * @test Prueba la función para obtener el id del espaco del este
 * @pre El id del espacio del este
 * @post La salida debe ser OK
 */
void test1_space_get_east();

/**
 * @test Prueba la función para obtener el id del espaco del este
 * @pre El id del espacio del este
 * @post La salida debe ser OK
 */
void test2_space_get_east();

/**
 * @test Prueba la función para obtener el id del espaco del oeste
 * @pre El id del espacio del oeste
 * @post La salida debe ser OK
 */
void test1_space_get_west();

/**
 * @test Prueba la función para obtener el id del espaco del oeste
 * @pre El id del espacio del oeste
 * @post La salida debe ser OK
 */
void test2_space_get_west();

/**
 * @test Prueba la función para obtener los ids de objetos del espacio
 * @pre El id del objeto añadido
 * @post La salida debe ser OK
 */
void test1_space_get_objects();

/**
 * @test Prueba la función para obtener los ids de objetos del espacio
 * @pre El id del objeto añadido
 * @post La salida debe ser OK
 */
void test2_space_get_objects();

/**
 * @test Prueba la función para obtener los ids de objetos del espacio
 * @pre El id del objeto añadido
 * @post La salida debe ser OK
 */
void test3_space_get_objects();

#endif
