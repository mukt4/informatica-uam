#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "types.h"

typedef struct _List List;
typedef struct _NodeList NodeList;

/* Tipos de los punteros a función soportados por la lista */
typedef void (*destroy_elementlist_function_type)(void*);
typedef void (*(*copy_elementlist_function_type)(const void*));
typedef int (*print_elementlist_function_type)(FILE *, const void*);

/* La siguiente función permite comparar dos elementos, devolviendo un número positivo, negativo o
cero según si el primer argumento es mayor, menor o igual que el segundo argumento */
typedef int (*cmp_elementlist_function_type)(const void*, const void*);

/* Inicializa la lista reservando memoria e inicializa todos sus elementos. */
List* list_ini(destroy_elementlist_function_type f1, copy_elementlist_function_type f2,
print_elementlist_function_type f3, cmp_elementlist_function_type f4);

/* Libera la lista y todos sus elementos. */
void list_free(List* list);

/* Inserta al principio de la lista realizando una copia del elemento. */
List* list_insertFirst(List* list, const void *elem);

/* Inserta al final de la lista realizando una copia del elemento. */
List* list_insertLast(List* list, const void *elem);

/* Inserta en orden en la lista realizando una copia del elemento. */
List* list_insertInOrder(List *list, const void *pElem);

/* Extrae del principio de la lista realizando una copia del elemento almacenado en dicho nodo. */
void * list_extractFirst(List* list);

/* Extrae del final de la lista realizando una copia del elemento almacenado en dicho nodo. */
void * list_extractLast(List* list);

/* Comprueba si una lista está vacía o no. */
Bool list_isEmpty(const List* list);

/* Devuelve el elemento i-ésimo almacenado en la lista. En caso de error, devuelve NULL. */
const void* list_get(const List* list, int i);

/* Devuelve el tamaño de una lista. */
int list_size(const List* list);

/* Imprime una lista devolviendo el número de caracteres escritos. */
int list_print(FILE *fd, const List* list);

/*Funcion que obtiene el siguiente NodeList*/
NodeList * nodeList_getNext(NodeList *node);

/*Funcion que obtiene la parte data de un NodeList*/
void * nodeList_getData(NodeList *node);

/*Funcion que obtiene NodeList de una lista*/
NodeList * list_getNode(List *list);

/*Funcion que reserva memoria para NodeList*/
NodeList * nodeList_ini();

/*Funcion que libera memoria de un NodeLis*/
void nodeList_destroy(NodeList *node);

/*Funcion que setea el next de un NodeList*/
NodeList * nodeList_set_next(NodeList *node,NodeList *add);

/*Funcion que setea el data de un NodeList*/
NodeList * nodeList_set_data(NodeList *node, void *data);
