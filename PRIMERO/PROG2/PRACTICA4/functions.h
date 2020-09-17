#ifndef FUNCTIONS_H
#define FUNCTIONS_H

/* Las siguientes se usarán cuando se quieran guardar enteros en la pila */
void destroy_intp_function(void* e);

void * copy_intp_function(const void* e);

int print_intp_function(FILE * f, const void* e);

int cmp_intp_function(const void*, const void*);

/* Las siguientes se usarán cuando se quieran guardar nodos en la pila */
void destroy_node_function(void* e);

void * copy_node_function(const void* e);

int print_node_function(FILE * f, const void* e);

int cmp_node_function(const void*, const void*);

/*Las siguientes se usarán cuando se quieran guardar cadenas de caracteres en la pila*/
void destroy_char_function(void* e);

void * copy_char_function(const void* e);

int print_char_function(FILE * f, const void* e);

int cmp_char_function(const void*, const void*);

#endif