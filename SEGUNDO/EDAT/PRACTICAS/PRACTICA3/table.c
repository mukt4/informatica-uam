#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "table.h"
#include "type.h"

struct table_ {
    int ncols;
    FILE* pf;
    void** values;
    type_t* types;
    long first;
};


/*
   Creates a file that stores an empty table. This function doesn't
   keep any information in memory: it simply creates the file, stores
   the header information, and closes it.
*/
void table_create(char* path, int ncols, type_t* types) {
    FILE * pf = NULL;
    
    pf = fopen(path, "r+b");
    
    if(fwrite(&ncols, sizeof(ncols), 1, pf) != 2){ 
        fclose(pf);
        return;
    }
    
    fwrite(types, sizeof(int), 1, pf);

    fclose(pf);
    
    return;
}

/* 
   Opens a table given its file name. Returns a pointer to a structure
   with all the information necessary to manage the table. Returns
   NULL is the file doesn't exist or if there is any error.
*/
table_t* table_open(char* path) {
    FILE *pf = NULL;
    table_t* table = NULL;
    
    pf = fopen(path, "r+b");
    
    if(!pf)
        return NULL;
        
    table = (table_t*)malloc(sizeof(table_t));
    
    if(!table){
        fclose(pf);
        return NULL;
    }
    fseek(pf,0, SEEK_SET);
    
    fread(&(table->ncols), sizeof(int), 1, pf);
    table->types = (type_t*)malloc(table_ncols(table)*sizeof(type_t));
    
    fread(table->types, sizeof(type_t), 1, pf);
    
    table->values = (void**)malloc(table_ncols(table)*sizeof(void*));
    table->pf = pf;
    table->first = ftell(pf);
    return table;
}


/* 
   Closes a table freeing the alloc'ed resources and closing the file
   in which the table is stored.
*/
void table_close(table_t* table) {
    int i;
    free(table->types);
        
    for(i=0; i<table_ncols(table); i++){
        free(table->values[i]);
    }
    free(table->values);
    
    fclose(table->pf);
    
    free(table);
    
    return;
}

/* 
   Returns the number of columns of the table 
*/
int table_ncols(table_t* table) {
    return table->ncols;
}

/* 
   Returns the array with the data types of the columns of the
   table. Note that typically this kind of function doesn't make a
   copy of the array, rather, it returns a pointer to the actual array
   contained in the table structure. This means that the calling
   program should not, under any circumstance, modify the array that
   this function returns.
 */
type_t* table_types(table_t* table) {
    if(!table)
        return NULL;
    
    return table->types;
}

/* 
   Returns the position in the file of the first record of the table 
*/
long table_first_pos(table_t* table) {
    if(!table)
        return -1L;
    
    
    return table->first;
}

/* 
   Returns the position in the file in which the table is currently
   positioned. 
*/
long table_cur_pos(table_t* table) {
    if(!table)
        return -1L;
    
    return ftell(table->pf);
}

/* 
   Returns the position just past the last byte in the file, where a
   new record should be inserted.
*/
long table_last_pos(table_t* table) {
    long aux;
    long current;
    if(!table)
        return -1L;
    current=ftell(table->pf);
    fseek(table->pf, 0, SEEK_END);
    aux=ftell(table->pf);
    fseek(table->pf, 0, current);
    return aux;
}

/* 
   Reads the record starting in the specified position. The record is
   read and stored in memory, but no value is returned. The value
   returned is the position of the following record in the file or -1
   if the position requested is past the end of the file.
*/
long table_read_record(table_t* table, long pos) {
    int i;
    int tamanio;
    char* aux;
    
    if(!table || pos > table_last_pos(table))
        return -1;
    
    fseek(table->pf, pos, SEEK_SET);
    
    fread(&tamanio, sizeof(int), 1, table->pf);
    
    aux = (char*)malloc(tamanio);
    fread(aux, tamanio, 1, table->pf);
    for(i = 0; i < table_ncols(table); i++){
        table->values[i] = aux;
        aux += value_length(table->types[i], table->values[i]);
    }
    return ftell(table->pf);
}

/*
  Returns a pointer to the value of the given column of the record
  currently in memory. The value is cast to a void * (it is always a
  pointer: if the column is an INT, the function will return a pointer
  to it).. Returns NULL if there is no record in memory or if the
  column doesn't exist.
*/
void *table_column_get(table_t* table, int col) {
    if(!table)
        return NULL;
        
    return table->values[col];
}


/* 
   Inserts a record in the last available position of the table. Note
   that all the values are cast to void *, and that there is no
   indication of their actual type or even of how many values we ask
   to store... why?
  */
void table_insert_record(table_t* table, void** values) {
    int i;
    int tamanio = 0;
    
    if(!table)
        return;
        
    for(i = 0; table_ncols(table); i++){
        tamanio += value_length(table->types[i], values[i]);
    }
    
    fseek(table->pf, 0, SEEK_END);
    
    fwrite(&tamanio, sizeof(int), 1, table->pf);
    
    for(i = 0; i < table_ncols(table) ; i++){
        fwrite(values[i], value_length(table->types[i], values[i]), 1, table->pf);
    }
}

