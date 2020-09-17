#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sql.h>
#include <sqlext.h>
#include "odbc.h"
#include "table.h"
#include "type.h"
/*NOSOTROS NO TENEMOS ID_LIBRO ASI QUE NO LO METEMOS EN EL FICHERO BINARIO*/


int main(int argc, char** argv){
	SQLHENV env;
    	SQLHDBC dbc;
    	SQLHSTMT stmt;
    	SQLRETURN ret; /* ODBC API return status */
    	char consulta[300];
	int i;
	int puntuacion;
	char titulo[500]={0};
	char ISBN[200];
	table_t* tabla;
	int ncols = 3;
	type_t type[3];
	void** values;
	
	
	/* CONNECT */
    ret = odbc_connect(&env, &dbc);

    if (!SQL_SUCCEEDED(ret)) {
        return EXIT_FAILURE;
    }
	type[0] = STR;
	type[1] = STR;
	type[2] = INT;
     /*Allocate a statement handle*/
        SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
	
	if(argc<3){
		printf("ERROR EN LOS ARGUMENTOS\n");	
		printf("./puntuar titulo puntuacion\n");
		SQLFreeHandle(SQL_HANDLE_STMT, stmt);
		/* DISCONNECT */
		ret = odbc_disconnect(env, dbc);
    	if (!SQL_SUCCEEDED(ret)) {
        	return EXIT_FAILURE;
    	}
		return EXIT_FAILURE;
	}
	for(i = 1; i < (argc - 1) ; i++){
	    if(i < (argc - 2)){
	    	strcat(titulo,argv[i]);
	    	strcat(titulo," ");
	    }
	    else{
	        strcat(titulo,argv[i]);
            }
	}
	values = (void**)malloc(ncols*sizeof(void*));
	table_create("puntuaciones.dat", ncols, type);	
	tabla = table_open("puntuaciones.dat");
	puntuacion = atoi(argv[argc - 1]);
	sprintf(consulta,"SELECT id_libro from libro where titulo='%s'", titulo);
	ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);

	if (!SQL_SUCCEEDED(ret)) {
		table_close(tabla);
		SQLFreeHandle(SQL_HANDLE_STMT, stmt);
		ret = odbc_disconnect(env, dbc);
    		if (!SQL_SUCCEEDED(ret)) {
        		return EXIT_FAILURE;
    		}
        	return EXIT_FAILURE;
	}
	ret = SQLFetch(stmt);
	ret = SQLGetData(stmt, 1, SQL_CHAR, ISBN, sizeof(ISBN), NULL);
	SQLFreeHandle(SQL_HANDLE_STMT, stmt);
	if (!SQL_SUCCEEDED(ret)) {
		table_close(tabla);
		ret = odbc_disconnect(env, dbc);
    		if (!SQL_SUCCEEDED(ret)) {
        		return EXIT_FAILURE;
    		}
        	return EXIT_FAILURE;
	}
	
	values[0] = (void*)ISBN;
	values[1] = (void*)titulo;
	values[2] = (void*)&puntuacion;
	
	/*Escritura en fichero binario*/
	table_insert_record(tabla, values);
	/* DISCONNECT */
   	ret = odbc_disconnect(env, dbc);
    if (!SQL_SUCCEEDED(ret)) {
       	return EXIT_FAILURE;
   	}
	table_close(tabla);
    return EXIT_SUCCESS;
}
	


	
