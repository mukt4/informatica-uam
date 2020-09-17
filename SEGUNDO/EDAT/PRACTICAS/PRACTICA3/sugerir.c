#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sql.h>
#include <sqlext.h>
#include "odbc.h"
#include "table.h"
/*NOSOTROS NO TENEMOS ID_LIBRO ASI QUE NO LO METEMOS EN EL FICHERO BINARIO*/


int main(int argc, char** argv){
	SQLHENV env;
   	SQLHDBC dbc;
   	SQLHSTMT stmt;
    	SQLRETURN ret; /* ODBC API return status */
    	char consulta[300];
	char autor[500];
	char ISBN[500];
	int puntuacion;
	int puntuacion2;
	int id_autor;
	char titulo[500];
	table_t* tabla;
	
	tabla = table_open("puntuaciones.dat");
	
	/* CONNECT */
    ret = odbc_connect(&env, &dbc);

    if (!SQL_SUCCEEDED(ret)) {
        return EXIT_FAILURE;
    }

    /* Allocate a statement handle */
    SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
	
	if(argc<2){
		printf("ERROR EN LOS ARGUMENTOS\n");	
		printf("./sugerir puntuacion\n");
		SQLFreeHandle(SQL_HANDLE_STMT, stmt);
		/* DISCONNECT */
		ret = odbc_disconnect(env, dbc);
    	if (!SQL_SUCCEEDED(ret)) {
        	return EXIT_FAILURE;
    	}
		return EXIT_FAILURE;
	}
	puntuacion=atoi(argv[argc-1]);
	
	while(table_cur_pos(tabla)!=table_last_pos(tabla)){
	    table_read_record(tabla,table_first_pos(tabla));
	    ISBN = tabla->values[0];
	    titulo = tabla->values[1];
	    puntuacion2 = tabla->values[2];
	    if(puntuacion2 == puntuacion){
        	sprintf(consulta,"SELECT id_autor from escribe where id_libro='%s'", ISBN);
        	ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
        
        	if (!SQL_SUCCEEDED(ret)) {
        		table_close(tabla);
        		SQLFreeHandle(SQL_HANDLE_STMT, stmt);
        		/* DISCONNECT */
        		ret = odbc_disconnect(env, dbc);
            		if (!SQL_SUCCEEDED(ret)) {
                		return EXIT_FAILURE;
            		}
                	return EXIT_FAILURE;
        	}
		ret = SQLFetch(stmt);
        	ret = SQLGetData(stmt, 1, SQL_C_SLONG, &id_autor, sizeof(SQLINTEGER), NULL);
        	SQLFreeHandle(SQL_HANDLE_STMT, stmt);
        
        	if (!SQL_SUCCEEDED(ret)) {
        		table_close(tabla);
        		/* DISCONNECT */
        		ret = odbc_disconnect(env, dbc);
            		if (!SQL_SUCCEEDED(ret)) {
                		return EXIT_FAILURE;
            		}
                	return EXIT_FAILURE;
        	}
        	SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
        	sprintf(consulta,"SELECT nombre from autor where id_autor=%d", id_autor);
        	ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
        
        	if (!SQL_SUCCEEDED(ret)) {
        		table_close(tabla);
        		SQLFreeHandle(SQL_HANDLE_STMT, stmt);
        		/* DISCONNECT */
        		ret = odbc_disconnect(env, dbc);
            		if (!SQL_SUCCEEDED(ret)) {
                		return EXIT_FAILURE;
            		}
                	return EXIT_FAILURE;
        	}
        	ret = SQLFetch(stmt);
        	ret = SQLGetData(stmt, 1, SQL_CHAR, autor, sizeof(autor), NULL);
        	SQLFreeHandle(SQL_HANDLE_STMT, stmt);
        
        	if (!SQL_SUCCEEDED(ret)) {
        		table_close(tabla);
        		/* DISCONNECT */
        		ret = odbc_disconnect(env, dbc);
            		if (!SQL_SUCCEEDED(ret)) {
                		return EXIT_FAILURE;
            		}
                	return EXIT_FAILURE;
        	}
        	printf("\n %s %s \n", autor, titulo);
        }
	}
	table_close(tabla);
	/* DISCONNECT */
   	ret = odbc_disconnect(env, dbc);
    if (!SQL_SUCCEEDED(ret)) {
       	return EXIT_FAILURE;
   	}
	
    return EXIT_SUCCESS;
}
	


	
