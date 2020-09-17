#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sql.h>
#include <sqlext.h>
#include "odbc.h"

int main(int argc, char** argv){
	SQLHENV env;
    SQLHDBC dbc;
    SQLHSTMT stmt;
    SQLRETURN ret; /* ODBC API return status */
    char consulta[300];
	int i;
	int id_oferta;
	

	
	/* CONNECT */
    ret = odbc_connect(&env, &dbc);

    if (!SQL_SUCCEEDED(ret)) {
        return EXIT_FAILURE;
    }

    /* Allocate a statement handle */
    SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
	
	if(argc<5){
		printf("ERROR EN LOS ARGUMENTOS\n");	
		printf("oferta descuento desde hasta isbn isbn .... isbn\n");
		SQLFreeHandle(SQL_HANDLE_STMT, stmt);
		/* DISCONNECT */
		ret = odbc_disconnect(env, dbc);
    	if (!SQL_SUCCEEDED(ret)) {
        	return EXIT_FAILURE;
    	}
		return EXIT_FAILURE;
	}

	sprintf(consulta,"INSERT INTO oferta(descuento,f_ini,f_fin) VALUES ('%s','%s','%s')",argv[1],argv[2],argv[3]);
	ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
    SQLFreeHandle(SQL_HANDLE_STMT, stmt);
	if (!SQL_SUCCEEDED(ret)) {
        	return EXIT_FAILURE;
	}

	SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
	sprintf(consulta,"SELECT max(id_oferta) from oferta");
	ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);

	if (!SQL_SUCCEEDED(ret)) {
		SQLFreeHandle(SQL_HANDLE_STMT, stmt);
		/* DISCONNECT */
		ret = odbc_disconnect(env, dbc);
    		if (!SQL_SUCCEEDED(ret)) {
        		return EXIT_FAILURE;
    		}
        	return EXIT_FAILURE;
	}

	ret = SQLGetData(stmt, 1, SQL_C_SLONG, &id_oferta, sizeof(SQLINTEGER), NULL);
	SQLFreeHandle(SQL_HANDLE_STMT, stmt);

	if (!SQL_SUCCEEDED(ret)) {
		/* DISCONNECT */
		ret = odbc_disconnect(env, dbc);
    		if (!SQL_SUCCEEDED(ret)) {
        		return EXIT_FAILURE;
    		}
        	return EXIT_FAILURE;
	}
	
	for(i=4;i<argc;i++){
		SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
		sprintf(consulta,"UPDATE edicion set edicion.id_oferta='%d' where edicion.id_libro='%s'",id_oferta,argv[i]);
		ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
		SQLFreeHandle(SQL_HANDLE_STMT, stmt);
		if (!SQL_SUCCEEDED(ret)) {
			/* DISCONNECT */
			ret = odbc_disconnect(env, dbc);
    			if (!SQL_SUCCEEDED(ret)) {
        			return EXIT_FAILURE;
    			}
        		return EXIT_FAILURE;
		}
	}

	/* DISCONNECT */
    	ret = odbc_disconnect(env, dbc);
    	if (!SQL_SUCCEEDED(ret)) {
        	return EXIT_FAILURE;
    	}

    	return EXIT_SUCCESS;
}
	


	
