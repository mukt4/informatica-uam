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
	FILE* pf;
	int id_usuario, id_venta;
	char p_ini[300],p_fin[300],fecha[300]; 
	

	
	/* CONNECT */
    ret = odbc_connect(&env, &dbc);

    if (!SQL_SUCCEEDED(ret)) {
        return EXIT_FAILURE;
    }

    /* Allocate a statement handle */
    SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
	
	if(argc<3){
		printf("ERROR EN LOS ARGUMENTOS\n");	
		printf("llena_ventas fichero de entrada\n");
		/* DISCONNECT */
    	ret = odbc_disconnect(env, dbc);
    	if (!SQL_SUCCEEDED(ret)) {
        	return EXIT_FAILURE;
    	}
		return EXIT_FAILURE;
	}
	pf=fopen(argv[2],"r");
	if(!pf){
		printf("ERROR EN LA LECTURA DEL FICHERO\n");
		/* DISCONNECT */
		ret = odbc_disconnect(env, dbc);
    	if (!SQL_SUCCEEDED(ret)) {
        	return EXIT_FAILURE;
    	}
		return EXIT_FAILURE;
	}

	while(!feof(pf)){
        SQLAllocHandle(SQL_HANDLE_STMT, dbc, &stmt);
		fscanf(pf,"%d %s %s %d %s",&id_usuario,p_ini,p_fin,&id_venta,fecha);
		sprintf(consulta,"INSERT INTO venta VALUES ('%d','%s','%s','%d','%s')",id_usuario,p_ini,p_fin,id_venta,fecha);
		ret = SQLExecDirect(stmt, (SQLCHAR*) consulta, SQL_NTS);
        SQLFreeHandle(SQL_HANDLE_STMT, stmt);
		if (!SQL_SUCCEEDED(ret)) {
			fclose(pf);
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
		fclose(pf);
        	return EXIT_FAILURE;
    	}
	fclose(pf);

    	return EXIT_SUCCESS;
}
	
		

