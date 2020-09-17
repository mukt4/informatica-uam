#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "afnd.h"

int main(int argc, char **argv) {
    /* DECLARACIÃ“N DE UN PUNTERO A UN AFND */
    AFND *p_afnd;

    /* INICIALIZACIÃ“N DE UN NUEVO AFND DE NOMBRE af1 Y CON 3 ESTADOS Y 2 SÃMBOLOS EN SU ALFABETO */
    p_afnd = AFNDNuevo("af1", 7, 3);
    
    /* DEFINICIÃ“N DEL ALFABETO DEL AFND */
    AFNDInsertaSimbolo(p_afnd,"0");
    AFNDInsertaSimbolo(p_afnd,"1");
    AFNDInsertaSimbolo(p_afnd,"2");

    /* DEFINICIÃ“N DEL CONJUNTO DE ESTADOS */
    AFNDInsertaEstado(p_afnd,"q0",INICIAL);
    AFNDInsertaEstado(p_afnd,"q1",NORMAL);
    AFNDInsertaEstado(p_afnd,"q2",NORMAL);
    AFNDInsertaEstado(p_afnd,"q3",NORMAL);
    AFNDInsertaEstado(p_afnd,"q4",NORMAL);
    AFNDInsertaEstado(p_afnd,"q5",NORMAL);
    AFNDInsertaEstado(p_afnd,"qf",FINAL);

    /* DEFINICIÃ“N DE LAS TRANSICIONES NO LAMBDA */
    AFNDInsertaTransicion(p_afnd, "q0", "0", "q0");
    AFNDInsertaTransicion(p_afnd, "q0", "2", "q0");
    AFNDInsertaTransicion(p_afnd, "q0", "1", "q1");
    AFNDInsertaTransicion(p_afnd, "q1", "2", "q1");
    AFNDInsertaTransicion(p_afnd, "q1", "0", "q0");
    AFNDInsertaTransicion(p_afnd, "q1", "1", "q2");
    AFNDInsertaTransicion(p_afnd, "q2", "0", "q1");
    AFNDInsertaTransicion(p_afnd, "q2", "1", "q3");
    AFNDInsertaTransicion(p_afnd, "q3", "2", "q1");
    AFNDInsertaTransicion(p_afnd, "q3", "0", "q2");
    AFNDInsertaTransicion(p_afnd, "q3", "1", "q4");
    AFNDInsertaTransicion(p_afnd, "q4", "2", "q4");
    AFNDInsertaTransicion(p_afnd, "q4", "0", "q3");
    AFNDInsertaTransicion(p_afnd, "q4", "1", "q5");
    AFNDInsertaTransicion(p_afnd, "q5", "2", "q0");
    AFNDInsertaTransicion(p_afnd, "q5", "0", "q4");
    AFNDInsertaTransicion(p_afnd, "q5", "1", "qf");

    /* SE MUESTRA EL AFND DEFINIDO */
    fprintf(stdout,"\n****************** AFND *********************\n");
    AFNDImprime(stdout,p_afnd);
    fprintf(stdout,"\n*********************************************\n");

    /* DEFINICIÃ“N DE LA CADENA DE ENTRADA [ 0 1 0 1 1 ] */
    p_afnd= AFNDInsertaLetra(p_afnd,"0");
    p_afnd= AFNDInsertaLetra(p_afnd,"2");
    p_afnd= AFNDInsertaLetra(p_afnd,"0");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");
    p_afnd= AFNDInsertaLetra(p_afnd,"2");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");
    p_afnd= AFNDInsertaLetra(p_afnd,"2");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");
    p_afnd= AFNDInsertaLetra(p_afnd,"0");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");
    p_afnd= AFNDInsertaLetra(p_afnd,"0");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");

    /* SE ESTABLECE COMO ESTADO ACTUAL DEL AUTÃ“MATA EL INICIAL */
    AFNDInicializaEstado (p_afnd);

    /* SE MUESTRA LA CADENA ACTUAL */
    fprintf(stdout,"\n**************** PROCESA CADENA *************\n");
    AFNDImprimeCadenaActual(stdout,p_afnd);
    fprintf(stdout,"*********************************************\n");

    /* SE PROCESA LA CADENA DE ENTRADA ACTUAL MOSTRANDO UNA TRAZA DEL FUNCIONAMIENTO DEL AUTOMATA: EN CADA PASO DE ANÃLISIS SE MUESTRA LA CADENA ACTUAL Y EL CONJUNTO DE ESTADOS EN LOS QUE SE ENCUENTRA EL AUTÃ“MATA */
    AFNDProcesaEntrada(stdout,p_afnd);

    /* DEFINICIÃ“N DE LA CADENA DE ENTRADA [ 0 1 1 0 0 ] */
    p_afnd= AFNDInsertaLetra(p_afnd,"1");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");
    p_afnd= AFNDInsertaLetra(p_afnd,"1");

    /* SE ESTABLECE COMO ESTADO ACTUAL DEL AUTÃ“MATA EL INICIAL */
    AFNDInicializaEstado (p_afnd);

    /* SE MUESTRA LA CADENA ACTUAL */
    fprintf(stdout,"\n**************** PROCESA CADENA *************\n");
    AFNDImprimeCadenaActual(stdout,p_afnd);
    fprintf(stdout,"*********************************************\n");


    /* SE PROCESA LA CADENA DE ENTRADA ACTUAL MOSTRANDO UNA TRAZA DEL FUNCIONAMIENTO DEL AUTOMATA: EN CADA PASO DE ANÃLISIS SE MUESTRA LA CADENA ACTUAL Y EL CONJUNTO DE ESTADOS EN LOS QUE SE ENCUENTRA EL AUTÃ“MATA */
    AFNDProcesaEntrada(stdout,p_afnd);

    /* SE LIBERAN TODOS LOS RECURSOS ASOCIADOS CON EL AFND */
    AFNDElimina(p_afnd);

    return 0;
}