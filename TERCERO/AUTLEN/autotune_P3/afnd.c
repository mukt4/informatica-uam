#include "afnd.h"
#include "estado.h"

/*COSAS QUE FALTAN POR TERMINAR:
    - PROCESA ENTRADA
*/

/*FUNCION PRIVADA QUE DEVUELVE UN ARRAY DE ESTADOS ACTUALES A 0*/
BOOL* inicializaActual(int maxEstados);

BOOL* inicializaActual(int maxEstados){
    BOOL* vector;
    int i;

    if(maxEstados <= 0)
        return NULL;

    vector = (BOOL*)malloc(sizeof(BOOL) * maxEstados);
    for(i = 0; i < maxEstados; i++)
        vector[i] = FALSE;

    return vector;
}

struct _AFND{
    char* nombre;
    int maxEstados;
    int maxSimbolos;
    int countEstadosActuales;
    Alfabeto* simbolos;
    Estado** estados;
    Palabra* palabra;
    BOOL*** transiciones;
    BOOL* actuales;
};

AFND* AFNDNuevo(char* nombre, int numEstados, int numSimbolos){
    AFND* newAFND = NULL;
    int i, j, k;

    if (nombre == NULL || numEstados <= 0 || numSimbolos < 0)
        return NULL;

    newAFND = (AFND*) malloc(sizeof(AFND));

    newAFND->nombre = (char*) malloc(strlen(nombre) * sizeof(char) + 1);
    strcpy(newAFND->nombre, nombre);
    newAFND->maxEstados = numEstados;
    newAFND->maxSimbolos = numSimbolos;
    newAFND->countEstadosActuales = 0;
    newAFND->simbolos = crearAlfabeto(newAFND->maxSimbolos);

    newAFND->estados = (Estado**) malloc(sizeof(Estado*) * numEstados);
    for (i = 0; i < numEstados; i++){
        newAFND->estados[i] = NULL;
    }

    newAFND->palabra = crearPalabra();

    newAFND->transiciones = (BOOL***) malloc(sizeof(BOOL**) * newAFND->maxEstados);
    for (i = 0; i < newAFND->maxEstados; i++){
        /* TODOS LOS SIMBOLOS + 1 PARA PODER CONTEMPLAR LAMBDA */
        newAFND->transiciones[i] = (BOOL**) malloc(sizeof(BOOL*) * ((newAFND->maxSimbolos) + 1));
        for (j = 0; j <= newAFND->maxSimbolos; j++){
            newAFND->transiciones[i][j] = (BOOL*) malloc(sizeof(BOOL) * newAFND->maxEstados);
        }
    }
    for (i = 0; i < newAFND->maxEstados; i++){
         /* INICIALIZAMOS LA MATRIZ DE TRANSICIONES INCLUYENDO LA INICIALIZACION DE LAS TRANSICIONES DE LAMBDA */
        for (j = 0; j <= newAFND->maxSimbolos; j++){
            for (k = 0; k < newAFND->maxEstados; k++)
                newAFND->transiciones[i][j][k] = FALSE;
        }
    }

    newAFND->actuales = (BOOL*) malloc(sizeof(BOOL) * newAFND->maxEstados);
    for (i = 0; i < newAFND->maxEstados; i++)
        newAFND->actuales[i] = FALSE;

    return newAFND;
}

BOOL AFNDInsertaSimbolo(AFND* afnd, char* simbolo){

    char* elementoInsertado = NULL;

    if(afnd == NULL || simbolo == NULL)
        return FALSE;

    elementoInsertado = insertarElemento(afnd->simbolos, simbolo);
    if (elementoInsertado == NULL){
        printf(">> El simbolo (%s) no se puede insertar debido a que superamos el limite o ya esta insertado!\n", simbolo);
        return FALSE;
    }
    return TRUE;
}

BOOL AFNDInsertaEstado(AFND* afnd, char* nombre, ESTADO estado){

    Estado* newEstado = NULL;
    int i;

    if (afnd == NULL || nombre == NULL)
        return FALSE;
    newEstado = crearEstado(nombre, estado);
    for (i = 0; i < afnd->maxEstados; i++) {
        if (afnd->estados[i] == NULL){
            afnd->estados[i] = newEstado;
            return TRUE;
        }
    }

    destruirEstado(newEstado);
    printf(">> El estado (%s) no se puede insertar debido a que superamos el limite!\n", nombre);
    return FALSE;
}

BOOL AFNDInsertaTransicion(AFND* afnd, char* estado1, char* simbolo, char* estado2){

    int indexEstI, indexElemento, indexEstF;

    if(!afnd || !estado1 || !simbolo || !estado2)
        return FALSE;

    indexEstI = get_indiceEstado(afnd, estado1);
    if (indexEstI == -1){
        printf(">> El estado (%s) no está reconocido en nuestro autómata!\n", estado1);
        return FALSE;
    }

    indexElemento = buscarElemento(afnd->simbolos, simbolo);
    if (indexElemento == -1){
        printf(">> El simbolo (%s) no esta reconocido en nuestro alfabeto!\n", simbolo);
        return FALSE;
    }

    indexEstF = get_indiceEstado(afnd, estado2);
    if (indexEstF == -1){
        printf(">> El estado (%s) no está reconocido en nuestro autómata!\n", estado2);
        return FALSE;
    }

    afnd->transiciones[indexEstI][indexElemento][indexEstF] = TRUE;

    return TRUE;
}

BOOL AFNDInsertaLTransicion(AFND* afnd, char* estado1, char* estado2){

    int indexEstI, indexElemento, indexEstF;

    if(!afnd || !estado1 || !estado2)
        return FALSE;

    indexEstI = get_indiceEstado(afnd, estado1);
    if (indexEstI == -1){
        printf(">> El estado (%s) no está reconocido en nuestro autómata!\n", estado1);
        return FALSE;
    }

    indexElemento = afnd->maxSimbolos;
    indexEstF = get_indiceEstado(afnd, estado2);

    if (indexEstF == -1){
        printf(">> El estado (%s) no está reconocido en nuestro autómata!\n", estado2);
        return FALSE;
    }

    afnd->transiciones[indexEstI][indexElemento][indexEstF] = TRUE;

    return TRUE;
}

void AFNDImprime(FILE* pf, AFND* afnd){
    int i, j, k;

    if (pf == NULL || afnd == NULL)
        return;

    fprintf(pf, "%s={\n\tnum_simbolos = %d\n\n\t", afnd->nombre, alfabeto_size(afnd->simbolos));
    imprimeAlfabeto(pf, afnd->simbolos);
    fprintf(pf, "\n\n\tnum_estados = %d", afnd->maxEstados);
    fprintf(pf, "\n\n\tQ = {");
    for(i = 0; i < afnd->maxEstados; i++){
        if(afnd->estados[i] != NULL){
            imprimeEstado(pf, afnd->estados[i]);
            fprintf(pf, " ");
        }
        else
            break;
    }
    fprintf(pf, "}\n\n\t");

    fprintf(pf, "RL++*={\n\t");
    for (i = 0; i < afnd->maxEstados; i++)
        fprintf(pf, "\t[%d]", i);
    fprintf(pf, "\n");

    for (i = 0; i < afnd->maxEstados; i++){
        fprintf(pf, "\t[%d]\t", i);
        for (j = 0; j < afnd->maxEstados; j++){
            fprintf(pf, "%d\t", afnd->transiciones[i][afnd->maxSimbolos][j]);
        }
        fprintf(pf, "\n");
    }
    fprintf(pf, "}\n\n\t");

    fprintf(pf, "Funcion de transicion = {\n\n");
    for(i = 0; i < afnd->maxEstados; i++){
        for(j = 0; j < afnd->maxSimbolos; j++){
            fprintf(pf, "\t\tf(%s, %s) = { ", estado_getNombre(afnd->estados[i]), alfabeto_get_i(afnd->simbolos, j));
            for (k = 0; k < afnd->maxEstados; k++){
                if (afnd->transiciones[i][j][k] == TRUE)
                    fprintf(pf, "%s ", estado_getNombre(afnd->estados[k]));
            }
            fprintf(pf, "}\n");
        }
    }
    fprintf(pf, "\t}\n");
    fprintf(pf, "}\n");
}

AFND* AFNDInsertaLetra(AFND* afnd, char* letra){

    char* ret;

    if (afnd == NULL || letra == NULL)
        return NULL;

    ret = insertarLetraPalabra(afnd->palabra, letra);

    if (ret == NULL){
        return NULL;
    }

    return afnd;
}

AFND* AFNDInicializaEstado(AFND* afnd){

    int i, j;
    ESTADO estado = NORMAL;

    if (afnd == NULL)
        return NULL;

    afnd->countEstadosActuales = 0;
    for (i = 0; i < afnd->maxEstados; i++)
        afnd->actuales[i] = FALSE;

    for (i = 0; i < afnd->maxEstados; i++){
        estado = estado_getTipo(afnd->estados[i]);
        if (estado == INICIAL){
            afnd->actuales[i] = TRUE;
            afnd->countEstadosActuales++;
            /*INICIALIZAMOS LOS ESTADOS ACTUALES EN FUNCION DE LAS TRANSICIONES LAMBDA*/
            for(j = 0; j < afnd->maxEstados; j++){
                if(afnd->transiciones[i][afnd->maxSimbolos][j] == TRUE){
                    afnd->actuales[j] = TRUE;
                    afnd->countEstadosActuales++;
                }
            }
        }
    }

    return afnd;
}

void AFNDImprimeCadenaActual(FILE* pf, AFND* afnd){
    int i, simbolosActuales;
    char** simbolosPalabra = NULL;

    simbolosActuales = getCountSimbolosPalabraAct(afnd->palabra) - getIndiceActual(afnd->palabra);
    fprintf(pf, "[");
    fprintf(pf, "(%d) ",simbolosActuales);
    simbolosPalabra = getSimbolosPalabraAct(afnd->palabra);
    for (i = getIndiceActual(afnd->palabra); i < getCountSimbolosPalabraAct(afnd->palabra); i++)
        fprintf(pf, "%s ", simbolosPalabra[i]);
    fprintf(pf, "]\n");
}

void AFNDProcesaEntrada(FILE* pf, AFND* afnd){

    int i, j, k, a;
    char* simboloActual = NULL;
    int indiceElemento;
    BOOL* nuevosActuales = NULL;
    int flag;

    if (pf == NULL || afnd == NULL || afnd->actuales == NULL)
        return;


    for (i = getIndiceActual(afnd->palabra); i < getCountSimbolosPalabraAct(afnd->palabra); i++){


        fprintf(pf, "ACTUALMENTE EN { ");                    /* Primera linea */

        for (j = 0; j < afnd->maxEstados; j++){        /* Imprimimos los nombres de los estados diferenciando si son FINAL o NO */
            if(afnd->actuales[j] == TRUE){
                imprimeEstado(pf, afnd->estados[j]);
                fprintf(pf, " ");
            }
        }

        fprintf(pf, "}\n");

        AFNDImprimeCadenaActual(pf, afnd);                      /* Imprimimos la cadena */

        simboloActual = getSimboloActual(afnd->palabra);        /* Conseguimos el simbolo siguiente a ser procesar */

        nuevosActuales = (BOOL*) malloc(sizeof(BOOL) * afnd->maxEstados);
        for (k = 0; k < afnd->maxEstados; k++)
            nuevosActuales[k] = FALSE;

        for (j = 0; j < afnd->maxEstados; j++){       /* Vamos a transitar todos los estados actuales */ /* countEstadosActuales es nuevo */
            if(afnd->actuales[j] == TRUE){

                indiceElemento = buscarElemento(afnd->simbolos, simboloActual);
                if (indiceElemento == -1){
                    printf(">> El simbolo (%s) no esta reconocido en nuestro alfabeto y no podemos reconocer la cadena!\n", simboloActual);
                    return;
                }
                flag = 0;

             /* Hacemos una OR logica con lo que tenemos y funcion de transicion*/

                for (k = 0; k < afnd->maxEstados; k++){
                    nuevosActuales[k] = nuevosActuales[k] || afnd->transiciones[j][indiceElemento][k];
                    if(nuevosActuales[k] == TRUE){
                        /*Cambiamos la flag de comprobacion de que se ha producido una transicion*/
                        flag++;
                        /* Realizamos las transiciones con nuestra matriz de transiciones con el simbolo lambda*/
                        for(a = 0; a < afnd->maxEstados; a++){
                            nuevosActuales[a] = nuevosActuales[a] || afnd->transiciones[k][afnd->maxSimbolos][a];
                        }
                    }
                }
            }
        }
        free(afnd->actuales);
        afnd->actuales = nuevosActuales; /* Igualamos el array antiguo al nuevo */

        if(flag != 0){
            procesarSimbolo(afnd->palabra);
        }
        else{
            printf(">> LA CADENA INSERTADA NO SE PUEDE PROCESAR! (NO HAY POSIBLE ESTADO SIGUIENTE)\n");
            setIndiceActual(afnd->palabra, getCountSimbolosPalabraAct(afnd->palabra));
            return;
        }
    }
    flag = 0;
    fprintf(pf, "ACTUALMENTE EN { ");                    /* Primera linea */
    for (j = 0; j < afnd->maxEstados; j++){        /* Imprimimos los nombres de los estados diferenciando si son FINAL o NO */
        if(afnd->actuales[j] == TRUE){
            imprimeEstado(pf, afnd->estados[j]);
            if (estado_getTipo(afnd->estados[j]) == FINAL)
                flag++;
            fprintf(pf, " ");
        }
    }
    fprintf(pf, "}\n");
    AFNDImprimeCadenaActual(pf, afnd);

    if(flag != 0)
        printf(">> LA CADENA INSERTADA SE HA PROCESADO CORRECTAMENTE!\n");
    else
        printf(">> LA CADENA INSERTADA NO SE PUEDE PROCESAR! (NO HEMOS LLEGADO A UN ESTADO FINAL)\n");

    return;
}

void AFNDElimina(AFND* afnd){
    int i, j;

    if(!afnd)
        return;

    for (i = 0; i < afnd->maxEstados; i++){
        destruirEstado(afnd->estados[i]);
    }
    free(afnd->estados);

    free(afnd->nombre);
    liberarAlfabeto(afnd->simbolos);
    liberarPalabra(afnd->palabra);
    free(afnd->actuales);

    for (i = 0; i < afnd->maxEstados; i++){
        for(j = 0; j <= afnd->maxSimbolos; j++)
            free(afnd->transiciones[i][j]);
        free(afnd->transiciones[i]);
    }
    free(afnd->transiciones);

    free(afnd);
}

int get_indiceEstado(AFND* afnd, char* estado){
    int i;

    if(!estado || !afnd)
        return -1;

    for(i = 0; i < afnd->maxEstados; i++){
        if(strcmp(estado, estado_getNombre(afnd->estados[i])) == 0)
            return i;
    }
    return -1;
}

int getCountSimbolosAFND(AFND* afnd){

    if(!afnd)
        return -1;

    return alfabeto_size(afnd->simbolos);
}

AFND* AFNDInicializaCadenaActual(AFND* afnd){

    if (afnd == NULL)
        return NULL;

    liberarPalabra(afnd->palabra);

    afnd->palabra = crearPalabra();

    if (afnd->palabra == NULL)
        return NULL;

    return afnd;
}

void AFNDCierraLTransicion(AFND* p_afnd_l){

    int k, j, i;
    BOOL** matrizLambda;

    matrizLambda = (BOOL**)malloc(sizeof(BOOL*) * p_afnd_l->maxEstados);

    for (i = 0; i < p_afnd_l->maxEstados; i++){
        matrizLambda[i] = (BOOL*)malloc(sizeof(BOOL) * p_afnd_l->maxEstados);
        for(j = 0; j < p_afnd_l->maxEstados; j++){
            matrizLambda[i][j] = FALSE;
        }
    }


    for (i = 0; i < p_afnd_l->maxEstados; i++){
        for (j = 0; j < p_afnd_l->maxEstados; j++)
            if (i == j)
                matrizLambda[i][j] = TRUE;
            else
                matrizLambda[i][j] = p_afnd_l->transiciones[i][p_afnd_l->maxSimbolos][j];
    }
    
    for (k = 0; k < p_afnd_l->maxEstados; k++){
        for (i = 0; i < p_afnd_l->maxEstados; i++){
            for (j = 0; j < p_afnd_l->maxEstados; j++){
                matrizLambda[i][j] = matrizLambda[i][j] || (matrizLambda[i][k] && matrizLambda[k][j]);
            }
        }
    }

    for (i = 0; i < p_afnd_l->maxEstados; i++){
            for (j = 0; j < p_afnd_l->maxEstados; j++){
                p_afnd_l->transiciones[i][p_afnd_l->maxSimbolos][j] = matrizLambda[i][j];
            }
        }

    for(i = 0; i < p_afnd_l->maxEstados; i++){
        free(matrizLambda[i]);
    }

    free(matrizLambda);
}

/*FUNCIONES AFND10*/
AFND* AFND1ODeSimbolo(char* simbolo)
{
    AFND* newAFND1O = NULL;
    char AFNDName[100] = "AFND1O_";

    strcat(AFNDName, simbolo);

    newAFND1O = AFNDNuevo(AFNDName, 2, 1);

    AFNDInsertaSimbolo(newAFND1O, simbolo);

    AFNDInsertaEstado(newAFND1O, "_S_q0", INICIAL);
    AFNDInsertaEstado(newAFND1O, "_S_q1", FINAL);

    AFNDInsertaTransicion(newAFND1O, "_S_q0", simbolo, "_S_q1");

    return newAFND1O;
}

AFND* AFND1ODeLambda(){

    AFND* afnd = NULL;

    afnd = AFNDNuevo("defaultLambda", 2, 0);

    AFNDInsertaEstado(afnd, "_L_q0", INICIAL);
    AFNDInsertaEstado(afnd, "_L_q1", FINAL);

    AFNDInsertaLTransicion(afnd, "_L_q0", "_L_q1");

    return afnd;
}


AFND* AFND1ODeVacio()
{
    AFND* newAFND1O = NULL;
    char AFNDName[100] = "AFND1O_Vacio";

    newAFND1O = AFNDNuevo(AFNDName, 1, 0);

    AFNDInsertaEstado(newAFND1O, "_V_q0", INICIAL_Y_FINAL);

    return newAFND1O;
}

/*FUNCION ACABADA PERO SE PUEDE HACER MEJOR*/
/*POSIBLES MEJORAS:
    - HACER TODAS LAS MODIFICACIONES DE UNA PASADA
    - NO CREAR ESTADOS INNECESARIOS, SI HAY UN SOLO ESTADO FINAL EN EL PRIMER AUTOMATA NO ES NECESARIO CREAR OTRO EN EL AUTOMATA MODIFICADO
  DUDAS:
    - CREAMOS UNA COPIA DEL AUTOMATA CON LAS MODIFICACIONES NECESARIAS, TENEMOS QUE HACER COPIA O MODIFICAR EL AUTOMATA QUE YA TENEMOS
*/
AFND* AFNDAAFND1O(AFND* p_afnd){
    int contador_inicial = 0;
    int contador_final = 0;
    int flag = 0;
    AFND* afnd_aux;
    int i,j,k;
    char aux[100];
    char inicial_y_final[100];

    if(!p_afnd)
        return NULL;

    for(i = 0; i < p_afnd->maxEstados; i++){
        switch(estado_getTipo(p_afnd->estados[i])){
            case INICIAL:
                contador_inicial++;
                break;
            case FINAL:
                contador_final++;
                break;
            case INICIAL_Y_FINAL:
                flag++;
                break;
            default:
                break;
        }
    }

    /*SI EL AFND CUMPLE LAS CONDICIONES DE 1 ESTADO INICIAL Y OTRO FINAL LO DEVOLVEMOS COMO ESTA*/
    if(contador_inicial == 1 && contador_final == 1 && flag == 0)
        return p_afnd;  /* Para saber si se modifica o no y para la buena liberacion */

    afnd_aux = AFNDNuevo("afnd_10", p_afnd->maxEstados + 2, p_afnd->maxSimbolos);

    AFNDInsertaEstado(afnd_aux, "_i_1O", INICIAL);
    AFNDInsertaEstado(afnd_aux, "_f_1O", FINAL);
    for (i = 0; i < p_afnd->maxEstados; i++){
        /*Inicializamos el nombre del estado que vamos a einsertar*/
        bzero(aux, 100);
        sprintf(aux, "_A1O_q%d", i);
        AFNDInsertaEstado(afnd_aux, aux, NORMAL);
        if(estado_getTipo(p_afnd->estados[i]) == INICIAL){
            bzero(inicial_y_final, 100);
            sprintf(inicial_y_final, "_i_1O");
            AFNDInsertaLTransicion(afnd_aux, inicial_y_final, aux);
        }
        else if(estado_getTipo(p_afnd->estados[i]) == FINAL){
            bzero(inicial_y_final, 100);
            sprintf(inicial_y_final, "_f_1O");
            AFNDInsertaLTransicion(afnd_aux, aux, inicial_y_final);
        }
        else if(estado_getTipo(p_afnd->estados[i]) == INICIAL_Y_FINAL){
            bzero(inicial_y_final, 100);
            sprintf(inicial_y_final, "_i_1O");
            AFNDInsertaLTransicion(afnd_aux, inicial_y_final, aux);
            bzero(inicial_y_final, 100);
            sprintf(inicial_y_final, "_f_1O");
            AFNDInsertaLTransicion(afnd_aux, aux, inicial_y_final);
        }
    }

    /*  Insertamos los simbolos del AFND1 como base del alfabeto del newAFND1O  */
    for (i = 0; i < p_afnd->maxSimbolos; i++)
        AFNDInsertaSimbolo(afnd_aux, alfabeto_get_i(p_afnd->simbolos, i));

    /*UNA VEZ INSERTADAS LAS TRANSICIONES LAMBDA NECESARIAS PASAMOS A COPIAR LA MATRIZ DE TRANSICIONES DE UN AUTOMATA A OTRO*/
    for(i = 0; i < p_afnd->maxEstados; i++){
        for(j = 0; j <= p_afnd->maxSimbolos; j++){
            for(k = 0; k < p_afnd->maxEstados; k++){
                /*INSERTAMOS LAS TRANSICIONES DOS POSICIONES DELANTE YA QUE HEMOS CREADO DOS ESTADOS NUEVOS*/
                afnd_aux->transiciones[i + 2][j][k + 2] = p_afnd->transiciones[i][j][k];
            }
        }
    }

    /*DE MOMENTO ELIMINAMOS TODOS LOS AFNDs EN EL MAIN*/
    AFNDElimina(p_afnd);

    return afnd_aux;
}


AFND* AFND1OUne(AFND* p_afnd1O_1, AFND* p_afnd1O_2)
{
    AFND* newAFND1O = NULL;
    AFND* retAFND1O = NULL;
    char AFNDString[100] = "AFND1O_U_";
    int numEstadosUnion = 0, numSimbolosUnion = 0;
    int i, j, k, x, y, z;

    strcat(AFNDString, p_afnd1O_1->nombre);
    strcat(AFNDString, "_");
    strcat(AFNDString, p_afnd1O_2->nombre);

    /* Get de los estados de ambos AFNDs */
    numEstadosUnion = p_afnd1O_1->maxEstados + p_afnd1O_2->maxEstados;

    /*  Vemos el total de simbolos a crear poniendo como base los del AFND1
        y para los del AFND2 solo ponemos los que no estan en AFND1 */
    numSimbolosUnion = alfabeto_size(p_afnd1O_1->simbolos);
    for (i = 0; i < alfabeto_size(p_afnd1O_2->simbolos); i++){
        if (buscarElemento(p_afnd1O_1->simbolos, alfabeto_get_i(p_afnd1O_2->simbolos, i)) == -1)
            numSimbolosUnion++;
    }

    newAFND1O = AFNDNuevo(AFNDString, numEstadosUnion, numSimbolosUnion);

    /*  Insertamos los estados del 1º en el newAFND1O   */
    for (i = 0; i < p_afnd1O_1->maxEstados; i++){
        bzero(AFNDString, 100);
        sprintf(AFNDString, "_U1_q%d", i);
        AFNDInsertaEstado(newAFND1O, AFNDString, estado_getTipo(p_afnd1O_1->estados[i]));
    }

    /*  Insertamos los estados del 2º en el newAFND1O   */
    for (i = 0; i < p_afnd1O_2->maxEstados; i++){
        bzero(AFNDString, 100);
        sprintf(AFNDString, "_U2_q%d", i);
        AFNDInsertaEstado(newAFND1O, AFNDString, estado_getTipo(p_afnd1O_2->estados[i]));
    }

    /*  Insertamos los simbolos del AFND1 como base del alfabeto del newAFND1O  */
    for (i = 0; i < p_afnd1O_1->maxSimbolos; i++)
        AFNDInsertaSimbolo(newAFND1O, alfabeto_get_i(p_afnd1O_1->simbolos, i));

    /*  Insertamos los simbolos del AFND2 que no esten aun en newAFND1O  */
    for (i = 0; i < p_afnd1O_2->maxSimbolos; i++){
        if (buscarElemento(newAFND1O->simbolos, alfabeto_get_i(p_afnd1O_2->simbolos, i)) == -1)
            AFNDInsertaSimbolo(newAFND1O, alfabeto_get_i(p_afnd1O_2->simbolos, i));
    }

    /*  Insertamos todas las transiciones del AFND1 en newAFND1O    *//* !!!!! */
    for (i = 0, x = 0; i < p_afnd1O_1->maxEstados; i++, x++){
        for (j = 0; j < p_afnd1O_1->maxSimbolos + 1; j++){
            if (j == p_afnd1O_1->maxSimbolos)
                y = newAFND1O->maxSimbolos;
            else
                y = buscarElemento(newAFND1O->simbolos, alfabeto_get_i(p_afnd1O_1->simbolos, j));
            for (k = 0, z = 0; k < p_afnd1O_1->maxEstados; k++, z++){
                newAFND1O->transiciones[x][y][z] = p_afnd1O_1->transiciones[i][j][k];
            }
        }
    }

    /*  Insertamos todas las transiciones del AFND2 en newAFND1O    */
    for (i = 0; i < p_afnd1O_2->maxEstados; i++, x++){
        for (j = 0; j < p_afnd1O_2->maxSimbolos + 1; j++){
            if (j == p_afnd1O_2->maxSimbolos)
                y = newAFND1O->maxSimbolos;
            else
                y = buscarElemento(newAFND1O->simbolos, alfabeto_get_i(p_afnd1O_2->simbolos, j));
            for (k = 0, z = p_afnd1O_1->maxEstados; k < p_afnd1O_2->maxEstados; k++, z++)
                newAFND1O->transiciones[x][y][z] = p_afnd1O_2->transiciones[i][j][k];
        }
    }

    retAFND1O = AFNDAAFND1O(newAFND1O);

    return retAFND1O;
}


AFND* AFND1OConcatena(AFND* p_afnd_origen1, AFND* p_afnd_origen2){
    AFND* concatena;
    char AFNDString[100] = "AFND10_C_";
    int i, j, k, numSimbolos = p_afnd_origen1->maxSimbolos;
    int indiceAux;
    
    strcat(AFNDString, p_afnd_origen1->nombre);
    strcat(AFNDString, "_");
    strcat(AFNDString, p_afnd_origen2->nombre);
    
    /*Comprobamos los simbolos que no se repiten*/
    for(i = 0; i < p_afnd_origen2->maxSimbolos; i++){
        if(buscarElemento(p_afnd_origen1->simbolos, alfabeto_get_i(p_afnd_origen2->simbolos, i)) == -1)
            numSimbolos++;
    }
    
    concatena = AFNDNuevo(AFNDString, p_afnd_origen1->maxEstados + p_afnd_origen2->maxEstados, numSimbolos);
    
    /*Insertamos los elemntos de nuestro alfabeto*/
    for(i = 0; i < p_afnd_origen1->maxSimbolos; i++){
        insertarElemento(concatena->simbolos,alfabeto_get_i(p_afnd_origen1->simbolos, i));
    }
    
    /*Insertamos los del segundo automata*/
    for(i = 0; i < p_afnd_origen2->maxSimbolos; i++){
        if(buscarElemento(concatena->simbolos, alfabeto_get_i(p_afnd_origen2->simbolos, i)) == -1){
            insertarElemento(concatena->simbolos, alfabeto_get_i(p_afnd_origen2->simbolos, i));
        }
    }
    
    /*Una vez inicialiazdo el automata pasamos a gestionar los estados que insertaremos*/
    for(i = 0; i < p_afnd_origen1->maxEstados; i++){
        bzero(AFNDString, 100);
        sprintf(AFNDString, "_C1_q%d", i);
        if(estado_getTipo(p_afnd_origen1->estados[i]) == FINAL){
            AFNDInsertaEstado(concatena, AFNDString, NORMAL);   
        }
        else if(estado_getTipo(p_afnd_origen1->estados[i]) == INICIAL_Y_FINAL){
            AFNDInsertaEstado(concatena, AFNDString, INICIAL);
        }
        else{
            AFNDInsertaEstado(concatena, AFNDString, estado_getTipo(p_afnd_origen1->estados[i]));
        }
    }
    
    for(i = 0; i < p_afnd_origen2->maxEstados; i++){
        bzero(AFNDString, 100);
        sprintf(AFNDString, "_C2_q%d", i);
        if(estado_getTipo(p_afnd_origen2->estados[i]) == INICIAL){
            AFNDInsertaEstado(concatena, AFNDString, NORMAL);
            /*Como es estado incial tenemos que concatenar nuestros estados finales de p_afnd_origen1 a los iniciales*/
            for(j = 0; j < p_afnd_origen1->maxEstados; j++){
                if((estado_getTipo(p_afnd_origen1->estados[j]) == FINAL) || estado_getTipo(p_afnd_origen1->estados[j]) == INICIAL_Y_FINAL){
                    AFNDInsertaLTransicion(concatena, estado_getNombre(concatena->estados[j]), AFNDString);
                }
            }
        }
        else if(estado_getTipo(p_afnd_origen2->estados[i]) == INICIAL_Y_FINAL){
            AFNDInsertaEstado(concatena, AFNDString, FINAL);
            /*Como es estado incial tenemos que concatenar nuestros estados finales de p_afnd_origen1 a los iniciales*/
            for(j = 0; j < p_afnd_origen1->maxEstados; j++){
                if((estado_getTipo(p_afnd_origen1->estados[j]) == FINAL) || estado_getTipo(p_afnd_origen1->estados[j]) == INICIAL_Y_FINAL){
                    AFNDInsertaLTransicion(concatena, estado_getNombre(concatena->estados[j]), AFNDString);
                }
            }
        }
        else{
            AFNDInsertaEstado(concatena, AFNDString, estado_getTipo(p_afnd_origen2->estados[i]));
        }
    }
    
    /*Pasamos a insertar las transiciones de los automotas*/
    /*Primero las del primer automata*/
    for(i = 0; i < p_afnd_origen1->maxEstados; i++){
        for(j = 0; j < p_afnd_origen1->maxSimbolos; j++){
            indiceAux = buscarElemento(concatena->simbolos, alfabeto_get_i(p_afnd_origen1->simbolos, j));
            for(k = 0; k < p_afnd_origen1->maxEstados; k++){
                concatena->transiciones[i][indiceAux][k] = p_afnd_origen1->transiciones[i][j][k];
                concatena->transiciones[i][concatena->maxSimbolos][k] = p_afnd_origen1->transiciones[i][p_afnd_origen1->maxSimbolos][k];
            }
        }
    }
    /*Despues de insertar las transiciones normales insertamos las lambda*/
    for(i = 0; i < p_afnd_origen2->maxEstados; i++){
        for(j =0; j < p_afnd_origen2->maxSimbolos; j++){
            indiceAux = buscarElemento(concatena->simbolos, alfabeto_get_i(p_afnd_origen2->simbolos, j));
            for(k = 0; k < p_afnd_origen2->maxEstados; k++){
                concatena->transiciones[i + p_afnd_origen1->maxEstados][indiceAux][k + p_afnd_origen1->maxEstados] = p_afnd_origen2->transiciones[i][j][k];
                concatena->transiciones[i + p_afnd_origen1->maxEstados][concatena->maxSimbolos][k + p_afnd_origen1->maxEstados] = p_afnd_origen2->transiciones[i][p_afnd_origen2->maxSimbolos][k];
            }
        }
    }
    
    /*Una vez creado nuestro nuevo afnd concatena lo transformamos a 10*/
    return AFNDAAFND1O(concatena);
}


AFND* AFND1OEstrella(AFND* p_afnd_origen)
{
    AFND* newAFND1O = NULL;
    AFND* retAFND1O = NULL;
    char AFNDString[100] = "AFND1O_E_";
    int i, j, k, x, y, z, idxEstadoIni = -1, idxEstadoFin = -1;

    strcat(AFNDString, p_afnd_origen->nombre);

    newAFND1O = AFNDNuevo(AFNDString, p_afnd_origen->maxEstados, p_afnd_origen->maxSimbolos);

    /*  Insertamos los estados del 1º en el newAFND1O   */
    for (i = 0; i < p_afnd_origen->maxEstados; i++){
        bzero(AFNDString, 100);
        sprintf(AFNDString, "_E_q%d", i);
        AFNDInsertaEstado(newAFND1O, AFNDString, estado_getTipo(p_afnd_origen->estados[i]));
    }
    /*  Insertamos los simbolos del AFND1 como base del alfabeto del newAFND1O  */
    for (i = 0; i < p_afnd_origen->maxSimbolos; i++)
        AFNDInsertaSimbolo(newAFND1O, alfabeto_get_i(p_afnd_origen->simbolos, i));
    /*  Insertamos todas las transiciones del AFND1 en newAFND1O    */
    for(i = 0, x = 0; i < p_afnd_origen->maxEstados; i++, x++){
        for(j = 0; j < p_afnd_origen->maxSimbolos + 1; j++){
            if (j == p_afnd_origen->maxSimbolos)
                y = newAFND1O->maxSimbolos;
            else{
                y = buscarElemento(newAFND1O->simbolos, alfabeto_get_i(p_afnd_origen->simbolos, j));
            }
            for(k = 0, z = 0; k < p_afnd_origen->maxEstados; k++, z++){
                newAFND1O->transiciones[x][y][z] = p_afnd_origen->transiciones[i][j][k];
            }
        }
    }
    
    retAFND1O = AFNDAAFND1O(newAFND1O);

    /*  DE MOMENTO ELIMINAMOS TODOS LOS AFNDs EN EL MAIN
    AFNDElimina(p_afnd_origen);
    */

    /*  Buscamos los indices del estado inicial y final en el retAFND1O
        El bucle se realizará hasta que se encuentren los indices de inicio y fin
        o hasta que se recorran todos los bucles    *//* EN VERDAD SON LOS INDICES 0 Y MAXESTADOS - 1*/
    for (i = 0; i < retAFND1O->maxEstados && (idxEstadoIni == -1 || idxEstadoFin == -1); i++){
        if (estado_getTipo(retAFND1O->estados[i]) == INICIAL)
            idxEstadoIni = i;
        if (estado_getTipo(retAFND1O->estados[i]) == FINAL)
            idxEstadoFin = i;
    }

    retAFND1O->transiciones[idxEstadoIni][retAFND1O->maxSimbolos][idxEstadoFin] = TRUE;
    retAFND1O->transiciones[idxEstadoFin][retAFND1O->maxSimbolos][idxEstadoIni] = TRUE;

    return retAFND1O;

}

void AFNDADot(AFND* p_afnd){

    FILE* fp = NULL;
    char AFNDFileName[100];
    int i, j, k, idxEstadoIni = -1;

    sprintf(AFNDFileName, "%s.dot", p_afnd->nombre);

    fp = fopen(AFNDFileName, "w");

    /* Inicializamos el grafo dirigido DOT */
    fprintf(fp, "digraph %s { rankdir=LR;\n", p_afnd->nombre);

    /* Inicializamos el nodo invisible */
    fprintf(fp, "\t _invisible [style=\"invis\"];\n");

    /* Inicializamos todos los nodos */
    for (i = 0; i < p_afnd->maxEstados; i++){

        if (idxEstadoIni == -1 && estado_getTipo(p_afnd->estados[i]) == INICIAL)
            idxEstadoIni = i;

        if (estado_getTipo(p_afnd->estados[i]) == FINAL)
            fprintf(fp, "\t %s [penwidth=\"2\"];\n", estado_getNombre(p_afnd->estados[i]));
        else
            fprintf(fp, "\t %s;\n", estado_getNombre(p_afnd->estados[i]));
    }

    fprintf(fp, "\t _invisible -> %s ;\n", estado_getNombre(p_afnd->estados[idxEstadoIni]));

    for(i = 0; i < p_afnd->maxEstados; i++){
        for(j = 0; j < p_afnd->maxSimbolos + 1; j++){
            for(k = 0; k < p_afnd->maxEstados; k++){
                if (p_afnd->transiciones[i][j][k] == TRUE && i != k){
                    if (j != p_afnd->maxSimbolos)
                        fprintf(fp, "\t %s -> %s [label=\"%s\"];\n",    estado_getNombre(p_afnd->estados[i]), \
                                                                        estado_getNombre(p_afnd->estados[k]), \
                                                                        alfabeto_get_i(p_afnd->simbolos, j));
                    else
                        fprintf(fp, "\t %s -> %s [label=\"&lambda;\"];\n",  estado_getNombre(p_afnd->estados[i]), \
                                                                            estado_getNombre(p_afnd->estados[k]));
                }
            }
        }
    }

    fprintf(fp, "}");

    fclose(fp);
}
