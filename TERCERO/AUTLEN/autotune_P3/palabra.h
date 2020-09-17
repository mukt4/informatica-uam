#ifndef PALABRA_H
#define PALABRA_H

typedef struct _Palabra Palabra;

Palabra* crearPalabra();
void liberarPalabra(Palabra* palabra);
char* insertarLetraPalabra(Palabra* palabra, char* simbolo);
int procesarSimbolo(Palabra* palabra);
char** getSimbolosPalabraAct(Palabra* palabra);
int getCountSimbolosPalabraAct(Palabra* palabra);
char* getSimboloActual(Palabra* palabra);
int getIndiceActual(Palabra* palabra);
int setIndiceActual(Palabra* palabra, int newIndice);

#endif