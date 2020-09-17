#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "functions.h"
#include "tree.h"

int main(int argc,char **argv){
    FILE *pf=NULL;
    Tree *tree=NULL;
    char aux[50];
    
    if(argc<2){
        fprintf(stdout,"Not enough parameters...");
        return -1;
    }
    pf=fopen(argv[1],"r");
    if(!pf){
        fprintf(stdout,"Error oading file...");
    }
    tree=tree_ini(destroy_char_function,copy_char_function,print_char_function,cmp_char_function);
    if(!tree){
        fprintf(stdout,"Error creating tree...");
        fclose(pf);
        return -1;
    }
    while(!feof(pf)){
        fscanf(pf,"%s\n",aux);
        tree_insert(tree,aux);
    }
    fprintf(stdout,"Numero de nodos: %d\n",tree_numNodes(tree));
    fprintf(stdout,"Profundidad: %d\n",tree_depth(tree));
    tree_inOrder(stdout,tree);
    fprintf(stdout,"\nIntroduce una cadena para buscar en el árbol (siguiendo el mismo formato que en el fichero de entrada):");
    scanf("%s",aux);
    fprintf(stdout,"Cadena introducida: %s\n",aux);
    if(tree_find(tree,aux)==TRUE){
        fprintf(stdout,"Elemento encontrado !\n");
    }
    else{
        fprintf(stdout,"Elemento no encontrado !\n");
    }
    tree_destroy(tree);
    fclose(pf);
    return 0;
}