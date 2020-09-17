#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "functions.h"
#include "tree.h"

int main(int argc, char ** argv){
    Tree *tree=NULL;
    FILE *file=NULL;
    int num;
    
    if(argc<2){
        fprintf(stdout,"Not enough parameters: <program> <txt_file>\n");
        return -1;
    }
    file=fopen(argv[1],"r");
    if(!file){
        fprintf(stdout,"Error opening file...\n");
        return -1;
    }
    tree=tree_ini(destroy_intp_function,copy_intp_function,print_intp_function,cmp_intp_function);
    while (!feof(file)) {
        fscanf(file,"%d",&num);
        tree_insert(tree,&num);
    }
    fprintf(stdout,"Numero de nodos: %d\n",tree_numNodes(tree));
    fprintf(stdout,"Profundidad: %d\n",tree_depth(tree));
    fprintf(stdout,"Introduzca un numero:");
    scanf("%d",&num);
    fprintf(stdout,"Numero introducido: %d\n",num);
    if(tree_find(tree,&num)==TRUE){
        fprintf(stdout,"El dato %d se encuentra dentro del arbol\n",num);
    }
    else{
        fprintf(stdout,"El dato %d no se encuentra dentro del arbol\n",num);
    }
    tree_destroy(tree);
    fclose(file);
    return 0;
}