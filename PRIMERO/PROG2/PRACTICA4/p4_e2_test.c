#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "functions.h"
#include "node.h"
#include "tree.h"

int main(int argc,char**argv){
    FILE *pf=NULL;
    Tree *tree=NULL;
    Node *aux=NULL;
    char name[300];
    int num;
    
    if(argc<2){
        fprintf(stdout,"Not enough parameters...<program> <txt_file>\n");
        return -1;
    }
    pf=fopen(argv[1],"r");
    if(!pf){
        fprintf(stdout,"Error opening file...");
        return -1;
    }
    tree=tree_ini(destroy_node_function,copy_node_function,print_node_function,cmp_node_function);
    if(!tree){
        fprintf(stdout,"Error creating tree...");
        fclose(pf);
        return -1;
    }
    while (!feof(pf)) {
        fscanf(pf,"%d %s",&num,name);
        aux=node_ini();
        node_setId(aux,num);
        node_setName(aux,name);
        tree_insert(tree,aux);
        node_destroy(aux);
    }
    fprintf(stdout,"Numero de nodos: %d\n",tree_numNodes(tree));
    fprintf(stdout,"Profundidad: %d\n",tree_depth(tree));
    fprintf(stdout,"Introduzca un numero:");
    scanf("%d",&num);
    fprintf(stdout,"Numero introducido: %d\n",num);
    aux=node_ini();
    node_setId(aux,num);
    if(tree_find(tree,aux)==TRUE){
        fprintf(stdout,"El dato %d se encuentra dentro del arbol\n",num);
    }
    else{
        fprintf(stdout,"El dato %d no se encuentra dentro del arbol\n",num);
    }
    tree_destroy(tree);
    node_destroy(aux);
    fclose(pf);
    return 0;
}