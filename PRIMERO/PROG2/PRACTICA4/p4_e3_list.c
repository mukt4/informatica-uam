#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "functions.h"
#include "tree.h"

int main(int argc, char ** argv){
    Tree *tree=NULL;
    FILE *file=NULL;
    List *list1=NULL;
    List *list2=NULL;
    List *list3=NULL;
    int num;
    
    if(argc<2){
        fprintf(stdout,"Not enough parameters: <program> <txt_file>");
        return -1;
    }
    
    file=fopen(argv[1],"r");
    
    if(!file){
        fprintf(stdout,"Error opening file...");
        return -1;
    }
    
    tree=tree_ini(destroy_intp_function,copy_intp_function,print_intp_function,cmp_intp_function);
    list1=list_ini(destroy_intp_function,copy_intp_function,print_intp_function,cmp_intp_function);
    list2=list_ini(destroy_intp_function,copy_intp_function,print_intp_function,cmp_intp_function);
    list3=list_ini(destroy_intp_function,copy_intp_function,print_intp_function,cmp_intp_function);
    
    while (!feof(file)) {
        fscanf(file,"%d",&num);
        tree_insert(tree,&num);
    }
    fprintf(stdout,"Numero de nodos: %d\n",tree_numNodes(tree));
    fprintf(stdout,"Profundidad: %d\n",tree_depth(tree));
    
    fprintf(stdout, "Orden previo: ");
    tree_preOrderToList(list1, tree);
    list_print(stdout,list1);
    fprintf(stdout, "\nOrden medio: ");
    tree_inOrderToList(list2, tree);
    list_print(stdout,list2);
    fprintf(stdout, "\nOrden posterior: ");
    tree_postOrderToList(list3, tree);
    list_print(stdout,list3);
    fprintf(stdout, "\n");
    
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
    list_free(list1);
    list_free(list2);
    list_free(list3);
    fclose(file);
    return 0;
}