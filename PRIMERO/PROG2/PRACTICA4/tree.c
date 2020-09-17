#include <stdio.h>
#include <stdlib.h>

#include "tree.h"
#include "node.h"
#include "functions.h"

struct _NodeBT {
    void* info;
    struct _NodeBT* left;
    struct _NodeBT* right;
};

struct _Tree {
    
    NodeBT *root;
    destroy_elementtree_function_type destroy_element_function;
    copy_elementtree_function_type copy_element_function;
    print_elementtree_function_type print_element_function;
    cmp_elementtree_function_type cmp_element_function;
    
};
/*Funciones privadas para recorrer de forma recursiva*/

void    tree_destroy_rec (Tree*pa , NodeBT *node);

Status  tree_insert_rec(Tree*pa, NodeBT **ppn,const void*po);

Bool    tree_find_rec(Tree* pa, NodeBT *node,const void* po);

int tree_depth_rec(const NodeBT *node);

int tree_numNodes_rec(const NodeBT *node);

NodeBT * nodeBT_ini();

void    nodeBT_destroy(Tree *pa,NodeBT * node);

Status tree_preOrder_rec(FILE * f, NodeBT *pn, print_elementtree_function_type print_element_function);

Status tree_inOrder_rec(FILE * f, NodeBT *pn, print_elementtree_function_type print_element_function);

Status tree_postOrder_rec(FILE * f, NodeBT *pn, print_elementtree_function_type print_element_function);

Status tree_preOrderToList_rec(List *l, NodeBT *pn);

Status tree_inOrderToList_rec(List *l, NodeBT *pn);

Status tree_postOrderToList_rec(List *l, NodeBT *pn);

/*************************************************************/

Tree*   tree_ini(destroy_elementtree_function_type f1, copy_elementtree_function_type f2, print_elementtree_function_type f3, cmp_elementtree_function_type f4){
    Tree *tree=NULL;
    tree=(Tree*)malloc(sizeof(Tree));
    tree->destroy_element_function=f1;
    tree->copy_element_function=f2;
    tree->print_element_function=f3;
    tree->cmp_element_function=f4;
    tree->root=NULL;
    return tree;
}

void    tree_destroy_rec (Tree*pa , NodeBT *node){
    if(!pa || !node)
        return;
    if(node->left!=NULL){
        tree_destroy_rec(pa,node->left);
    }
    if(node->right!=NULL){
        tree_destroy_rec(pa,node->right);
    }
    nodeBT_destroy(pa,node);
}

void    tree_destroy(Tree*pa){
    if(!pa)
        return;
    if(tree_isEmpty(pa)==FALSE){
        tree_destroy_rec(pa,pa->root);
    }
    free(pa);
}

Status  tree_insert_rec(Tree*pa, NodeBT **ppn,const void*po){
    int cmp;
    if (!*ppn) {
        *ppn = nodeBT_ini();
        (*ppn)->info = pa->copy_element_function(po);
        if (!*ppn) {
            return ERROR;
        }
    }
    cmp =  pa->cmp_element_function(po, (*ppn)->info);
    if (cmp < 0) {
        return tree_insert_rec(pa,&(*ppn)->left, po);
    }
    if (cmp > 0) {
        return tree_insert_rec(pa,&(*ppn)->right, po);
    }
    if (cmp == 0) {
        return OK; 
    }
    return ERROR;
}

Status  tree_insert(Tree*pa, const void*po){
     if (!pa || !po) {
        return ERROR;
    }
    return tree_insert_rec(pa,&(pa->root), po);
}

Bool    tree_isEmpty( const Tree*pa){
    if(!pa)
        return TRUE;
    if(pa->root==NULL)
        return TRUE;
    return FALSE;
}

Bool    tree_find_rec(Tree* pa, NodeBT *node,const void* po){
    int cmp;
    if (node == NULL || po == NULL)
        return FALSE;
    cmp = pa->cmp_element_function(node->info,po);
    if (cmp == 0)
        return TRUE;
    else if (cmp > 0)
        return tree_find_rec(pa,node->left,po);
    else
        return tree_find_rec(pa,node->right,po);
    return FALSE;
}

Bool    tree_find(Tree* pa, const void* po){
    if (!pa || !po) {
        return FALSE;
    }
    if(tree_isEmpty(pa) == TRUE){
        return FALSE;
    }
    return tree_find_rec(pa,pa->root, po);
}

int tree_depth_rec(const NodeBT * ppn){
    
    int aux1, aux2;
    
    if(!ppn)
        return -1;
    
    aux1 = tree_depth_rec(ppn->left) + 1;
    aux2 = tree_depth_rec(ppn->right) + 1;
    return ( aux1 > aux2) ? aux1 : aux2;
}

int tree_depth(const Tree*pa){
    if (!pa || tree_isEmpty(pa) == TRUE) {
        return -1;
    }
    return tree_depth_rec(pa->root);
}

int tree_numNodes_rec(const NodeBT *node){
    if(!node){
        return 0;
    }
    return 1 + tree_numNodes_rec(node->left)+tree_numNodes_rec(node->right);
}

int tree_numNodes(const Tree*pa){
     if (!pa || tree_isEmpty(pa) == TRUE) {
        return -1;
    }
    return tree_numNodes_rec(pa->root);
}

/*******Funciones de recorrido del árbol(P4_E3)********/

Status  tree_preOrder(FILE * f, const Tree * pa){
    
    if(!f || !pa)
        return ERROR;
        
    pa->print_element_function(f, pa->root->info);
    
    return tree_preOrder_rec(f, pa->root, pa->print_element_function);
}

Status tree_preOrder_rec(FILE *f, NodeBT *pn, print_elementtree_function_type print_element_function){
    
    if(pn->left != NULL){
        print_element_function(f, pn->left->info); 
        tree_preOrder_rec(f, pn->left, print_element_function);
    }
    
    if(pn->right != NULL){
        print_element_function(f, pn->right->info);
        tree_preOrder_rec(f, pn->right, print_element_function);
    }
    
    return OK;
}

Status tree_inOrder(FILE * f, const Tree * pa){

    if(!f || !pa)
        return ERROR;
        

    return tree_inOrder_rec(f, pa->root, pa->print_element_function);
}

Status tree_inOrder_rec(FILE *f, NodeBT *pn, print_elementtree_function_type print_element_function){
    
    if(pn->left != NULL){
    tree_inOrder_rec(f, pn->left, print_element_function);
    }
    
    if(pn != NULL)
        print_element_function(f, pn->info);
    
    if(pn->right != NULL){
        tree_inOrder_rec(f, pn->right, print_element_function);
    }
    
    return OK;
}


Status  tree_postOrder(FILE*f, const Tree*pa){
    
    if(!f || !pa)
        return ERROR;
        
    tree_postOrder_rec(f, pa->root, pa->print_element_function);
    pa->print_element_function(f, pa->root->info);
    
    return OK;
}

Status tree_postOrder_rec(FILE * f, NodeBT * pn, print_elementtree_function_type print_element_function){
    
    if(pn->left != NULL){
        tree_postOrder_rec(f, pn->left, print_element_function);
        print_element_function(f, pn->left->info);
    }
    
    if(pn->right != NULL){
        tree_postOrder_rec(f, pn->right, print_element_function);
        print_element_function(f, pn->right->info);
    }
    
    return OK;
}

/******Para el ejercicio opcional de P4_E3 *******/

Status tree_preOrderToList_rec(List *l, NodeBT *pn){
    
    if(pn->left != NULL){
        list_insertLast(l, pn->left->info); 
        tree_preOrderToList_rec(l, pn->left);
    }
    
    if(pn->right != NULL){
        list_insertLast(l, pn->right->info);
        tree_preOrderToList_rec(l, pn->right);
    }
    
    return OK;
}

Status tree_preOrderToList(List*l, const Tree*pa){
    if(!l || !pa)
        return ERROR;
        
    list_insertLast(l, pa->root->info);
    
    return tree_preOrderToList_rec(l, pa->root); 
}

Status tree_inOrderToList_rec(List *l, NodeBT *pn){
    if(pn->left != NULL){
        tree_inOrderToList_rec(l, pn->left);
    }
    
    if(pn != NULL)
        list_insertLast(l, pn->info);
    
    if(pn->right != NULL){
        tree_inOrderToList_rec(l, pn->right);
    }
    
    return OK;
}

Status  tree_inOrderToList(List*l, const Tree*pa){
     if(!l || !pa)
        return ERROR;
        

    return tree_inOrderToList_rec(l, pa->root);
}

Status tree_postOrderToList_rec(List *l, NodeBT *pn){
    if(pn->left != NULL){
        tree_postOrderToList_rec(l, pn->left);
        list_insertLast(l, pn->left->info);
    }
    
    if(pn->right != NULL){
        tree_postOrderToList_rec(l, pn->right);
        list_insertLast(l, pn->right->info);
    }
    
    return OK;
}

Status  tree_postOrderToList(List*l, const Tree*pa){
    if(!l || !pa)
        return ERROR;
        
    tree_postOrderToList_rec(l, pa->root);
    list_insertLast(l, pa->root->info);
    
    return OK;
}

/******Funciones primitivas de nodeBT *********/

NodeBT * nodeBT_ini(){
    NodeBT *node=NULL;
    node=(NodeBT*)malloc(sizeof(NodeBT));
    node->info=NULL;
    node->left=NULL;
    node->right=NULL;
    return node;
}

void    nodeBT_destroy(Tree *pa,NodeBT * node){
    if(!node || !pa)
        return;
    pa->destroy_element_function(node->info);
    free(node);
}