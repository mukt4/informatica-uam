#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "node.h"

#define TAM 100

#define DEBUG 1

struct _Node {
    char name[100];
    Bool discovery;
    int id;
};

Node * node_ini(){
    Node *n;
    n=(Node *)malloc(sizeof(Node));
    if(!n)
        return ERROR;    
        
    n->name[0] = '\0';
    n->discovery = FALSE;
    n->id=0;
    return n;
}

void node_destroy(Node * n){
    if(!n)
        return;
    free(n);
}

int node_getId(const Node * n){
    if(!n || !(n->id))
        return -1;
    return (int)n->id;
}

char * node_getName(const Node * n){
    if(!n || !(n->name))
        return NULL;;
    return (char*) n->name;
}

Node * node_setId(Node * n, const int id){
    if(!n || !id)
        return NULL;
    n->id=id;
    return n;
}

Node * node_setName(Node * n, const char* name){
    if(!n || !name)
        return NULL;
    strcpy(n->name,name);
    return n;
}

Bool node_equals(const Node * n1, const Node * n2){
    if(!n1 || !n2)
        return FALSE;
    if(!strcmp(n1->name,n2->name))
        if(n1->id==n2->id)
            return TRUE;
    return FALSE;
}


Node * node_copy(const Node * src){
    Node *copy;
    if(!src)
        return NULL;
    copy=node_ini();
    node_setName(copy,src->name);
    node_setId(copy,src->id);
    copy->discovery = src->discovery;
    return copy;
}

int node_print(FILE *pf, const Node * n){
    int count = 0;
    if(!n || !pf)
        return -1;
    count+= fprintf(pf, "[%d,%s]", n->id, node_getName(n));

    return count;
}

Bool node_get_discovered(const Node * n){
    if(!n)
        return FALSE;
    return n->discovery;
}

Status node_set_discovered(Node *n){
    if(!n)
        return ERROR;
    n->discovery = TRUE;
    return OK;
}