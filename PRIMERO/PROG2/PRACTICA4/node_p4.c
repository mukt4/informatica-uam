#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "node.h"

#define TAM 100

#define DEBUG 1

struct _Node {
    char * name;
    int id;
};

Node * node_ini(){
    Node *n;
    n=(Node *)malloc(sizeof(Node));
    if(!n)
        return ERROR;    
    n->name=NULL;
    n->id=0;
    return n;
}

void node_destroy(Node * n){
    if(!n)
        return;
    if(n->name!=NULL)
        free(n->name);
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
    return n->name;
}

Node * node_setId(Node * n, const int id){
    if(!n || !id)
        return NULL;
    n->id=id;
    return n;
}

Node * node_setName(Node * n, const char* name){
    int length=0;
    if(!n || !name)
        return NULL;
    length=strlen(name);
    n->name = (char *) malloc(sizeof(char) * length + 1);
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
    return copy;
}

int node_print(FILE *pf, const Node * n){
    int count = 0;
    if(!n || !pf)
        return -1;
    count+= fprintf(pf, "[%d,%s]", n->id, node_getName(n));

    return count;
}