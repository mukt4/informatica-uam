#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "list.h"

struct _NodeList {
    void* data;
    struct _NodeList *next;
};

struct _List {
    NodeList *node;
    destroy_elementlist_function_type destroy_element_function;
    copy_elementlist_function_type copy_element_function;
    print_elementlist_function_type print_element_function;
    cmp_elementlist_function_type cmp_element_function;
};


List* list_ini(destroy_elementlist_function_type f1, copy_elementlist_function_type f2, print_elementlist_function_type f3, cmp_elementlist_function_type f4){
    List *list=NULL;
    list=(List *)malloc(sizeof(List));
    list->node=(NodeList*)malloc(sizeof(NodeList));
    list->node->data=NULL;
    list->node->next=NULL;
    list->destroy_element_function=f1;
    list->copy_element_function=f2;
    list->print_element_function=f3;
    list->cmp_element_function=f4;
    return list;
}

void list_free(List* list){
    NodeList *aux=NULL;
    if(!list)
        return;
    if(list_isEmpty(list)==TRUE){
        free(list->node);
        free(list);
        return;
    }
    else{
        while(list->node->next!=NULL){
            aux=list->node;
            list->node=list->node->next;
            list->destroy_element_function(aux->data);
            aux->next=NULL;
            free(aux);
        }
        list->destroy_element_function(list->node->data);
        free(list->node);
        free(list);
    }
}

List* list_insertFirst(List* list, const void *elem){
    void *aux=NULL;
    NodeList *first=NULL;
    if(!list)
        return NULL;
    aux=list->copy_element_function(elem);
    if(list_isEmpty(list)==TRUE){
        list->node->data=aux;
    }
    else{
        first=(NodeList*)malloc(sizeof(NodeList));
        first->data=aux;
        first->next=list->node;
        list->node=first;
    }
    return list;
}

List* list_insertLast(List* list, const void *elem){
    void *aux=NULL;
    NodeList *first=NULL;
    NodeList *last=NULL;
    if(!list)  
        return NULL;
   
    aux=list->copy_element_function(elem);
    if(list_isEmpty(list)==TRUE){
        list->node->data=aux;
    }
    else {
        last=(NodeList*)malloc(sizeof(NodeList));
        last->data=aux;
        last->next=NULL;
        first=list->node;
        while(list->node->next!=NULL){
            list->node=list->node->next;
        }
        list->node->next=last;
        list->node=first;
    }
    return list;
}

List* list_insertInOrder(List *list, const void *pElem){
    NodeList *aux=NULL;
    void *elem=NULL;
    if(!list || !pElem)
        return NULL;
    list_insertFirst(list,pElem);
    aux=list->node;
    while(aux->next!=NULL){
        if((list->cmp_element_function(aux->data,aux->next->data))>0){
            elem=aux->data;
            aux->data=aux->next->data;
            aux->next->data=elem;
        }
        else{
            aux=aux->next;
        }
    }
    aux->next=NULL;
    return list;
}

void * list_extractFirst(List* list){
    NodeList *aux=NULL;
    void *elem;
    if(!list)
        return NULL;
    if(list_isEmpty(list)==TRUE)
        return NULL;
    aux=list->node;
    if(aux->next!=NULL){
        list->node=aux->next;
        elem=list->copy_element_function(aux->data);
        list->destroy_element_function(aux->data);
        free(aux);
    }
    else{
        elem=list->copy_element_function(aux->data);
        list->destroy_element_function(aux->data);
        aux->data=NULL;
    }
    return elem;
}

void * list_extractLast(List* list){
    NodeList *aux=NULL;
    NodeList *last=NULL;
    void *elem;
    if(!list)
        return NULL;
    if(list_isEmpty(list)==TRUE)
        return NULL;
    aux=list->node;
    if(aux->next!=NULL){
        while(aux->next!=NULL){
            last=aux;
            aux=aux->next;
        }
        last->next=NULL;
        elem=list->copy_element_function(aux->data);
        list->destroy_element_function(aux->data);
        aux->data=NULL;
        free(aux);
    }
    else{
        elem=list->copy_element_function(aux->data);
        list->destroy_element_function(aux->data);
        aux->next=NULL;
        aux->data=NULL;
    }
    return elem;
}

Bool list_isEmpty(const List* list){
    if(!list){
        return TRUE;
    }
    if(list->node->data==NULL){
        return TRUE;
    }
    
    return FALSE;
}

const void* list_get(const List* list, int i){
    NodeList *aux=NULL;
    int j=0;
    if(!list)
        return NULL;
    aux=list->node;
    for(j=0;j<i && aux!=NULL;j++){
        aux=aux->next;
    }
    return aux->data;
}

int list_size(const List* list){
    NodeList *aux=NULL;
    int count=1;
    if(!list)
        return -1;
    if(list_isEmpty(list)==TRUE){
        return 0;
    }
    else{
        aux=list->node;
        while(aux->next!=NULL){
            aux=aux->next;
            count++;
        }
    }
    return count;
}
int list_print(FILE *fd, const List* list){
    int count=0;
    NodeList *aux=NULL;
    if(!list || !fd){
        return -1;
    }
    count+=fprintf(fd,"\nLista con %d elementos:\n",list_size(list));
    if(list_isEmpty(list)==FALSE){
        aux=list->node;
        count+=list->print_element_function(fd,aux->data);
        count+=fprintf(fd,"\n");
        while(aux->next!=NULL){
            aux=aux->next;
            count+=list->print_element_function(fd,aux->data);
            count+=fprintf(fd,"\n");
        }
    }
    return count;
}

NodeList * nodeList_getNext(NodeList *node){
    if(!node)
        return NULL;
    return node->next;
}

void * nodeList_getData(NodeList *node){
    if(!node)
        return NULL;
    return node->data;
}

NodeList * list_getNode(List *list){
    if(!list)
        return NULL;
    return list->node;
}

NodeList * nodeList_ini(){
    NodeList *node=NULL;
    node=(NodeList *)malloc(sizeof(NodeList));
    node->data=NULL;
    node->next=NULL;
    return node;
}

/*Funcion que libera memoria de un NodeList, solo libera los NodeList vacios ya que deja el data sin liberar al no saber el tipo*/
void nodeList_destroy(NodeList *node){
    if(!node)
        return;
    free(node);
}

/*Funcion que setea el next de un NodeList*/
NodeList * nodeList_set_next(NodeList *node,NodeList *add){
    if(!node || !add)
        return NULL;
    node->next=add;
    return node;
}

/*Funcion que setea el data de un NodeList*/
NodeList* nodeList_set_data(NodeList *node, void *data){
    if(!node || !data)
        return NULL;
    node->data=data;
    return node;
}