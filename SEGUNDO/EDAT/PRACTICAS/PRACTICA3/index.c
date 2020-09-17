#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "index.h"


typedef struct {
    int key;
    int nptrs;
    long *pointer;
} irecord;

struct index_ {
    int nkey;
    int type;
    irecord **rec;
};

int search(int key, index_t *index) {
  int i=0;

  for(i=0;i<index->nkey;i++){
  	if(key == index->rec[i]->key){
  		return i;
  	}
  }
  return -1;

}

/* 
   Creates a file for saving an empty index. The index is initialized
   to be of the specific tpe (in the basic version this is always INT)
   and to contain 0 entries.
 */
int index_create(int type, char* path) {
  	FILE *pf = fopen(path, "w");
	int n = 0;
	if(fwrite(&n, sizeof(int), 1, pf) != 2)
		return -1;
	if(fwrite(&type, sizeof(int), 1, pf) != 2)
		return -1;
	fclose(pf);
	return 0;
}

/* 
   Opens a previously created index: reads the contents of the index
   in an index_t structure that it allocates, and returns a pointer to
   it (or NULL if the files doesn't exist or there is an error). 

   NOTE: the index is stored in memory, so you can open and close the
   file in this function. However, when you are asked to save the
   index, you will not be given the path name again, so you must store
   in the structure either the FILE * (and in this case you must keep
   the file open) or the path (and in this case you will open the file
   again).
 */
index_t* index_open(char* path) {
  	FILE *pf = fopen(path, "r+");
	index_t* index;
  	int i =0;
  	index = (index_t*) malloc(sizeof(index_t));

	fread(&(index->nkey), sizeof(int), 1, pf);
	fread(&(index->type), sizeof(int), 1, pf);
	index->rec = (irecord **) malloc(index->nkey*sizeof(irecord *));

	for (i=0; i<index->nkey; i++) {
		index->rec[i] = (irecord *) malloc(sizeof(irecord));
		if(fread(&(index->rec[i]->key), sizeof(int), 1, pf) != 2)
			return NULL;
		if(fread(&(index->rec[i]->nptrs), sizeof(int), 1, pf) != 2)
			return NULL;

		index->rec[i]->pointer = (long *) malloc(index->rec[i]->nptrs * sizeof(long));

		fread(index->rec[i]->pointer, sizeof(long), index->rec[i]->nptrs, pf);
	}

	fclose(pf);
	return index;
}

/* 
   Saves the current state of index in the file it came from. See the
   NOTE to index_open.
*/
int index_save(index_t* index, char* path) {
  	FILE *pf = fopen(path, "w");
    	int i =0;
	fwrite(&(index->nkey), sizeof(int), 1, pf);
	fwrite(&(index->type), sizeof(int), 1, pf);
	for (i=0; i<index->nkey; i++) {
		if(fwrite(&(index->rec[i]->key), sizeof(int), 1, pf) != 2)
			return -1;
		if(fwrite(&(index->rec[i]->nptrs), sizeof(int), 1, pf) != 2)
			return -1;
		fwrite(index->rec[i]->pointer, sizeof(long), index->rec[i]->nptrs, pf);
	}
	fclose(pf);
	return 0;
}


/* 
   Puts a pair key-position in the index. Note that the key may be
   present in the index or not... you must manage both situation. Also
   remember that the index must be kept ordered at all times.
*/
int index_put(index_t *index, int key, long pos) {
  	int k;
	k = search(key, index);
	if(k == -1){
		index->nkey += 1;
		index->rec = realloc(index->rec, index->nkey);
		for (k=index->nkey-2; k>=0 && index->rec[k]->key > key; k--)
			index->rec[k+1] = index->rec[k];
		index->rec[k+1] = (irecord *) malloc(sizeof(irecord));
		index->rec[k+1]->key = key;
		index->rec[k+1]->nptrs = 1;
		index->rec[k+1]->pointer = (long *) malloc(sizeof(long));
		index->rec[k+1]->pointer[0] = pos;
		for (k=0; k<index->nkey; k++) {
			printf("%d\n", index->rec[k]->key);
		}

	} else{
		index->rec[k]->nptrs += 1;
		index->rec[k]->pointer = (long*) realloc(index->rec[k]->pointer, index->rec[k]->nptrs);
		index->rec[k]->pointer[index->rec[k]->nptrs-1] = pos;

	}
  return 0;
}

/* 
   Retrieves all the positions associated with the key in the index. 
   
   NOTE: the parameter nposs is not an array of integers: it is
   actually an integer variable that is passed by reference. In it you
   must store the number of elements in the array that you return,
   that is, the number of positions associated to the key. The call
   will be something like this:

   int n
   long **poss = index_get(index, key, &n);

   for (int i=0; i<n; i++) {
       Do something with poss[i]
   }

   ANOTHER NOTE: remember that the search for the key MUST BE DONE
   using binary search.

*/
long *index_get(index_t *index, int key, int* nposs) {
  	int k;
	k = search(key, index);

	if(k == -1){
		return NULL;
	}
	else{
		*nposs = index->rec[k]->nptrs;
		return index->rec[k]->pointer;
	}
}

/* 
   Closes the index by freeing the allocated resources 
*/
void index_close(index_t *index) {
	int i;
  	for (i=index->nkey-1; i>=0; i--) {
      		free(index->rec[i]);
      		printf("%i", i);
  	}
	free(index->rec);
	free(index);
	return;	
}


