/*
 * Code for managing the model compiler flags for the model preprocessor 
 */

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define HASHSIZE 101

static void tolowercase(char *s);

struct nlist {
    char *flag;
    struct nlist *next;
};

static struct nlist *hashtab[HASHSIZE];

static unsigned char hash(char *s) {

    /* Form hash value for string s */

    unsigned int hashval;
    for (hashval = 0; *s != '\0'; s++) {
        hashval = *s + 31 * hashval;
    }
    return hashval % HASHSIZE;
}

static struct nlist *lookup(char *s)  {

    /* look for s in hashtab. The function returns a pointer to the 
     * element containing string s */
    
    struct nlist *np;
    for (np = hashtab[hash(s)]; np != NULL; np = np->next) {
        if (strcmp(s, np->flag) == 0) {
            return np; /* found */
        }
    }
    return NULL;       /* not found */
}

int add_flag(char *flag) {

    /* adds a flag to the hash table. Returns 0 on succes, and
     * 1 if there is not enough memory to add the flag to the table */

    struct nlist *np;
    unsigned int hashval;

    tolowercase(flag);
    
    if ((np = lookup(flag)) == NULL) { /* flag not yet present */
        np = (struct nlist *) malloc(sizeof(struct nlist));
        if (np == NULL) {
            return 1;  /* not enough memory */
        }
        np->flag = flag;
        hashval = hash(flag);
        np->next = hashtab[hashval];
        hashtab[hashval] = np;
    }

    return 0;
}

int flag_present(char *flag) {

    /* Returns 1 if flag is present in the table, and 0 if not */

    tolowercase(flag);
    return lookup(flag) != NULL;
}

void clear_flags(void) {
     
    /* Remove all flags */

    unsigned int hashval;
    struct nlist *head;
    struct nlist *to_free;

    for (hashval =  0; hashval < HASHSIZE; hashval++) {
       head = hashtab[hashval];
       if (head != NULL) {
           while (head != NULL) {
               to_free = head;
               head = head->next;
               free(to_free->flag);
               free(to_free);
           }
           hashtab[hashval] = NULL;
       }
    }
}

static void tolowercase(char *s) {

    /* convert string s to lowercase characters */
    
    char *p = s;
    while (*p != '\0') {
        *p = toupper(*p);
        p++;
    }
}

