#include <stdio.h>
#include <string.h>
#include "dependencies.h"
#include "util.h"

/* This file contains several functions that are used to generate
 * a file with full dependency information. This file is created
 * if the option gen_dep has been specified.
 * The dependency information file gives for each equation a list of
 * right hand side variables with a list of specified lags and leads.
 * The current implementation only works for models without user functions. */

/* local variables */
static dependencies *root = NULL;
static int first_lag;

/* local function defintions */
static dependencies *dep_alloc(char *name);
static struct offset_node *offset_alloc(void);
static struct offset_node *add_offset(struct offset_node *p, int offset);
static void offset_tree_print(FILE *f, struct offset_node *p);

/* add_dependency: add a new dependency to the dependency list */
void add_dependency(char *name, int lower, int upper) {
    /* Input: 
     *  *name  pointer to the name of a variable that occurs in the 
     *         right hand side of an equation
     *  lower  lower index of the offset range (lags/leads)
     *  upper  upper index of the offset range (lags/leads)
     */
    dependencies *deps = root;
    if (root == NULL) {
        deps = (root = dep_alloc(name));
    } else {
        dependencies *prev;
        while (deps != NULL) {
            if (strcmp(deps->name, name) == 0) {
                break;
            }
            prev = deps;
            deps = deps->next;
        }
        if (deps == NULL) {
            deps = dep_alloc(name);
            prev->next = deps;
        }
    }

    int offset;
    for (offset = lower; offset <= upper; offset++) {
        deps->offset_tree = add_offset(deps->offset_tree, offset);
    }
}

/* close_dependencies: returns a pointer to the dependency list,
 * are resets the root pointer */
dependencies *close_dependencies(void) {

    dependencies *res;
    res = root;
    root = NULL;
    return res;
}

/* add_offset: add an offset to the offset tree */
struct offset_node *add_offset(struct offset_node *p, int offset) {
    /* put the offset in a tree. Ordered in such a way that offset 0 comes
     * first. */
    if (p == NULL) {
        p = offset_alloc();
        p->offset = offset;
    } else if (offset < p->offset) {
        p->left = add_offset(p->left, offset);
    } else if (offset > p->offset) {
        p->right = add_offset(p->right, offset);
    }
    return p;
}

/* offset_tree_print: print the tree of offsets (lags/ leads) */
void offset_tree_print(FILE *f, struct offset_node *p) {
    if (p != NULL) {
        offset_tree_print(f, p->left);
	if (!first_lag) fprintf(f, " ");
        fprintf(f, "%d", p->offset);
	if (first_lag) first_lag = 0;
        offset_tree_print(f, p->right);
    }
}

/* dep_alloc: allocate and initialise a new dependency structure */
static dependencies *dep_alloc(char *name) {
    dependencies *deps;
    deps = (dependencies *) emalloc(sizeof(dependencies));
    deps->name = name;
    deps->offset_tree = NULL;
    deps->next = NULL;
    return deps;
}

/* offset_alloc: allocate and initialise a new node for the offset tree */
static struct offset_node *offset_alloc(void) {
    struct offset_node *node;
    node = (struct offset_node *) emalloc(sizeof(struct offset_node));
    node->left = node->right = NULL;
    return node;
}

void print_dependencies(FILE *f, const char *lhs_name, dependencies *deps) {
    while (deps != NULL) {
        fprintf(f, "%s,", lhs_name);
        fprintf(f, "%s,", deps->name);
	first_lag = 1;
        offset_tree_print(f, deps->offset_tree);
        fprintf(f, "\n");
        deps = deps->next;
    }
}

/* TODO: add code to free the allocated memory */
