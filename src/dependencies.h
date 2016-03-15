#ifndef DEPENDENCIES_H
#define DEPENDENCIES_H

struct offset_node { /* a tree with all the lags/leads occuring in the model */
    int offset; /* offset (lag or lead) */
    struct offset_node *left;
    struct offset_node *right;
};

typedef struct dep {
    char *name;
    struct offset_node *offset_tree;
    struct dep *next;
} dependencies;

/* function definitions */
void add_dependency(char *name, int lower, int upper);
dependencies *close_dependencies(void);
void print_dependencies(FILE *f, dependencies *deps);

#endif
