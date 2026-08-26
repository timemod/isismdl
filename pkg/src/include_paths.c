#include<stdarg.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

#include "include_paths.h"
#include "util.h"

/* futils.h is located in libc */
#include "futils.h"

#define SEARCH_PATHS_INC 5
#ifndef max
	#define max( a, b ) ( ((a) > (b)) ? (a) : (b) )
#endif

/* The pointer of all allocated filenames for include files are stored
 * on a filename_pool. These pointers are save until close_include_paths()
 * is called.
 */

typedef struct FilenameNode {
    char *name;
    struct FilenameNode *next;
} FilenameNode;

static FilenameNode *filename_pool = NULL;

static int include_dir_count = 0;
static char **include_dirs = NULL;
static int include_dirs_size = 0;
static int max_path_length = 0;

/* Stores a copy of string s in the filename pool */
static char *filename_pool_strdup(const char *s) {
    FilenameNode *n = emalloc(sizeof *n);
    n->name = estrdup(s);
    n->next = filename_pool;
    filename_pool = n;
    return n->name; /* safe pointer until close_include_paths() */
}

/* Add a directory path to the search path.
 * dirname is a directory name without final directory seperator */
void add_include_path(const char *dirname) {
    if (include_dirs == NULL) {
        include_dirs = emalloc(SEARCH_PATHS_INC * sizeof(char *));
        include_dirs_size = SEARCH_PATHS_INC;
    } else if (include_dir_count >= include_dirs_size) {
        include_dirs_size += SEARCH_PATHS_INC;
        include_dirs = erealloc(include_dirs,
                                include_dirs_size * sizeof(char *));
    }

    char *path = emalloc(strlen(dirname) + 2);
    strcpy(path, dirname);
    strcat(path, "/");
    include_dirs[include_dir_count++] = path;

    max_path_length = max(strlen(path), max_path_length);
}


/* Returns the path of an include file.
 * Arguments:
 *   includename:      Pointer to string with the name of the include file.
 *   current_dirname:  Pointer to a string with the name of the directory
 *                     of the file in the buffer. The string may be
 *                     am empty string; if not empty it ends with a directory 
 *                     separator.
 */
char *get_includefilename(char *includename, char *current_dirname) {

     /* First check the standard directory */
     char *filename = emalloc(strlen(includename) + strlen(current_dirname) + 1);
     strcpy(filename, current_dirname);
     strcat(filename, includename);
     if (file_exists(filename)) {
        char *retval = filename_pool_strdup(filename);
        free(filename);
        return retval;
     } else {
        free(filename);
     }

    /*
     * The file has not been found in the standard directory.
     * Now search in the specified include directories
     */
    if (include_dir_count > 0) {
        filename = emalloc(strlen(includename) + max_path_length + 1);
        int i;
        for (i = 0; i < include_dir_count; i++) {
            strcpy(filename, include_dirs[i]);
            strcat(filename, includename);
            if (file_exists(filename)) {
                char *retval = filename_pool_strdup(filename);
                free(filename);
                return retval;
            }
        }
        free(filename); /* not found */
    }

    /* include file not found. Check if the file exists in the current directory.
     * includename may also be an absolute path.
     */
    if (file_exists(includename)) {
        return filename_pool_strdup(includename);
    } else {
        return NULL;
    }
}

static void free_filename_pool(void) {
    FilenameNode *n = filename_pool;
    while (n) {
        FilenameNode *next = n->next;
        free(n->name);
        free(n);
        n = next;
    }
    filename_pool = NULL;
}


/* Reset include_paths settings and clean memory */
void reset_include_paths(void) {

    free_filename_pool();

    if (include_dirs != NULL) {
        int i;
        for (i = 0; i < include_dir_count; i++) {
            free(include_dirs[i]);    /* free each individual path string */
            include_dirs[i] = NULL;
        }
        free(include_dirs);           /* then free the array itself */
        include_dirs = NULL;
    }

    include_dir_count = 0;
    include_dirs_size = 0;
    max_path_length = 0;
}
