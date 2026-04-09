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

static int include_dir_count = 0;
static char **include_dirs;
static int include_dirs_size;
static int max_path_length = 0;

void init_include_dirs(void) {
  int i;
  if (include_dirs != NULL) {
    for (i = 0; i < include_dir_count; i++) {
      free(include_dirs[i]);    /* free each individual path string */
    }
    free(include_dirs);           /* then free the array itself */
    include_dirs = NULL;
  }
  include_dir_count = 0;
  include_dirs_size = 0;
  max_path_length = 0;
}

void add_include_path(const char *dirname) {
    /*
     * Add a path to the search path
     */

    if (include_dirs == NULL) {
        include_dirs = malloc(SEARCH_PATHS_INC * sizeof(char *));
        include_dirs_size = SEARCH_PATHS_INC;
    } else if (include_dir_count >= include_dirs_size) {
        include_dirs_size = include_dir_count + SEARCH_PATHS_INC - 1;
        include_dirs = realloc(include_dirs,
                               include_dirs_size * sizeof(char *));
    }

    char * path = malloc(strlen(dirname) + 2);
    strcpy(path, dirname);
    path = strcat(path, "/");
    include_dirs[include_dir_count++] = path;

    max_path_length = max(strlen(path), max_path_length);
}


/* Returns the path of an include file.
 * Arguments:
 *   includename:      Pointer to string with the name of the include file.
 *   current_dirname:  Pointer to a string with the name of the directory of
 *                     file for the current buffer.
*/
char * get_includefilename(char *includename, char *current_dirname)
{

    /* First check the standard directory
     */
     char *filename = malloc(strlen(includename) + strlen(current_dirname) + 1);
     strcpy(filename, current_dirname);
     filename = strcat(filename, includename);
     if (file_exists(filename)) {
        return filename;
     } else {
        free(filename);
     }

    /*
     * The file has not been found in the standard directory.
     * Now search in the specified include directories
     */
    if (include_dir_count > 0) {
        filename = malloc(strlen(includename) + max_path_length + 1);
        int i;
        for (i = 0; i < include_dir_count; i++) {
            strcpy(filename, include_dirs[i]);
            filename = strcat(filename, includename);
            if (file_exists(filename)) {
                return filename;
            }
        }
        free(filename); /* not found */
    }

    /* include file not found. Check if the file exists in the current directory.
     * includename may also be an absolute path.
     */
    if (file_exists(includename)) {
        char *filename = malloc(strlen(includename) + 1);
        strcpy(filename, includename);
        return filename;
    } else {
        return NULL;
    }
}
