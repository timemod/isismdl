/*
 * filename handling functions
 *
 * fnsplit splits fname int path, basename and extension components
 *
 * fnjoin  does the reverse
 *
 */

void    fnsplit( char *fname, char *path, char *base, char *ext);
char    *get_path_name(char *fname);
char    *fnjoin ( char *fname, char *path, char *base, char *ext);
