/*
 * definitions for name handling
 */

typedef struct name_
{
        char    *namemem;       /* string memory        */
        size_t  ncnt;           /* # of names           */
        size_t  nlen;           /* total length of
                                 * namemen
                                 */

        size_t  *nstart;        /* index in namemen of
                                 * start of name
                                 */

        size_t  *nsort;         /* sort index           */
}
Names;


