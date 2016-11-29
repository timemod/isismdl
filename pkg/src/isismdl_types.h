/*
 * map fortran types to Ctypes
 *
 * Lahey Fortran 95
 *  32 bit stuff
 */

#ifdef X86_64
typedef long long int   FINT    ;        /* 64 bit integer          */
typedef int             FINT4   ;        /* 32 bit integer          */
typedef long long unsigned int FUINT   ; /* 64 bit unsigend integer */
typedef long long unsigned int ADRESS;   /* integer type large enough to hold
                                            a  memory addres */
#define FINT_MAX LLONG_MAX
#define FINT_MIN LLONG_MIN
#else
typedef int          FINT    ;   /* 32 bit integer          */
typedef int          FINT4   ;   /* 32 bit integer          */
typedef unsigned int FUINT   ;   /* 32 bit unsigned integer */
typedef unsigned int ADRESS;  /* integer type large enough to hold
                                 a  memory addres */
#define FINT_MAX INT_MAX
#define FINT_MIN INT_MIN
#endif
typedef int             MCINT   ;        /* 32 bit integer (for model) */

typedef double          FREAL8  ;   /* real*8           */
typedef unsigned char   FUCHAR  ;   /* unsigned char    */

/*
 * Lahey prepends and appends a _ to public symbols
 * C prepends so here we must append an _ explicitly
 *
 */

#define FNAME(x) x ## _


/* Declarations of C functions. Only C functions used by other C 
 * functions have to be declared.
 */

FINT FNAME(vsiofs) (const FINT *VAR, const FINT *base);
FINT FNAME(zfexist)(FUCHAR filename[], FINT* filenamelen);


