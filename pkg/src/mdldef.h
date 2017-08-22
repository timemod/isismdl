/* This code is used by the model conversion utils 
 *
 * variables for storing equation/param symbol table pointers,
 * used for model conversion
 */

extern Symbol  **eqnp;
extern size_t  ecnt;
extern size_t  ecntmax;

extern Symbol  **parp;
extern size_t  pcnt;
extern size_t  pcntmax;

extern size_t  maxpnamelen;
extern size_t vcnt;
extern size_t vcntmax;
extern Symbol **varp;
extern size_t endocnt;
extern size_t exocnt;
extern size_t maxvnamelen;

void new_eqn( Symbol *sp );
void new_par( Symbol *sp );

void mkvarlist();

void mdldef_init(void);
