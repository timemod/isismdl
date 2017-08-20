
/*
 * various output functions
 */

void out_omdl (FILE *fp, int subst);  /* mdl output with
                                       * or without substitution of ufuncs */
void out_tmdl(FILE *fp, FILE *fparam);  /* troll */
void out_vmdl(FILE *fp, char *mdlname); /* eviews model with ufuncs substitued */

/*
 * Functions used for the model compilation in Isis
 */
void out_ipcode(Symbol *sp); /* generate ip-code */
void free_polish(void);  /* free memory used to generate ip-code */
void export_symtab (void); /* export symbol table to arrays in rf7-code */
