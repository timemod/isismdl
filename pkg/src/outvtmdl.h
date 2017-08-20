/*
 * Common variables and functions for output of model
 * for Eviews and Troll (outvmdl.c and outtmdl.c)
 */

static  void out_simplepar(Symbol *sp, int lagtype, int offset);
static  void out_hypot( Enode *ebase, Enode *ep );

static int get_oprec(int opcode) 
{
    /*
     * Returns the precedence of the operator for Eviews and Troll models
     */
    int oprec;

    switch(opcode) {
        case E_ADD : case E_SUB :
            oprec = 4;
            break;
        case E_MUL : case E_DIV :
            oprec = 5;
            break;
        case E_POW :
            oprec = 6;
            break;
        case E_NEG:
            oprec = 7;
            break;
        case E_AND : case E_OR :
            oprec = 1;
            break;
        case E_NOT :
            oprec = 2;
            break;
        default:
            oprec = 3;
            break;
     }

    return oprec;
}

static  void out_par( Symbol *sp, Enode *ep )
{
    int     lagtype;
    int     offset;

    if( ep )
    {
        lagtype = ep->second.offset;
        offset  = ep->third.offset;
    }
    else
        lagtype = 0;

    out_simplepar(sp, lagtype, offset);
}

static void out_arg_substbuiltin( Enode *ebase, Enode *ep)  {

    /*
     * output argument for substitution of built-in functions
     * (hypot and fibur). Only used for model output in Eviews 
     * and Troll
     */ 
    int arg_op, print_par;

    arg_op    = (ebase+ep->second.ep)->operator;
    print_par = (arg_op >= E_ADD && arg_op <= E_NEG) || arg_op == E_CALL
             || arg_op == E_IF;
    if (print_par) oprintf("(");
    out_enode(ebase, ep->second.ep);
    if (print_par) oprintf(")");
}

static  void out_fibur( Enode *ebase, Enode *ep )
{
    /* Substitute built-in function out_fibur.
     * Only used for Troll and Eviews
     */
    size_t arglist = ep->third.ep;

    oprintf("(");
    out_hypot(ebase, ep);

    ep = ebase + arglist;

    oprintf(" - ");
    out_arg_substbuiltin(ebase, ep);
    oprintf(" - ");
    ep = ebase + (arglist = ep->first.ep);
    out_arg_substbuiltin(ebase, ep);

    oprintf(")");
}

