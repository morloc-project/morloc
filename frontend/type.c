#include "type.h"

#define IS_ATOMIC(t) ((t)->cls == FT_ATOMIC)
#define IS_GENERIC(t) ((t)->cls == FT_GENERIC)
#define IS_ARRAY(t) ((t)->cls == FT_ARRAY)
#define IS_STAR(t) (IS_ATOMIC((t)) && strcmp(g_string((t)), __WILD__) == 0)
#define IS_MULTI(t) (strcmp(g_string((t)), __MULTI__) == 0)

#define EQUAL_ATOMICS(a,b)                                \
    (                                                     \
      IS_ATOMIC(a) && IS_ATOMIC(b)                        \
      &&                                                  \
      strcmp(g_string(g_rhs(a)), g_string(g_rhs(a))) == 0 \
    )


// ================================================================== //
//                                                                    //
//                I N I T I A L I Z E    D E F A U L T S              //
//                                                                    //
// ================================================================== //

void _set_default_type(W* w);

void _set_default_types(Ws* ws){
    ws_rcmod(
        ws,
        ws_recurse_most,
        w_is_manifold,
        _set_default_type
    );
}

void _set_default_type(W* w){
    Manifold* m = g_manifold(g_rhs(w));
    // If the manifold is completely untyped
    if(!m->type){
        int ninputs = ws_length(m->inputs);
        int ntypes = ninputs ? ninputs + 1 : 2;
        for(int i = 0; i < ntypes; i++){
            W* star = NULL;
            if(i == 0 && ninputs == 0){
                star = w_new(FT_ATOMIC, __IO__);
            } else {
                star = w_new(FT_ATOMIC, __WILD__);
            }
            m->type = ws_add(m->type, star);
        }
    }
    // TODO: deprecate the MULTI feature
    // If the manifold has the form `? -> a`
    // Then expand `?` to stars according to number of inputs, e.g.
    //   `? -> a` with 2 inputs (of any kind) --> `* -> * -> a`
    // (Currently all other usage of `?` is illegal)
    else if(
       m->type &&
       m->type->head->cls == FT_ATOMIC &&
       strcmp(g_string(m->type->head), __MULTI__) == 0 &&
       ws_length(m->type) == 2
    ){
        if(ws_length(m->inputs) > 1){
            W* output = w_isolate(m->type->last);
            m->type = NULL;
            for(W* i = m->inputs->head; i != NULL; i = i->next){
                m->type = ws_add(m->type, w_new(FT_ATOMIC, __WILD__));
            }
            m->type = ws_add(m->type, output);
        } else {
            // TODO: for legacy reasons, I have to use __WILD__ here, because
            // the grammars downstream are stupid. But this really should be
            // __VOID__
            m->type = ws_add(NULL, w_new(FT_ATOMIC, __WILD__));
        }
    }
}


/* ========================================================================= //
//                                                                           //
//                           I N F E R   T Y P E S                           //
//                                                                           //
// ========================================================================= //

Build a list of lists, where each list is a set of elements of that must have
the same type. These sets include:

1. io groups
   Given the forms (a_i. -> a_i. -> ... -> b_i), where in a_ij, i is the id of
   the terminal type (b_i) and j is the id of the input type.  If b_i is
   generic, b_i and all a_.i will form pairs, [(b_i, a_.i)].  If b_i is of
   unknown type, but not generic, b_i and all a_.i form one set of uniform
   type.

2. star groups
   stars differ from generics in that they are unknown but are the same across
   all instances of the function.

3. entangled generic groups
   For example, in the signature `a -> a -> [a]`, each `a` must have the same
   type in a given function. The type may differ between functions, though.

Once the list of lists is built, all explicitly given types and all type
primitives are added. Any type that is known within a group propagates to all
other elements in its group. If a given manifold is present in two groups, the
groups may be merged.
---------------------------------------------------------------------------- */


void _unify(HomoSet* a, HomoSet* b, ManifoldList* mlist, GenericList* glist){

    // If the a is a star, swap it with b
    if(IS_STAR(a->type)){
       HomoSet* c = a; a = b; b = c;
    }

    if(IS_STAR(b->type)){
        // transfer type from a to b
        b->type->cls = a->type->cls;
        b->type->value = a->type->value;
    }

    if(IS_GENERIC(a->type)){
       HomoSet* c = a; a = b; b = c;
    }

    if(IS_GENERIC(b->type)){
        char b_symbol = g_string(g_lhs(b->type))[0];
        size_t b_gid = get_generic_id_from_uid(b->mid, b_symbol);
        Ws* b_glist = glist->list[b_gid];
        /* both are generic, merge */
        if(IS_GENERIC(a->type)){
            char a_symbol = g_string(g_lhs(a->type))[0];
            size_t a_gid = get_generic_id_from_uid(a->mid, a_symbol);
            Ws* a_glist = glist->list[a_gid];
            // If they are equal, then they have already been merged, don't repeat
            if(a_glist != b_glist){
               b_glist = ws_join(b_glist, a_glist);
               a_glist = b_glist;
            }
        }

        // one is generic, transfer type
        else {
            // transfer types from a to all instances of b
            // FT_GENERIC have the form (generic_label, inferred_type), e.g.
            // (a, Int), so we just have to set the rhs of all b to type(a)
            for(W* g = ws_head(b_glist); g; g = g->next){
                s_rhs(g, a->type);
            }
        }
    }
}

void infer_types(Ws* ws_top, bool verbose){
    // If untyped --> star inputs with appropriate number of arguments
    // This also expands `?` types to many stars
    _set_default_types(ws_top);

    ManifoldList * ml  = create_ManifoldList(ws_top);
    HomoSetList  * hsl = create_HomoSetList(ml);
    GenericList  * gl  = create_GenericList(ml);

    if(hsl){
        while(hsl->prev) { hsl = hsl->prev; }
        HomoSetList* hsl_root = hsl;

        for(; hsl; hsl = hsl->next ){
            // This works for pairs, which is all I currently use
            HomoSet* hs = hsl->set;
            while(hs->prev) { hs = hs->prev; }
            _unify(hs, hs->next, ml, gl);
        }
        hsl = hsl_root;
    }

    if(verbose){
        print_ManifoldList(ml);
        print_HomoSetList(hsl);
        print_GenericList(gl);
    }
}


/* bool _constructor_compatible(W* a, W* b);                                      */
/* bool _types_are_compatible(W* a, W* b);                                        */
/*                                                                                */
/* void _scream_about_compatibility(W* a, W* b){                                  */
/*     char* a_str = type_str(a);                                                 */
/*     char* b_str = type_str(b);                                                 */
/*     fprintf(stderr, "Types '%s' and '%s' are not compatible\n", a_str, b_str); */
/* }                                                                              */
/* void all_io_types_are_compatible(Ws* ws_top){                                  */
/*     Ws* man = get_manifolds(ws_top);                                           */
/*     for(W* w = ws_head(man); w; w = w->next){                                  */
/*         Manifold* m = g_manifold(g_rhs(w));                                    */
/*         W* x = ws_head(m->inputs);                                             */
/*         W* b = ws_head(m->type);                                               */
/*         for(; x && b; x = x->next, b = b->next){                               */
/*             W* a = x;                                                          */
/*             if(x->cls == C_MANIFOLD){                                          */
/*                 a = ws_last(g_manifold(g_rhs(x))->type);                       */
/*             }                                                                  */
/*             if(! _types_are_compatible(a,b) ){                                 */
/*                 _scream_about_compatibility(a,b);                              */
/*             }                                                                  */
/*         }                                                                      */
/*     }                                                                          */
/* }                                                                              */


/* // ========================================================================= //   */
/* //                                                                           //   */
/* //                             T Y P E C H E C K                             //   */
/* //                                                                           //   */
/* // ========================================================================= //   */
/*                                                                                   */
/* // takes in all data                                                              */
/* W* _typecheck_derefs(Ws* ws_top, W* msg);                                         */
/*                                                                                   */
/* W* _type_compatible(W* i, W* t, W* msg);                                          */
/*                                                                                   */
/* #define LOG_ERROR(st, w, s)                                 \                     */
/*     do{                                                     \                     */
/*         W* wstr = w_new(P_STRING, strdup(s));               \                     */
/*         Couplet* cerr = couplet_new(w_clone(w), wstr, '='); \                     */
/*         W* werr = w_new(P_COUPLET, cerr);                   \                     */
/*         if(st){                                             \                     */
/*             s_ws(st, ws_add(g_ws(st), werr));               \                     */
/*         } else {                                            \                     */
/*             Ws* wserr = ws_new(werr);                       \                     */
/*             st = w_new(P_WS, wserr);                        \                     */
/*         }                                                   \                     */
/*     } while(0)                                                                    */
/*                                                                                   */
/* W* _typecheck(W* w, W* msg){                                                      */
/*     Manifold* m = g_manifold(g_rhs(w));                                           */
/*                                                                                   */
/*     if(                                                                           */
/*         ws_length(m->type) == 2 &&                                                */
/*         m->type->head->cls == FT_ATOMIC &&                                        */
/*         strcmp(g_string(m->type->head), __MULTI__) == 0                           */
/*     ){                                                                            */
/*         return msg;                                                               */
/*     }                                                                             */
/*                                                                                   */
/*     if(!m->type){                                                                 */
/*         LOG_ERROR(msg, w, "no declared type");                                    */
/*         return msg;                                                               */
/*     }                                                                             */
/*                                                                                   */
/*                                                                                   */
/*     int n_types = ws_length(m->type) - 1 - type_is_well(m->type);                 */
/*     int n_inputs = ws_length(m->inputs);                                          */
/*                                                                                   */
/*     if(ws_length(m->type) < 2){                                                   */
/*         LOG_ERROR(msg, w, "fewer than 2 terms in type");                          */
/*     }                                                                             */
/*                                                                                   */
/*     if(n_inputs && n_inputs < n_types){                                           */
/*         LOG_ERROR(msg, w, "too few inputs (currying is not supported)");          */
/*     }                                                                             */
/*                                                                                   */
/*     if(n_inputs > n_types){                                                       */
/*         LOG_ERROR(msg, w, "too many inputs");                                     */
/*     }                                                                             */
/*                                                                                   */
/*     Ws* itypes;                                                                   */
/*     if(type_is_well(m->type)){                                                    */
/*         itypes = NULL;                                                            */
/*         return msg;                                                               */
/*     } else {                                                                      */
/*         itypes = ws_init(m->type);                                                */
/*     }                                                                             */
/*     msg = ws_szap(m->inputs, itypes, msg, _type_compatible);                      */
/*                                                                                   */
/*     return msg;                                                                   */
/* }                                                                                 */
/*                                                                                   */
/* W* type_check(Ws* ws){                                                            */
/*     W* w = ws_scrap(ws, NULL, ws_recurse_composition, w_is_manifold, _typecheck); */
/*     w = _typecheck_derefs(ws, w);                                                 */
/*     return w;                                                                     */
/* }                                                                                 */
/*                                                                                   */
/* W* _typecheck_derefs(Ws* ws, W* msg){                                             */
/*     [> STUB <]                                                                    */
/*     return msg;                                                                   */
/* }                                                                                 */
/*                                                                                   */
/* W* _type_compatible(W* o, W* t, W* msg){                                          */
/*     switch(o->cls){                                                               */
/*         case C_DEREF:                                                             */
/*         case C_REFER:                                                             */
/*         case C_ARGREF:                                                            */
/*             [> I currently do no type checking on these <]                        */
/*             break;                                                                */
/*         case C_MANIFOLD:                                                          */
/*         {                                                                         */
/*             Manifold *m = g_manifold(g_rhs(o));                                   */
/*             if(!m->type){                                                         */
/*                 LOG_ERROR(msg, o, "cannot check usage of untyped output");        */
/*             }                                                                     */
/*             else if(!m->as_function){                                             */
/*                 char* o_type = type_str(m->type->last);                           */
/*                 char* i_type = type_str(t);                                       */
/*                 if( ! cmp_type(o_type, i_type)){                                  */
/*                     char* fmt = "type conflict '%s' vs '%s'\n";                   */
/*                     size_t size =                                                 */
/*                         strlen(fmt)    - // length of format string               */
/*                         4              + // subtract the '%s'                     */
/*                         strlen(o_type) + // string lengths                        */
/*                         strlen(i_type) + // ''                                    */
/*                         1;               // add 1 for \0                          */
/*                     char* errmsg = (char*)malloc(size * sizeof(char));            */
/*                     sprintf(errmsg, fmt, o_type, i_type);                         */
/*                     LOG_ERROR(msg, o, errmsg);                                    */
/*                 }                                                                 */
/*             }                                                                     */
/*         }                                                                         */
/*             break;                                                                */
/*         case C_POSITIONAL:                                                        */
/*         {                                                                         */
/*             char* o_type = g_string(g_lhs(o));                                    */
/*             char* i_type = type_str(t);                                           */
/*             if( ! cmp_type(o_type, i_type)){                                      */
/*                 char* fmt = "type conflict positional ('%s') '%s' vs '%s'\n";     */
/*                 size_t size =                                                     */
/*                     strlen(fmt)                - // length of the format string   */
/*                     6                          + // subtract the '%s'             */
/*                     strlen(o_type)             + // add length of type string     */
/*                     strlen(g_string(g_rhs(o))) + // ''                            */
/*                     strlen(i_type)             + // ''                            */
/*                     1;                           // add 1 for \0                  */
/*                 char* errmsg = (char*)malloc(size * sizeof(char));                */
/*                 sprintf(errmsg, fmt, o_type, g_string(g_rhs(o)), i_type);         */
/*                 LOG_ERROR(msg, o, errmsg);                                        */
/*             }                                                                     */
/*         }                                                                         */
/*             break;                                                                */
/*         default:                                                                  */
/*             break;                                                                */
/*     }                                                                             */
/*     return msg;                                                                   */
/* }                                                                                 */
/*                                                                                   */
/* void print_error(W* msg){                                                         */
/*     if(!msg) return;                                                              */
/*     for(W* w = g_ws(msg)->head; w; w = w->next){                                  */
/*         switch(g_lhs(w)->cls){                                                    */
/*             case C_MANIFOLD:                                                      */
/*             {                                                                     */
/*                 warn(                                                             */
/*                     "TYPE ERROR in %s: %s\n",                                     */
/*                     g_manifold(g_rhs(g_lhs(w)))->function,                        */
/*                     g_string(g_rhs(w))                                            */
/*                 );                                                                */
/*             }                                                                     */
/*                 break;                                                            */
/*             case C_POSITIONAL:                                                    */
/*             {                                                                     */
/*                 warn(                                                             */
/*                     "TYPE ERROR: positional is of type %s, but got %s\n",         */
/*                     g_string(g_lhs(g_lhs(w))),                                    */
/*                     g_string(g_rhs(w))                                            */
/*                 );                                                                */
/*             }                                                                     */
/*             default:                                                              */
/*                 break;                                                            */
/*         }                                                                         */
/*     }                                                                             */
/* }                                                                                 */


/* void _infer_multi_type(W* w);                                                          */
/* void _infer_generic_type(W* w);                                                        */
/* void _infer_star_type(W* w);                                                           */
/* void _transfer_star_type(W* type, W* input);                                           */
/* W* _transfer_generic_type(W* t, W* i, W* m);                                           */
/* void _horizontal_generic_propagation(W* t, Ws* inputs, Ws* types);                     */
/* W* _conditional_propagation(W* input, W* type, W* propagule);                          */
/*                                                                                        */
/* void infer_star_types(Ws* ws){                                                         */
/*     ws_rcmod(                                                                          */
/*         ws,                                                                            */
/*         ws_recurse_most,                                                               */
/*         w_is_manifold,                                                                 */
/*         _infer_star_type                                                               */
/*     );                                                                                 */
/* }                                                                                      */
/* void _infer_star_type(W* w){                                                           */
/*     Manifold* m = g_manifold(g_rhs(w));                                                */
/*     ws_zip_mod(                                                                        */
/*         m->type,                                                                       */
/*         m->inputs,                                                                     */
/*         _transfer_star_type                                                            */
/*     );                                                                                 */
/* }                                                                                      */
/* void _transfer_star_type(W* type, W* input){                                           */
/*     if(input->cls == C_MANIFOLD){                                                      */
/*         W* itype = g_manifold(g_rhs(input))->type->last;                               */
/*         if(                                                                            */
/*                 IS_ATOMIC(type) && IS_STAR(type) &&                                    */
/*                 ! (IS_ATOMIC(itype) && ( IS_STAR(itype) || IS_MULTI(itype)))           */
/*           ){                                                                           */
/*             type->value = itype->value;                                                */
/*             type->cls = itype->cls;                                                    */
/*         }                                                                              */
/*         if(                                                                            */
/*                 IS_ATOMIC(itype) && IS_STAR(itype) &&                                  */
/*                 ! (IS_ATOMIC(type) && ( IS_STAR(type) || IS_MULTI(type)))              */
/*           ){                                                                           */
/*             itype->value = type->value;                                                */
/*             itype->cls = type->cls;                                                    */
/*         }                                                                              */
/*     }                                                                                  */
/* }                                                                                      */
/*                                                                                        */
/*                                                                                        */
/* void infer_multi_types(Ws* ws){                                                        */
/*     ws_rcmod(                                                                          */
/*         ws,                                                                            */
/*         ws_recurse_most,                                                               */
/*         w_is_manifold,                                                                 */
/*         _infer_multi_type                                                              */
/*     );                                                                                 */
/* }                                                                                      */
/* void _infer_multi_type(W* w){                                                          */
/*     Manifold *wm, *im;                                                                 */
/*     wm = g_manifold(g_rhs(w));                                                         */
/*     if(wm->type &&                                                                     */
/*        wm->type->head->cls == FT_ATOMIC &&                                             */
/*        strcmp(g_string(wm->type->head), __MULTI__) == 0 &&                             */
/*        ws_length(wm->type) == 2 &&                                                     */
/*        ws_length(wm->inputs) > 1                                                       */
/*     ){                                                                                 */
/*         W* output = w_isolate(wm->type->last);                                         */
/*         wm->type = NULL;                                                               */
/*         for(W* i = wm->inputs->head; i != NULL; i = i->next){                          */
/*             switch(i->cls){                                                            */
/*                 case C_ARGREF:                                                         */
/*                     break;                                                             */
/*                 case C_POSITIONAL:                                                     */
/*                     {                                                                  */
/*                         char* ptype = g_string(g_lhs(i));                              */
/*                         wm->type = ws_add(wm->type, w_new(FT_ATOMIC, ptype));          */
/*                     }                                                                  */
/*                     break;                                                             */
/*                 case C_MANIFOLD:                                                       */
/*                     {                                                                  */
/*                         im = g_manifold(g_rhs(i));                                     */
/*                         if(ws_length(im->type) > 1){                                   */
/*                             wm->type = ws_add(wm->type, w_clone(im->type->last));      */
/*                         } else {                                                       */
/*                             wm->type = ws_add(wm->type, w_new(FT_ATOMIC, __WILD__));   */
/*                         }                                                              */
/*                     }                                                                  */
/*                     break;                                                             */
/*                 default:                                                               */
/*                     warn("Unexpected input type (%s:%d)\n", __func__, __LINE__);       */
/*             }                                                                          */
/*         }                                                                              */
/*         wm->type = ws_add(wm->type, output);                                           */
/*     }                                                                                  */
/* }                                                                                      */
/*                                                                                        */
/*                                                                                        */
/* void infer_generic_types(Ws* ws){                                                      */
/*     // 1) find all manifolds and infer their generic types                             */
/*     ws_rcmod(                                                                          */
/*         ws,                                                                            */
/*         ws_recurse_most,                                                               */
/*         w_is_manifold,                                                                 */
/*         _infer_generic_type                                                            */
/*     );                                                                                 */
/* }                                                                                      */
/*                                                                                        */
/* void _infer_generic_type(W* w){                                                        */
/*     Manifold* m = g_manifold(g_rhs(w));                                                */
/*     // 2) iterate through each type/input pair                                         */
/*     ws_szap(                                                                           */
/*         m->type,                                                                       */
/*         m->inputs,                                                                     */
/*         w, // this handle is needed to propagate types                                 */
/*         _transfer_generic_type                                                         */
/*     );                                                                                 */
/* }                                                                                      */
/*                                                                                        */
/* W* _transfer_generic_type(W* tw, W* iw, W* m){                                         */
/*     if(tw->cls != FT_GENERIC){                                                         */
/*         return m;                                                                      */
/*     }                                                                                  */
/*     W* old_type = g_rhs(tw);                                                           */
/*     W* new_type = NULL;                                                                */
/*     switch(iw->cls){                                                                   */
/*         case C_MANIFOLD:                                                               */
/*             new_type = ws_last(g_manifold(g_rhs(iw))->type);                           */
/*             break;                                                                     */
/*         case C_POSITIONAL:                                                             */
/*             new_type = w_new(FT_ATOMIC, g_string(g_lhs(iw)));                          */
/*             break;                                                                     */
/*         case C_ARGREF:                                                                 */
/*             fprintf(stderr, "ARGREF is not yet handled by %s\n", __func__);            */
/*             break;                                                                     */
/*         case C_DEREF:                                                                  */
/*         case C_GRPREF:                                                                 */
/*         case C_NEST:                                                                   */
/*         case C_REFER:                                                                  */
/*             fprintf(stderr, "These should have been resolved %s:%d\n",                 */
/*                     __func__, __LINE__);                                               */
/*             break;                                                                     */
/*         default:                                                                       */
/*             fprintf(stderr, "Weird case at %s:%d\n", __func__, __LINE__);              */
/*             break;                                                                     */
/*     }                                                                                  */
/*     Manifold* man = g_manifold(g_rhs(m));                                              */
/*     // 3) transfer types from input to type                                            */
/*     char* old_str = type_str(old_type);                                                */
/*     char* new_str = type_str(new_type);                                                */
/*     if(                                                                                */
/*         strcmp(new_str, __WILD__) != 0 &&                                              */
/*         strcmp(old_str, __WILD__) != 0 &&                                              */
/*         strcmp(new_str, old_str) != 0                                                  */
/*     ){                                                                                 */
/*         fprintf(stderr,                                                                */
/*             "TYPE ERROR: in '%s'(m%d in %s) expected type '%s', but got '%s'",         */
/*             man->function, man->uid, man->lang, old_str, new_str);                     */
/*     }                                                                                  */
/*     // transfer type                                                                   */
/*     s_rhs(tw, new_type);                                                               */
/*     // 4) for each inferred generic propagate types, die on conflict                   */
/*     _horizontal_generic_propagation(tw, man->inputs, man->type);                       */
/*     free(old_str);                                                                     */
/*     free(new_str);                                                                     */
/*     return m;                                                                          */
/* }                                                                                      */
/*                                                                                        */
/* void _horizontal_generic_propagation(W* t, Ws* inputs, Ws* types){                     */
/*     ws_szap(                                                                           */
/*         inputs, // the inputs                                                          */
/*         types,  // the types                                                           */
/*         t,      // the propagule, a generic type, FT_GENERIC<P_STRING,FT_*>            */
/*         _conditional_propagation                                                       */
/*     );                                                                                 */
/* }                                                                                      */
/*                                                                                        */
/* // Return true if a value was copied, false otherwise                                  */
/* bool _copy_type_from_a_to_b(W* a, W* b){                                               */
/*     bool result = false;                                                               */
/*     if(!(w_is_ptype(a) && w_is_ptype(b))){                                             */
/*         fprintf(stderr, "ERROR: Expected a and b to both be types in %s\n", __func__); */
/*     }                                                                                  */
/*     W* ra = g_rhs(a);                                                                  */
/*     W* rb = g_rhs(b);                                                                  */
/*     char* a_str = type_str(ra);                                                        */
/*     char* b_str = type_str(rb);                                                        */
/*     if(                                                                                */
/*         // they are different (there is something to do)                               */
/*         strcmp(a_str, b_str) != 0                                                      */
/*         // AND a is defined (there is something to transfer)                           */
/*         && strcmp(a_str, "*") != 0                                                     */
/*     ){                                                                                 */
/*         if(strcmp(b_str, "*") == 0){                                                   */
/*             // Input:                                                                  */
/*             //  t - FT_GENERIC<P_STRING,FT_*>                                          */
/*             //  g - FT_GENERIC<P_STRING,FT_*>                                          */
/*             // transfer type                                                           */
/*             b->value = a->value;                                                       */
/*             b->cls   = a->cls;                                                         */
/*             result = true;                                                             */
/*         } else {                                                                       */
/*             fprintf(stderr,                                                            */
/*                 "TYPE ERROR: during generic propagation, "                             */
/*                 "expected type '%s', but got '%s'",                                    */
/*                 a_str, b_str);                                                         */
/*         }                                                                              */
/*     }                                                                                  */
/*     return result;                                                                     */
/* }                                                                                      */
/*                                                                                        */
/* // 5) for each inferred generic 1..(k-1) transfer type to input, if needed             */
/* // 6) if k is an inferred generic,                                                     */
/* //    a. transfer it to its outputs                                                    */
/* //    b. if the output is generic, call #2 on it                                       */
/* W* _conditional_propagation(W* input, W* type, W* propagule){                          */
/*     fprintf(stderr, " --input %s --\n", w_str(input));                                 */
/*     fprintf(stderr, " --type  %s --\n", w_str(type));                                  */
/*     fprintf(stderr, " --prop  %s --\n", w_str(propagule));                             */
/*     // propagate to type if                                                            */
/*     if(                                                                                */
/*         // is a generic type                                                           */
/*         type->cls == FT_GENERIC                                                        */
/*         // AND is the same generic type                                                */
/*         && strcmp(g_string(g_lhs(type)), g_string(g_lhs(propagule))) == 0              */
/*     ){                                                                                 */
/*         fprintf(stderr, "copying\n");                                                  */
/*         // copy propagule type to current manifold type slot                           */
/*         _copy_type_from_a_to_b(propagule, type);                                       */
/*         fprintf(stderr, " --input %s --\n", w_str(input));                             */
/*         fprintf(stderr, " --type  %s --\n", w_str(type));                              */
/*         fprintf(stderr, " --prop  %s --\n", w_str(propagule));                         */
/*         fprintf(stderr, "\n");                                                         */
/*     }                                                                                  */
/*                                                                                        */
/*     if(input->cls == C_MANIFOLD){                                                      */
/*         Manifold* input_man = g_manifold(g_rhs(input));                                */
/*         W* input_type = ws_last(input_man->type);                                      */
/*         if(_copy_type_from_a_to_b(propagule, input_type)){                             */
/*             Ws* iinputs = input_man->inputs;                                           */
/*             Ws* itypes  = input_man->type;                                             */
/*             _horizontal_generic_propagation(input_type, itypes, iinputs);              */
/*         }                                                                              */
/*     }                                                                                  */
/*     // to make ws_szap hof happy, it requires a return W*                              */
/*     return NULL;                                                                       */
/* }                                                                                      */
/*                                                                                        */
/*                                                                                        */


/* // Let a and b be nodes in a type tree.                                       */
/* // Where                                                                      */
/* //   Node :: Generic | Primitive | Constructor [Node]                         */
/* // The Constructor nodes have 1 or more Node children                         */
/* //                                                                            */
/* // a and b are compatible if they are specifications of a common Node         */
/* //                                                                            */
/* // comp a b =                                                                 */
/* //   | Generic _ = TRUE                                                       */
/* //   | _ Generic = TRUE                                                       */
/* //   | Primitive Primitive = a == b                                           */
/* //   | Constructor Constructor = class(a) == class(b) AND all (map (zip a b)) */
/* //   | otherwise = FALSE                                                      */
/* // TODO: This is broken for 1) non-terminating cases `(a,(a,b)) | ((c,d),c)`  */
/* //       and 2) cases with multi-use generics, e.g. `(a,a) | (Int,Num)`       */
/* bool _types_are_compatible(W* a, W* b){                                       */
/*     return                                                                    */
/*        ( IS_GENERIC(a) || IS_GENERIC(b) )                                     */
/*        ||                                                                     */
/*        EQUAL_ATOMICS(a,b)                                                     */
/*        ||                                                                     */
/*        _constructor_compatible(a,b)                                           */
/*     ;                                                                         */
/* }                                                                             */
/* bool _constructor_compatible(W* a, W* b){                                     */
/*     bool compatible = false;                                                  */
/*     if( a->cls == b->cls ){                                                   */
/*         if( wws_length(a) == wws_length(b) ){                                 */
/*             W* aa = wws_head(a);                                              */
/*             W* bb = wws_head(b);                                              */
/*             for(;                                                             */
/*                 aa && bb;                                                     */
/*                 aa = aa->next, bb = bb->next)                                 */
/*             {                                                                 */
/*                 if(! _types_are_compatible(aa, bb)){                          */
/*                     return false;                                             */
/*                 }                                                             */
/*             }                                                                 */
/*             compatible = true;                                                */
/*         }                                                                     */
/*     }                                                                         */
/*     return compatible;                                                        */
/* }                                                                             */
