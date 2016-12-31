#include "lil.h"

void print_manifold_lil(Manifold* m){
    /* if(m){                                                                                */
    /*     printf("EMIT m%d\n", m->uid);                                                     */
    /*     if(m->function)                                                                   */
    /*         printf("FUNC m%d %s\n", m->uid, m->function);                                 */
    /*     if(m->inputs){                                                                    */
    /*         int i = 0;                                                                    */
    /*         for(W* w = m->inputs->head; w; w = w->next){                                  */
    /*             switch(w->type){                                                          */
    /*                 case C_MANIFOLD:                                                      */
    /*                     printf("INPM m%d %d m%d\n", m->uid, i++, w->value.manifold->uid); */
    /*                     break;                                                            */
    /*                 case C_POSITIONAL:                                                    */
    /*                     printf("INPP m%d %d %s\n", m->uid, i++, w->value.string);         */
    /*                     break;                                                            */
    /*                 case C_DEREF:                                                         */
    /*                     printf("NORM d%d\n", w->id->uid);                                 */
    /*                     // STUB                                                           */
    /*                     // - build the whole graph                                        */
    /*                     // - specify inputs                                               */
    /*                     // - check for singular output                                    */
    /*                     printf("INPD m%d %d d%d\n", m->uid, i++, w->id->uid);             */
    /*                     break;                                                            */
    /*                 default:                                                              */
    /*                     break;                                                            */
    /*             }                                                                         */
    /*         }                                                                             */
    /*     }                                                                                 */
    /*     if(m->effect)                                                                     */
    /*         printf("EFCT m%d %s\n", m->uid, m->effect);                                   */
    /* }                                                                                     */
}

void print_prolog(Ws* ws_top){ }

void print_manifolds(Ws* t_top){
    /* Ws* ws_man = ws_recursive_get_type(ws_top, P_MANIFOLD); */
    /* for(W* w = ws_man->head; w; w = w->next){               */
    /*     print_manifold_lil(w->value.manifold);              */
    /*     if(w->next){                                        */
    /*         printf("\n");                                   */
    /*     }                                                   */
    /* }                                                       */
}

void print_epilog(Ws* ws_top){ }

void print_lil(Ws* ws_top){
    if(ws_top && ws_top->head){
ws_print(ws_top, ws_recurse_all);
        print_prolog(ws_top);
        print_manifolds(ws_top);
        print_epilog(ws_top);
    } else {
        fprintf(stderr, "The symbol table is empty - nothing to do\n");
    }
}
