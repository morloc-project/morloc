#include "type_util.h"

bool cmp_type(char* a, char* b){
    return
        ( strcmp(a,  b ) == 0 ) ||
        ( strcmp(a, __WILD__) == 0 ) ||
        ( strcmp(b, __WILD__) == 0 );
}

bool type_is_io(W* w){
    return
        w->cls == FT_ATOMIC &&
        strcmp(g_string(w), __IO__) == 0;
}

bool type_is_well(Ws* type){
    return type_is_io(type->head) && !type_is_io(type->last);
}

bool type_is_pipe(Ws* type){
    return !type_is_io(type->head) && !type_is_io(type->last);
}

bool type_is_sink(Ws* type){
    return !type_is_io(type->head) && type_is_io(type->last);
}

size_t get_generic_id_from_uid(size_t uid, char c){
    return uid * ('z'-'a'+1) + (c - 'a');
}

size_t get_generic_size(int max_uid){
    return max_uid * ('z'-'a'+1);
}

size_t get_generic_id(W* w, char c){
    Manifold* m = g_manifold(g_rhs(w));
    return get_generic_id_from_uid(m->uid, c);
}

size_t get_generic_id_offset(size_t uid){
    return get_generic_id_from_uid(uid, 0);
}

int type_str_r(W* w, char* s, int p){
#define CHK(x)                                              \
if((p + x) >= MAX_TYPE_LENGTH) {                            \
    warn("Type buffer exceeded, truncating type string\n"); \
    return p;                                               \
}

    switch(w->cls){
        case FT_FUNCTION:
        {
            int i = 0;
            CHK(1)
            s[p++] = '(';
            for(W* wt = wws_head(w); wt != NULL; wt = wt->next){
                if(i > 0){
                    CHK(2)
                    s[p++] = '-';
                    s[p++] = '>';
                }
                p = type_str_r(wt, s, p);
                i++;
            }
            CHK(1)
            s[p++] = ')';
        }
            break;
        case FT_TUPLE:
        {
            int i = 0;
            s[p++] = '(';
            for(W* wt = wws_head(w); wt != NULL; wt = wt->next){
                if(i > 0){
                    CHK(1)
                    s[p++] = ',';
                }
                p = type_str_r(wt, s, p);
                i++;
            }
            s[p++] = ')';
        }
            break;
        case FT_ARRAY:
        {
            CHK(1)
            s[p++] = '[';
            p = type_str_r(wws_head(w), s, p);
            CHK(1)
            s[p++] = ']';
        }
            break;
        case FT_GENERIC:
            // - The lhs holds the generic label (e.g. 'a')
            // - The rhs of a generic holds the inferred type it will be of type
            // FT_*, so can be thrown into the next cycle
            p = type_str_r(g_rhs(w), s, p);
            break;
        case FT_ATOMIC:
        {
            char* atom = g_string(w);
            size_t atom_size = strlen(atom);
            CHK(atom_size)
            strcpy(s + p, atom);
            p += atom_size;
            break;
        }
        case C_POSITIONAL:
        {
            char* atom = g_string(g_lhs(w));
            size_t atom_size = strlen(atom);
            CHK(atom_size)
            strcpy(s + p, atom);
            p += atom_size;
            break;
        }
        case C_ARGREF:
        {
            CHK(1)
            s[p++] = '*';
            break;
        }
        case C_REFER:
        {
            w = ws_last(g_manifold(g_rhs(w))->type);
            p = type_str_r(w, s, p);
            break;
        }
        default:
            warn("Expected FT_* at (%s:%d), got %s", __func__, __LINE__, w_str(w)); 
            break;
    }
    return p;
#undef CHK
}

char* type_str(W* w){
   char* s = (char*)malloc(MAX_TYPE_LENGTH * sizeof(char));
   int p = type_str_r(w, s, 0);
   s[p] = '\0';
   char* ss = strdup(s);
   free(s);
   return ss;
}
