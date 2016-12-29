#include "class.h"

W* w_new(Class cls, void* value){ }

void w_free(W* o){ }

W* w_set_value(W* o, void* v){ }

W* w_add_over(W* a, W* b){ }

W* w_add_down(W* a, W* b){ }

W* w_next(const W* o){ }

W* w_down(const W* o){ }

W* w_head(const W* o){ }

W* w_tail(const W* o){ }

W* w_map( const W* o, W*(*map)(W*) ){ }

W* w_dmap( const W* o, W*(*map)(W*), int depth ){ }

W* w_rmap( const W* o, W*(*map)(W*) ){ }

W* w_dig( const W* o, W*(*digger)(W*), W*(*map)(W*) ){ }

W* w_zmap( const W* a, const W* b, W*(*map)(W*, W*) ){ }

W* w_zdmap( const W* a, const W* b, W*(*map)(W*, W*), int depth ){ }

W* w_zrmap( const W* a, const W* b, W*(*map)(W*, W*) ){ }

W* w_filter( const W* o, bool(*filter)(W*) ){ }

W* w_flatten(const W* o){ }

bool w_is_alone(const W* o){ }

bool w_is_iterative(const W* o){ }

bool w_is_recursive(const W* o){ }

bool w_is_collective(const W* o){ }

bool w_is_homogenous(const W* o){ }
