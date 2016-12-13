#include "manifold.h"
#include <stdio.h>

int main(int argc, char * argv[]){
    LL* ll = new_LL();
    ll = add(ll, (void*)new_Manifold());
    ll = add(ll, (void*)new_Manifold());
    free_LL(ll, vfree_Manifold);
    return 0;
}
