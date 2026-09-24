template <class G>
int make_and_apply(G g, int x) {
    return g(x)(x);
}

template <class G>
int make_and_apply3(G g, int x) {
    return g(x)(x)(x);
}
