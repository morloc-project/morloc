template <class A, class B>
struct Mapping { int src; int dst; };

Mapping<int,int> mk(int x){ return Mapping<int,int>{x, 10 * x}; }
