#include <stdlib.h>

class K {
    public:
        K *x;
        K() { };
        ~K() { if(x) free(x); };
        K * get() { return x; }
        void set() { x = new K(); }
};

int main() {
    K *k = new K();
    if(k->x) {
       bool n = k->get();
       return n;
    } else {
        k->set();
    }
    int ret;
    k->x?ret=1:ret=0;
    return ret;
}
