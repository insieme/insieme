
class A;
class B;
class C;
class D;
class E;

#define DEF(X) \
    class X { \
	A* a; B* b; C* c; D* d; E* e; \
	A& getA() const { return *a; } \
	B& getB() const { return *b; } \
	C& getC() const { return *c; } \
	D& getD() const { return *d; } \
	E& getE() const { return *e; } \
   }; 

DEF(A);
DEF(B);
DEF(C);
DEF(D);
DEF(E);

int main() {

	A a;
	
	return 0;
}
