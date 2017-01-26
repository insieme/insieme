

typedef struct _A {
	void (*f)(struct _A*);
} A;


int main() {


	A a;

	return 0;
}
