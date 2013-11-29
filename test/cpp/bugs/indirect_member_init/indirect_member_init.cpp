struct A {
	struct {
		int a;
		float b;
	};
public:
	A(int val) 
		: a(val)
	{
	}
};

int main(){
	A x(1);
	return 0;
}
