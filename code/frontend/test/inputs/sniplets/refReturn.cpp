
class A{

	private:
		int& a;
	
	public:

		A(int& v):
			a(v)
		{}

		int retValue(){
			return a;
		}
};


int main (){

	int i = 6;
	A a(i);

	a.retValue();
	return 0;
}
