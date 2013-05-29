class Obj{
	public:
		int i;

		Obj(Obj& o)
			:i(o.i)
		{}

		Obj(int a)
			:i(a)
		{}
};



int main(){
	Obj a(3);
	Obj b(4);

	b = a;
}
