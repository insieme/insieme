
class o{
	public:
	int a();
	int b();

};

int o::a(){
	return b();
}

int o::b(){
	return a();
}

int main (){
	o c;
	c.a();
}
