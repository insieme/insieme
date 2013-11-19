struct Base{
	int a;

};

struct Derived : public Base{

	float b;
	Derived() : Base() {}

};

int main (){

	if (__is_base_of(Base, Derived) == true)
		return 0;
	else 
		return 1;
}
