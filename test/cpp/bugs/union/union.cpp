



union thing{

	int interger;
	float real;

};


struct {
union{
	int a;
	char b;
}var;

} caca;


int main (){

	thing b;
	b.interger = 1;
	b.real = 3.0;

	return 0;
}
