void f(const int& a){
}

void g(int& a){
}

int main(){

	int a =5;

	g(a);
	
	f(3);
	f(a);

}
