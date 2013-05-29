

typedef struct {
	int a;
} t;


void f(t ta){

	int b = ta.a;
}


void g(int i){
	int a = i;
}

int main (){
	t str;
	f(str);
	g(4);
}
