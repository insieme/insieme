
struct a{
};

typedef struct a* ptr_t;

int f(ptr_t p){
	if (p)
		return -1;
	else
		return 0;
}

int main (){

	ptr_t a = (ptr_t)0;

	return f(ptr_t());
}
