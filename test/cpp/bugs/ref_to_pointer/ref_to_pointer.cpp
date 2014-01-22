
typedef int* intPtr;
typedef float* fPtr;

int f(const intPtr& i){
	return *i;
}
float f(const fPtr& i){
	return *i;
}

int main(){
	int a=0;
	float b=0.0;

	return f(&a) + f(&b);
}
