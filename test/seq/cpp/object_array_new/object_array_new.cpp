
struct S {
	int m;
	S() { m= 12; }
};
	
int main() {
	S* arr = new S[50];
	int ret = arr[42].m != 12;
	delete [] arr;
	return ret;
}
