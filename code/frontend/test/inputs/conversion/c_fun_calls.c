
int rec(int a) {
	if(a < 1)
		return a;
	return rec(a-1);
}

int i_to_i(int a) {
	return a;
}

int ii_to_i(int a, int b) {
	return b;
}

int main() {
	i_to_i(1);
	i_to_i(i_to_i(1));
	
	ii_to_i(1,2);
	ii_to_i(i_to_i(1),2);
	ii_to_i(i_to_i(1),ii_to_i(i_to_i(1),2));

	rec(3);

	return 0;
}
