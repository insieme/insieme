
int even(unsigned);

int odd(unsigned x) {
	return (x==0)?0:even(x-1);
}