typedef struct
{
	int a;
	int b;
} __mpq_struct;
 
typedef __mpq_struct mpq_t[1];

mpq_t& ret() {
	mpq_t a;
	return a;
}


int main() {
	ret();
	return 0;
}
