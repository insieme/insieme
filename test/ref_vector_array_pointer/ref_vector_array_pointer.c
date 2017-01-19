
/**
 * A small test case covering the conversion of references, arrays, vectors and pointers within declarations,
 * structs, unions and function parameters.
 *
 */

#define bool int

#define true 1
#define false 0

extern int printf(const char *, ...);


typedef struct _data {
	char s1;
	char* s2;
	char s3[5];
	char s4[5][3];
	char* s5;
	char** s6;
	char(* s7)[4];
	char(* s8)[4][3];
	char(** s9)[4];
	char(** s10)[4][3];
} Data;

char funArray(char a, char b[5], char c[5][2], char d[5][3][2]) {
	return b[3];
}

char funPointer(char a, char* b, char** c, char*** d) {
	return b[3];
}


char funStruct(Data data) {
	return data.s1;
}

int main(int argc, char* argv[]) {

	// test within declarations

	// simple scalar
	char a1 = 'a';

	// single / multidimensional C-arrays
	char b1[6] = "Hallo";
	char b2[3][2] = {"X", "Y", "Z" };
	char b3[5][3][2] = { {"X", "Y", "Z" } };

	// arrays / pointer
	char* c1 = b1;
	char** c2 = 0;
	char*** c3 = 0;

	// mixed constructs
	char(* d1)[3];
	char(** d2)[3][2];
	char(*** d3)[3][2][1];
	char(*** d4)[3];
	char(* d5)[3][2][1];

	// mixed constructs (2)
	char* e1[3];
	char** e2[2][3];
	char*** e3[1][2][3];

	// within a struct
	Data s;
	s.s1 = '#';

	// printf("size of s %d\n", sizeof(Data));

	printf("size of b1 %d\n", sizeof(b1));
	printf("size of b2 %d\n", sizeof(b2));
	printf("size of b3 %d\n", sizeof(b3));

	printf("size of c1 %d\n", sizeof(c1));
	printf("size of c2 %d\n", sizeof(c2));
	printf("size of c3 %d\n", sizeof(c3));

	printf("size of d1 %d\n", sizeof(d1));
	printf("size of d2 %d\n", sizeof(d2));
	printf("size of d3 %d\n", sizeof(d3));
	printf("size of d4 %d\n", sizeof(d4));
	printf("size of d5 %d\n", sizeof(d5));

	printf("size of e1 %d\n", sizeof(e1));
	printf("size of e2 %d\n", sizeof(e2));
	printf("size of e3 %d\n", sizeof(e3));

	printf("Letter Array:   %c\n", funArray(a1, b1, b2, b3));
	printf("Letter Pointer: %c\n", funPointer(a1, c1, c2, c3));
	printf("Letter Struct:  %c\n", funStruct(s));

	char *strings[] = { "This", "is", "a", "little", "test"};

	printf("String:  %s\n", strings[0]);
	printf("String:  %s\n", strings[1]);
	printf("String:  %s\n", strings[2]);
	printf("String:  %s\n", strings[3]);
	printf("String:  %s\n", strings[4]);

	return 0;
}
