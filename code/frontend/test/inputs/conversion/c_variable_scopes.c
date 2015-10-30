
int main() {

	#pragma test expect_ir("{ 1; var ref<int<4>,f,f> v0; ref_deref(v0); }")
	{
		#pragma test expect_num_vars(0)
		1; // dummy statement to attach to
		int x;
		#pragma test expect_num_vars(1)
		x;
	}

	#pragma test expect_num_vars(0)
	1;

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; { var ref<int<4>,f,f> v1; ref_deref(v0); ref_deref(v1); } }")
	{
		int x;
		{
			int y;
			#pragma test expect_num_vars(2)
			x;
			y;
		}
	}
	
	#pragma test expect_ir("{ { var ref<int<4>,f,f> v1; } 1; }")
	{
		{
			int x;
		}
		#pragma test expect_num_vars(0)
		1;
	}

}

int x;

void bla() {
	#pragma test expect_num_vars(1)
	1;

	int c;
	#pragma test expect_num_vars(2)
}

void bla2(int x) {
	#pragma test expect_num_vars(2)
	x;
}

void bla3(char* filename);
void bla4(char* filename) { filename; }

typedef struct { int i; } Image;
typedef unsigned long long ull;

void write_image(Image target, Image dist, char* filename, ull minSteps, ull maxSteps) {
	
	#pragma test expect_num_vars(6)
	bla3(filename);
	bla4(filename);
}
