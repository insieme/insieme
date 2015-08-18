
#pragma test expect_ir("lit(\"x\": ref<int<4>,f,f>)")
int x;

#pragma test expect_ir("lit(\"y\": ref<real<4>,t,f>)")
const float y;

typedef enum { Bla, Alb } enum_t;
#pragma test expect_ir("lit(\"globalEnum\": ref<__insieme_enum<enum_t,Bla,Alb>,f,f>)")
enum_t globalEnum;

//typedef struct { int x } IAmTheTagType;
//pragma test expect_ir("lit(\"y\": ref<real<4>,t,f>)"
//IAmTheTagType tt;

int main() {
	globalEnum;
	y;
	return x;
}