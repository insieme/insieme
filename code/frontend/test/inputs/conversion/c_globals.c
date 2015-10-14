
#pragma test expect_ir("lit(\"x\": ref<int<4>,f,f>)")
int x;

#pragma test expect_ir("lit(\"y\": ref<real<4>,t,f>)")
const float y;

typedef enum { Bla, Alb } enum_t;
#pragma test expect_ir("lit(\"globalEnum\": ref<struct { enum_def<IMP_enum_t,enum_entry<Bla,0>,enum_entry<Alb,1>> enum_type; uint<4> value },f,f>)")
enum_t globalEnum;

typedef struct { int x; } IAmTheTagType;
#pragma test expect_ir("lit(\"tt\": ref<struct IMP_IAmTheTagType { int<4> x; },f,f>)")
IAmTheTagType tt;

int main() {
	globalEnum;
	y;
	tt;
	return x;
}
