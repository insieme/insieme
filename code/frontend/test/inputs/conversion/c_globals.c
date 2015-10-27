
#pragma test expect_ir("lit(\"x\": ref<int<4>,f,f>)")
int x;

#pragma test expect_ir("lit(\"y\": ref<real<4>,t,f>)")
const float y;

typedef enum { Bla, Alb } enum_t;
#pragma test expect_ir("lit(\"globalEnum\": ref<struct enum { enum_def<IMP_enum_t,enum_entry<Bla,0>,enum_entry<Alb,1>> enum_type; uint<4> value },f,f>)")
enum_t globalEnum;

typedef struct { int x; } IAmTheTagType;
#pragma test expect_ir("lit(\"tt\": ref<struct IMP_IAmTheTagType { int<4> x; },f,f>)")
IAmTheTagType tt;

typedef struct _omp_lock_t { int x; } omp_lock_t;
void omp_set_lock(omp_lock_t* lock);
#pragma test expect_ir("lit(\"lck\": ref<struct IMP__omp_lock_t { int<4> x; },f,f>)")
omp_lock_t lck;

int main() {
	globalEnum;
	y;
	tt;
	#pragma test expect_ir("STRING","ptr_from_ref(lck)")
	&lck;
	#pragma test expect_ir("STRING","omp_set_lock(ptr_from_ref(lck))")
	omp_set_lock(&lck);
	return x;
}
