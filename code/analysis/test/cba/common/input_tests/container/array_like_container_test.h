#include "../cba.h"

template<typename Container>
void test() {
	// test an uninitialized array
	Container a;

	cba_expect_undefined_int(a[0]);
	cba_expect_undefined_int(a[1]);
	cba_expect_undefined_int(a[2]);
	cba_expect_undefined_int(a[3]);

	a[1] = 12;

	cba_expect_undefined_int(a[0]);
	cba_expect_eq_int(12,a[1]);
	cba_expect_undefined_int(a[2]);
	cba_expect_undefined_int(a[3]);

	a[2] = 14;

	cba_expect_undefined_int(a[0]);
	cba_expect_eq_int(12,a[1]);
	cba_expect_eq_int(14,a[2]);
	cba_expect_undefined_int(a[3]);


	cba_expect_undefined_int(*(&(a[0])+0));
	cba_expect_eq_int(12,*(&(a[0])+1));
	cba_expect_eq_int(14,*(&(a[0])+2));
	cba_expect_undefined_int(*(&(a[0])+3));

	cba_expect_eq_int(12,*(&(a[1])+0));
	cba_expect_eq_int(14,*(&(a[1])+1));
	cba_expect_undefined_int(*(&(a[1])+2));

	cba_expect_eq_int(14,*(&(a[2])+0));
	cba_expect_undefined_int(*(&(a[2])+1));

	a[3] = 16;

	cba_expect_undefined_int(*(&(a[0])+0));
	cba_expect_eq_int(12,*(&(a[0])+1));
	cba_expect_eq_int(14,*(&(a[0])+2));
	cba_expect_eq_int(16,*(&(a[0])+3));

	cba_expect_eq_int(12,*(&(a[1])+0));
	cba_expect_eq_int(14,*(&(a[1])+1));
	cba_expect_eq_int(16,*(&(a[1])+2));

	cba_expect_eq_int(14,*(&(a[2])+0));
	cba_expect_eq_int(16,*(&(a[2])+1));

	*(&a[0] + 1) = 18;

	cba_expect_undefined_int(*(&(a[0])+0));
	cba_expect_eq_int(18,*(&(a[0])+1));
	cba_expect_eq_int(14,*(&(a[0])+2));
	cba_expect_eq_int(16,*(&(a[0])+3));

	cba_expect_eq_int(18,*(&(a[1])+0));
	cba_expect_eq_int(14,*(&(a[1])+1));
	cba_expect_eq_int(16,*(&(a[1])+2));

	cba_expect_eq_int(14,*(&(a[2])+0));
	cba_expect_eq_int(16,*(&(a[2])+1));

	*(&a[2] + 1) = 20;

	cba_expect_undefined_int(*(&(a[0])+0));
	cba_expect_eq_int(18,*(&(a[0])+1));
	cba_expect_eq_int(14,*(&(a[0])+2));
	cba_expect_eq_int(20,*(&(a[0])+3));

	cba_expect_eq_int(18,*(&(a[1])+0));
	cba_expect_eq_int(14,*(&(a[1])+1));
	cba_expect_eq_int(20,*(&(a[1])+2));

	cba_expect_eq_int(14,*(&(a[2])+0));
	cba_expect_eq_int(20,*(&(a[2])+1));


	// test reference analysis
	cba_expect_is_alias(&a[0], &a[0]);
	cba_expect_is_alias(&a[1], &a[1]);
	cba_expect_is_alias(&a[2], &a[2]);
	cba_expect_is_alias(&a[3], &a[3]);

	cba_expect_not_alias(&a[0], &a[1]);
	cba_expect_not_alias(&a[0], &a[2]);
	cba_expect_not_alias(&a[0], &a[3]);

	cba_expect_not_alias(&a[1], &a[2]);
	cba_expect_not_alias(&a[1], &a[3]);

	cba_expect_not_alias(&a[2], &a[3]);


//	// an initialized array
//	Container b = { 1, 2, 3, 4 };
//
//	cba_expect_eq_int(1,b[0]);
//	cba_expect_eq_int(2,b[1]);
//	cba_expect_eq_int(3,b[2]);
//	cba_expect_eq_int(4,b[3]);
}
