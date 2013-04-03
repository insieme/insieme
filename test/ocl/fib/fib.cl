/*
int _fib50(int n) { return 0; }
int _fib49(int n) { return (n <= 1)?1:(_fib50(n-1) + _fib50(n-2)); }
int _fib48(int n) { return (n <= 1)?1:(_fib49(n-1) + _fib49(n-2)); }
int _fib47(int n) { return (n <= 1)?1:(_fib48(n-1) + _fib48(n-2)); }
int _fib46(int n) { return (n <= 1)?1:(_fib47(n-1) + _fib47(n-2)); }
int _fib45(int n) { return (n <= 1)?1:(_fib46(n-1) + _fib46(n-2)); }
int _fib44(int n) { return (n <= 1)?1:(_fib45(n-1) + _fib45(n-2)); }
int _fib43(int n) { return (n <= 1)?1:(_fib44(n-1) + _fib44(n-2)); }
int _fib42(int n) { return (n <= 1)?1:(_fib43(n-1) + _fib43(n-2)); }
int _fib41(int n) { return (n <= 1)?1:(_fib42(n-1) + _fib42(n-2)); }
int _fib40(int n) { return (n <= 1)?1:(_fib41(n-1) + _fib41(n-2)); }
int _fib39(int n) { return (n <= 1)?1:(_fib40(n-1) + _fib40(n-2)); }
int _fib38(int n) { return (n <= 1)?1:(_fib39(n-1) + _fib39(n-2)); }
int _fib37(int n) { return (n <= 1)?1:(_fib38(n-1) + _fib38(n-2)); }
int _fib36(int n) { return (n <= 1)?1:(_fib37(n-1) + _fib37(n-2)); }
int _fib35(int n) { return (n <= 1)?1:(_fib36(n-1) + _fib36(n-2)); }
int _fib34(int n) { return (n <= 1)?1:(_fib35(n-1) + _fib35(n-2)); }
int _fib33(int n) { return (n <= 1)?1:(_fib34(n-1) + _fib34(n-2)); }
int _fib32(int n) { return (n <= 1)?1:(_fib33(n-1) + _fib33(n-2)); }
int _fib31(int n) { return (n <= 1)?1:(_fib32(n-1) + _fib32(n-2)); }
int _fib30(int n) { return (n <= 1)?1:(_fib31(n-1) + _fib31(n-2)); }
int _fib29(int n) { return (n <= 1)?1:(_fib30(n-1) + _fib30(n-2)); }
int _fib28(int n) { return (n <= 1)?1:(_fib29(n-1) + _fib29(n-2)); }
int _fib27(int n) { return (n <= 1)?1:(_fib28(n-1) + _fib28(n-2)); }
int _fib26(int n) { return (n <= 1)?1:(_fib27(n-1) + _fib27(n-2)); }
int _fib25(int n) { return (n <= 1)?1:(_fib26(n-1) + _fib26(n-2)); }
int _fib24(int n) { return (n <= 1)?1:(_fib25(n-1) + _fib25(n-2)); }
int _fib23(int n) { return (n <= 1)?1:(_fib24(n-1) + _fib24(n-2)); }
int _fib22(int n) { return (n <= 1)?1:(_fib23(n-1) + _fib23(n-2)); }
int _fib21(int n) { return (n <= 1)?1:(_fib22(n-1) + _fib22(n-2)); }
int _fib20(int n) { return (n <= 1)?1:(_fib21(n-1) + _fib21(n-2)); }
int _fib19(int n) { return (n <= 1)?1:(_fib20(n-1) + _fib20(n-2)); }
int _fib18(int n) { return (n <= 1)?1:(_fib19(n-1) + _fib19(n-2)); }
int _fib17(int n) { return (n <= 1)?1:(_fib18(n-1) + _fib18(n-2)); }
int _fib16(int n) { return (n <= 1)?1:(_fib17(n-1) + _fib17(n-2)); }
int _fib15(int n) { return (n <= 1)?1:(_fib16(n-1) + _fib16(n-2)); }
int _fib14(int n) { return (n <= 1)?1:(_fib15(n-1) + _fib15(n-2)); }
int _fib13(int n) { return (n <= 1)?1:(_fib14(n-1) + _fib14(n-2)); }
int _fib12(int n) { return (n <= 1)?1:(_fib13(n-1) + _fib13(n-2)); }
int _fib11(int n) { return (n <= 1)?1:(_fib12(n-1) + _fib12(n-2)); }
int _fib10(int n) { return (n <= 1)?1:(_fib11(n-1) + _fib11(n-2)); }
int _fib9(int n) { return (n <= 1)?1:(_fib10(n-1) + _fib10(n-2)); }
int _fib8(int n) { return (n <= 1)?1:(_fib9(n-1) + _fib9(n-2)); }
int _fib7(int n) { return (n <= 1)?1:(_fib8(n-1) + _fib8(n-2)); }
int _fib6(int n) { return (n <= 1)?1:(_fib7(n-1) + _fib7(n-2)); }
int _fib5(int n) { return (n <= 1)?1:(_fib6(n-1) + _fib6(n-2)); }
int _fib4(int n) { return (n <= 1)?1:(_fib5(n-1) + _fib5(n-2)); }
int _fib3(int n) { return (n <= 1)?1:(_fib4(n-1) + _fib4(n-2)); }
int _fib2(int n) { return (n <= 1)?1:(_fib3(n-1) + _fib3(n-2)); }
int _fib1(int n) { return (n <= 1)?1:(_fib2(n-1) + _fib2(n-2)); }
int _fib0(int n) { return (n <= 1)?1:(_fib1(n-1) + _fib1(n-2)); }
*/
int _fib0(int n) { return (n <= 1)?1:(_fib0(n-1) + _fib0(n-2)); }

__kernel void fib(__global int* a, __global int* res, int size) {
	int gid = get_global_id(0);

	if (a[gid] < 0) {
		res[gid] = 0;
		return;
	}

	res[gid] = _fib0(a[gid]);
}
