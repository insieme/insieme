// --- Generated Inspire Code ---
#define bool int
#define true 1
#define false 0
// --- Entry Point ---

// start code fragment :: fundef_codefragment_printf //
int printf(char*, ...);

// start code fragment :: fundef_codefragment___insieme_fun_1 //
int __insieme_fun_1(<?>((uint<4>)->int<4>)</?> var_10, unsigned int var_11){
	if((var_11 == ((unsigned int)(0)))) 0 else var_10__insieme_expr_2((var_11 - ((unsigned int)(1))));
}

// start code fragment :: fundecl_codefragment_even //
int even(unsigned int x);

// start code fragment :: fundecl_codefragment_odd //
int odd(unsigned int x);

// start code fragment :: fundef_codefragment_odd //
int odd(unsigned int x){
	return __insieme_fun_1(, x);;
}

// start code fragment :: fundef_codefragment___insieme_fun_3 //
int __insieme_fun_3(<?>((uint<4>)->int<4>)</?> var_7, unsigned int var_8){
	if((var_8 == ((unsigned int)(0)))) 1 else var_7__insieme_expr_4((var_8 - ((unsigned int)(1))));
}

// start code fragment :: fundef_codefragment_even //
int even(unsigned int x){
	return __insieme_fun_3(, x);;
}

// start code fragment :: fundef_codefragment___insieme_fun_0 //
char* __insieme_fun_0(int* var_12){
	if(((bool)(even(((unsigned int)((*var_12))))))) ((char*)("true")) else ((char*)("false"));
}

// start code fragment :: fundef_codefragment___insieme_fun_5 //
char* __insieme_fun_5(int* var_13){
	if(((bool)(odd(((unsigned int)((*var_13))))))) ((char*)("true")) else ((char*)("false"));
}

// start code fragment :: fundef_codefragment_main //
int main(int argc, char** argv){
	int x = 10;
	printf(((char*)("x=%d\n")), x);
	printf(((char*)("even(x)=%s\n")), __insieme_fun_0(&x));
	printf(((char*)("odd(x)=%s\n")), __insieme_fun_5(&x));
	return 0;;
}

// start code fragment :: unnamed //
