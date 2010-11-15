// --- Generated Inspire Code ---
#define bool int
#define true 1
#define false 0
// --- Entry Point ---

// start code fragment :: fundef_codefragment___insieme_fun_0 //
int __insieme_fun_0(int* var_6){
	int var_5 = var_6[0];
	var_6[0] = var_6[0]+((int)(1));
	return var_5;;
}

// start code fragment :: fundef_codefragment_f //
int f(int* a){
	return __insieme_fun_0(a);;
}

// start code fragment :: fundef_codefragment_printf //
int printf(char*, ...);

// start code fragment :: fundef_codefragment_main //
int main(int argc, char** argv){
	int var_7 = argc;
	{
		int a = 0;
		f((int[]) {a});
		f((int[]) {var_7});
		printf(((char*)("%d\n")), var_7);var_7 = a;
		printf(((char*)("%d\n")), var_7);
	};
}

// start code fragment :: unnamed //
