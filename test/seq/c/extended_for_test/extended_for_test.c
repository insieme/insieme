#include <stdio.h>

int main() {
/******* FORMAL SYNTAX TEST *********/
//this is how a for statement should look like according to the cppreference
//attr(optional) for ( init-statement condition(optional) ; iteration_expression(optional) ) statement

//for () statement
{
	for(;;) { printf("FORMAL: empty_for\n"); break; }
}

//for () statement
{
	int i=0;
	for(;;) {
		i++;
		if(i==10) break;
	}
	printf("FORMAL: simple for\n");
}

//for ( init-statement; ; ) statement
{
	int i=0;
	for(i=8;;) {
		printf("FORMAL: init statement only: %i\n", i);
		i++;
		if(i==10) break;
	}
}

//for ( ; condition; ) statement
{
	int i=0;
	for(;i<10;) {
		i++;
	}
	printf("FORMAL: condition only: %i\n", i);
}

//for ( ; ; iteration_expression ) statement
{
	int i=0;
	for(;;++i) {
		if(i==10) break;
	}
	printf("FORMAL: iteration expr only: %i\n", i);
}

//for ( init-statement; ; iteration_expression ) statement
{
	int i=0;
	for(i=7;;i++) {
		printf("FORMAL: init statement and iteration expr: %i\n", i);
		if(i==10) break;
	}
}

//for ( init-statement; condition ; ) statement
{
	int i=0;
	for(i=7;i<10;) {
		printf("FORMAL: init statement and condition: %i\n", i);
		i++;
	}
}

//for ( ; condition ; iteration_expression ) statement
{
	int i=8;
	for(;i<10;++i) { printf("FORMAL: condition and iteration expr: %i\n", i); }
}

//for ( init-statement; condition ; iteration_expression ) statement
{
	int i=8;
	for(i=5;i<10;++i) { printf("FORMAL: init statement, condition++, and iteration expr: %i\n", i); }
}

//for ( init-statement; condition ; iteration_expression ) statement
{
	int i=8;
	for(i=10;i>5;--i) { printf("FORMAL: init statement, condition--, and iteration expr: %i\n", i); }
}

//for ( init-statement; condition ; iteration_expression ) statement
{
	int i=-10;
	for(i=5;i<20;i*=2) { printf("FORMAL: init statement, condition*=2, and iteration expr: %i\n", i); }
}

/****** INFORMAL SYNTAX TEST ********/
//attr(optional) for ( declaration-or-expression(optional) ; declaration-or-expression(optional) ; expression(optional) ) statement

//for ( expression; expression; ) statement
{
	int i=0;
	for(5;20;) { 
		i++; 
		printf("INFORMAL: expression and expression: %i\n", i);
		if(i==3) break;
	}
}

//for ( expression; expression; expression ) statement
{
	int i=0;
	for(5;20;++i) { 
		printf("INFORMAL: expression, expression, and expression: %i\n", i);
		if(i==3) break;
	}
}

//for ( expression; declaration; ) statement
{
	int i=8;
	for(5;(int){i%10};) { 
		i++;
		printf("INFORMAL: expression and declaration: %i\n", i);
	}
}

//for ( expression; declaration; expression ) statement
{
	int i=8;
	for(5;(int){i%10};i++) { 
		printf("INFORMAL: expression, declaration, and expression: %i\n", i);
	}
}

//for ( declaration; expression; ) statement
{
	int i=8;
	for(int i=5;i<9;) {
		i++;		
		printf("INFORMAL: declaration and expression: %i\n", i);
	}
}

//for ( declaration; expression; expression ) statement
{
	int i=8;
	for(int i=5;i<7;++i) {
		printf("INFORMAL: declaration, expression, and expression: %i\n", i);
	}
}

//for ( declaration; declaration; ) statement
{
	int i=8;
	for(int i=5;(int){i%10};) {
		i++;
		printf("INFORMAL: declaration and declaration: %i\n", i);
	}
}

//for ( declaration; declaration; expression ) statement
{
	int i=8;
	for(int i=5;(int){i%10};++i) {
		printf("INFORMAL: declaration, declaration, and expression: %i\n", i);
	}
}

return 0;
}
