 #include <stdio.h>
 #include <string.h>
 
static char str[128] = "12345\t";
 
 int main(int argc, char** argv)
 {
	char s[128] = "1\t2\t3\t4\t5\t";
	if(strlen(s) != 10)
			return 1;
	else
			return 0;
	if(strlen(str) != 6)
		return 1;
	else
			return 0;
}
