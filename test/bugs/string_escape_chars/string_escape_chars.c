 #include <stdio.h>
 #include <string.h>
 
static char str[128] = "12345\t";
 
 int main(int argc, char** argv)
 {
	char s[128] = "1\t2\t3\t4\t5\t";
	printf("%s%s\n", str, s);
	if(strlen(s) != 10) printf("ERROR\n");
	if(strlen(str) != 6) printf("ERROR\n");
	 return 0;
}
