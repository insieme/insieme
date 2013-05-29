//#include <stdio.h>
//#include <stdlib.h>
//#include <unistd.h>
//#include <string.h>
//#include <time.h>
//#include <sys/time.h>
//#ifndef _WIN32
//	#include <sys/utsname.h>
//	#include <sys/resource.h>
//#endif
//
//
//
//
//void bots_get_architecture(char *str) {
//	int ncpus = sysconf(_SC_NPROCESSORS_CONF);
//	struct utsname architecture;
//	uname(&architecture);
//	snprintf(str, 512, "%s-%s;%d" ,architecture.sysname, architecture.machine, ncpus);
//}
//
//
//int main(){
//	char str[512];
//
//	bots_get_architecture(str);
//
//}


typedef struct {
	char str[20];
} t_mine;

extern int printf(char *, ...);

int main (){
	t_mine a;
	printf("%s",a.str);
	printf("%s","asas");
}
