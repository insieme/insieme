#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lib_icl_ext.h"

icl_args* icl_init_args(){
	icl_args* args = (icl_args*)malloc(sizeof(icl_args));
	args->size = 1024;
	args->local_size = 512;
	args->check_result = true;
	return args;
}

void icl_parse_args(int argc, const char** argv, icl_args* args) {
	for(int i = 1; i < argc; i++) { // usually argv[0] is the program name
		// params requiring an extra value
		if(i + 1 != argc) {
			if(!strcmp(argv[i], "-local")) {
				args->local_size = (size_t) (atoi(argv[i + 1]));
			}   
                        if(!strcmp(argv[i], "-size")) {
				args->size = (size_t) (atoi(argv[i + 1])); 
			}   
                }   
                // params requiring only one flag
		if(!strcmp(argv[i], "-check"))
			args->check_result = true;
		if(!strcmp(argv[i], "-help")){
			printf("Program supported arguments:\n"
                                " -size <S>\tinput size, default is 1024\n"
				" -local <S>\tOpenCL local size\n"
                                " -check\tcompares output results with a CPU host-based reference implementation\n"
                                " -help\tprints this info box\n"
			);
		}
	}
}

void icl_print_args(const icl_args *args){
	printf("arguments: size %d, local %d, check %s\n", args->size, args->local_size, (args->check_result)? "YES":"NO");
}

void icl_release_args(icl_args *args){
	free(args);
}
