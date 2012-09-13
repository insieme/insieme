#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "lib_icl.h"
#include "lib_icl_ext.h"

icl_args* icl_init_args(){
	icl_args* args = (icl_args*)malloc(sizeof(icl_args));
	args->device_type = CL_DEVICE_TYPE_ALL;
	args->device_id = 0;
	args->size = 1024;
	args->local_size = 256;
	args->loop_iteration = 1;
	args->check_result = false;
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
			if(!strcmp(argv[i], "-device")) {
				if(!strcmp(argv[i+1], "cpu"))   args->device_type = CL_DEVICE_TYPE_CPU;
				if(!strcmp(argv[i+1], "gpu"))   args->device_type = CL_DEVICE_TYPE_GPU;
				if(!strcmp(argv[i+1], "acl"))   args->device_type = CL_DEVICE_TYPE_ACCELERATOR;
				if(!strcmp(argv[i+1], "all"))   args->device_type = CL_DEVICE_TYPE_ALL;
			}
                        if(!strcmp(argv[i], "-id")) {
                                args->device_id = (size_t) (atoi(argv[i + 1]));
                        }
			if(!strcmp(argv[i], "-loop")) {
				args->loop_iteration = (size_t) (atoi(argv[i + 1]));
			}
                }
                // params requiring only one flag
		if(!strcmp(argv[i], "-check"))
			args->check_result = true;
		if(!strcmp(argv[i], "-help")){
			printf("Program supported arguments:\n"
                                " -device <S>\tdevice type, default is all\n"
                                " -id <S>\tdevice id, default is 0\n"
                                " -size <S>\tinput size, default is 1024\n"
				" -local <S>\tOpenCL local size\n"
				" -loop <S>\tnumber of iteration of the buffer & kernel code\n"
                                " -check\tcompares output results with a CPU host-based reference implementation\n"
                                " -help\tprints this info box\n"
			);
		}
	}
}

static const char* _icl_get_device_type_string(cl_device_type type) {
        switch(type){
                case CL_DEVICE_TYPE_CPU: return "CPU"; break;
                case CL_DEVICE_TYPE_GPU: return "GPU"; break;
                case CL_DEVICE_TYPE_ACCELERATOR: return "ACL"; break;
                case CL_DEVICE_TYPE_ALL: return "ALL"; break;
                default: return "Default";
        }
}


void icl_print_args(const icl_args *args){
	printf("arguments: device %s, id %d, size %d, local %d, loop %d, check %s\n", _icl_get_device_type_string(args->device_type), args->device_id, args->size, args->local_size, args->loop_iteration, (args->check_result)? "YES":"NO");
}

void icl_release_args(icl_args *args){
	free(args);
}

#ifdef ICL_ENABLE_ENERGY_MEASUREMENT

#include "CInterface.h"

void icl_start_energy_measurement() {
        pmCreateNewSession("Test", "192.168.64.187", 5027);
        pmAbortSession("Test");

        if(pmStartSession(0) != 0) {
                pmAbortSession("Test");
                printf("Start session failed!\n");
                return -1;
        }
}

void icl_stop_energy_measurement() {
	char* key = pmStopSession();

	if(pmRetrieveResults(0, key, NULL)) {
		pmAbortSession("Test");
		printf("Retrieve results failed!\n");
		return -1;
        }

	double result;
	if( pmCalculateDiff(0, 0, Whr, &result)) {
		pmAbortSession("Test");
		printf("Calc diff failed! %f\n", result);
        } else {
		FILE* energy = fopen("energy.log", "w");
		fprintf(energy, "%f\n", result);
		fclose(energy);
	}

}
#else

void icl_start_energy_measurement(){}
void icl_stop_energy_measurement(){}

#endif
