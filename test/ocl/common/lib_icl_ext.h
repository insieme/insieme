#ifndef _LIB_ICL_EXT_
#define _LIB_ICL_EXT_
#include <stdbool.h>

typedef struct _icl_args {
	cl_device_type device_type;
	int device_id;
	int size;
	int local_size;
	int loop_iteration;
	bool check_result;
} icl_args;

icl_args* icl_init_args();
void icl_parse_args(int argc, const char** argv, icl_args* args);
void icl_print_args(const icl_args *args);
void icl_release_args(icl_args *args);
#endif
