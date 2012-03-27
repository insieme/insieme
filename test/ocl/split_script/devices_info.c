#include <stdlib.h>
#include <stdio.h>
#include "lib_icl.h"

int main(int argc, char* argv[]) {
	icl_init_devices(ICL_ALL);
	int num = icl_get_num_devices();
	if (num != 0) {
		for (int i = 0; i < num; ++i) {
			icl_device* dev = icl_get_device(i);
			icl_print_device_short_info(dev);
		}
	}
	
	icl_release_devices();
}
