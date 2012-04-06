#ifndef _WIN32
	#include <alloca.h>
#else
	#include <malloc.h>
#endif
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "lib_icl.h"

/*
 * =====================================================================================
 *  OpenCL Static Internal Functions Declarations
 * =====================================================================================
 */

static inline cl_uint _icl_get_num_platforms();
static inline void _icl_get_platforms(cl_uint num_platforms, cl_platform_id* platforms);

static cl_uint _icl_get_num_devices(cl_platform_id* platform, cl_device_type device_type);
static inline void _icl_get_devices(cl_platform_id* platform, cl_device_type device_type, cl_uint num_devices, cl_device_id* devices);

static const char* _icl_get_device_type_string(cl_device_type type);
static char* _icl_get_name(cl_device_id* device);
static cl_device_type _icl_get_type(cl_device_id* device);
static char* _icl_get_vendor(cl_device_id* device);
static char* _icl_get_version(cl_device_id* device);
static char* _icl_get_driver_version(cl_device_id* device);
static char* _icl_get_profile(cl_device_id* device);

static cl_uint _icl_get_max_compute_units(cl_device_id* device);
static cl_uint _icl_get_max_clock_frequency(cl_device_id* device);
static cl_uint _icl_get_max_work_item_dimensions(cl_device_id* device);
static size_t* _icl_get_max_work_item_sizes(cl_device_id* device);
static size_t _icl_get_max_work_group_size(cl_device_id* device);

static cl_bool _icl_has_image_support(cl_device_id* device);
static cl_device_fp_config _icl_get_single_fp_config(cl_device_id* device);
static cl_bool _icl_is_endian_little(cl_device_id* device);
static char* _icl_get_extensions(cl_device_id* device);

static cl_ulong _icl_get_global_mem_size(cl_device_id* device);
static cl_ulong _icl_get_max_mem_alloc_size(cl_device_id* device);
static cl_device_mem_cache_type _icl_get_global_mem_cache_type(cl_device_id* device);
static cl_uint _icl_get_global_mem_cacheline_size(cl_device_id* device);
static cl_ulong _icl_get_global_mem_cache_size(cl_device_id* device);

static cl_ulong _icl_get_max_constant_buffer_size(cl_device_id* device);

static cl_device_local_mem_type _icl_get_local_mem_type(cl_device_id* device);
static cl_ulong _icl_get_local_mem_size(cl_device_id* device);

static char* _icl_load_program_source (const char* filename, size_t* filesize);
static void _icl_save_program_binary (cl_program program, const char* binary_filename);
static const char* _icl_error_string (cl_int err_code);
static inline cl_profiling_info _icl_flag_to_profile(icl_event_flag event_flag);


/*
 * =====================================================================================
 *  OpenCL Device Functions
 * =====================================================================================
 */

void icl_init_devices(cl_device_type device_type) {
	devices = NULL;
	num_devices = 0;
	cl_platform_id* cl_platforms;
	cl_uint cl_num_platforms = _icl_get_num_platforms();
	if (cl_num_platforms != 0) {
		cl_platforms = (cl_platform_id*)alloca(cl_num_platforms * sizeof(cl_platform_id));
		_icl_get_platforms(cl_num_platforms, cl_platforms);
		
		for (cl_uint i = 0; i < cl_num_platforms; i++) {
			cl_uint cl_num_devices = _icl_get_num_devices(&cl_platforms[i], device_type);
			num_devices += cl_num_devices;
		}
	}
	if (num_devices != 0) {
		devices = (icl_device*)malloc(num_devices * sizeof(icl_device));
		cl_uint index = 0;
		cl_platform_id* cl_platforms;
		cl_uint cl_num_platforms = _icl_get_num_platforms();
		if (cl_num_platforms != 0) {
			cl_platforms = (cl_platform_id*)alloca(cl_num_platforms * sizeof(cl_platform_id));
			_icl_get_platforms(cl_num_platforms, cl_platforms);
		
			for (cl_uint i = 0; i < cl_num_platforms; i++) {
				cl_device_id* cl_devices;
				cl_uint cl_num_devices = _icl_get_num_devices(&cl_platforms[i], device_type);
				if (cl_num_devices != 0) {
					cl_devices = (cl_device_id*)alloca(cl_num_devices * sizeof(cl_device_id));
					_icl_get_devices(&cl_platforms[i], device_type, cl_num_devices, cl_devices);
					for (cl_uint j = 0; j < cl_num_devices; ++j, ++index) {
						cl_int err_code;
						icl_device* dev = &devices[index];
						dev->device = cl_devices[j];
						dev->context = clCreateContext(NULL, 1, &dev->device, NULL, NULL, &err_code);
						ICL_ASSERT(err_code == CL_SUCCESS &&dev->context != NULL, "Error creating context: \"%s\"", _icl_error_string(err_code));
						dev->queue = clCreateCommandQueue(dev->context, dev->device, CL_QUEUE_PROFILING_ENABLE, &err_code);
						ICL_ASSERT(err_code == CL_SUCCESS && dev->queue != NULL, "Error creating queue: \"%s\"", _icl_error_string(err_code));
						
						// Device Info
						dev->name = _icl_get_name(&dev->device);
						dev->type = _icl_get_type(&dev->device);
						dev->vendor = _icl_get_vendor(&dev->device);
						dev->version = _icl_get_version(&dev->device);
						dev->driver_version = _icl_get_driver_version(&dev->device);
						dev->profile = _icl_get_profile(&dev->device);

						dev->max_compute_units = _icl_get_max_compute_units(&dev->device);
						dev->max_clock_frequency = _icl_get_max_clock_frequency(&dev->device);
						dev->max_work_item_dimensions = _icl_get_max_work_item_dimensions(&dev->device);
						dev->max_work_item_sizes = _icl_get_max_work_item_sizes(&dev->device);
						dev->max_work_group_size = _icl_get_max_work_group_size(&dev->device);

						dev->image_support = _icl_has_image_support(&dev->device);
						dev->single_fp_config = _icl_get_single_fp_config(&dev->device);
						dev->endian_little = _icl_is_endian_little(&dev->device);
						dev->extensions = _icl_get_extensions(&dev->device);
	
						dev->mem_cache_type = _icl_get_global_mem_cache_type(&dev->device);
						dev->global_mem_cacheline_size = _icl_get_global_mem_cacheline_size(&dev->device);
						dev->global_mem_cache_size = _icl_get_global_mem_cache_size(&dev->device);

						dev->max_constant_buffer_size = _icl_get_max_constant_buffer_size(&dev->device);

						dev->local_mem_type = _icl_get_local_mem_type(&dev->device);
						dev->local_mem_size = _icl_get_local_mem_size(&dev->device);

						// Buffer Info 
						dev->mem_size = _icl_get_global_mem_size(&dev->device);
						dev->mem_available = _icl_get_global_mem_size(&dev->device);
						dev->max_buffer_size = _icl_get_max_mem_alloc_size(&dev->device);
					}
				}
			}
		}
	}
}

void icl_release_devices() {
	cl_int err_code;
	for (cl_uint i = 0; i < num_devices; ++i) {
		icl_device* dev = &devices[i];

		// release the queue
		err_code = clReleaseCommandQueue(dev->queue);
		ICL_ASSERT(err_code == CL_SUCCESS, "Error releasing command queue: \"%s\"", _icl_error_string(err_code));
		
		// release the context
		err_code = clReleaseContext(dev->context);
		ICL_ASSERT(err_code == CL_SUCCESS, "Error releasing context: \"%s\"", _icl_error_string(err_code));

		// release the info		
		free(dev->name);
		free(dev->vendor);
		free(dev->version);
		free(dev->driver_version);
		free(dev->profile);
		free(dev->extensions);
		free(dev->max_work_item_sizes);
	}
	free(devices);
}


inline cl_uint icl_get_num_devices() {
	return num_devices;
}

inline icl_device* icl_get_device(cl_uint id) {
	ICL_ASSERT(id < num_devices, "Error accessing device with wrong ID");
	return &devices[id];
}

/*
 * =====================================================================================
 *  OpenCL Buffer Functions
 * =====================================================================================
 */

icl_buffer* icl_create_buffer(icl_device* dev, cl_mem_flags flags, size_t size) {
	ICL_ASSERT(size <= dev->max_buffer_size, "Error creating buffer: \"Buffer size is too big\"");
	ICL_ASSERT(size <= dev->mem_available, "Error creating buffer: \"Out of memory\"");

	// create buffer
	icl_buffer* buf = (icl_buffer*)malloc(sizeof(icl_buffer));
	cl_int err_code;
	buf->mem = clCreateBuffer(dev->context, flags, size, NULL, &err_code);
	ICL_ASSERT(err_code == CL_SUCCESS, "Error creating buffer: \"%s\"", _icl_error_string(err_code));
	buf->size = size;
	buf->dev = dev;
	dev->mem_available -= size;
	//ICL_INFO("Created Buffer: %lu  Available Memory:%lu\n", size, dev->mem_available);
	return buf;
}

inline void icl_release_buffer(icl_buffer* buf) {
	if (buf != NULL) {
		// free the buffer
		cl_int err_code = clReleaseMemObject(buf->mem);
		ICL_ASSERT(err_code == CL_SUCCESS, "Error releasing cl_mem: \"%s\"", _icl_error_string(err_code));
		buf->dev->mem_available += buf->size;
		//ICL_INFO("Released Buffer: %lu  Available Memory:%lu\n", buf->size, buf->dev->mem_available);
		free(buf);
		buf = NULL;
	}
}

void icl_release_buffers(cl_uint num, ...){
	va_list arg_list;
	va_start(arg_list, num);
	for (cl_uint i = 0; i < num; i++){
		icl_release_buffer(va_arg(arg_list, icl_buffer*));
	}
	va_end(arg_list);
}

inline static void _icl_set_event(icl_event* wait_event, icl_event* event, cl_event** wait_ev, cl_event** ev, cl_uint* num){ // pay attention they are pointer to pointer
	if (event != NULL) {
		ICL_ASSERT(event->num_event == 1, "Error in events handler: Expected one single event");
		*ev = event->event;
	}

	if (wait_event != NULL) {
		*num = wait_event->num_event;
		*wait_ev = wait_event->event;
	}	
}

inline void icl_write_buffer(icl_buffer* buf, cl_bool blocking, size_t size, const void* source_ptr, icl_event* wait_event, icl_event* event) {
	cl_event* ev = NULL; cl_event* wait_ev = NULL; cl_uint num = 0;
	_icl_set_event(wait_event, event, &wait_ev, &ev, &num);	

	cl_int err_code = clEnqueueWriteBuffer(buf->dev->queue, buf->mem, blocking, 0, size, source_ptr, num, wait_ev, ev);
	ICL_ASSERT(err_code == CL_SUCCESS, "Error writing buffer: \"%s\"",  _icl_error_string(err_code));
}

inline void icl_read_buffer(const icl_buffer* buf, cl_bool blocking, size_t size, void* source_ptr, icl_event* wait_event, icl_event* event) {
	cl_event* ev = NULL; cl_event* wait_ev = NULL; cl_uint num = 0;
	_icl_set_event(wait_event, event, &wait_ev, &ev, &num);

	cl_int err_code = clEnqueueReadBuffer(buf->dev->queue, buf->mem, blocking, 0, size, source_ptr, num, wait_ev, ev);
	ICL_ASSERT(err_code == CL_SUCCESS, "Error reading buffer: \"%s\"",  _icl_error_string(err_code));
}

inline void* icl_map_buffer(icl_buffer* buf, cl_bool blocking, cl_map_flags map_flags, size_t size, icl_event* wait_event, icl_event* event) {
	cl_event* ev = NULL; cl_event* wait_ev = NULL; cl_uint num = 0;
	_icl_set_event(wait_event, event, &wait_ev, &ev, &num);
	
	cl_int err_code;
	void* ptr = clEnqueueMapBuffer(buf->dev->queue, buf->mem, blocking, map_flags, 0, size, num, wait_ev, ev, &err_code); 
	ICL_ASSERT(err_code == CL_SUCCESS, "Error mapping buffer: \"%s\"",  _icl_error_string(err_code));
	return ptr;
}

inline void icl_unmap_buffer(icl_buffer* buf, void* mapped_ptr, icl_event* wait_event, icl_event* event) {
	cl_event* ev = NULL; cl_event* wait_ev = NULL; cl_uint num = 0;
	_icl_set_event(wait_event, event, &wait_ev, &ev, &num);

	cl_int err_code = clEnqueueUnmapMemObject(buf->dev->queue, buf->mem, mapped_ptr, num, wait_ev, ev);
	ICL_ASSERT(err_code == CL_SUCCESS, "Error unmapping buffer: \"%s\"",  _icl_error_string(err_code));
}

inline void icl_copy_buffer(icl_buffer* src_buf, icl_buffer* dest_buf, size_t size, icl_event* wait_event, icl_event* event) {
	cl_event* ev = NULL; cl_event* wait_ev = NULL; cl_uint num = 0;
	_icl_set_event(wait_event, event, &wait_ev, &ev, &num);

	ICL_ASSERT(src_buf->dev->queue  == dest_buf->dev->queue, "Error: source and destination buffer have a different queue");
	cl_int err_code = clEnqueueCopyBuffer(src_buf->dev->queue, src_buf->mem, dest_buf->mem, 0, 0, size, num, wait_ev, ev);
	ICL_ASSERT(err_code == CL_SUCCESS, "Error copying buffer: \"%s\"",  _icl_error_string(err_code));
}

/*
 * =====================================================================================
 *  OpenCL Print & Profile Functions
 * =====================================================================================
 */

void icl_print_device_infos(icl_device* dev) {
	ICL_INFO("name:                   %s\n", dev->name);
	ICL_INFO("type:                   %s\n", _icl_get_device_type_string(dev->type));
	ICL_INFO("vendor:                 %s\n", dev->vendor);
	ICL_INFO("version:                %s\n", dev->version);
	ICL_INFO("driver version:         %s\n", dev->driver_version);
	ICL_INFO("profile:                %s\n", dev->profile);
	ICL_INFO("\n");
	ICL_INFO("max compute-units:      %u\n", dev->max_compute_units);
	ICL_INFO("max clock frequency:    %u\n", dev->max_clock_frequency);
	ICL_INFO("max work-item dims:     %u\n", dev->max_work_item_dimensions);
	ICL_INFO("max work-item sizes:    [");
	for (cl_uint i = 0; i < dev->max_work_item_dimensions; i++) {
		ICL_INFO("%lu", (unsigned long) dev->max_work_item_sizes[i]);
		if(i < dev->max_work_item_dimensions - 1) ICL_INFO(", ");
	}
	ICL_INFO("]\n");
	ICL_INFO("max work-group size:    %lu\n", (unsigned long) dev->max_work_group_size);

	ICL_INFO("image support:          %s", dev->image_support ? "yes\n" : "no\n");
	ICL_INFO("single fp config:      ");
	if(dev->single_fp_config & CL_FP_DENORM) ICL_INFO(" denorm");
	if(dev->single_fp_config & CL_FP_INF_NAN) ICL_INFO(" inf_nan");
	if(dev->single_fp_config & CL_FP_ROUND_TO_NEAREST) ICL_INFO(" round_to_nearest");
	if(dev->single_fp_config & CL_FP_ROUND_TO_ZERO) ICL_INFO(" round_to_zero");
	if(dev->single_fp_config & CL_FP_ROUND_TO_INF) ICL_INFO(" round_to_inf");
	if(dev->single_fp_config & CL_FP_FMA) ICL_INFO(" fma");
	ICL_INFO("\n");
	ICL_INFO("endian little:          %s", dev->endian_little ? "yes\n" : "no\n");
	ICL_INFO("extensions:             %s\n", dev->extensions);
	ICL_INFO("\n");
	ICL_INFO("global mem size:        %lu MB\n", (unsigned long) (dev->mem_size / 1024 /1024));
	ICL_INFO("max mem alloc size:     %lu MB\n", (unsigned long) (dev->max_buffer_size / 1024 /1024));

	if(dev->mem_cache_type == CL_NONE) {
		ICL_INFO("global mem cache:	 none\n");	
	} else {
		ICL_INFO("global mem cache:	%s", dev->mem_cache_type == CL_READ_ONLY_CACHE ? "read only\n" : "write_only\n");		
		ICL_INFO("global mem cline size:  %lu byte\n", (unsigned long) (dev->global_mem_cacheline_size));
		ICL_INFO("global mem cache size:  %lu kB\n", (unsigned long) (dev->global_mem_cache_size / 1024));
	}
	ICL_INFO("\n");	
	ICL_INFO("max const buffer size:  %lu kB\n", (unsigned long) (dev->max_constant_buffer_size / 1024));
	ICL_INFO("dedicated local mem:    %s", dev->local_mem_type == CL_LOCAL ? "yes\n" : "no\n");
	ICL_INFO("local mem size:         %lu kB\n", (unsigned long) (dev->local_mem_size / 1024));
}


void icl_print_device_short_info(icl_device* dev) {
	ICL_INFO("%s: %s | %s | %s\n", _icl_get_device_type_string(dev->type), dev->name, dev->vendor, dev->version);
}

icl_event* icl_create_event() {
	icl_event* icl_ev =  (icl_event*)malloc(sizeof(icl_event));
	icl_ev->event = (cl_event*)malloc(sizeof(cl_event));
	icl_ev->num_event = 1;
	return icl_ev;
}

icl_event* icl_create_event_list(cl_uint num_event, ...){
	icl_event* icl_ev =  (icl_event*)malloc(sizeof(icl_event));
	icl_event* arg_event;
	va_list arg_list;
	va_start(arg_list, num_event);
	cl_event* array = (cl_event*)malloc(sizeof(cl_event) * num_event);
	for (cl_uint i = 0; i < num_event; i++){
		arg_event = va_arg (arg_list, icl_event*);
		array[i] = *(arg_event->event);
	}
	va_end(arg_list);
	icl_ev->event = array;
	icl_ev->num_event = num_event;
	return icl_ev;
}

void icl_release_event(icl_event* event){
	if (event != NULL) {
		if (event->num_event == 1)
			clReleaseEvent(*(event->event));
		free(event->event);
		free(event);
	}
}

void icl_release_events(cl_uint num, ...){
	va_list arg_list;
	va_start(arg_list, num);
	for (cl_uint i = 0; i < num; i++){
		icl_release_event(va_arg(arg_list, icl_event*));
	}
	va_end(arg_list);
}

inline double icl_profile_event(icl_event* event, icl_event_flag event_start, icl_event_flag event_end, icl_time_flag time_flag) {
	return icl_profile_events(event, event_start, event, event_end, time_flag);
}

static inline cl_profiling_info _icl_flag_to_profile(icl_event_flag event_flag){
	switch(event_flag) {
		case ICL_ENQUEUED: return CL_PROFILING_COMMAND_QUEUED;
		case ICL_SUBMITTED: return CL_PROFILING_COMMAND_SUBMIT;
		case ICL_STARTED: return CL_PROFILING_COMMAND_START;
		case ICL_FINISHED: return CL_PROFILING_COMMAND_END;
	}
	ICL_ASSERT(false, "Error: this event flag is not present");
	return 0;
}

double icl_profile_events(icl_event* event_one, icl_event_flag event_one_command, icl_event* event_two, icl_event_flag event_two_command, icl_time_flag time_flag) {
	ICL_ASSERT(event_one != NULL && event_two != NULL, "Error profiling events: \"One of the events is NULL\"");
	cl_ulong event_one_start, event_two_end;
	cl_int err_code;

	err_code = clGetEventProfilingInfo(*(event_two->event), _icl_flag_to_profile(event_two_command), sizeof(cl_ulong), &event_two_end, NULL);
	ICL_ASSERT(err_code == CL_SUCCESS, "Error getting profiling info: \"%s\"",  _icl_error_string(err_code));
	err_code = clGetEventProfilingInfo(*(event_one->event), _icl_flag_to_profile(event_one_command), sizeof(cl_ulong), &event_one_start, NULL);
	ICL_ASSERT(err_code == CL_SUCCESS, "Error getting profiling info: \"%s\"", _icl_error_string(err_code));
	double time = 0.0;
	switch(time_flag) {
		case ICL_NANO:
			time = (double)(event_two_end - event_one_start);
			break;
		case ICL_MILLI:
			time = (double)(event_two_end - event_one_start) * 1.0e-6f;
			break;
		case ICL_SEC:
			time = (double)(event_two_end - event_one_start) * 1.0e-9f;
		 	break;
	}
	return time;
}

icl_timer* icl_init_timer(icl_time_flag time_flag){
	icl_timer* timer = (icl_timer*)malloc(sizeof(icl_timer));
	timer->start = 0;
	timer->clocks = 0;
#ifdef _WIN32
	timer->freq = 0;
#endif
	timer->time_flag = time_flag;
	return timer;
}

void icl_start_timer(icl_timer* timer){
#ifdef _WIN32
	QueryPerformanceCounter((LARGE_INTEGER *)&(timer->start));
#else
	struct timespec tmp;
	clock_gettime(CLOCK_REALTIME, &tmp);
	timer->start = (time_int)tmp.tv_sec * 1e9 + (time_int)tmp.tv_nsec;
#endif
}

void icl_restart_timer(icl_timer* timer){
	icl_start_timer(timer);	
	timer->clocks = 0;
}

double icl_stop_timer(icl_timer* timer){
	time_int stop;
#ifdef _WIN32
	QueryPerformanceCounter((LARGE_INTEGER *)&stop);
#else
	struct timespec tmp;
	clock_gettime(CLOCK_REALTIME, &tmp);
	stop = (time_int)tmp.tv_sec * 1e9 + (time_int)tmp.tv_nsec;
#endif 
	timer->clocks += (stop - timer->start);
	timer->start = 0;

	double time = 0.0;
	switch(timer->time_flag) {
		case ICL_NANO: 
			#ifdef _WIN32
				time = (double)timer->clocks * (double) 1e9 / (double) timer->freq;
			#else
				time = (double)timer->clocks;
			#endif
			break;
		case ICL_MILLI: 
			#ifdef _WIN32
				time = (double)timer->clocks * (double) 1e3 / (double) timer->freq;
			#else
				time = (double)timer->clocks / (double) 1e6;
			#endif
			break;
		case ICL_SEC: 
			#ifdef _WIN32
				time = (double)timer->clocks / (double) timer->freq;
			#else
				time = (double)timer->clocks / (double) 1e9;
			#endif
			break;
	}
	return time;
}

void icl_release_timer(icl_timer* timer){
	free(timer);
}

/*
 * =====================================================================================
 *  OpenCL Kernel Functions
 * =====================================================================================
 */

icl_kernel*  icl_create_kernel(icl_device* dev, const char* file_name, const char* kernel_name, const char* build_options, icl_create_kernel_flag flag) {
	cl_program program = NULL;
	char* binary_name = NULL;
	size_t filesize = 0;
	cl_int err_code;
	if (flag != ICL_STRING) {
		// create the binary name
		size_t len, binary_name_size, cl_param_size;
		char* device_name;
		err_code = clGetDeviceInfo(dev->device, CL_DEVICE_NAME, 0, NULL, &cl_param_size);
		ICL_ASSERT(err_code == CL_SUCCESS, "Error getting device name: \"%s\"", _icl_error_string(err_code));
		device_name = (char*)alloca(cl_param_size);
		err_code = clGetDeviceInfo(dev->device, CL_DEVICE_NAME, cl_param_size, device_name, NULL);
		ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting device name: \"%s\"", _icl_error_string(err_code));

		const char* file_ptr = strrchr(file_name, '/'); // remove the path from the file_name
		if (file_ptr)
			file_ptr++; // remove the last '/'
		else
			file_ptr = file_name;

		len = strlen(file_ptr);
		//ICL_ASSERT(len >= 0, "Error size of file_name");

		char* converted_file_name = (char*)alloca(len + 1); // +1 for the \0 in the end
		strcpy (converted_file_name, file_ptr);
		for (size_t i = 0; i < len; ++i) {
			if (!isalnum ((int)converted_file_name[i])) {
				converted_file_name[i] = '_';
			}
		}
		binary_name_size = len;

		len = strlen(device_name);
		char* converted_device_name = alloca(len + 1); // +1 for the \0 in the end
		strcpy (converted_device_name, device_name);
		for (size_t i = 0; i < len; ++i) {
			if (!isalnum ((int)converted_device_name[i])) {
				converted_device_name[i] = '_';
			}
		}
		binary_name_size += len;
		binary_name_size += 6; // file_name.device_name.bin\0

		binary_name = (char*)alloca(binary_name_size);

		sprintf (binary_name, "%s.%s.bin", converted_file_name, converted_device_name);
		//printf("%s\n", binary_name);
	}
	if ((flag == ICL_SOURCE) || (flag == ICL_STRING)) {
		if (flag == ICL_SOURCE) {
			char* program_source = _icl_load_program_source(file_name, &filesize);
			ICL_ASSERT(program_source != NULL, "Error loading kernel program source");
			program = clCreateProgramWithSource (dev->context, 1, (const char **) &program_source, NULL, &err_code);
			ICL_ASSERT(err_code == CL_SUCCESS && program != NULL, "Error creating compute program: \"%s\"", _icl_error_string(err_code));
			free(program_source);
		} else { // case of ICL_STRING we use the file_name to pass the string
			program = clCreateProgramWithSource (dev->context, 1, (const char **) &file_name, NULL, &err_code);
			ICL_ASSERT(err_code == CL_SUCCESS && program != NULL, "Error creating compute program: \"%s\"", _icl_error_string(err_code));
		}

		err_code = clBuildProgram(program, 1, &(dev->device), build_options, NULL, NULL);

		// If there are build errors, print them to the screen
		if(err_code != CL_SUCCESS) {
			ICL_INFO("Kernel program failed to build.\n");
			char *buildLog;
			size_t buildLogSize;
			err_code = clGetProgramBuildInfo(program,dev->device, CL_PROGRAM_BUILD_LOG, 0, NULL, &buildLogSize);
			ICL_ASSERT(err_code == CL_SUCCESS, "Error getting program build info: \"%s\"", _icl_error_string(err_code));
			buildLog = (char*)malloc(buildLogSize);
			err_code = clGetProgramBuildInfo(program,dev->device, CL_PROGRAM_BUILD_LOG, buildLogSize, buildLog, NULL);
			ICL_ASSERT(err_code == CL_SUCCESS, "Error getting program build info: \"%s\"", _icl_error_string(err_code));
			buildLog[buildLogSize-1] = '\0';
			ICL_INFO("Device Build Log:\n%s\n", buildLog);
			free(buildLog);
		}
		ICL_ASSERT(err_code == CL_SUCCESS, "Error building compute program: \"%s\"", _icl_error_string(err_code));
	}

	if (flag == ICL_SOURCE) _icl_save_program_binary(program, binary_name); // We don't save in case of ICL_STRING

	if (flag == ICL_BINARY) {
		cl_int binary_status;
		unsigned char* program_s = (unsigned char*) _icl_load_program_source(binary_name, &filesize);
		program = clCreateProgramWithBinary (dev->context, 1, &(dev->device), &filesize, (const unsigned char **) &program_s, &binary_status, &err_code);
		free(program_s);
		ICL_ASSERT(err_code == CL_SUCCESS && binary_status == CL_SUCCESS && program != NULL, "Error creating compute program: \"%s\"", _icl_error_string(err_code));
		err_code = clBuildProgram(program, 1, &(dev->device), build_options, NULL, NULL);
		ICL_ASSERT(err_code == CL_SUCCESS, "Error building compute program: \"%s\"", _icl_error_string(err_code));
	}

	icl_kernel* kernel = NULL;
	if ((kernel_name != NULL) && (*kernel_name != 0)) {
		kernel = (icl_kernel*)malloc(sizeof(icl_kernel));
		kernel->kernel = clCreateKernel(program, kernel_name, &err_code);
		ICL_ASSERT(err_code == CL_SUCCESS, "Error creating kernel: \"%s\"", _icl_error_string(err_code));
		kernel->dev = dev;
	}
	err_code = clReleaseProgram(program);
	ICL_ASSERT(err_code == CL_SUCCESS, "Error releasing compute program: \"%s\"", _icl_error_string(err_code));
	return kernel; // When kernel name = "", I want only to compile the program 
}

inline void icl_release_kernel(icl_kernel* kernel) {
	cl_int err_code = clReleaseKernel(kernel->kernel);
	ICL_ASSERT(err_code == CL_SUCCESS, "Error releasing kernel");
	free(kernel);
}

void icl_run_kernel(const icl_kernel* kernel, cl_uint work_dim, const size_t* global_work_size, const size_t* local_work_size, icl_event* wait_event, icl_event* event, cl_uint num_args, ...) {
	cl_event* ev = NULL; cl_event* wait_ev = NULL; cl_uint num = 0;
	_icl_set_event(wait_event, event, &wait_ev, &ev, &num);
	
	//loop through the arguments and call clSetKernelArg for each argument
	cl_uint arg_index;
	size_t arg_size;
	const void *arg_val;
	va_list arg_list;
	cl_int err_code;
	va_start (arg_list, num_args);
	for (unsigned i = 0; i < num_args; i++) {
		arg_index = i;
		arg_size = va_arg (arg_list, size_t);
		arg_val = va_arg (arg_list, void *);
		if (arg_size == 0){
			icl_buffer* buf = (icl_buffer*) arg_val;
			arg_size = sizeof(cl_mem);
			arg_val = (void*) &(buf->mem);
		}
		err_code = clSetKernelArg(kernel->kernel, arg_index, arg_size, arg_val);
		ICL_ASSERT(err_code == CL_SUCCESS, "Error setting kernel arguments: \"%s\"", _icl_error_string(err_code));    
	}
  	va_end (arg_list);

	err_code = clEnqueueNDRangeKernel((kernel->dev)->queue, 
						kernel->kernel, 
						work_dim,
						NULL, 
						global_work_size,
						local_work_size,
						num, wait_ev, ev);
	ICL_ASSERT(err_code == CL_SUCCESS, "Error enqueuing NDRange Kernel: \"%s\"", _icl_error_string(err_code));
}




// ----------------------------------------------------------- OpenCL Internal Functions --------------------------------------------------

/*
 * =====================================================================================
 *  OpenCL Internal Platform Functions
 * =====================================================================================
 */

inline static cl_uint _icl_get_num_platforms() {
	cl_uint cl_num_platforms;
	if (clGetPlatformIDs(0, NULL, &cl_num_platforms) != CL_SUCCESS) return 0;
	return cl_num_platforms;
}

inline static void _icl_get_platforms(cl_uint num_platforms, cl_platform_id* platforms) {
	cl_int err_code = clGetPlatformIDs(num_platforms, platforms, NULL);	
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting platforms: \"%s\"", _icl_error_string(err_code));
}

/* 
 * =====================================================================================
 *  OpenCL Internal Device Functions
 * =====================================================================================
 */

cl_uint _icl_get_num_devices(cl_platform_id* platform, cl_device_type device_type) {
	cl_uint cl_num_devices;
	if (clGetDeviceIDs(*platform, device_type, 0, NULL, &cl_num_devices) != CL_SUCCESS) return 0;
	return cl_num_devices;
}

inline static void _icl_get_devices(cl_platform_id* platform, cl_device_type device_type, cl_uint num_devices, cl_device_id* devices) {
	cl_int err_code = clGetDeviceIDs(*platform, device_type, num_devices, devices, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting devices: \"%s\"", _icl_error_string(err_code)); 
}


/* 
* =====================================================================================
*  OpenCL Internal Load, Save, Error Functions
* =====================================================================================
*/

static char* _icl_load_program_source (const char* filename, size_t* filesize) { // remember to free the returned source
	ICL_ASSERT(filename != NULL && filesize != NULL, "Error input parameters");
	FILE* fp = fopen(filename, "rb");
	ICL_ASSERT(fp != NULL, "Error opening kernel file");
	ICL_ASSERT(fseek(fp, 0, SEEK_END) == 0, "Error seeking to end of file");
	long unsigned int size = ftell(fp);
	ICL_ASSERT(fseek(fp, 0, SEEK_SET) == 0, "Error seeking to begin of file");
	char* source = (char*)malloc(size+1);
	ICL_ASSERT(source != NULL, "Error allocating space for program source");
	ICL_ASSERT(fread(source, 1, size, fp) == size, "Error reading file");
	source[size] = '\0';
	*filesize = size; // this is the size useful for create program from binary
	ICL_ASSERT(fclose (fp) == 0, "Error closing the file");
	return source;
}

static void _icl_save_program_binary (cl_program program, const char* binary_filename) {
	ICL_ASSERT(binary_filename != NULL && program != NULL, "Error input parameters");
	size_t size_ret;
	cl_int err_code;
	err_code = clGetProgramInfo (program, CL_PROGRAM_BINARY_SIZES, 0, NULL, &size_ret);
	ICL_ASSERT(err_code == CL_SUCCESS, "Error getting program info: \"%s\"", _icl_error_string(err_code));
	size_t* binary_size = (size_t *) alloca (size_ret);
	ICL_ASSERT(binary_size != NULL, "Error allocating binary_size");
	err_code = clGetProgramInfo (program, CL_PROGRAM_BINARY_SIZES, size_ret, binary_size, NULL);
	ICL_ASSERT(err_code == CL_SUCCESS,  "Error getting program info: \"%s\"", _icl_error_string(err_code));
    	unsigned char* binary = (unsigned char *) alloca (sizeof (unsigned char) * (*binary_size));
	ICL_ASSERT(binary != NULL, "Error allocating binary");

	// get the binary
	err_code = clGetProgramInfo (program, CL_PROGRAM_BINARIES, sizeof (unsigned char *), &binary, NULL);
	ICL_ASSERT(err_code == CL_SUCCESS,  "Error getting program info: \"%s\"", _icl_error_string(err_code));

	FILE *fp = fopen (binary_filename, "w");
	ICL_ASSERT(fp != NULL, "Error opening binary file");
	ICL_ASSERT(fwrite (binary, 1, *binary_size, fp) ==  (size_t) *binary_size, "Error writing file");
	ICL_ASSERT(fclose (fp) == 0, "Error closing the file");
}

static const char* _icl_error_string (cl_int errcode) {
	switch (errcode) {
		case CL_SUCCESS:
			return "SUCCESS";
		case CL_DEVICE_NOT_FOUND:
			return "Device not found";
		case CL_DEVICE_NOT_AVAILABLE:
			return "Device not available";
		case CL_COMPILER_NOT_AVAILABLE:
			return "Compiler not available";
		case CL_MEM_OBJECT_ALLOCATION_FAILURE:
			return "Memory Object allocation failure";
		case CL_OUT_OF_RESOURCES:
			return "Out of resources";
		case CL_OUT_OF_HOST_MEMORY:
			return "Out of host memory";
		case CL_PROFILING_INFO_NOT_AVAILABLE:
			return "Profiling info not available";
		case CL_MEM_COPY_OVERLAP:
			return "Mem copy overlap";
		case CL_IMAGE_FORMAT_MISMATCH:
			return "Image format mismatch";
		case CL_IMAGE_FORMAT_NOT_SUPPORTED:
			return "Image format not supported";
		case CL_BUILD_PROGRAM_FAILURE:
			return "Build program failure";
		case CL_MAP_FAILURE:
			return "Map failure";
		case CL_MISALIGNED_SUB_BUFFER_OFFSET:
			return "Misaligned sub buffer offset";
		case CL_EXEC_STATUS_ERROR_FOR_EVENTS_IN_WAIT_LIST:
			return "Status Error for events in wait list";
		case CL_INVALID_VALUE:
			return "Invalid value";
		case CL_INVALID_DEVICE_TYPE:
			return "Invalid device type";
		case CL_INVALID_PLATFORM:
			return "Invalid platform";
		case CL_INVALID_DEVICE:
			return "Invalid device";
		case CL_INVALID_CONTEXT:
			return "Invalid context";
		case CL_INVALID_QUEUE_PROPERTIES:
			return "Invalid queue properties";
		case CL_INVALID_COMMAND_QUEUE:
			return "Invalid command queue";
		case CL_INVALID_HOST_PTR:
			return "Invalid host pointer";
		case CL_INVALID_MEM_OBJECT:
			return "Invalid memory object";
		case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:
			return "Invalid image format descriptor";
		case CL_INVALID_IMAGE_SIZE:
			return "Invalid image size";
		case CL_INVALID_SAMPLER:
			return "Invalid sampler";
		case CL_INVALID_BINARY:
			return "Invalid Binary";
		case CL_INVALID_BUILD_OPTIONS:
			return "Invalid build options";
		case CL_INVALID_PROGRAM:
			return "Invalid program";
		case CL_INVALID_PROGRAM_EXECUTABLE:
			return "Invalid program executable";
		case CL_INVALID_KERNEL_NAME:
			return "Invalid kernel name";
		case CL_INVALID_KERNEL_DEFINITION:
			return "Invalid kernel definition";
		case CL_INVALID_KERNEL:
			return "Invalid kernel";
		case CL_INVALID_ARG_INDEX:
			return "Invalid arg index";
		case CL_INVALID_ARG_VALUE:
			return "Invalid arg value";
		case CL_INVALID_ARG_SIZE:
			return "Invalid arg size";
		case CL_INVALID_KERNEL_ARGS:
			return "Invalid kernel args";
		case CL_INVALID_WORK_DIMENSION:
			return "Invalid work dimension";
		case CL_INVALID_WORK_GROUP_SIZE:
			return "Invalid work group size";
		case CL_INVALID_WORK_ITEM_SIZE:
			return "Invalid work item size";
		case CL_INVALID_GLOBAL_OFFSET:
			return "Invalid global offset";
		case CL_INVALID_EVENT_WAIT_LIST:
			return "Invalid wait list";
		case CL_INVALID_EVENT:
			return "Invalid event";
		case CL_INVALID_OPERATION:
			return "Invalid operation";
		case CL_INVALID_GL_OBJECT:
			return "Invalid gl object";
		case CL_INVALID_BUFFER_SIZE:
			return "Invalid buffer size";
		case CL_INVALID_MIP_LEVEL:
			return "Invalid mip level";
		case CL_INVALID_GLOBAL_WORK_SIZE:
			return "Invalid global work size";
		case CL_INVALID_PROPERTY:
			return "Invalid property";
		default:
			return "Unknown";
	};
}


/*
 * =====================================================================================
 *  OpenCL Internal Device Info Functions
 * =====================================================================================
 */

const char* _icl_get_device_type_string(cl_device_type type) {
	switch(type){
		case CL_DEVICE_TYPE_CPU: return "CPU"; break;
		case CL_DEVICE_TYPE_GPU: return "GPU"; break;
		case CL_DEVICE_TYPE_ACCELERATOR: return "ACL"; break;
		default: return "Default";
	}
}

char* _icl_get_name(cl_device_id* device) {
	size_t size = 0;
	clGetDeviceInfo(*device, CL_DEVICE_NAME, size, NULL, &size);
	char* retval = (char*) malloc(sizeof(char) * size);
	cl_int err_code =  clGetDeviceInfo(*device, CL_DEVICE_NAME, size, retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device name\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

cl_device_type _icl_get_type(cl_device_id* device) {
	cl_device_type retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_TYPE, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device type\" info: \"%s\"", _icl_error_string(err_code));
	return retval;
}

char* _icl_get_vendor(cl_device_id* device) {
	size_t size = 0;
	clGetDeviceInfo(*device, CL_DEVICE_VENDOR, size, NULL, &size);
	char* retval = (char*) malloc(sizeof(char) * size);
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_VENDOR, size, retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device vendor\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

char* _icl_get_version(cl_device_id* device) {
	size_t size = 0;
	clGetDeviceInfo(*device, CL_DEVICE_VERSION, size, NULL, &size);
	char* retval = (char*) malloc(sizeof(char) * size);
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_VERSION, size, retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device version\" info: \"%s\"", _icl_error_string(err_code));
	return retval;			
}

char* _icl_get_driver_version(cl_device_id* device) {
	size_t size = 0;
	clGetDeviceInfo(*device, CL_DRIVER_VERSION, size, NULL, &size);
	char* retval = (char*) malloc(sizeof(char) * size);
	cl_int err_code = clGetDeviceInfo(*device, CL_DRIVER_VERSION, size, retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device driver version\" info: \"%s\"", _icl_error_string(err_code));
	return retval;		
}

char* _icl_get_profile(cl_device_id* device) {
	size_t size = 0;
	clGetDeviceInfo(*device, CL_DEVICE_PROFILE, size, NULL, &size);
	char* retval = (char*) malloc(sizeof(char) * size);
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_PROFILE, size, retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device profile\" info: \"%s\"", _icl_error_string(err_code));
	return retval;		
}

cl_uint _icl_get_max_compute_units(cl_device_id* device) {
	cl_uint retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_COMPUTE_UNITS, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device max compute units\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	;	
}

cl_uint _icl_get_max_clock_frequency(cl_device_id* device) {
	cl_uint retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_CLOCK_FREQUENCY, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device max clock frequency\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

cl_uint _icl_get_max_work_item_dimensions(cl_device_id* device) {
	cl_uint retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device max work item dimensions\" info: \"%s\"", _icl_error_string(err_code));
	return retval;		
}

size_t* _icl_get_max_work_item_sizes(cl_device_id* device) {
	cl_uint size = _icl_get_max_work_item_dimensions(device);
	size_t *retval = (size_t*) malloc(size * sizeof(size_t));
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_WORK_ITEM_SIZES, sizeof(size_t) * size, retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device max work item sizes\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

size_t _icl_get_max_work_group_size(cl_device_id* device) {
	size_t retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device max work group size\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	;
}

cl_bool _icl_has_image_support(cl_device_id* device) {
	cl_bool retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_IMAGE_SUPPORT, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device image support\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

cl_device_fp_config _icl_get_single_fp_config(cl_device_id* device) {
	cl_device_fp_config retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_SINGLE_FP_CONFIG, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device single floating point configuration\" info: \"%s\"", _icl_error_string(err_code));
	return retval;
}

cl_bool _icl_is_endian_little(cl_device_id* device)  {
	cl_bool retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_ENDIAN_LITTLE, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device endian little\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

char* _icl_get_extensions(cl_device_id* device) {
	size_t size = 0;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_EXTENSIONS, size, NULL, &size);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device extensions\" info: \"%s\"", _icl_error_string(err_code));
	char* retval = (char*) malloc(sizeof(char) * size);
	err_code = clGetDeviceInfo(*device, CL_DEVICE_EXTENSIONS, size, retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device extensions\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

cl_ulong _icl_get_global_mem_size(cl_device_id* device) {
	cl_ulong retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_GLOBAL_MEM_SIZE, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device global memory size\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

cl_ulong _icl_get_max_mem_alloc_size(cl_device_id* device) {
	cl_ulong retval;
	cl_int err_code =  clGetDeviceInfo(*device, CL_DEVICE_MAX_MEM_ALLOC_SIZE, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device max memory alloc size\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

cl_device_mem_cache_type _icl_get_global_mem_cache_type(cl_device_id* device) {
	cl_device_mem_cache_type retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_GLOBAL_MEM_CACHE_TYPE, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device global mem cache type\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

cl_uint _icl_get_global_mem_cacheline_size(cl_device_id* device) {
	cl_uint retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device global mem cache line size\" info: \"%s\"", _icl_error_string(err_code));
	return retval;		
}

cl_ulong _icl_get_global_mem_cache_size(cl_device_id* device) {
	cl_ulong retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_GLOBAL_MEM_CACHE_SIZE, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device global mem cache size\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

cl_ulong _icl_get_max_constant_buffer_size(cl_device_id* device) {
	cl_ulong retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device max constant buffer size\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

cl_device_local_mem_type _icl_get_local_mem_type(cl_device_id* device) {
	cl_device_local_mem_type retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_LOCAL_MEM_TYPE, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device local memory type\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}

cl_ulong _icl_get_local_mem_size(cl_device_id* device) {
	cl_ulong retval;
	cl_int err_code = clGetDeviceInfo(*device, CL_DEVICE_LOCAL_MEM_SIZE, sizeof(retval), &retval, NULL);
	ICL_ASSERT(err_code  == CL_SUCCESS, "Error getting \"device local memory size\" info: \"%s\"", _icl_error_string(err_code));
	return retval;	
}
