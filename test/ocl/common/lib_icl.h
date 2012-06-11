#ifndef _LIB_ICL_
#define _LIB_ICL_

#include <CL/cl.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#ifndef _WIN32
	#include <time.h>
	#include <stdbool.h>
#else
	#include <windows.h>
#endif

#define ICL_ASSERT(__condition, __message, ...) \
if(!(__condition)) { \
	fprintf(stderr, "OpenCL Assertion failure in %s#%d:\n", __FILE__, __LINE__); \
	printf(__message, ##__VA_ARGS__); \
	printf("\n"); \
	exit(-1); \
}

#define ICL_INFO(__message, ...) { \
	printf(__message, ##__VA_ARGS__); \
}

#define ICL_CPU	CL_DEVICE_TYPE_CPU
#define ICL_GPU	CL_DEVICE_TYPE_GPU
#define ICL_ACL	CL_DEVICE_TYPE_ACCELERATOR
#define ICL_ALL	CL_DEVICE_TYPE_ALL

typedef struct _icl_device {
	// internal info
	cl_device_id device;
	cl_context context;
	cl_command_queue queue;

	// buffers info
	cl_ulong mem_size; // memory of the device
	cl_ulong mem_available; // memory still available, reduced after each buffer allocation
	cl_ulong max_buffer_size; // max size of a buffer
	
	// device info
	char *name;
	cl_device_type type;
	char *vendor;
	char *version;
	char *driver_version;
	char *profile;

	cl_uint max_compute_units;
	cl_uint max_clock_frequency;
	cl_uint max_work_item_dimensions;
	size_t* max_work_item_sizes;
	size_t max_work_group_size;


	cl_bool image_support;
	cl_device_fp_config single_fp_config;
	cl_bool endian_little;
	char *extensions;
	
	cl_device_mem_cache_type mem_cache_type;
	cl_ulong global_mem_cacheline_size;
	cl_ulong global_mem_cache_size;

	cl_ulong max_constant_buffer_size;

	cl_device_local_mem_type local_mem_type;
	cl_ulong local_mem_size;
} icl_device;

typedef struct _icl_buffer {
	cl_mem mem;
	size_t size;
	icl_device* dev; // derive the device directly from the buffer
} icl_buffer;

typedef enum {ICL_SOURCE, ICL_BINARY, ICL_STRING, ICL_NO_CACHE} icl_create_kernel_flag;

typedef struct _icl_kernel {
	cl_kernel kernel;
	icl_device* dev;
} icl_kernel;

typedef struct _icl_event {
	cl_event* event;
	cl_uint num_event;
} icl_event;


icl_device* devices;
cl_uint num_devices;
cl_uint* sorted_dev_id;

void icl_init_devices(cl_device_type device_type);
cl_uint icl_get_num_devices();
icl_device* icl_get_device(cl_uint id);
void icl_release_devices();

icl_buffer* icl_create_buffer(icl_device* dev, cl_mem_flags flags, size_t size);
void icl_read_buffer(const icl_buffer* buf, cl_bool blocking, size_t size, void* source_ptr, icl_event* wait_event, icl_event* event);
void icl_write_buffer(icl_buffer* buf, cl_bool blocking, size_t size, const void* source_ptr, icl_event* event_wait, icl_event* event);
void icl_copy_buffer(icl_buffer* src_buf, icl_buffer* dest_buf, size_t size, icl_event* event_wait, icl_event* event);
void* icl_map_buffer(icl_buffer* buf, cl_bool blocking, cl_map_flags map_flags, size_t size, icl_event* wait_event, icl_event* event);
void icl_unmap_buffer(icl_buffer* buf, void* mapped_ptr, icl_event* wait_event, icl_event* event);
void icl_release_buffer(icl_buffer* buf);
void icl_release_buffers(cl_uint num, ...);

icl_kernel* icl_create_kernel(icl_device* dev, const char* file_name, const char* kernel_name, const char* build_options, icl_create_kernel_flag flag);
void icl_run_kernel(const icl_kernel* kernel, cl_uint work_dim, const size_t* global_work_size, const size_t* local_work_size, icl_event* wait_event, icl_event* event, cl_uint num_args, ...);
void icl_release_kernel(icl_kernel* kernel);

void icl_print_device_short_info(icl_device* dev);
void icl_print_device_infos(icl_device* dev);

typedef enum {ICL_SEC, ICL_MILLI, ICL_NANO} icl_time_flag;

#ifdef _WIN32
typedef __int64 time_int;
#else
typedef long long time_int;
#endif

typedef struct _icl_timer {
	time_int start;
	time_int clocks;
#ifdef _WIN32
	time_int freq;
#endif	
	double current_time;
	icl_time_flag time_flag;
} icl_timer;

icl_timer* icl_init_timer(icl_time_flag time_flag);
void icl_start_timer(icl_timer* timer);
void icl_restart_timer(icl_timer* timer);
double icl_stop_timer(icl_timer* timer);
void icl_release_timer(icl_timer* timer);

typedef enum {ICL_ENQUEUED, ICL_SUBMITTED, ICL_STARTED, ICL_FINISHED} icl_event_flag;

icl_event* icl_create_event();
icl_event* icl_create_event_list(cl_uint num_event, ...);
void icl_release_event(icl_event* event);
void icl_release_events(cl_uint num, ...);
double icl_profile_event(icl_event* event, icl_event_flag event_start, icl_event_flag event_end, icl_time_flag time_flag);
double icl_profile_events(icl_event* event_one, icl_event_flag event_one_command, icl_event* event_two, icl_event_flag event_two_command, icl_time_flag time_flag);

#endif
