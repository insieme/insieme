if(NOT OPENCL_ROOT AND DEFINED $ENV{OPENCL_ROOT})
	set(OPENCL_ROOT "$ENV{OPENCL_ROOT}" CACHE PATH "OpenCL base directory location")
endif()

if(OPENCL_ROOT)
	list(APPEND CMAKE_PREFIX_PATH ${OPENCL_ROOT})
endif()

find_package(OpenCL)
if(OpenCL_FOUND)
	get_filename_component(OPENCL_ROOT ${OpenCL_INCLUDE_DIR} DIRECTORY)
	add_definitions(-DOPENCL_ROOT="${OpenCL_ROOT}")
endif()
