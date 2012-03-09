#
# Builds the insieme default driver
#
project(insieme_driver CXX)

include(../environment.cmake)

# enable XML if requested
if (USE_XML) 	
	list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/xml/cmake")
	find_package(Xerces)
	if ( NOT ${Xerces_FOUND} )
		message("Xerces library not found, XML support will be disabled!")
		set(USE_XML OFF)
	else ()
		add_definitions(-DUSE_XML)
	endif()
endif (USE_XML)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

configure_file(
		${CMAKE_CURRENT_SOURCE_DIR}/cmake/driver_config.h.in 
		${CMAKE_CURRENT_SOURCE_DIR}/include/insieme/driver/driver_config.h 
)

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_driver_srcs 	src/*.cpp )
file(GLOB_RECURSE insieme_driver_incs 	include/*.h )
file(GLOB_RECURSE insieme_driver_defs 	include/*.def )

# --------------------------------------------------------------------------------------
# We repeat the search for Xerces library here because adding this line to 
# the environment.cmake file will produce the same error message every time 
# the file is included (therefore for every module)

option (USE_XML "Build XML support in Insieme" ON) 
list(APPEND CMAKE_MODULE_PATH "${insieme_code_dir}/xml/cmake")

if (USE_XML) 
	find_package(Xerces)
	if ( NOT ${Xerces_FOUND} )
		set(USE_XML OFF)
	endif()
endif (USE_XML)
# --------------------------------------------------------------------------------------

# Build the driver library
add_library(insieme_driver ${LINKING_TYPE} ${insieme_driver_srcs} ${insieme_driver_incs} ${insieme_driver_defs})


# list all include paths
include_directories( ${insieme_core_include_dir} )
include_directories( ${insieme_utils_include_dir} )
include_directories( ${insieme_frontend_include_dir} )
include_directories( ${insieme_backend_include_dir} )
include_directories( ${insieme_analysis_include_dir} )
include_directories( ${insieme_transform_include_dir} )
include_directories( ${insieme_simple_backend_include_dir} )
include_directories( ${insieme_annotations_include_dir} )
include_directories( ${insieme_driver_include_dir} )
include_directories( ${insieme_machine_learning_include_dir} )
if (USE_XML)
	include_directories( ${insieme_xml_include_dir} )
endif (USE_XML)

set( insieme_libraries 
	insieme_utils 
	insieme_core 
	insieme_frontend 
	insieme_backend 
	insieme_simple_backend
	insieme_annotations 
	insieme_analysis
	insieme_transform
	insieme_driver
	insieme_machine_learning
	${shark_LIB}
	${kompex_LIB}
)

if (USE_XML)
	set( insieme_libraries ${insieme_libraries} insieme_xml )
endif (USE_XML)

# build executables
add_definitions( -fopenmp )
file(GLOB_RECURSE executables src/*.cxx)
foreach ( exec_file ${executables})

	get_filename_component( exec_name ${exec_file} NAME_WE )

	# Build dummy executable
	add_executable( ${exec_name} ${exec_file} ${insieme_driver_incs} ${insieme_driver_defs})
	add_dependencies( ${exec_name} ${insieme_libraries} )

	target_link_libraries( ${exec_name} ${insieme_libraries} ${Boost_PROGRAM_OPTIONS_LIBRARY} gomp dl)

endforeach(exec_file)


# add boost_program_options library

set ( ut_prefix  ut_driver_ )
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} ${insieme_libraries})
	
	if (USE_XML) 
		target_link_libraries(${case_name} insieme_xml)
	endif (USE_XML)

	target_link_libraries(${case_name} ${Boost_PROGRAM_OPTIONS_LIBRARY})
	target_link_libraries(${case_name} dl)
	target_link_libraries(${case_name} ${pthread_LIB})

	add_unit_test(${case_name} off)
endforeach(case_file)


# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	INSTALL(TARGETS main DESTINATION bin)
endif(DO_INSTALL)
