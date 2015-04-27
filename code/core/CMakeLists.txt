#
# Builds the insieme core library
#
project(insieme_core CXX)

#include(../environment.cmake)

# States that CMake required version must be >= 2.8
cmake_minimum_required(VERSION 2.8)

# find modules
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/../cmake" ${CMAKE_MODULE_PATH})

# Create a variable called insieme_compiler_core_SOURCES containing all .cpp files:
file(GLOB_RECURSE insieme_core_srcs 	src/*.cpp src/*.def src/*.inc )
file(GLOB_RECURSE insieme_core_incs 	include/*.h include/*.def include/*.inc )
file(GLOB_RECURSE insieme_core_yy 	    src/*.y src/*.yy )
file(GLOB_RECURSE insieme_core_ll 	    src/*.l src/*.ll )

# list all include paths
#include_directories( ${insieme_core_include_dir} )
#include_directories( ${insieme_utils_include_dir} )

# automatically run the script for generating IRBuilder interface
execute_process(WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} COMMAND ruby builderbuilder.rb  RESULT_VARIABLE builderbuilderSuccess)

if (NOT ${builderbuilderSuccess} EQUAL 0)
	message(FATAL_ERROR "Ruby builder builder script failed to execute!")
endif()

# =============================================  bison builds the parser  =====================================

if(NOT MSVC) 
	set(BISON_ROOT ${third_part_libs_home}/bison-latest)
	FIND_PACKAGE(BISON 3.0 REQUIRED)

	#FIND_PROGRAM(BISON_EXECUTABLE bison HINTS ${third_part_libs_home}/bison-latest/bin)
	IF(BISON_EXECUTABLE-NOTFOUND)
	    message(FATAL_ERROR "bisson not found")

	ELSE()
	    message(STATUS "bison found @ ${BISON_EXECUTABLE}")

	    set(BISON_INPUT ${insieme_core_yy})
	    set(BISON_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bison-flex/)
	    file(MAKE_DIRECTORY ${BISON_OUTPUT_PATH})
	    set(BISON_O_FILE inspire_parser.cpp)
	    set(BISON_OUTPUT ${BISON_OUTPUT_PATH}/${BISON_O_FILE})

	    ADD_CUSTOM_COMMAND(
	      SOURCE ${BISON_INPUT}
	      COMMAND ${BISON_EXECUTABLE}
	      ARGS -d
		    --report=all
		   -o ${BISON_O_FILE}
		   --graph 
		  ${BISON_INPUT} 
	      COMMENT "Generating inspire parser"
	      DEPENDS ${BISON_INPUT}
	      WORKING_DIRECTORY ${BISON_OUTPUT_PATH}
	      OUTPUT ${BISON_OUTPUT}
	)
	ENDIF()

# =============================================  flex builds the scanner  =====================================

	set(FLEX_ROOT ${third_part_libs_home}/flex-latest)
	FIND_PACKAGE(FLEX 2.5 REQUIRED)

	IF(FLEX_EXECUTABLE-NOTFOUND)
	    message(FATAL_ERROR "flex not found")
	ELSE()
	    message(STATUS "flex found @ ${FLEX_EXECUTABLE}")

	    set(FLEX_INPUT ${insieme_core_ll})
	    set(FLEX_OUTPUT_PATH ${CMAKE_BINARY_DIR}/bison-flex/)
	    set(FLEX_OUTPUT ${FLEX_OUTPUT_PATH}/inspire_scanner.cpp)

	    ADD_CUSTOM_COMMAND(
	      SOURCE ${FLEX_INPUT}
	      COMMAND ${FLEX_EXECUTABLE}
	      ARGS -o ${FLEX_OUTPUT}
		  ${FLEX_INPUT} 
	      COMMENT "Generating inspire scanner"
	      DEPENDS ${FLEX_INPUT}
	      WORKING_DIRECTORY ${FLEX_OUTPUT_PATH}
	      OUTPUT ${FLEX_OUTPUT}
	)
	ENDIF()

	# make the build aware of generated headers
	include_directories( ${FLEX_OUTPUT_PATH} )
	include_directories( ${BISON_OUTPUT_PATH} )

	# flex needs a header in installation
	include_directories( ${FLEX_INCLUDE_DIRS} )
endif()

# =============================================  build insieme core .so  =====================================

# Build the core library
add_library(insieme_core ${LINKING_TYPE} ${insieme_core_srcs} ${insieme_core_incs} ${insieme_core_defs} ${FLEX_OUTPUT} ${BISON_OUTPUT})
target_include_directories(insieme_core PUBLIC ${insieme_core_include_dir} )
#target_include_directories(insieme_core PUBLIC ${insieme_utils_include_dir} )
target_link_libraries(insieme_core gomp)

# add dependencies
target_link_libraries(insieme_core insieme_utils)

# lookup CUDD library
target_link_libraries(insieme_core ${CUDD_LIBRARIES}) 
target_include_directories(insieme_core PUBLIC ${CUDD_INCLUDE_DIRS} )

# lookup Boost library
insieme_find_package(NAME Boost COMPONENTS regex) 
target_include_directories(insieme_core SYSTEM PRIVATE ${Boost_INCLUDE_DIRS})
target_link_libraries(insieme_core ${Boost_REGEX_LIBRARY})

# =============================================  TESTING  =====================================
set( ut_prefix  ut_core)
file(GLOB_RECURSE test_cases test/*.cc)
foreach ( case_file ${test_cases})
	get_filename_component( case_name ${case_file} NAME_WE )
	
	set ( case_name ${ut_prefix}_${case_name} )
	add_executable(${case_name} ${case_file})
	target_link_libraries(${case_name} insieme_core)
	target_link_libraries(${case_name} insieme_utils)
	target_link_libraries(${case_name} ${CUDD_LIBRARIES})
	target_include_directories(${case_name} PUBLIC test/include/)
	
	# exclude lua tests from valgrind since it fails for versions newer than 3.7.0
	set(VFLAG ${TRUE})
	if(case_file MATCHES ".*lua.*")
		set(VFLAG FALSE)
	endif(case_file MATCHES ".*lua.*")
	add_unit_test(${case_name} ${ut_prefix} ${VFLAG})
endforeach(case_file)

# --- INSTALL --------------------------------------------------------------------
if(DO_INSTALL)
	install (DIRECTORY ${insieme_core_include_dir}/insieme DESTINATION include)
	# install (FILES include/insieme/core/transform/manipulation.h DESTINATION include/core/)

	install(TARGETS insieme_core LIBRARY DESTINATION lib)
endif(DO_INSTALL)
