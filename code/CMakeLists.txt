#
#
#
#
# READ build_readme before using the CMake or changing the CMake script
#
#
#
#
#
project(insieme_code CXX)

cmake_minimum_required(VERSION 3.5)

# enable ctest support for testing - generate a CTest file in the ${build_dir}/code
enable_testing()

# -- if no option is set we build EVERYTHING:
#		C/C++
#		all the core stuff
#		the runtime

option(BUILD_RUNTIME "Build the Runtime of Insieme" ON)

option(BUILD_COMPILER "Builds the Insieme Compiler" ON)

# load environment settings common to both compiler and runtime
include(environment_common.cmake)

add_subdirectory(common)

if(BUILD_RUNTIME)
	add_subdirectory(runtime)
endif()

if(BUILD_COMPILER)
	include(environment.cmake)

	add_subdirectory(analysis)
	add_subdirectory(utils)
	add_subdirectory(core)
	add_subdirectory(transform)
	add_subdirectory(annotations)
	add_subdirectory(frontend)
	add_subdirectory(backend)
	add_subdirectory(driver)
endif()

set(CONFIG_INTEGRATION_TESTS TRUE)

# configure the integration tests
if(CONFIG_INTEGRATION_TESTS)
	# create global integration test configuration file

	#FIXME: cpp/boost integration tests rely on boost to be set?
	insieme_find_package(NAME Boost)

	configure_file(integration_test_config.in ${CMAKE_BINARY_DIR}/integration_test_config  @ONLY IMMEDIATE)
endif()

#-- Add an Option to toggle the generation of the API documentation
option(BUILD_DOXYGEN "Use Doxygen to create the HTML based API documentation" OFF)
if(BUILD_DOXYGEN)
	find_package(Doxygen)
	if (NOT DOXYGEN_FOUND)
		message(FATAL_ERROR "Doxygen is needed to build the documentation. Please install it correctly")
	endif()
	#-- Configure the Template Doxyfile for our specific project
	configure_file(Doxyfile.in ${PROJECT_BINARY_DIR}/Doxyfile  @ONLY IMMEDIATE)
	
	#-- Add a custom target to run Doxygen when ever the project is built
	add_custom_target (doxygen ALL 
  		COMMAND ${DOXYGEN_EXECUTABLE} ${PROJECT_BINARY_DIR}/Doxyfile
  		SOURCES ${PROJECT_BINARY_DIR}/Doxyfile)

	# IF you do NOT want the documentation to be generated EVERY time you build the project
	# then leave out the 'ALL' keyword from the above command.
endif()

# Print a warning if the master branch is selected
if (${insieme_branch} MATCHES "master")
message("



	================================ ${PrintColorBoldRed}WARNING${PrintColorReset} ================================
	|                                                                       |
	|  The master branch of Insieme is currently undergoing severe changes  |
	|  and refactoring. Until these changes are complete, several features  |
	|  will be missing and all interfaces are in flux. If you just want to  |
	|  try Insieme as a user or want to do research building on top of its  |
	|  infrastructure please use the ${PrintColorBold}inspire_1.3${PrintColorReset} branch.                    |
	|                                                                       |
	=========================================================================



")
endif ()
