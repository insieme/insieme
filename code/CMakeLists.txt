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

cmake_minimum_required(VERSION 3.2)

# enable ctest support for testing - generate a CTest file in the ${build_dir}/code
enable_testing()

# -- if no option is set we build EVERYTHING:
#		C/C++
#		all the core stuff
#		the runtime

# -- option to build only the Runtime part
option(BUILD_RUNTIME_ONLY "Build only the Runtime of Insieme" OFF)

# -- option to build Insieme (C/C++) FE + core + transform/analysis + BE ... 
option(BUILD_CLANG "Builds the Insieme Frontend + Insieme CORE + Insieme Backend" OFF)

# load environment settings common to both compiler and runtime
include(environment_common.cmake)

if(BUILD_RUNTIME_ONLY)
	# Build only the RUNTIME part
	# to avoid dependency on other libraries NOT used by runtime (boost for example)
	add_subdirectory(common)
	add_subdirectory(runtime)

	#Configure the integration test
	set(CONFIG_INTEGRATION_TESTS FALSE)

elseif(BUILD_CLANG)
	# Build compiler c/c++ FE + core + backend + runtime 
	
	# CAREFULL we want to setup runtime before environment.cmake to avoid problems with Werror/Wall/
	# pedantic when compiling runtime unittests as runtime has C features which are not supported by
	# C++ -> therefore Werror/Wall/pedantic shows them as error
	add_subdirectory(runtime)

	include(environment.cmake)

	add_subdirectory(analysis)
	add_subdirectory(common)
	add_subdirectory(utils)
	add_subdirectory(core)
	add_subdirectory(transform)
	add_subdirectory(annotations)
	add_subdirectory(frontend)
	add_subdirectory(backend)
	add_subdirectory(driver)

	#Configure the integration test
	set(CONFIG_INTEGRATION_TESTS TRUE)

else()
	# Build compiler (c/c++ FE) + core + backend  + runtime -- everything
	get_filename_component( curdir ${CMAKE_CURRENT_LIST_FILE} PATH )
	file(GLOB children RELATIVE ${curdir} ${curdir}/*)

	# CAREFULL we want to setup runtime before environment.cmake to avoid problems with Werror/Wall/
	# pedantic when compiling runtime unittests as runtime has C features which are not supported by
	# C++ -> therefore Werror/Wall/pedantic shows them as error
	add_subdirectory(runtime)


	include(environment.cmake)
	
	# include _all_ modules (but runtime is handled in before -> because of certain Wall problems)
	foreach( child ${children} )
		if ( EXISTS ${curdir}/${child}/CMakeLists.txt 
			AND NOT (${child} STREQUAL runtime)
			)
			add_subdirectory( ${child} )
		endif()
	endforeach()

	#Configure the integration test
	set(CONFIG_INTEGRATION_TESTS TRUE)
endif()

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
