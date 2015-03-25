project(insieme_code C CXX)

cmake_minimum_required(VERSION 2.8)

# enable ctest support for testing - generate a CTest file in the ${build_dir}/code
enable_testing()

# -- if no option is set we build EVERYTHING:
#		C/C++, IWIR Frontends
#		all the core stuff
#		the runtime

# -- option to build only the Runtime part
option(BUILD_RUNTIME_ONLY "Build only the Runtime of Insieme" OFF)

# -- option to build IWIR FE + Insieme Core
option(BUILD_IWIR "Builds the IWIR Frontend + Insieme Core" OFF)

# -- option to build Insieme (C/C++) FE + core + transform/analysis + BE ... 
option(BUILD_CLANG "Builds the Insieme Frontend + Insieme CORE + Insieme Backend" OFF)

if(BUILD_RUNTIME_ONLY)
	# Build only the RUNTIME part
	# to avoid dependency on other libraries NOT used by runtime (boost for example)
	add_subdirectory(common)
	add_subdirectory(runtime)

	#Configure the integration test
	set(CONFIG_INTEGRATION_TESTS FALSE)

elseif(BUILD_IWIR)

	# Build compiler IWIR FE + core + backend 
	include(environment.cmake)

	#add_subdirectory(transform)	#CURRENTLY NOT USED FOR IWIR
	#add_subdirectory(analysis)		#CURRENTLY NOT USED FOR IWIR
	add_subdirectory(utils)
	add_subdirectory(annotations)
	add_subdirectory(common) # needed by annotations...
	add_subdirectory(core)
	add_subdirectory(xml)
	add_subdirectory(iwir)
	
	#Configure the integration test
	set(CONFIG_INTEGRATION_TESTS FALSE)

elseif(BUILD_CLANG)
	# Build compiler c/c++ FE + core + backend + runtime 
	include(environment.cmake)

	add_subdirectory(common)
	add_subdirectory(utils)
	add_subdirectory(core)
	add_subdirectory(transform)
	add_subdirectory(analysis)
	add_subdirectory(annotations)
	add_subdirectory(frontend)
	add_subdirectory(backend)
	add_subdirectory(driver)
	add_subdirectory(machine_learning)
	add_subdirectory(runtime)

	#Configure the integration test
	set(CONFIG_INTEGRATION_TESTS TRUE)

else()
	# Build compiler (iwir and c/c++ FE) + core + backend  + runtime -- everything
	get_filename_component( curdir ${CMAKE_CURRENT_LIST_FILE} PATH )
	file(GLOB children RELATIVE ${curdir} ${curdir}/*)

	include(environment.cmake)
	
	# include _all_ modules
	foreach( child ${children} )
		if ( EXISTS ${curdir}/${child}/CMakeLists.txt )
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
	insieme_find_package(Boost)

	#FIXME: we have integration-tests relying on gmp/mpfr to be configured in the integration test
	#configuration file... --> there we need to have GMP_HOME/MPFR_HOME to be set...
	# lookup GMP library
	insieme_find_package(GMP)

	# lookup MPFR library
	insieme_find_package(MPFR)

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

