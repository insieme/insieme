project(insieme_code CXX)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# enable ctest support for testing
enable_testing()

# include modules
add_subdirectory( utils )
add_subdirectory( core )
add_subdirectory( annotations )
add_subdirectory( frontend )
add_subdirectory( backend )
add_subdirectory( analysis )
add_subdirectory( transform )
add_subdirectory( xml )
add_subdirectory( driver )
add_subdirectory( runtime )
add_subdirectory( machine_learning )
add_subdirectory( playground )

#-- Add an Option to toggle the generation of the API documentation
option(BUILD_DOCUMENTATION "Use Doxygen to create the HTML based API documentation" OFF)
if(BUILD_DOCUMENTATION)
	FIND_PACKAGE(Doxygen)
	if (NOT DOXYGEN_FOUND)
		message(FATAL_ERROR "Doxygen is needed to build the documentation. Please install it correctly")
	endif()
	#-- Configure the Template Doxyfile for our specific project
	configure_file(Doxyfile.in ${PROJECT_BINARY_DIR}/Doxyfile  @ONLY IMMEDIATE)
	
	#-- Add a custom target to run Doxygen when ever the project is built
	add_custom_target (Docs ALL 
  		COMMAND ${DOXYGEN_EXECUTABLE} ${PROJECT_BINARY_DIR}/Doxyfile
  		SOURCES ${PROJECT_BINARY_DIR}/Doxyfile)

	# IF you do NOT want the documentation to be generated EVERY time you build the project
	# then leave out the 'ALL' keyword from the above command.
endif()

#-- Install the examples for using insieme
install (FILES ../extra/insieme_lib/insieme_test.cpp DESTINATION examples/)
install (FILES ../extra/insieme_lib/infile.c DESTINATION examples/)
install (FILES ../extra/insieme_lib/Makefile DESTINATION examples/)
