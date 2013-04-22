project(insieme_code CXX)

# States that CMake required version must be >= 2.6
cmake_minimum_required(VERSION 2.6)

# enable ctest support for testing
enable_testing()

# include modules
get_filename_component( curdir ${CMAKE_CURRENT_LIST_FILE} PATH )
file(GLOB children RELATIVE ${curdir} ${curdir}/*)
foreach( child ${children} )
	if ( EXISTS ${curdir}/${child}/CMakeLists.txt )
		add_subdirectory( ${child} )
	endif()
endforeach()


#-- Add an Option to toggle the generation of the API documentation
option(BUILD_DOXYGEN "Use Doxygen to create the HTML based API documentation" OFF)
if(BUILD_DOXYGEN)
	FIND_PACKAGE(Doxygen)
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

