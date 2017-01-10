project(insieme_docs NONE)

cmake_minimum_required(VERSION 3.5)

include(UseLATEX.cmake)

# add all sub-directories containing a CMakeLists.txt file
get_filename_component( curdir ${CMAKE_CURRENT_LIST_FILE} PATH )
file(GLOB children RELATIVE ${curdir} ${curdir}/*)
foreach( child ${children} )
	if ( EXISTS ${curdir}/${child}/CMakeLists.txt )
		add_subdirectory( ${child} )
	endif()
endforeach()

