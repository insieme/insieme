option(BUILD_SHARED_LIBS "Link libraries dynamically" ON)
option(BUILD_TESTS "Enable testing" ON)
option(BUILD_DOCS "Enable documentation" OFF)

if(NOT DEFINED CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif()

if(BUILD_TESTS)
	enable_testing()
endif()

set(CMAKE_C_STANDARD 99)
set(CMAKE_CXX_STANDARD 14)

if(("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang") OR ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU"))
	# base C flags
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra")
	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -g3 -ggdb")
	set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O2")

	# base C++ flags
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0 -g3 -ggdb")
	set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O2")

	# allow arbitrary library linking order (in case `-as-needed` is default)
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wl,--no-as-needed")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wl,--no-as-needed")

	# Yo Dawg, I heard you like templates!
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ftemplate-depth-900")
elseif(MSVC)
	include(msvc_settings)
else()
	message(FATAL_ERROR "Unhandled Compiler: ${CMAKE_CXX_COMPILER_ID}")
endif()
