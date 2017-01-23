option(BUILD_SHARED_LIBS "Link libraries dynamically" ON)
option(BUILD_TESTS "Enable testing" ON)
option(BUILD_DOCS "Enable documentation" OFF)
option(USE_ASSERT "Enable assertions" ON)
option(USE_VALGRIND "Allow Valgrind for unit tests" ON)

set_property(GLOBAL PROPERTY USE_FOLDERS ON)

if(NOT DEFINED CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif()

if(BUILD_TESTS)
	enable_testing()
endif()

if(CMAKE_CXX_COMPILER_ID STREQUAL "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
	# base C flags
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wall -Wextra -std=c99")
	set(CMAKE_C_FLAGS_DEBUG "${CMAKE_C_FLAGS_DEBUG} -O0 -g3 -ggdb")
	set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O2")

	# base C++ flags
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -std=c++14")
	set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -O0 -g3 -ggdb")
	set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O2")

	# allow arbitrary library linking order (in case `-as-needed` is default)
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Wl,--no-as-needed")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wl,--no-as-needed")

	# remove NDEBUG from release flags
	string(REPLACE "-DNDEBUG" "" CMAKE_C_FLAGS_RELEASE ${CMAKE_C_FLAGS_RELEASE})
	string(REPLACE "-DNDEBUG" "" CMAKE_CXX_FLAGS_RELEASE ${CMAKE_CXX_FLAGS_RELEASE})

	# Yo Dawg, I heard you like templates!
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -ftemplate-depth-900")

	# \[T]/ Praise the sun. There be dragons ahead.
	if(NOT USE_ASSERT)
		set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DNDEBUG")
		set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DNDEBUG")
	endif()
elseif(MSVC)
	include(msvc_settings)
	set(USE_VALGRIND OFF)
else()
	message(FATAL_ERROR "Unhandled Compiler: ${CMAKE_CXX_COMPILER_ID}")
endif()
