if(MSVC)
	# enable minimal rebuild
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Gm")
	# enable debug information(required for /Gm)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Zi")
	# disable optimizations(compilation speed)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /Od")
	# disable some warnings
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /D_CRT_SECURE_NO_WARNINGS")
	# Boost: No auto-lib
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /DBOOST_ALL_NO_LIB")
	# disable warning "assignment operator could not be generated"
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4512\"")
	# disable warning "nonstandard extension: enum '[EnumName::ENUM]' used in qualified name"
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4482\"")
	# disable warning "unkown pragma"
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4068\"")
	# disable warning "declaration hides class member"
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4458\"")
	# disable warning "forcing value to bool 'true' or 'false'(performance warning)"
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4800\"")
	# disable warning "symbol will be dynamically initialized(implementation limitation)" because MSVC 2015.1 is still buggy on that
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /wd\"4592\"")

	# properly configure how to link the MSVC runtime library, static <-> shared and debug <-> release
	if(BUILD_SHARED_LIBS)
		message(STATUS "MSVC: using dynamically-linked runtime")
		set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /MDd")
		set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /MD")
	else()
		message(STATUS "MSVC: using statically-linked runtime")
		set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /MTd")
		set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /MT")
	endif()

	# windows library naming policies
	set(CMAKE_FIND_LIBRARY_PREFIXES "")
	set(CMAKE_FIND_LIBRARY_SUFFIXES ".lib") # if you're thinking about adding ".dll" here, read up on "import libraries" in Windows

	# library output
	if(${CMAKE_BUILD_TYPE} STREQUAL Release)
		set(LIBRARY_OUTPUT_DIRECTORY Release)
	else()
		set(LIBRARY_OUTPUT_DIRECTORY Debug)
	endif()

	# solution configuration
	set(CMAKE_CONFIGURATION_TYPES ${CMAKE_BUILD_TYPE} CACHE STRING "Visual Studio Solution Configuration" FORCE)

	# no ZERO_CHECK target
	set(CMAKE_SUPPRESS_REGENERATION true)
endif()
