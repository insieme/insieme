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
	if(MSVC_SHARED_RUNTIME)
		set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MD")
		set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /MDd")
		set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} /MDd")
		set(CMAKE_CXX_FLAGS_RELASE "${CMAKE_CXX_FLAGS_RELASE} /MD")
		set(CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} /MD")
	else()
		set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /MT")
		set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /MTd")
		set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "${CMAKE_CXX_FLAGS_RELWITHDEBINFO} /MTd")
		set(CMAKE_CXX_FLAGS_RELASE "${CMAKE_CXX_FLAGS_RELASE} /MT")
		set(CMAKE_CXX_FLAGS_MINSIZEREL "${CMAKE_CXX_FLAGS_MINSIZEREL} /MT")
	endif()

	# windows library naming policies
	set(CMAKE_FIND_LIBRARY_PREFIXES "")
	set(CMAKE_FIND_LIBRARY_SUFFIXES ".lib") # if you're thinking about adding ".dll" here, read up on "import libraries" in Windows

	# TODO create own cmake file for boost stuff
	# Boost linking options
	set(Boost_USE_STATIC_LIBS OFF) # default: OFF
	set(Boost_USE_DEBUG_RUNTIME ON) # default: ON
	set(Boost_USE_MULTITHREADED ON) # default: ON
	if(MSVC_SHARED_RUNTIME)
		set(Boost_USE_STATIC_RUNTIME OFF) # default: platform-dependent
	else()
		set(Boost_USE_STATIC_RUNTIME ON)
	endif()
endif()
