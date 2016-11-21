set(Boost_VERSION 1.59.0 CACHE STRING "Boost Version")

if(MSVC)
	# Boost linking options
	set(Boost_USE_STATIC_LIBS OFF)     # default: OFF
	set(Boost_USE_DEBUG_RUNTIME ON)    # default: ON
	set(Boost_USE_MULTITHREADED ON)    # default: ON
	if(MSVC_SHARED_RUNTIME)
		set(Boost_USE_STATIC_RUNTIME OFF) # default: platform-dependent
	else()
		set(Boost_USE_STATIC_RUNTIME ON)
	endif()
endif()
