set(Boost_VERSION 1.59.0 CACHE STRING "Boost Version")

if(MSVC)
	set(Boost_USE_STATIC_LIBS OFF)
	set(Boost_USE_DEBUG_RUNTIME ON)
	set(Boost_USE_MULTITHREADED ON)
endif()
