
find_package( Boost )

include_directories(BOOST_INCLUDE_DIRS)

if(MSVC) 
	set(CMAKE_C_FLAGS ${CMAKE_C_FLAGS} /D "_CRT_SECURE_NO_WARNINGS")
endif()

