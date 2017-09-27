set(GMP_VERSION 6.0.0 CACHE STRING "GMP Version")

if(NOT MSVC)
	find_package(GMP ${GMP_VERSION} REQUIRED)
endif()
