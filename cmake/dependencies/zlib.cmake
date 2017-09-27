set(ZLIB_VERSION 1.2.8 CACHE STRING "ZLib Version")

if(NOT MSVC)
	find_package(ZLIB ${ZLIB_VERSION} REQUIRED)
endif()
