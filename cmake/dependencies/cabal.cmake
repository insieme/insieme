set(CABAL_VERSION 2.0.0.0 CACHE STRING "cabal Version")

if(NOT MSVC)
	find_package(CABAL ${CABAL_VERSION} REQUIRED)
endif()
