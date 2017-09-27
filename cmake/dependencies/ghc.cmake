set(GHC_VERSION 8.0 CACHE STRING "ghc Version")

if(NOT MSVC)
	find_package(GHC ${GHC_VERSION} REQUIRED)
endif()
