if(USE_PAPI)
	find_package(PAPI REQUIRED)
	add_definitions(-DPAPI_ROOT="${PAPI_ROOT}/")
endif()
