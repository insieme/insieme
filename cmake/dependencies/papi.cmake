if(USE_PAPI)
	find_package(PAPI REQUIRED)
	add_definitions(-DUSE_PAPI -DPAPI_ROOT="${PAPI_ROOT}/")
endif()
