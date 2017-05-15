if(USE_VALGRIND)
	find_package(Valgrind REQUIRED)

	set(Valgrind_FLAGS
		--leak-check=full
		--show-reachable=no
		--track-fds=yes
		--error-exitcode=1
		--track-origins=no
	)
endif()
