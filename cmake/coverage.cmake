if(BUILD_COVERAGE)
	add_custom_target(coverage
		COMMAND ${PROJECT_SOURCE_DIR}/../scripts/coverage/create
			${PROJECT_BINARY_DIR}
	)
endif()
