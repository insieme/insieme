if(BUILD_DOCS)
	configure_file(${PROJECT_SOURCE_DIR}/../.doxygen ${PROJECT_BINARY_DIR}/Doxyfile @ONLY)

	find_package(Doxygen REQUIRED)
	add_custom_target(
		${PROJECT_NAME}_doxygen
		ALL
		COMMAND ${DOXYGEN_EXECUTABLE} ${PROJECT_BINARY_DIR}/Doxyfile
		SOURCES ${PROJECT_SOURCE_DIR}/../.doxygen
	)
endif()
