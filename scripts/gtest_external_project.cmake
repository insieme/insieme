insieme_find_package(NAME GTest SUPERBUILD TRUE)

if(NOT GTest_FOUND)

	message(STATUS "we install GTest (Version ${GTEST_VERSION})")

	include(ProcessorCount)
	ProcessorCount(SLOT)

	#gtest - downloads only the source!
	ExternalProject_Add (gtest
		URL http://googletest.googlecode.com/files/gtest-${GTEST_VERSION}.zip
		CONFIGURE_COMMAND cmake ..
		PREFIX ${THIRD_PARTY_LIBS_HOME}
		INSTALL_COMMAND ./b2 install
	)

endif()
