insieme_find_package(NAME Boost SUPERBUILD TRUE)

if(NOT Boost_FOUND)

	message(STATUS "we install Boost (Version ${BOOST_VERSION})")
	#download url expects version without dots...
	string(REPLACE "." "_" VERSION_STRING ${BOOST_VERSION})

	include(ProcessorCount)
	ProcessorCount(SLOT)

	ExternalProject_Add (boost
		URL http://sourceforge.net/projects/boost/files/boost/${BOOST_VERSION}/boost_${VERSION_STRING}.tar.bz2/download
		CONFIGURE_COMMAND ./bootstrap.sh --prefix=${insieme_lib}/boost-${BOOST_VERSION}/ --with-libraries=filesystem,program_options,random,system,regex,thread,serialization
		BUILD_COMMAND ./b2 "cxxflags=-mtune=native -O3" release -j${SLOTS}
		BUILD_IN_SOURCE 1
		INSTALL_COMMAND ./b2 install
	)

endif()
