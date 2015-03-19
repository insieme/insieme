cmake_minimum_required(VERSION 2.8)
set(insieme_lib ~/libs)

include(ProcessorCount)
ProcessorCount(SLOTS)

set(gmp_version 6.0.0)
set(gmp_subversion a)
set(mpfr_version 3.1.2)
set(mpc_version 1.0.2)
set(isl_version 0.12)
set(cloog_version 0.18.1)
set(gcc_version 4.9.2)

include(ExternalProject)

ExternalProject_Add(gmp_project
	PREFIX			${insieme_lib}/gmp_${gmp_version}${gmp_subversion}_tmp
	URL 			ftp://ftp.gmplib.org/pub/gmp-${gmp_version}/gmp-${gmp_version}${gmp_subversion}.tar.bz2
	CONFIGURE_COMMAND 	./configure --prefix=${insieme_lib}/gmp-${gmp_version} --enable-cxx
	BUILD_COMMAND		make -j${SLOTS}
	TEST_COMMAND		make check
	TEST_BEFORE_INSTALL	1
	INSTALL_COMMAND		make install
	BUILD_IN_SOURCE		1
	LOG_DOWNLOAD		1
	LOG_CONFIGURE		1
	LOG_BUILD		1
	LOG_TEST		1
	LOG_INSTALL		1
)

add_custom_target(gmp)
add_dependencies(gmp gmp_project)

ExternalProject_Add(mpfr_project
	DEPENDS			gmp_project
	PREFIX			${insieme_lib}/mpfr_${mpfr_version}_tmp
	URL 			http://www.mpfr.org/mpfr-${mpfr_version}/mpfr-${mpfr_version}.tar.bz2
	CONFIGURE_COMMAND 	./configure --prefix=${insieme_lib}/mpfr-${mpfr_version} --with-gmp=${insieme_lib}/gmp-${gmp_version}
	BUILD_COMMAND		make -j${SLOTS}
	TEST_COMMAND		make check
	TEST_BEFORE_INSTALL	1
	INSTALL_COMMAND		make install
	BUILD_IN_SOURCE		1
	LOG_DOWNLOAD		1
	LOG_CONFIGURE		1
	LOG_BUILD		1
	LOG_TEST		1
	LOG_INSTALL		1
)

add_custom_target(mpfr)
add_dependencies(mpfr mpfr_project)

ExternalProject_Add(mpc_project
        DEPENDS                 mpfr_project
        PREFIX                  ${insieme_lib}/mpc_${mpc_version}_tmp
        URL                     http://www.multiprecision.org/mpc/download/mpc-${mpc_version}.tar.gz
        CONFIGURE_COMMAND       ./configure --prefix=${insieme_lib}/mpc-${mpc_version} --with-gmp=${insieme_lib}/gmp-${gmp_version} --with-mpfr=${insieme_lib}/mpfr-${mpfr_version}
        BUILD_COMMAND           make -j${SLOTS}
        TEST_COMMAND            make check
        TEST_BEFORE_INSTALL     1
        INSTALL_COMMAND         make install
        BUILD_IN_SOURCE         1
        LOG_DOWNLOAD            1
        LOG_CONFIGURE           1
        LOG_BUILD               1
        LOG_TEST                1
        LOG_INSTALL             1
)

add_custom_target(mpc)
add_dependencies(mpc mpc_project)

ExternalProject_Add(isl_project
	DEPENDS			gmp_project
	PREFIX			${insieme_lib}/isl_${isl_version}_tmp
	URL 			http://isl.gforge.inria.fr/isl-${isl_version}.tar.gz
	CONFIGURE_COMMAND 	./configure --prefix=${insieme_lib}/isl-${isl_version} --with-gmp=system --with-gmp-prefix=${insieme_lib}/gmp-${gmp_version}
	BUILD_COMMAND		make -j${SLOTS}
	TEST_COMMAND		make check
	TEST_BEFORE_INSTALL	1
	INSTALL_COMMAND		make install
	BUILD_IN_SOURCE		1
	LOG_DOWNLOAD		1
	LOG_CONFIGURE		1
	LOG_BUILD		1
	LOG_TEST		1
	LOG_INSTALL		1
)

add_custom_target(isl)
add_dependencies(isl isl_project)

ExternalProject_Add(cloog_project
        DEPENDS                 gmp_project isl_project
        PREFIX                  ${insieme_lib}/cloog-gcc_${cloog_version}_tmp
        URL                      http://www.bastoul.net/cloog/pages/download/count.php3?url=./cloog-${cloog_version}.tar.gz
        CONFIGURE_COMMAND       ./configure --prefix=${insieme_lib}/cloog-gcc-${cloog_version} --with-gmp=system --with-gmp-prefix=${insieme_lib}/gmp-${gmp_version}
        BUILD_COMMAND           make -j${SLOTS}
        TEST_COMMAND            make check
        TEST_BEFORE_INSTALL     1
        INSTALL_COMMAND         make install
        BUILD_IN_SOURCE         1
        LOG_DOWNLOAD            1
        LOG_CONFIGURE           1
        LOG_BUILD               1
        LOG_TEST                1
        LOG_INSTALL             1
)

add_custom_target(cloog)
add_dependencies(cloog cloog_project)


ExternalProject_Add(gcc_project
        DEPENDS                 cloog_project mpc_project
        PREFIX                  ${insieme_lib}/gcc_${gcc_version}_tmp
        URL                     http://gcc.igor.onlinedirect.bg/releases/gcc-${gcc_version}/gcc-${gcc_version}.tar.bz2
        CONFIGURE_COMMAND       ./configure --prefix=${insieme_lib}/gcc-${gcc_version} --with-mpc=${insieme_lib}/mpc-${mpc_version} --with-gmp=${insieme_lib}/gmp-${gmp_version} --with-mpfr=${insieme_lib}/mpfr-${mpfr_version} --with-cloog=${insieme_lib}/cloog-gcc-${cloog_version} --with-isl=${insieme_lib}/isl-${isl_version} --enable-cloog-backend=isl --enable-languages=c,c++ --disable-multilib --enable-lto
        BUILD_COMMAND           make -j${SLOTS}
        INSTALL_COMMAND         make install
        BUILD_IN_SOURCE         1
        LOG_DOWNLOAD            1
        LOG_CONFIGURE           1
        LOG_BUILD               1
        LOG_TEST                1
        LOG_INSTALL             1
)

add_custom_target(gcc)
add_dependencies(gcc gcc_project)
