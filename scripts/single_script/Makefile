include ../environment.mk

AWK_CMD = awk '/^VERSION=([0-9]+.)+[0-9]+/ { print $0 }'

# GCC STUFF
GMP_LIB =  ${PREFIX}/gmp-$(shell ${AWK_CMD} gmp_install.sh | cut -d= -f2 )
MPFR_LIB = ${PREFIX}/mpfr-$(shell ${AWK_CMD} mpfr_install.sh | cut -d= -f2 )
MPC_LIB =  ${PREFIX}/mpc-$(shell ${AWK_CMD} mpc_install.sh | cut -d= -f2 )
GCC_LIB =  ${PREFIX}/gcc-$(shell ${AWK_CMD} gcc_install.sh | cut -d= -f2 )

# POLYHEDEAL STUFF
NTL_LIB := 	 	${PREFIX}/ntl-$(shell ${AWK_CMD} ntl_install.sh | cut -d= -f2 )
ISL_LIB :=  	${PREFIX}/isl-$(shell ${AWK_CMD} isl_install.sh | cut -d= -f2 )
PPL_LIB :=  	${PREFIX}/ppl-$(shell ${AWK_CMD} ppl_install.sh | cut -d= -f2 )
CLOOG_LIB :=  	${PREFIX}/cloog-$(shell ${AWK_CMD} cloog_install.sh | cut -d= -f2 )
CLOOG_GCC_LIB := ${PREFIX}/cloog-$(shell ${AWK_CMD} cloog_gcc_install.sh | cut -d= -f2 )
BARVINOK_LIB :=  ${PREFIX}/barvinok-$(shell ${AWK_CMD} barvinok_install.sh | cut -d= -f2 )

# INSIEME STUFF
LLVM_LIB := ${PREFIX}/llvm-$(shell ${AWK_CMD} llvm_install.sh | cut -d= -f2 )
BOOST_LIB := ${PREFIX}/boost-$(shell ${AWK_CMD} boost_install.sh | cut -d= -f2 )
CMAKE_LIB := ${PREFIX}/cmake-$(shell ${AWK_CMD} cmake_install.sh | cut -d= -f2 )
GTEST_LIB := ${PREFIX}/gtest-$(shell ${AWK_CMD} gtest_install.sh | cut -d= -f2 )
LUAJIT_LIB := ${PREFIX}/luajit-$(shell ${AWK_CMD} luajit_install.sh | cut -d= -f2 )
CUDD_LIB := ${PREFIX}/cudd-$(shell ${AWK_CMD} cudd_install.sh | cut -d= -f2 )
SHARK_LIB := ${PREFIX}/shark-$(shell ${AWK_CMD} shark_install.sh | cut -d= -f2 )
KOMPEX_LIB := ${PREFIX}/kompex-$(shell ${AWK_CMD} kompex_install.sh | cut -d= -f2 )
XERCES_LIB := ${PREFIX}/xerces-$(shell ${AWK_CMD} xerces_install.sh | cut -d= -f2 )
PYTHON_LIB := ${PREFIX}/python-$(shell ${AWK_CMD} python_install.sh | cut -d= -f2 )
RUBY_LIB := ${PREFIX}/ruby-$(shell ${AWK_CMD} ruby_install.sh | cut -d= -f2 )
PAPI_LIB := ${PREFIX}/papi-$(shell ${AWK_CMD} papi_install.sh | cut -d= -f2 )

NEW_CXX:=${PREFIX}/gcc-latest/bin/g++
NEW_CC:=${PREFIX}/gcc-latest/bin/gcc

all: INSIEME

## GMP
${GMP_LIB}: gmp_install.sh 
	@echo "#### INSTALL GMP ${GMP_LIB} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh gmp_install.sh
	@echo "@@@@ Done! @@@@"

## MPFR
${MPFR_LIB}: mpfr_install.sh ${GMP_LIB}
	@echo "#### INSTALL MPFR ${MPFR_LIB} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh mpfr_install.sh
	@echo "@@@@ Done! @@@@"

## MPC
${MPC_LIB}: mpc_install.sh ${GMP_LIB} ${MPFR_LIB}
	@echo "#### INSTALL MPC ${MPC_LIB} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh mpc_install.sh
	@echo "@@@@ Done! @@@@"

## PPL
${PPL_LIB}: ppl_install.sh ${GMP_LIB} 	
	@echo "#### INSTALL PPL ${PPL_LIB} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh ppl_install.sh
	@echo "@@@@ Done! @@@@"

## CLOOG
${CLOOG_GCC_LIB}: cloog_gcc_install.sh ${GMP_LIB}  
	@echo "#### INSTALL CLOOG GCC ${CLOOG_GCC_LIB} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh cloog_gcc_install.sh
	@echo "@@@@ Done! @@@@"

## GCC
${GCC_LIB}: gcc_install.sh ${GMP_LIB} ${MPFR_LIB} ${MPC_LIB} ${PPL_LIB} ${CLOOG_GCC_LIB}
	@echo "#### INSTALL MPFR ${GCC_LIB} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh gcc_install.sh
	@echo "@@@@ Done! @@@@"

## LIBTOOL
${LIBTOOL_LIB}: libtool_install.sh ${GMP_LIB}
	@echo "#### INSTALL LIBTOOL ${LIBTOOL_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${CC} SLOTS=${SLOTS} sh libtool_install.sh
	@echo "@@@@ Done! @@@@"

## NTL
${NTL_LIB}: ntl_install.sh ${LIBTOOL_LIB} ${GCC_LIB}
	@echo "#### INSTALL NTL ${NTL_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh ntl_install.sh
	@echo "@@@@ Done! @@@@"

## ISL
${ISL_LIB}: isl_install.sh ${GMP_LIB} ${GCC_LIB}
	@echo "#### INSTALL ISL ${ISL_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh isl_install.sh
	@echo "@@@@ Done! @@@@"

## CLOOG
${CLOOG_LIB}: cloog_install.sh ${GMP_LIB} ${ISL_LIB} ${GCC_LIB}
	@echo "#### INSTALL CLOOG ${CLOOG_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh cloog_install.sh
	@echo "@@@@ Done! @@@@"

## BARVINOK
${BARVINOK_LIB}: barvinok_install.sh ${GMP_LIB} ${ISL_LIB} ${NTL_LIB} ${GCC_LIB} ${CLOOG_LIB}
	@echo "#### INSTALL BARVINOK ${BARVINOK_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh barvinok_install.sh
	@echo "@@@@ Done! @@@@"

## LLVM
${LLVM_LIB}: llvm_install.sh ${GCC_LIB}
	@echo "#### INSTALL LLVM ${LLVM_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh llvm_install.sh
	@echo "@@@@ Done! @@@@"

## BOOST
${BOOST_LIB}: boost_install.sh ${GCC_LIB}
	@echo "#### INSTALL BOOST ${BOOST_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh boost_install.sh
	@echo "@@@@ Done! @@@@"

## CMAKE
${CMAKE_LIB}: cmake_install.sh ${GCC_LIB}
	@echo "#### INSTALL CMAKE ${CMAKE_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh cmake_install.sh
	@echo "@@@@ Done! @@@@"

## GTEST
${GTEST_LIB}: gtest_install.sh ${GCC_LIB} ${CMAKE_LIB}
	@echo "#### INSTALL GTEST ${GTEST_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh gtest_install.sh
	@echo "@@@@ Done! @@@@"

## LUAJIT
${LUAJIT_LIB}: luajit_install.sh ${GCC_LIB}
	@echo "#### INSTALL LUAJIT ${LUAJIT_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh luajit_install.sh
	@echo "@@@@ Done! @@@@"

## CUDD
${CUDD_LIB}: cudd_install.sh ${GCC_LIB}
	@echo "#### INSTALL CUDD ${CUDD_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh cudd_install.sh
	@echo "@@@@ Done! @@@@"

## SHARK
${SHARK_LIB}: shark_install.sh ${GCC_LIB} ${BOOST_LIB} ${CMAKE_LIB}
	@echo "#### INSTALL SHARK ${SHARK_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh shark_install.sh
	@echo "@@@@ Done! @@@@"

## KOMPEX
${KOMPEX_LIB}: kompex_install.sh ${GCC_LIB}
	@echo "#### INSTALL KOMPEX ${KOMPEX_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh kompex_install.sh
	@echo "@@@@ Done! @@@@"

## XERCES
${XERCES_LIB}: xerces_install.sh ${GCC_LIB}
	@echo "#### INSTALL XERCES ${XERCES_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh xerces_install.sh
	@echo "@@@@ Done! @@@@"

## PYTHOHN
${PYTHON_LIB}: python_install.sh ${GCC_LIB}
	@echo "#### INSTALL PYTHON ${PYTHON_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh python_install.sh
	@echo "@@@@ Done! @@@@"

## RUBY
${RUBY_LIB}: ruby_install.sh ${GCC_LIB}
	@echo "#### INSTALL RUBY ${RUBY_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh ruby_install.sh
	@echo "@@@@ Done! @@@@"

## PAPI
${PAPI_LIB}: papi_install.sh ${GCC_LIB}
	@echo "#### INSTALL PAPI ${PAPI_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh papi_install.sh
	@echo "@@@@ Done! @@@@"

INSIEME: ${GCC_LIB} ${ISL_LIB} ${CLOOG_LIB} ${BARVINOK_LIB} ${LLVM_LIB} ${BOOST_LIB} \
		 ${CMAKE_LIB} ${GTEST_LIB} ${LUAJIT_LIB} ${CUDD_LIB} ${SHARK_LIB} ${KOMPEX_LIB} \
		 ${XERCES_LIB} ${PYTHON_LIB} ${RUBY_LIB} ${PAPI_LIB}
