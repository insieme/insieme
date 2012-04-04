include ../environment.mk

AWK_CMD = awk '/^VERSION=([0-9]+.)+[0-9]+/ { print $0 }'

# GCC STUFF
GMP 	= ${PREFIX}/gmp-$(shell ${AWK_CMD} gmp_install.sh | cut -d= -f2 )
MPFR 	= ${PREFIX}/mpfr-$(shell ${AWK_CMD} mpfr_install.sh | cut -d= -f2 )
MPC 	= ${PREFIX}/mpc-$(shell ${AWK_CMD} mpc_install.sh | cut -d= -f2 )
GCC 	= ${PREFIX}/gcc-$(shell ${AWK_CMD} gcc_install.sh | cut -d= -f2 )

LIBTOOL	= ${PREFIX}/libtool-$(shell ${AWK_CMD} libtool_install.sh | cut -d= -f2 )

# POLYHEDEAL STUFF
NTL 	:= ${PREFIX}/ntl-$(shell ${AWK_CMD} ntl_install.sh | cut -d= -f2 )
ISL 	:= ${PREFIX}/isl-$(shell ${AWK_CMD} isl_install.sh | cut -d= -f2 )
PPL 	:= ${PREFIX}/ppl-$(shell ${AWK_CMD} ppl_install.sh | cut -d= -f2 )
CLOOG 	:= ${PREFIX}/cloog-$(shell ${AWK_CMD} cloog_install.sh | cut -d= -f2 )
CLOOG_GCC := ${PREFIX}/cloog-$(shell ${AWK_CMD} cloog_gcc_install.sh | cut -d= -f2 )
BARVINOK := ${PREFIX}/barvinok-$(shell ${AWK_CMD} barvinok_install.sh | cut -d= -f2 )

# INSIEME STUFF
LLVM 	:= ${PREFIX}/llvm-$(shell ${AWK_CMD} llvm_install.sh | cut -d= -f2 )
BOOST 	:= ${PREFIX}/boost-$(shell ${AWK_CMD} boost_install.sh | cut -d= -f2 )
CMAKE 	:= ${PREFIX}/cmake-$(shell ${AWK_CMD} cmake_install.sh | cut -d= -f2 )
GTEST 	:= ${PREFIX}/gtest-$(shell ${AWK_CMD} gtest_install.sh | cut -d= -f2 )
LUAJIT 	:= ${PREFIX}/luajit-$(shell ${AWK_CMD} luajit_install.sh | cut -d= -f2 )
CUDD 	:= ${PREFIX}/cudd-$(shell ${AWK_CMD} cudd_install.sh | cut -d= -f2 )
SHARK 	:= ${PREFIX}/shark-$(shell ${AWK_CMD} shark_install.sh | cut -d= -f2 )
KOMPEX 	:= ${PREFIX}/kompex-$(shell ${AWK_CMD} kompex_install.sh | cut -d= -f2 )
XERCES 	:= ${PREFIX}/xerces-$(shell ${AWK_CMD} xerces_install.sh | cut -d= -f2 )
PYTHON 	:= ${PREFIX}/python-$(shell ${AWK_CMD} python_install.sh | cut -d= -f2 )
RUBY 	:= ${PREFIX}/ruby-$(shell ${AWK_CMD} ruby_install.sh | cut -d= -f2 )
PAPI 	:= ${PREFIX}/papi-$(shell ${AWK_CMD} papi_install.sh | cut -d= -f2 )

NEW_CXX	= ${PREFIX}/gcc-latest/bin/g++
NEW_CC	= ${PREFIX}/gcc-latest/bin/gcc

all: INSIEME

## GMP
${GMP}: gmp_install.sh 
	@echo "#### INSTALL GMP ${GMP} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh gmp_install.sh
	@echo "@@@@ Done! @@@@"

## MPFR
${MPFR}: mpfr_install.sh ${GMP}
	@echo "#### INSTALL MPFR ${MPFR} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh mpfr_install.sh
	@echo "@@@@ Done! @@@@"

## MPC
${MPC}: mpc_install.sh ${GMP} ${MPFR}
	@echo "#### INSTALL MPC ${MPC} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh mpc_install.sh
	@echo "@@@@ Done! @@@@"

## PPL
${PPL}: ppl_install.sh ${GMP} 	
	@echo "#### INSTALL PPL ${PPL} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh ppl_install.sh
	@echo "@@@@ Done! @@@@"

## CLOOG
${CLOOG_GCC}: cloog_gcc_install.sh ${GMP}  
	@echo "#### INSTALL CLOOG GCC ${CLOOG_GCC} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh cloog_gcc_install.sh
	@echo "@@@@ Done! @@@@"

## GCC
${GCC}: gcc_install.sh ${GMP} ${MPFR} ${MPC} ${PPL} ${CLOOG_GCC}
	@echo "#### INSTALL GCC ${GCC} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh gcc_install.sh
	@echo "@@@@ Done! @@@@"

## LIBTOOL
${LIBTOOL}: libtool_install.sh ${GMP}
	@echo "#### INSTALL LIBTOOL ${LIBTOOL} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${CC} SLOTS=${SLOTS} sh libtool_install.sh
	@echo "@@@@ Done! @@@@"

## NTL
${NTL}: ntl_install.sh ${LIBTOOL} ${GCC}
	@echo "#### INSTALL NTL ${NTL} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh ntl_install.sh
	@echo "@@@@ Done! @@@@"

## ISL
${ISL}: isl_install.sh ${GMP} ${GCC}
	@echo "#### INSTALL ISL ${ISL} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh isl_install.sh
	@echo "@@@@ Done! @@@@"

## CLOOG
${CLOOG}: cloog_install.sh ${GMP} ${ISL} ${GCC}
	@echo "#### INSTALL CLOOG ${CLOOG} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh cloog_install.sh
	@echo "@@@@ Done! @@@@"

## BARVINOK
${BARVINOK}: barvinok_install.sh ${GMP} ${ISL} ${NTL} ${GCC} ${CLOOG}
	@echo "#### INSTALL BARVINOK ${BARVINOK} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh barvinok_install.sh
	@echo "@@@@ Done! @@@@"

## LLVM
${LLVM}: llvm_install.sh ${GCC}
	@echo "#### INSTALL LLVM ${LLVM} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh llvm_install.sh
	@echo "@@@@ Done! @@@@"

## BOOST
${BOOST}: boost_install.sh ${GCC}
	@echo "#### INSTALL BOOST ${BOOST} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh boost_install.sh
	@echo "@@@@ Done! @@@@"

## CMAKE
${CMAKE}: cmake_install.sh ${GCC}
	@echo "#### INSTALL CMAKE ${CMAKE} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh cmake_install.sh
	@echo "@@@@ Done! @@@@"

## GTEST
${GTEST}: gtest_install.sh ${GCC} ${CMAKE}
	@echo "#### INSTALL GTEST ${GTEST} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh gtest_install.sh
	@echo "@@@@ Done! @@@@"

## LUAJIT
${LUAJIT}: luajit_install.sh ${GCC}
	@echo "#### INSTALL LUAJIT ${LUAJIT} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh luajit_install.sh
	@echo "@@@@ Done! @@@@"

## CUDD
${CUDD}: cudd_install.sh ${GCC}
	@echo "#### INSTALL CUDD ${CUDD} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh cudd_install.sh
	@echo "@@@@ Done! @@@@"

## SHARK
${SHARK}: shark_install.sh ${GCC} ${BOOST} ${CMAKE}
	@echo "#### INSTALL SHARK ${SHARK} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh shark_install.sh
	@echo "@@@@ Done! @@@@"

## KOMPEX
${KOMPEX}: kompex_install.sh ${GCC}
	@echo "#### INSTALL KOMPEX ${KOMPEX} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh kompex_install.sh
	@echo "@@@@ Done! @@@@"

## XERCES
${XERCES}: xerces_install.sh ${GCC}
	@echo "#### INSTALL XERCES ${XERCES} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh xerces_install.sh
	@echo "@@@@ Done! @@@@"

## PYTHOHN
${PYTHON}: python_install.sh ${GCC}
	@echo "#### INSTALL PYTHON ${PYTHON} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh python_install.sh
	@echo "@@@@ Done! @@@@"

## RUBY
${RUBY}: ruby_install.sh ${GCC}
	@echo "#### INSTALL RUBY ${RUBY} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh ruby_install.sh
	@echo "@@@@ Done! @@@@"

## PAPI
${PAPI}: papi_install.sh ${GCC}
	@echo "#### INSTALL PAPI ${PAPI} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh papi_install.sh
	@echo "@@@@ Done! @@@@"

INSIEME: ${GCC} ${ISL} ${CLOOG} ${BARVINOK} ${LLVM} ${BOOST} \
		 ${CMAKE} ${GTEST} ${LUAJIT} ${CUDD} ${SHARK} ${KOMPEX} \
		 ${XERCES} ${PYTHON} ${RUBY} ${PAPI}
