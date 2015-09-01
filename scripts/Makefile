include ./environment.mk

AWK_CMD = awk '/^VERSION=([0-9]+.)+[0-9]+/ { print $0 }'

# GCC STUFF
GMP 	= ${PREFIX}/gmp-$(shell ${AWK_CMD} gmp_install.sh | cut -d= -f2 )
MPFR 	= ${PREFIX}/mpfr-$(shell ${AWK_CMD} mpfr_install.sh | cut -d= -f2 )
MPC 	= ${PREFIX}/mpc-$(shell ${AWK_CMD} mpc_install.sh | cut -d= -f2 )
GCC 	= ${PREFIX}/gcc-$(shell ${AWK_CMD} gcc_install.sh | cut -d= -f2 )

# INSIEME STUFF
LLVM 	:= ${PREFIX}/llvm-$(shell ${AWK_CMD} llvm_install.sh | cut -d= -f2 )
BOOST 	:= ${PREFIX}/boost-$(shell ${AWK_CMD} boost_install.sh | cut -d= -f2 )
CMAKE 	:= ${PREFIX}/cmake-$(shell ${AWK_CMD} cmake_install.sh | cut -d= -f2 )
LUAJIT 	:= ${PREFIX}/luajit-$(shell ${AWK_CMD} luajit_install.sh | cut -d= -f2 )
CUDD 	:= ${PREFIX}/cudd-$(shell ${AWK_CMD} cudd_install.sh | cut -d= -f2 )
BISON 	:= ${PREFIX}/bison-$(shell ${AWK_CMD} bison_install.sh | cut -d= -f2 )
FLEX 	:= ${PREFIX}/flex-$(shell ${AWK_CMD} flex_install.sh | cut -d= -f2 )
RUBY 	:= ${PREFIX}/ruby-$(shell ${AWK_CMD} ruby_install.sh | cut -d= -f2 )
PAPI 	:= ${PREFIX}/papi-$(shell ${AWK_CMD} papi_install.sh | cut -d= -f2 )

CCACHE	:= ${PREFIX}/ccache-$(shell ${AWK_CMD} ccache_install.sh | cut -d= -f2 )
HWLOC	:= ${PREFIX}/hwloc-$(shell ${AWK_CMD} hwloc_install.sh | cut -d= -f2 )

all: INSIEME

water: ${GCC} ${BOOST} ${CMAKE} ${OPENMPI}

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

## GCC
${GCC}: gcc_install.sh ${GMP} ${MPFR} ${MPC}
	@echo "#### INSTALL GCC ${GCC} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh gcc_install.sh
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

## CCache
${CCACHE}: ccache_install.sh ${GCC}
	@echo "#### INSTALL CCACHE ${CCACHE} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh ccache_install.sh
	@echo "@@@@ Done! @@@@"

## HWLOC
${HWLOC}: hwloc_install.sh ${GCC}
	@echo "#### INSTALL HWLOC ${HWLOC} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh hwloc_install.sh
	@echo "@@@@ Done! @@@@"

## BISON
${BISON}: bison_install.sh ${GCC}
	@echo "#### INSTALL BISON ${BISON} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh bison_install.sh
	@echo "@@@@ Done! @@@@"

## FLEX
${FLEX}: flex_install.sh ${GCC}
	@echo "#### INSTALL FLEX ${FLEX} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh flex_install.sh
	@echo "@@@@ Done! @@@@"


INSIEME: ${GCC} ${LLVM} ${BOOST} \
		 ${CMAKE} ${LUAJIT} ${CUDD} \
		 ${RUBY} ${PAPI} ${CCACHE} \
		 ${BISON} ${FLEX} ${HWLOC}
