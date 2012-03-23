include ../environment.mk

AWK_CMD = awk '/^VERSION=([0-9]+.)+[0-9]+/ { print $0 }'

# GCC STUFF
GMP_LIB =  ${PREFIX}/gmp-$(shell ${AWK_CMD} gmp_install.sh | cut -d= -f2 )
MPFR_LIB = ${PREFIX}/mpfr-$(shell ${AWK_CMD} mpfr_install.sh | cut -d= -f2 )
MPC_LIB =  ${PREFIX}/mpc-$(shell ${AWK_CMD} mpc_install.sh | cut -d= -f2 )
GCC_LIB =  ${PREFIX}/gcc-$(shell ${AWK_CMD} gcc_install.sh | cut -d= -f2 )

# POLYHEDEAL STUFF
LIBTOOL_LIB =  ${PREFIX}/gcc-$(shell ${AWK_CMD} libtool_install.sh | cut -d= -f2 )
NTL_LIB =  ${PREFIX}/gcc-$(shell ${AWK_CMD} ntl_install.sh | cut -d= -f2 )
ISL_LIB =  ${PREFIX}/gcc-$(shell ${AWK_CMD} isl_install.sh | cut -d= -f2 )
CLOOG_LIB =  ${PREFIX}/gcc-$(shell ${AWK_CMD} cloog_install.sh | cut -d= -f2 )
BARVINOK_LIB =  ${PREFIX}/gcc-$(shell ${AWK_CMD} barvinok_install.sh | cut -d= -f2 )

NEW_CXX=${PREFIX}/gcc-latest/bin/g++
NEW_CC=${PREFIX}/gcc-latest/bin/gcc

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

## GCC
${GCC_LIB}: gcc_install.sh ${GMP_LIB} ${MPFR_LIB} ${MPC_LIB}
	@echo "#### INSTALL MPFR ${MPFR_LIB} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh gcc_install.sh
	@echo "@@@@ Done! @@@@"

## LIBTOOL
${LIBTOOL_LIB}: libtool_install.sh ${GMP_LIB}
	@echo "#### INSTALL LIBTOOL ${LIBTOOL_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${CC} SLOTS=${SLOTS} sh libtool_install.sh
	@echo "@@@@ Done! @@@@"

## NTL
${NTL_LIB}: ntl_install.sh ${LIBTOOL_LIB}
	@echo "#### INSTALL NTL ${NTL_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh ntl_install.sh
	@echo "@@@@ Done! @@@@"

## ISL
${ISL_LIB}: isl_install.sh ${GMP_LIB}
	@echo "#### INSTALL ISL ${ISL_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh isl_install.sh
	@echo "@@@@ Done! @@@@"

## CLOOG
${CLOOG_LIB}: cloog_install.sh ${GMP_LIB} ${ISL_LIB}
	@echo "#### INSTALL CLOOG ${CLOOG_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh cloog_install.sh
	@echo "@@@@ Done! @@@@"

## BARVINOK
${BARVINOK_LIB}: cloog_install.sh ${GMP_LIB} ${ISL_LIB} ${NTL_LIB}
	@echo "#### INSTALL BARVINOK ${BARVINOK_LIB} ####"
	PREFIX=${PREFIX} CXX=${NEW_CXX} CC=${NEW_CC} SLOTS=${SLOTS} sh barvinok_install.sh
	@echo "@@@@ Done! @@@@"

INSIEME: ${GCC_LIB} ${ISL_LIB} ${CLOOG_LIB} ${BARVINOK_LIB}
