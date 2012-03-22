include ../environment.mk

GMP_LIB = ${PREFIX}/gmp-$(shell awk '/^VERSION=([0-9]+.)+[0-9]+/ { print $0 }' gmp_install.sh | cut -d= -f2 )
MPFR_LIB = ${PREFIX}/mpfr-$(shell awk '/^VERSION=([0-9]+.)+[0-9]+/ { print $0 }' mpfr_install.sh | cut -d= -f2 )
MPC_LIB = ${PREFIX}/mpc-$(shell awk '/^VERSION=([0-9]+.)+[0-9]+/ { print $0 }' mpc_install.sh | cut -d= -f2 )
GCC_LIB = ${PREFIX}/gcc-$(shell awk '/^VERSION=([0-9]+.)+[0-9]+/ { print $0 }' gcc_install.sh | cut -d= -f2 )

all: INSIEME

${GMP_LIB}: gmp_install.sh 
	@echo "#### INSTALL GMP ${GMP_LIB} ####"
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh gmp_install.sh

${MPFR_LIB}: mpfr_install.sh ${GMP_LIB}
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh mpfr_install.sh

${MPC_LIB}: mpc_install.sh ${GMP_LIB} ${MPFR_LIB}
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh mpc_install.sh

${GCC_LIB}: gcc_install.sh ${GMP_LIB} ${MPFR_LIB} ${MPC_LIB}
	PREFIX=${PREFIX} CXX=${CXX} CC=${CC} SLOTS=${SLOTS} sh gcc_install.sh

INSIEME: ${GMP_LIB} ${MPC_LIB} ${MPFR_LIB} ${GCC_LIB}
