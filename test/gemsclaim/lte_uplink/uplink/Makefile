################################################################################
#                       LTE UPLINK RECEIVER PHY BENCHMARK                      #
#                                                                              #
# This file is distributed under the license terms given by LICENSE.TXT        #
################################################################################
# Author: Magnus Sjalander                                                     #
################################################################################

define HELP_TEXT_UPLINK
echo "Uplink Make Targets"
echo "--------------------------------------------------------------------------------"
echo "uplink             Compiles the uplink benchmark for the Tilera architecture"
echo "uplink x86=1       Compiles the uplink benchmark for the x86 architecture"
echo "uplink serial=1    Compiles the serial uplink benchmark for the x86 architecture"
echo "uplink cilk=1      Compiles the cilk uplink benchmark for the x86 architecture"
echo "clean_uplink       Deletes all object files and the executable"
echo "objdump_uplink:    Dumps the uplink executable as assembler"
echo "help_uplink:       Prints the help text of this section"
echo ""
endef
.PHONY: help_uplink
help_uplink:
	@${HELP_TEXT_UPLINK}

###########################################################
#                UPLINK BENCHMARK TARGETS                 #
###########################################################
ifndef ROOT
ROOT       = ..
EXECUTABLE = ${UPLINK}/uplink
endif

UPLINK     = ${ROOT}/uplink
LIB        = ${ROOT}/lib
ULRSGEN    = ${LIB}/ulRsGen
TIME_LIB   = ${LIB}/time
KERNELS    = ${LIB}/kernels
FLAGS      = -O3 -Wall -I ${UPLINK} -I ${TIME_LIB} -I ${KERNELS} -I ${ULRSGEN}/common/inc/ -I ${ULRSGEN}/inc/ -I ${ULRSGEN}/src/ 

OBJECTS = \
	${UPLINK}/uplink_parameters.o			\
	${UPLINK}/uplink_alarm.o			\
	${UPLINK}/uplink_verify.o			\
	${KERNELS}/ant_comb_7.o				\
	${KERNELS}/chest_5.o				\
	${KERNELS}/complex_def.o			\
	${KERNELS}/fft_8.o				\
	${KERNELS}/interleave_11.o			\
	${KERNELS}/soft_demap_9.o			\
	${KERNELS}/mf_4.o				\
	${KERNELS}/weight_calc_6.o			\
	${KERNELS}/mmse_by_cholsolve_4xX_complex.o	\
	${KERNELS}/crc_13.o				\
	${KERNELS}/turbo_dec_12.o			\
	${ULRSGEN}/src/alpha_n_prs.o			\
	${ULRSGEN}/src/n_prs_all_ns.o			\
	${ULRSGEN}/src/r_alpha.o			\
	${ULRSGEN}/src/ulRsGen.o			\
	${ULRSGEN}/common/src/feldspar.o		\
	${ULRSGEN}/common/src/missingFunc.o		\
	${ULRSGEN}/common/src/feldspar_functions.o

ifdef x86
CC     = gcc
COMP   = ${CC}
CFLAGS = -g ${FLAGS} -DX86
OBJECTS_ARCH = \
	${UPLINK}/uplink.o				\
	${UPLINK}/uplink_task.o				\
	${UPLINK}/uplink_user.o				\
	${UPLINK}/affinity_x86.o 			\
	${UPLINK}/spr_x86.o 				\
	${UPLINK}/fence_x86.o 				\
	${UPLINK}/active_x86.o
else ifdef cilk
CC     = gcc
COMP   = ${ROOT}/lib/cilk/bin/cilkc
CFLAGS = -g ${FLAGS} -DCILK
OBJECTS_ARCH = \
	${UPLINK}/uplink_user.cilk			\
	${UPLINK}/uplink.cilk
else ifdef cilk_pci
CC     = $(BIN)tile-cc
COMP   = $(CC)
CFLAGS = -g ${FLAGS} -DCILK  -L${CILK_TILE}/lib -L${CILK_TILE}/lib/cilk -lcilkrt0 -lcilk -Wl,-rpath,${CILK_TILE}/lib -pthread
OBJECTS_ARCH = \
	${UPLINK}/uplink_user_cilk.i			\
	${UPLINK}/uplink_cilk.i
else ifdef serial
CC     = /software-local/insieme-libs/gcc-latest/bin/gcc
#CC     = gcc
COMP   = ${CC}
CFLAGS = -g ${FLAGS}
OBJECTS_ARCH = \
	${UPLINK}/uplink_serial.o
else ifdef insieme-serial 
CC     = /software-local/insieme-libs/gcc-latest/bin/gcc
#CC     = gcc
COMP   = ${CC}
CFLAGS = -E -g ${FLAGS}
OBJECTS_ARCH = \
	${UPLINK}/uplink_serial.o
else
CC     = $(BIN)tile-cc
COMP   = ${CC}
CFLAGS = -g ${FLAGS} -lilib -ltmc
OBJECTS_ARCH = \
	${UPLINK}/uplink.o				\
	${UPLINK}/uplink_task.o				\
	${UPLINK}/uplink_user.o				\
	${UPLINK}/affinity_tilera.o 			\
	${UPLINK}/spr_tilera.o 				\
	${UPLINK}/fence_tilera.o 			\
	${UPLINK}/active_tilera.o
endif

.PHONY: uplink
uplink: ${EXECUTABLE}


${EXECUTABLE}: ${OBJECTS} ${OBJECTS_ARCH} ${UPLINK}/def.h
	${COMP} ${CFLAGS} -lm -lpthread ${OBJECTS_ARCH} ${OBJECTS} -o ${EXECUTABLE}

.PHONY: clean_uplink
clean_uplink: 
	rm -f ${OBJECTS}
	rm -f ${UPLINK}/affinity_*.o ${UPLINK}/spr_*.o ${UPLINK}/active_*.o ${UPLINK}/uplink.o ${UPLINK}/uplink_task.o ${UPLINK}/uplink_user.o ${UPLINK}/uplink_serial.o
	rm -rf ${UPLINK}/uplink*.cilkc ${UPLINK}/uplink*.cilki ${UPLINK}/uplink_*cilk.i ${UPLINK}/uplink_*cilk.o
	rm -f ${EXECUTABLE}

.PHONY: objdump_uplink
objdump_uplink: ${UPLINK}/uplink.S
	@grep nap ${UPLINK}/uplink.S

${UPLINK}/uplink.S: ${UPLINK}/uplink
	@/opt/tilepro/bin/tile-objdump -D ${UPLINK}/uplink > ${UPLINK}/uplink.S

${UPLINK}/uplink.o: ${UPLINK}/def.h
${UPLINK}/uplink_task.o: ${UPLINK}/def.h
${UPLINK}/uplink_user.o: ${UPLINK}/def.h
${UPLINK}/uplink_time.o: ${UPLINK}/def.h
${UPLINK}/uplink_parameters.o: ${UPLINK}/def.h ${UPLINK}/uplink_verify.h
${UPLINK}/uplink_verify.o: ${UPLINK}/uplink_verify.h ${UPLINK}/def.h


${UPLINK}/uplink.cilki: ${UPLINK}/uplink.cilk ${UPLINK}/def.h
	@$(BIN)/tile-cc -E -xc -include ${CILK_TILE}/include/cilk/cilk.h ${UPLINK}/uplink.cilk ${FLAGS} -DCILK -I${CILK_TILE}/include/cilk -D__CILK__ -D__CILK2C__ -D__REENTRANT -pthread > $@

${UPLINK}/uplink.cilkc: ${UPLINK}/uplink.cilki
	@${CILK_ROOT}/lib/cilk/cilk2c ${UPLINK}/uplink.cilki -o $@ -W5

${UPLINK}/uplink_cilk.i: ${UPLINK}/uplink.cilkc
	@$(BIN)/tile-cc -E -I${CILK_TILE}/include/cilk -D_REENTRANT -x c ${UPLINK}/uplink.cilkc -pthread > $@

${UPLINK}/uplink_user.cilki: ${UPLINK}/uplink_user.cilk
	@$(BIN)/tile-cc -E -xc -include ${CILK_TILE}/include/cilk/cilk.h ${UPLINK}/uplink_user.cilk ${FLAGS} -DCILK -I${CILK_TILE}/include/cilk -D__CILK__ -D__CILK2C__ -D__REENTRANT -pthread > $@

${UPLINK}/uplink_user.cilkc: ${UPLINK}/uplink_user.cilki
	@${CILK_ROOT}/lib/cilk/cilk2c ${UPLINK}/uplink_user.cilki -o $@ -W5

${UPLINK}/uplink_user_cilk.i: ${UPLINK}/uplink_user.cilkc
	@$(BIN)/tile-cc -E -I${CILK_TILE}/include/cilk -D_REENTRANT -x c ${UPLINK}/uplink_user.cilkc -pthread > $@
