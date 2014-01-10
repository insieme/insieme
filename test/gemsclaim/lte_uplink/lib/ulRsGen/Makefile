################################################################################
#                       LTE UPLINK RECEIVER PHY BENCHMARK                      #
#                                                                              #
# This file is distributed under the license terms given by LICENSE.TXT        #
################################################################################

CC=gcc 
CFLAGS=-ggdb -Wall -I ./common/inc/ -I ./inc/ -I ./src/ -lm

OBJECTS = \
	src/alpha_n_prs.o		\
	src/n_prs_all_ns.o		\
	src/r_alpha.o			\
	src/ulRsGen.o			\
	common/src/feldspar.o		\
	common/src/missingFunc.o	\
	common/src/feldspar_functions.o \
	test/testUlRsGen.o

testUlRsGen: ${OBJECTS}
	${CC} -ggdb -lm ${OBJECTS} -o testUlRsGen

run: testUlRsGen
	cp testUlRsGen /tmp && /tmp/testUlRsGen

clean:
	rm -f ${OBJECTS}
	rm -f testUlRsGen
