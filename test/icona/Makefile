#CPPFLAGS =	-O3 -Wall -fmessage-length=0
CPPFLAGS =	-O3 -Wall -fmessage-length=0 -fopenmp
# -g
# -g for debugging

OBJ_F =		icona.o IFParser.o Calc.o FETask.o Node.o Element.o Material.o Step.o HC8A8E9.o ElemHC8A8E9.o 

#LIBS = 		-larmadillo
LIBS = 		-lglut -lGLU

TARGET =	icona

BOPT = 		g
NP = 		4

#include ${PETSC_DIR}/conf/variables
#include ${PETSC_DIR}/conf/rules

${TARGET}:	${OBJ_F} chkopts
	${CLINKER} -o ${TARGET} ${OBJ_F} ${LIBS} ${PETSC_LIB} ${CPPFLAGS}
# debugging=1 for debugging mode of petsc	
	${RM} ${OBJ_F}
	
all:	${TARGET}


run:
	${MPIRUN} -np ${NP} ${PGM} ${ARGS}

clean-local:
	rm -f ${OBJ_F} ${TARGET}
