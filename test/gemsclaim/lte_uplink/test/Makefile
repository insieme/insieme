CC = gcc
EXECUTABLE = test.o
CFLAGS = -E

OBJECTS = common.o utils.o

${EXECUTABLE}: ${OBJECTS}
	gcc -E main.c ${OBJECTS} -o ${EXECUTABLE}

all: ${EXECUTABLE}


clean:
	rm -f *.o test
