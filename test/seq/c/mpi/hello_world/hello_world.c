
#include <stdio.h>
#include <mpi.h>

void main(int argc, char **argv) {

	// just start the MPI process, get the rank and comm size and be done

	int rank;
	int num;

	// start up MPI
	MPI_Init(&argc, &argv);

	// get rank and size
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &num);

	// print something
	printf("Hello, I'm #%d of %d\n", rank, num);

	// done
	MPI_Finalize();
}

