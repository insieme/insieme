/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 * regarding third party software licenses.
 */

// some typedefs and forward declarations
// this program doesn't need to do useful work, it's just input source for compiler tests

#define MPI_COMM_WORLD 0
#define MPI_INT 0
#define NULL 0

typedef int MPI_Comm;
typedef int MPI_Datatype;
typedef int MPI_Status;
typedef int MPI_Request;

int MPI_Init(int*, char***);
int MPI_Finalize();
int MPI_Comm_rank(MPI_Comm, int*);
int MPI_Comm_size(MPI_Comm, int*);
int MPI_Send(const void*, int, MPI_Datatype, int, int, MPI_Comm);
int MPI_Recv(const void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Status*);
int MPI_Isend(const void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request*);
int MPI_Irecv(const void*, int, MPI_Datatype, int, int, MPI_Comm, MPI_Request*);
int MPI_Test(MPI_Request*, int* flag, MPI_Status*);
int MPI_Wait(MPI_Request*, MPI_Status*);

int main(int argc, char** argv) {
	int a = 0;
	int num_procs = 0;
	int my_rank = 0;
	int flag = 0;
	MPI_Request bla;

	MPI_Init(&argc, &argv);

	MPI_Comm_size(MPI_COMM_WORLD, &num_procs);
	MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);

	for(int i = 0; i < 5; ++i) {
		if(my_rank == 0) {
			MPI_Isend(&a, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &bla);

			MPI_Test(&bla, &flag, NULL);
			while(!flag) {
				MPI_Test(&bla, &flag, NULL);
			}

			MPI_Isend(&a, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &bla);

			MPI_Wait(&bla, NULL);

			MPI_Isend(&a, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &bla);

			MPI_Test(&bla, &flag, NULL);
			while(!flag) {
				MPI_Test(&bla, &flag, NULL);
			}

			MPI_Send(&a, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);

			MPI_Send(&a, 1, MPI_INT, 1, 0, MPI_COMM_WORLD);
		}

		if(my_rank == 1) {
			MPI_Irecv(&a, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &bla);
			MPI_Wait(&bla, NULL);

			MPI_Irecv(&a, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &bla);

			MPI_Test(&bla, &flag, NULL);
			while(!flag) {
				MPI_Test(&bla, &flag, NULL);
			}

			MPI_Irecv(&a, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &bla);
			MPI_Wait(&bla, NULL);

			MPI_Recv(&a, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &flag);

			MPI_Recv(&a, 1, MPI_INT, 1, 0, MPI_COMM_WORLD, &flag);
		}
	}

	MPI_Finalize();

	return 0;
}
