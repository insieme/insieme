/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
