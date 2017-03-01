#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include "mpi.h"
#include <iostream>

#include "RestartIO_GLEAN.h"

using namespace std;

int main (int argc, char * argv[]) 
{
	char* fname = 0;	
	char* buf = 0;
	int numtasks, myrank, status;
	MPI_File fh;

	status = MPI_Init(&argc, &argv);
    if ( MPI_SUCCESS != status)
    {
        printf(" Error Starting the MPI Program \n");
        MPI_Abort(MPI_COMM_WORLD, status);
    }

    MPI_Comm_size(MPI_COMM_WORLD, &numtasks);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

	if (argc != 3)
    {
        printf (" USAGE <exec> <particles/rank>  < Full file path>  ");
        MPI_Abort(MPI_COMM_WORLD, -1);
    }
    
	int64_t num_particles =  atoi(argv[1]);
	
	
	fname = (char*)malloc(strlen(argv[2]) +1);
	strncpy (fname, argv[2], strlen(argv[2]));
	fname[strlen(argv[2])] = '\0';
    
   
	RestartIO_GLEAN* rst = new RestartIO_GLEAN();
	
	rst->Initialize(MPI_COMM_WORLD);

	rst->PrintIOCoordInfo();

	rst->CreateCheckpoint (fname, num_particles);
    
	rst->Close();
    
    int64_t my_particles;

	my_particles = rst->OpenRestart (fname);
    
    if (my_particles != num_particles)
	{
		cout << " Particles Counts Do NOT MATCH " <<  endl;
		MPI_Abort(MPI_COMM_WORLD, -1);
	}
   
	rst->Close();

   
	rst->Finalize();

	delete rst;
	rst = 0;
  
	MPI_Finalize();

	return 0;

}



