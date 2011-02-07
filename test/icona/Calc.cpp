/*
 * runCalc.cpp
 *
 *  Created on: Jan 18, 2011
 *      Author: c7031006
 */
#include <iostream>
#include <fstream>
#include <map>
#include <vector>
#include <string>
#include <cmath>
#include <ctime>
#include <omp.h>

#include "petsc.h"

using namespace std;

#include "config.h"
#include "Node.h"
#include "Element.h"
#include "Material.h"
#include "Step.h"
#include "ElemHC8A8E9.h"
#include "FETask.h"

#define TOL_U 0.0001
#define TOL_F 0.0001
#define MAX_ITERATIONS 50
#define N_ITER_INC_DELTA 0.01
#define N_ITER_RUNNING 1
#define N_ITER_FINISHED 2
#define N_ITER_FAILED 3

string doubleToStr( double nr) {
	stringstream ssOut;

	ssOut << nr;
	return ssOut.str();
}

void appendToLogFile( string message, string lfPathName) {
	ofstream file;

	file.open( lfPathName.c_str(), ios::app);

	file << message;

	file.close();
}

 //----------------------------------------
int runCalc( PetscInt rank, PetscInt size, FETask &feJob) {
	int maxIter=MAX_ITERATIONS;			//maximum iterations
	double Utol=TOL_U;					//displacement tolerance
	double Ftol=TOL_F;					//force tolerance

	int newtonIterStatus;		//status of iteration
	int iterations;				//number of iterations

	int i, rc;
	MPI_Status status;

	int errorNr=0;

	double Lambda, lamEnd, lamInc;

	string fname, logMessage;

	struct tm *timeinfo;
	time_t rawTimeStart=feJob.getStartTime();;
	time_t rawTimeInterim=feJob.getInterimTime();
	time_t rawTimeEnd;

	PetscScalar interiorWork, maxU, maxG, sum1, sum2, critF, critU, critE;
	PetscInt numDOF, nrBC;
	PetscInt *rows;
	PetscScalar *values;
	PetscViewer viewer;

	PetscErrorCode ierr;

	bool success=false;

	if( rank==0) {
		time( &rawTimeEnd);
		timeinfo=localtime( &rawTimeEnd);

		logMessage="------------------------------------------------------------\n";
		logMessage.append( "starting analyis @ ");
		logMessage.append( asctime( timeinfo));
		logMessage.append( "T=");
		logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeStart)).c_str());
		logMessage.append( "s\nusing ");
		logMessage.append( FETask::intToStr( feJob.getOmpNrThreads()));
		logMessage.append( " threads for OpenMP\npreparing data ... ");

		PetscPrintf( PETSC_COMM_WORLD, "%s\n", logMessage.c_str());
		appendToLogFile( logMessage, feJob.getLogFileName());

		feJob.renumberNodeNrsElements();		//resorting Nodes and Elements

		//if given set initial temperature
		if( feJob.getNrOfInitialTemperatureNodes()>0) {
			feJob.setInitialTemperature();
		}

		numDOF=feJob.getNrOfNodes()*feJob.getDOFPerNode();
		//printf( "%d -- numDOF -- %d\n", rank, numDOF);

		for( i=1; i<size; i++) {
			rc=MPI_Send( &numDOF, 1, MPI_INT, i, 101, PETSC_COMM_WORLD);
			//PetscPrintf( PETSC_COMM_WORLD, "sende an %d\n", i);
		}
	}
	else {
		rc=MPI_Recv( &numDOF, 1, MPI_INT, 0, 101, PETSC_COMM_WORLD, &status);
		//printf( "%d empfange %d\n", rank, numDOF);
	}

	PetscBarrier( (PetscObject) PETSC_NULL);

	Mat Kt;						//global stiffnessmatrix
	//ierr=MatCreateSeqAIJ( PETSC_COMM_WORLD, numDOF, numDOF, 81, PETSC_NULL, &Kt); CHKERRQ( ierr);
	ierr=MatCreateMPIAIJ( PETSC_COMM_WORLD, PETSC_DECIDE, PETSC_DECIDE, numDOF, numDOF, 81, PETSC_NULL, 81, PETSC_NULL, &Kt); CHKERRQ( ierr);
	ierr=MatSetFromOptions( Kt); CHKERRQ( ierr);

	Vec uT;						//total displacement
	//ierr=VecCreateSeq( PETSC_COMM_WORLD, numDOF, &uT); CHKERRQ( ierr);
	ierr=VecCreateMPI( PETSC_COMM_WORLD, PETSC_DECIDE, numDOF, &uT); CHKERRQ( ierr);
	ierr=VecSetFromOptions( uT); CHKERRQ( ierr);

	Vec Du;						//incremental displacement
	//ierr=VecCreateSeq( PETSC_COMM_WORLD, numDOF, &Du); CHKERRQ( ierr);
	ierr=VecCreateMPI( PETSC_COMM_WORLD, PETSC_DECIDE, numDOF, &Du); CHKERRQ( ierr);
	ierr=VecSetFromOptions( Du); CHKERRQ( ierr);

	Vec du;						//iterative displacement
	//ierr=VecCreateSeq( PETSC_COMM_WORLD, numDOF, &du); CHKERRQ( ierr);
	ierr=VecCreateMPI( PETSC_COMM_WORLD, PETSC_DECIDE, numDOF, &du); CHKERRQ( ierr);
	ierr=VecSetFromOptions( du); CHKERRQ( ierr);

	Vec fin;					//internal force
	//ierr=VecCreateSeq( PETSC_COMM_WORLD, numDOF, &fin); CHKERRQ( ierr);
	ierr=VecCreateMPI( PETSC_COMM_WORLD, PETSC_DECIDE, numDOF, &fin); CHKERRQ( ierr);
	ierr=VecSetFromOptions( fin); CHKERRQ( ierr);

	Vec f;						//force
	//ierr=VecCreateSeq( PETSC_COMM_WORLD, numDOF, &f); CHKERRQ( ierr);
	ierr=VecCreateMPI( PETSC_COMM_WORLD, PETSC_DECIDE, numDOF, &f); CHKERRQ( ierr);
	ierr=VecSetFromOptions( f); CHKERRQ( ierr);

	Vec fex;					//external force
	//ierr=VecCreateSeq( PETSC_COMM_WORLD, numDOF, &fex); CHKERRQ( ierr);
	ierr=VecCreateMPI( PETSC_COMM_WORLD, PETSC_DECIDE, numDOF, &fex); CHKERRQ( ierr);
	ierr=VecSetFromOptions( fex); CHKERRQ( ierr);

	Vec Dfex;					//incremental force
	//ierr=VecCreateSeq( PETSC_COMM_WORLD, numDOF, &Dfex); CHKERRQ( ierr);
	ierr=VecCreateMPI( PETSC_COMM_WORLD, PETSC_DECIDE, numDOF, &Dfex); CHKERRQ( ierr);
	ierr=VecSetFromOptions( Dfex); CHKERRQ( ierr);

	Vec outVec;					//residuum
	//ierr=VecCreateSeq( PETSC_COMM_WORLD, numDOF, &outVec); CHKERRQ( ierr);
	ierr=VecCreateMPI( PETSC_COMM_WORLD, PETSC_DECIDE, numDOF, &outVec); CHKERRQ( ierr);
	ierr=VecSetFromOptions( outVec); CHKERRQ( ierr);

	Vec tmpVec;					//temporary vector
	//ierr=VecCreateSeq( PETSC_COMM_WORLD, numDOF, &tmpVec); CHKERRQ( ierr);
	ierr=VecCreateMPI( PETSC_COMM_WORLD, PETSC_DECIDE, numDOF, &tmpVec); CHKERRQ( ierr);
	ierr=VecSetFromOptions( tmpVec); CHKERRQ( ierr);

	Vec uTkopie, Dukopie;
	VecScatter vecscatter1, vecscatter2;

	KSP ksp;

	if( rank==0) {
		time( &rawTimeEnd);
		timeinfo=localtime( &rawTimeEnd);

		logMessage="t=";
		logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeInterim)).c_str());
		logMessage.append( "s\n------------------------------------------------------------\n");
		logMessage.append( "starting analysis ...\nT=");
		logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeStart)).c_str());
		logMessage.append( "s\npreparing step-data ...");

		PetscPrintf( PETSC_COMM_WORLD, "%s\n", logMessage.c_str());
		appendToLogFile( logMessage, feJob.getLogFileName());

		rawTimeInterim=rawTimeEnd;

		//add general boundary conditions to boundary conditions of current step and renumber
		feJob.mergeBoundaryConditions();
		nrBC=feJob.getNrOfBoundaries();

		PetscMalloc( nrBC*sizeof(PetscInt), &rows);
		PetscMalloc( nrBC*sizeof(PetscScalar), &values);

		for( i=0; i<nrBC; i++) {
			rows[i]=feJob.rows[i];
			values[i]=feJob.values[i];
		}
	}

	PetscBarrier( (PetscObject) PETSC_NULL);
	//initialize vectors
	ierr=VecZeroEntries( Dfex); CHKERRQ( ierr);
	ierr=VecZeroEntries( fex); CHKERRQ( ierr);
	ierr=VecZeroEntries( du); CHKERRQ( ierr);
	ierr=VecZeroEntries( uT); CHKERRQ( ierr);

	if( rank==0) {
		feJob.applyCLoads( f);
	}

	PetscBarrier( (PetscObject) PETSC_NULL);
	ierr=VecAssemblyBegin( f);
	ierr=VecAssemblyEnd( f);

	//loop load
	Lambda=0;

	if( rank==0) {
		lamEnd=feJob.getLoadFactor();
		lamInc=feJob.getLoadFactorInc();

		for( i=1; i<size; i++) {
			rc=MPI_Send( &lamEnd, 1, MPI_DOUBLE, i, 102, PETSC_COMM_WORLD);
			rc=MPI_Send( &lamInc, 1, MPI_DOUBLE, i, 103, PETSC_COMM_WORLD);

			rc=MPI_Send( &nrBC, 1, MPI_INT, i, 104, PETSC_COMM_WORLD);
			rc=MPI_Send( rows, nrBC, MPI_INT, i, 105, PETSC_COMM_WORLD);
			rc=MPI_Send( values, nrBC, MPI_DOUBLE, i, 106, PETSC_COMM_WORLD);
			//PetscPrintf( PETSC_COMM_WORLD, "sende an %d\n", i);
		}
	}
	else {
		rc=MPI_Recv( &lamEnd, 1, MPI_DOUBLE, 0, 102, PETSC_COMM_WORLD, &status);
		//printf( "%d empfange End %e\n", rank, lamEnd);
		rc=MPI_Recv( &lamInc, 1, MPI_DOUBLE, 0, 103, PETSC_COMM_WORLD, &status);
		//printf( "%d empfange Inc %e\n", rank, lamInc);

		rc=MPI_Recv( &nrBC, 1, MPI_DOUBLE, 0, 104, PETSC_COMM_WORLD, &status);
		//printf( "%d empfange nrBC %d\n", rank, nrBC);

		PetscMalloc( nrBC*sizeof(PetscInt), &rows);
		PetscMalloc( nrBC*sizeof(PetscScalar), &values);

		rc=MPI_Recv( rows, nrBC, MPI_INT, 0, 105, PETSC_COMM_WORLD, &status);
		rc=MPI_Recv( values, nrBC, MPI_DOUBLE, 0, 106, PETSC_COMM_WORLD, &status);
	}

	PetscBarrier( (PetscObject) PETSC_NULL);

	//  load load load load load load load load load load load load load load load load load load load load
	// load load load load load load load load load load load load load load load load load load load load
	//load load load load load load load load load load load load load load load load load load load load
	newtonIterStatus=N_ITER_RUNNING;

	while( ( newtonIterStatus!=N_ITER_FAILED) && ( Lambda<lamEnd)) {
		Lambda+=lamInc;

		if( Lambda>lamEnd) {
			Lambda=lamEnd;
		}

		if( abs( Lambda-lamEnd)<=( lamInc*N_ITER_INC_DELTA)) {
			Lambda=lamEnd;
		}

		if( rank==0) {
			time( &rawTimeEnd);
			timeinfo=localtime( &rawTimeEnd);

			logMessage="t=";
			logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeInterim)).c_str());
			logMessage.append( "s\n------------------------------------------------------------\n");
			logMessage.append( "lambda=");
			logMessage.append( FETask::doubleToStr( Lambda).c_str());
			logMessage.append( "\nT=");
			logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeStart)).c_str());
			logMessage.append( "s\nstarting predictor - assembling ... ");

			PetscPrintf( PETSC_COMM_WORLD, "%s\n", logMessage.c_str());
			appendToLogFile( logMessage, feJob.getLogFileName());

			rawTimeInterim=rawTimeEnd;
		}

		PetscBarrier( (PetscObject) PETSC_NULL);
		//ierr=MatZeroEntries( Kt); CHKERRQ( ierr);
		ierr=MatDestroy( Kt); CHKERRQ( ierr);
	        ierr=MatCreateMPIAIJ( PETSC_COMM_WORLD, PETSC_DECIDE, PETSC_DECIDE, numDOF, numDOF, 81, PETSC_NULL, 81, PETSC_NULL, &Kt); CHKERRQ( ierr);
        	ierr=MatSetFromOptions( Kt); CHKERRQ( ierr);

		ierr=VecZeroEntries( Du); CHKERRQ( ierr);
		ierr=VecAssemblyBegin( Du); CHKERRQ( ierr);			//notwendig?
		ierr=VecAssemblyEnd( Du); CHKERRQ( ierr);

		ierr=VecZeroEntries( fin); CHKERRQ( ierr);
		ierr=VecAssemblyBegin( fin); CHKERRQ( ierr);		//notwendig?
		ierr=VecAssemblyEnd( fin); CHKERRQ( ierr);

		VecScatterCreateToZero( uT, &vecscatter1, &uTkopie);
		VecScatterBegin( vecscatter1, uT, uTkopie, INSERT_VALUES, SCATTER_FORWARD);
		VecScatterEnd( vecscatter1, uT, uTkopie, INSERT_VALUES, SCATTER_FORWARD);

		VecScatterCreateToZero( Du, &vecscatter2, &Dukopie);
		VecScatterBegin( vecscatter2, Du, Dukopie, INSERT_VALUES, SCATTER_FORWARD);
		VecScatterEnd( vecscatter2, Du, Dukopie, INSERT_VALUES, SCATTER_FORWARD);

		if( rank==0) {
			feJob.assembleStiffnessMatrix( Dukopie, uTkopie, Kt, fin);
		}

		PetscBarrier( (PetscObject) PETSC_NULL);
		ierr=MatAssemblyBegin( Kt, MAT_FINAL_ASSEMBLY); CHKERRQ( ierr);
		ierr=MatAssemblyEnd( Kt, MAT_FINAL_ASSEMBLY); CHKERRQ( ierr);
		ierr=VecAssemblyBegin( fin); CHKERRQ( ierr);
		ierr=VecAssemblyEnd( fin); CHKERRQ( ierr);

		//*********************************************
		//PetscViewerASCIIOpen( PETSC_COMM_WORLD, "K-vor", &viewer);
		//MatView( Kt, viewer);
		//PetscViewerDestroy( viewer);
		//*********************************************

		VecScatterDestroy( vecscatter1);
		VecDestroy( uTkopie);

		VecScatterDestroy( vecscatter2);
		VecDestroy( Dukopie);

		//copy of f time lambda
		ierr=VecCopy( f, Dfex); CHKERRQ( ierr);
		ierr=VecScale( Dfex, Lambda); CHKERRQ( ierr);
		ierr=VecAXPY( Dfex, (-1.0), fin); CHKERRQ( ierr);

		//apply boundary conditions
		ierr=MatZeroRows( Kt, nrBC, rows, 1); CHKERRQ( ierr);
		ierr=VecSetValues( Dfex, nrBC, rows, values, INSERT_VALUES); CHKERRQ( ierr);

		//PetscBarrier( (PetscObject) PETSC_NULL);
		ierr=MatAssemblyBegin( Kt, MAT_FINAL_ASSEMBLY); CHKERRQ( ierr);
		ierr=MatAssemblyEnd( Kt, MAT_FINAL_ASSEMBLY); CHKERRQ( ierr);
		ierr=VecAssemblyBegin( Dfex); CHKERRQ( ierr);
		ierr=VecAssemblyEnd( Dfex); CHKERRQ( ierr);

		//*********************************************
		//PetscViewerASCIIOpen( PETSC_COMM_WORLD, "K-nach", &viewer);
		//MatView( Kt, viewer);
		//PetscViewerDestroy( viewer);
		//*********************************************

		if( rank==0) {
			time( &rawTimeEnd);
			timeinfo=localtime( &rawTimeEnd);

			logMessage="t=";
			logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeInterim)).c_str());
			logMessage.append( "s\npredictor - solving ... ");

			PetscPrintf( PETSC_COMM_WORLD, "%s\n", logMessage.c_str());
			appendToLogFile( logMessage, feJob.getLogFileName());

			rawTimeInterim=rawTimeEnd;
		}

		PetscBarrier( (PetscObject) PETSC_NULL);
		//solve first time
		ierr=KSPCreate( PETSC_COMM_WORLD, &ksp); CHKERRQ( ierr);
		ierr=KSPSetFromOptions( ksp); CHKERRQ( ierr);
		ierr=KSPSetOperators( ksp, Kt, Kt, DIFFERENT_NONZERO_PATTERN); CHKERRQ( ierr);
		ierr=KSPSolve( ksp, Dfex, Du); CHKERRQ( ierr);

		//interior work
		ierr=VecDot( Du, Dfex, &interiorWork); CHKERRQ( ierr);

		iterations=1;
		newtonIterStatus=N_ITER_RUNNING;

		//  newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton
		// newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton
		//newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton   newton
		while( newtonIterStatus==N_ITER_RUNNING) {
			//PetscBarrier( (PetscObject) PETSC_NULL);

			if( rank==0) {
				time( &rawTimeEnd);
				timeinfo=localtime( &rawTimeEnd);

				logMessage="t=";
				logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeInterim)).c_str());
				logMessage.append( "s\n------------------------------------------------------------\n");
				logMessage.append( "iteration ");
				logMessage.append( FETask::intToStr( iterations).c_str());
				logMessage.append( "/");
				logMessage.append( FETask::intToStr( maxIter).c_str());
				logMessage.append( "\nT=");
				logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeStart)).c_str());
				logMessage.append( "s\nassembling ... ");

				PetscPrintf( PETSC_COMM_WORLD, "%s\n", logMessage.c_str());
				appendToLogFile( logMessage, feJob.getLogFileName());

				rawTimeInterim=rawTimeEnd;
			}

			PetscBarrier( (PetscObject) PETSC_NULL);
			//ierr=MatZeroEntries( Kt); CHKERRQ( ierr);
			ierr=MatDestroy( Kt); CHKERRQ( ierr);
		        ierr=MatCreateMPIAIJ( PETSC_COMM_WORLD, PETSC_DECIDE, PETSC_DECIDE, numDOF, numDOF, 81, PETSC_NULL, 81, PETSC_NULL, &Kt); CHKERRQ( ierr);
		        ierr=MatSetFromOptions( Kt); CHKERRQ( ierr);

			ierr=VecZeroEntries( fin); CHKERRQ( ierr);
			ierr=VecAssemblyBegin( fin); CHKERRQ( ierr);
			ierr=VecAssemblyEnd( fin); CHKERRQ( ierr);

			VecScatterCreateToZero( uT, &vecscatter1, &uTkopie);
			VecScatterBegin( vecscatter1, uT, uTkopie, INSERT_VALUES, SCATTER_FORWARD);
			VecScatterEnd( vecscatter1, uT, uTkopie, INSERT_VALUES, SCATTER_FORWARD);

			VecScatterCreateToZero( Du, &vecscatter2, &Dukopie);
			VecScatterBegin( vecscatter2, Du, Dukopie, INSERT_VALUES, SCATTER_FORWARD);
			VecScatterEnd( vecscatter2, Du, Dukopie, INSERT_VALUES, SCATTER_FORWARD);
			
			if( rank==0) {
				feJob.assembleStiffnessMatrix( Dukopie, uTkopie, Kt, fin);
			}

			//if( rank==0) {
			//	feJob.assembleStiffnessMatrix( Du, uT, Kt, fin);
			//}

			PetscBarrier( (PetscObject) PETSC_NULL);
			ierr=MatAssemblyBegin( Kt, MAT_FINAL_ASSEMBLY); CHKERRQ( ierr);
			ierr=MatAssemblyEnd( Kt, MAT_FINAL_ASSEMBLY); CHKERRQ( ierr);
			ierr=VecAssemblyBegin( fin); CHKERRQ( ierr);
			ierr=VecAssemblyEnd( fin); CHKERRQ( ierr);

			VecScatterDestroy( vecscatter1);
			VecDestroy( uTkopie);

			VecScatterDestroy( vecscatter2);
			VecDestroy( Dukopie);

			ierr=VecCopy( f, fex); CHKERRQ( ierr);
			ierr=VecScale( fex, Lambda); CHKERRQ( ierr);
			ierr=VecAXPY( fex, (-1.0), fin); CHKERRQ( ierr);

			//apply BoundaryConditions
			ierr=MatZeroRows( Kt, nrBC, rows, 1); CHKERRQ( ierr);
			ierr=VecSetValues( fex, nrBC, rows, values, INSERT_VALUES); CHKERRQ( ierr);

			//PetscBarrier( (PetscObject) PETSC_NULL);
			ierr=MatAssemblyBegin( Kt, MAT_FINAL_ASSEMBLY); CHKERRQ( ierr);
			ierr=MatAssemblyEnd( Kt, MAT_FINAL_ASSEMBLY); CHKERRQ( ierr);
			ierr=VecAssemblyBegin( fex); CHKERRQ( ierr);
			ierr=VecAssemblyEnd( fex); CHKERRQ( ierr);

			if( rank==0) {
				time( &rawTimeEnd);
				timeinfo=localtime( &rawTimeEnd);

				logMessage="t=";
				logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeInterim)).c_str());
				logMessage.append( "s\niteration - solving ... ");

				PetscPrintf( PETSC_COMM_WORLD, "%s\n", logMessage.c_str());
				appendToLogFile( logMessage, feJob.getLogFileName());

				rawTimeInterim=rawTimeEnd;
			}

			PetscBarrier( (PetscObject) PETSC_NULL);
			//solve
			//ierr=KSPCreate( PETSC_COMM_WORLD, &ksp); CHKERRQ( ierr);
			ierr=KSPSetFromOptions( ksp); CHKERRQ( ierr);
			ierr=KSPSetOperators( ksp, Kt, Kt, DIFFERENT_NONZERO_PATTERN); CHKERRQ( ierr);
			ierr=KSPSolve( ksp, fex, du); CHKERRQ( ierr);

			if( rank==0) {
				time( &rawTimeEnd);
				timeinfo=localtime( &rawTimeEnd);

				logMessage="t=";
				logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeInterim)).c_str());
				logMessage.append( "s\niteration - checking solution ...\n");

				PetscPrintf( PETSC_COMM_WORLD, "%s\n", logMessage.c_str());
				appendToLogFile( logMessage, feJob.getLogFileName());

				rawTimeInterim=rawTimeEnd;
			}

			PetscBarrier( (PetscObject) PETSC_NULL);
			//update incremental displacements
			ierr=VecAXPY( Du, 1, du); CHKERRQ( ierr);

			//check convergence
			ierr=VecCopy( du, tmpVec); CHKERRQ( ierr);
			ierr=VecAbs( tmpVec); CHKERRQ( ierr);
			ierr=VecMax( tmpVec, PETSC_NULL, &maxU); CHKERRQ( ierr);

			ierr=VecCopy( fex, tmpVec); CHKERRQ( ierr);
			ierr=VecAbs( tmpVec); CHKERRQ( ierr);
			ierr=VecMax( tmpVec, PETSC_NULL, &maxG); CHKERRQ( ierr);

			ierr=VecCopy( fin, tmpVec); CHKERRQ( ierr);
			ierr=VecAbs( tmpVec); CHKERRQ( ierr);
			ierr=VecSum( tmpVec, &sum1); CHKERRQ( ierr);

			ierr=VecCopy( f, tmpVec); CHKERRQ( ierr);
			ierr=VecScale( tmpVec, Lambda); CHKERRQ( ierr);
			ierr=VecSum( tmpVec, &sum2); CHKERRQ( ierr);

			critF=maxG / ((sum1+sum2) / numDOF);

			ierr=VecCopy( Du, tmpVec); CHKERRQ( ierr);
			ierr=VecAbs( tmpVec); CHKERRQ( ierr);
			ierr=VecMax( tmpVec, PETSC_NULL, &critU); CHKERRQ( ierr);

			critU=maxU / critU;

			ierr=VecDot( du, fex, &critE); CHKERRQ( ierr);

			if( rank==0) {
				logMessage="\tcritE:\t";
				logMessage.append( FETask::doubleToStr( critE));
				logMessage.append( "\n\tcritU:\t");
				logMessage.append( FETask::doubleToStr( critU));
				logMessage.append( "\n\tcritF:\t");
				logMessage.append( FETask::doubleToStr( critF));
				logMessage.append( "\n");

				PetscPrintf( PETSC_COMM_WORLD, "%s\n", logMessage.c_str());
				appendToLogFile( logMessage, feJob.getLogFileName());
			}

			PetscBarrier( (PetscObject) PETSC_NULL);

			if( (critU<Utol) && (critF<Ftol)) {
				newtonIterStatus=N_ITER_FINISHED;
			}
			if( newtonIterStatus==N_ITER_RUNNING) {
				iterations++;

				if( iterations>maxIter) {
					newtonIterStatus=N_ITER_FAILED;
				}
			}
		} //loop newton iteration end

		PetscBarrier( (PetscObject) PETSC_NULL);

		if( newtonIterStatus!=N_ITER_FAILED) {
			//update
			ierr=VecAXPY( uT, 1, Du); CHKERRQ( ierr);

			//save solution
			fname="solution-";
			fname.append( doubleToStr( Lambda));
			fname.append( ".data");

			//write solution-file
			VecScatterCreateToZero( uT, &vecscatter1, &uTkopie);
			VecScatterBegin( vecscatter1, uT, uTkopie, INSERT_VALUES, SCATTER_FORWARD);
			VecScatterEnd( vecscatter1, uT, uTkopie, INSERT_VALUES, SCATTER_FORWARD);

			PetscViewerASCIIOpen( PETSC_COMM_SELF, fname.c_str(), &viewer);
			VecView( uTkopie, viewer);
			PetscViewerDestroy( viewer);

			VecScatterDestroy( vecscatter1);
			VecDestroy( uTkopie);

			if( rank==0) {
				logMessage="solution found - look at --> ";
				logMessage.append( fname);
				logMessage.append( " <--\n");
			}
		}
		else {
			if( rank==0) {
				logMessage="!!! no convergence - failure --> stopped !!!\n";
			}
		}

		PetscBarrier( (PetscObject) PETSC_NULL);

		if( rank==0) {
			time( &rawTimeEnd);
			timeinfo=localtime( &rawTimeEnd);

			logMessage.append( "t=");
			logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeInterim)).c_str());
			logMessage.append( "s\n------------------------------------------------------------\n");

			PetscPrintf( PETSC_COMM_WORLD, "%s\n", logMessage.c_str());
			appendToLogFile( logMessage, feJob.getLogFileName());

			rawTimeInterim=rawTimeEnd;
		}

		PetscBarrier( (PetscObject) PETSC_NULL);
	}//loop load end

	if( newtonIterStatus!=N_ITER_FAILED) {
		success=true;

		/*Node tmpNode;
		PetscInt indices[3];
		PetscScalar xyzU[3];

		i=0;

		appendToLogFile( "\n-------------------------\n");

		for( nodesIT=nodes.begin(); nodesIT!=nodes.end(); nodesIT++) {
			tmpNode=(*nodesIT);

			for( j=0; j<3; j++) {
				indices[j]=i;
				i++;
			}

			ierr=VecGetValues( uT, 3, indices, xyzU); CHKERRQ( ierr);

			appendToLogFile( FETask::intToStr( tmpNode.getOrigNodeNr()));
			appendToLogFile( "\t");
			appendToLogFile( FETask::intToStr( tmpNode.getNodeNr()));
			appendToLogFile( "\t");
			appendToLogFile( FETask::doubleToStr( xyzU[0]));
			appendToLogFile( "\t");
			appendToLogFile( FETask::doubleToStr( xyzU[1]));
			appendToLogFile( "\t");
			appendToLogFile( FETask::doubleToStr( xyzU[2]));
			appendToLogFile( "\n");

			for( j=0; j<3; j++) {
				tmpNode.nodeCoords[j]=xyzU[j];
			}

			nodesU.push_back( tmpNode);
		}*/
	}

	if( rank==0) {
		time( &rawTimeEnd);
		timeinfo=localtime( &rawTimeEnd);

		logMessage="t=";
		logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeInterim)).c_str());
		logMessage.append( "s\n------------------------------------------------------------\n");
		logMessage.append( "analysis ");

		if( success) {
			logMessage.append( "finished");
			errorNr=CALC_SUCCESS;
		}
		else {
			logMessage.append( "failed");
			errorNr=CALC_FAIL;
		}

		logMessage.append( "\nT=");
		logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeStart)).c_str());
		logMessage.append( "s\n\ndone ...\n");

		PetscPrintf( PETSC_COMM_WORLD, "%s\n", logMessage.c_str());
		appendToLogFile( logMessage, feJob.getLogFileName());
	}

	PetscBarrier( (PetscObject) PETSC_NULL);

	ierr=MatDestroy( Kt); CHKERRQ( ierr);	

	return errorNr;
}
