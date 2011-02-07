/*
 * ElemHC8A8E9.cpp
 *
 *  Created on: 25.06.2010
 *      Author: clemens
 */

#include <cmath>
#include <iostream>
#include <vector>
#include <map>

#include "petsc.h"

using namespace std;

#include "Element.h"
#include "Node.h"
#include "Material.h"
#include "ElemHC8A8E9.h"

#define NUMDOFN 3		//number of dof per node
#define NUMNODES 8		//number of nodes per element

extern "C" {
	void uel_( double *rhs, double *amatrx, double *energy, int *ndofel, double *props, int *nprops, double *coords, int *nnod, double *u, int *ndload, int *jdltyp, double *adlmag, double *predef, int *npredf);
}

ElemHC8A8E9::ElemHC8A8E9() {
}

ElemHC8A8E9::ElemHC8A8E9( Element &element, vector<Node> &nodes,  Vec &du_, Vec &ut_, Material &material, map< int, elData> &allKte) {
	PetscErrorCode ierr;
	int i, j, globalNr;

	double globalCoords[20][3];
	double props[3];
	double u[8][3];

	props[0]=material.getEModul();
	props[1]=material.getNue();
	props[2]=material.getAlphaT();
  
	PetscInt indices[3];
	PetscScalar a[3], b[3];

	int ndofel=24;
	int npros=3;
	int nnod=8;
	int ndload=1;

	int jdltype[ndload];
	jdltype[0]=0;

	double adlmag[ndload];
	adlmag[0]=0;

	int npredf=2;

	double predef[npredf*nnod];

	int nr=element.getElementNr();

	for( i=0; i<24; i++) {
		for( j=0; j<24; j++) {
			allKte[nr].kte[i][j]=0;
		}
		allKte[nr].rhs[i]=0;
	}

	for( i=0; i<NUMNODES; i++) {
		globalNr=element.getGlobalNrOfNodeNr( i);

		globalCoords[i][0]=nodes[globalNr].getX();
		globalCoords[i][1]=nodes[globalNr].getY();
		globalCoords[i][2]=nodes[globalNr].getZ();

	    predef[2*i]=nodes[globalNr].getTemperature();
	    predef[2*i+1]=nodes[globalNr].getInitialTemperature();

		for( j=0; j<3; j++) {
			indices[j]=NUMDOFN*globalNr + j;
		}

		#pragma omp critical
		{
			ierr=VecGetValues( ut_, 3, indices, a); //CHKERRV( ierr);
			//#pragma omp critical
			ierr|=VecGetValues( du_, 3, indices, b); //CHKERRV( ierr);
		}

		//CHKERRV( ierr);

		for( j=0; j<3; j++) {
			u[i][j]=a[j] + b[j];
		}
	}

	//uel_( &rhs[0], &kte[0][0], &energy, &ndofel, &props[0], &npros, &globalCoords[0][0], &nnod, &u[0][0], &ndload, &jdltype[0], &adlmag[0], &predef[0], &npredf);

	uel_( &allKte[nr].rhs[0], &allKte[nr].kte[0][0], &energy, &ndofel, &props[0], &npros, &globalCoords[0][0], &nnod, &u[0][0], &ndload, &jdltype[0], &adlmag[0], &predef[0], &npredf);

	for( i=0; i<24; i++) {
		allKte[nr].rhs[i]=allKte[nr].rhs[i]*(-1);
	}
}

ElemHC8A8E9::~ElemHC8A8E9() {
}
