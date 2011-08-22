/*
 * openCLCallerQMHS4.h
 *
 *  Created on: 02.03.2011
 *      Author: manfred
 */

#ifndef OPENCLCALLERQMHS4_H_
#define OPENCLCALLERQMHS4_H_

#include "CL/cl.h"
#include "Element.h"
#include "config.h"
#include "Material.h"

#include "ght_hash_table.h"

struct Element;
struct CLElemQMHS4 {
	int DOFPerNode;
	double xi;
	double xi1[4];
	double xi2[4];
	double dNdxi[2*4*4];
	ght_hash_table_t* icoShellSections; // std::map<const char*, icoSSData* >
	ght_hash_table_t* issSize;	// std::map<const char*
	ght_hash_table_t* elements; // std::map< int, Element>
	ght_hash_table_t* elsets; // std::map<const char*, int* >
	ght_hash_table_t* elsetSize; // std::map<const char*, size_t >

	size_t* nrOfOrder;
	
//	ght_hash_table_t* ShInter; // std::map< int, int4>
	int elast;
	
	ght_hash_table_t* mat; //std::map<const char*, Material>
	unsigned int numShellSections;
	int sumNumLay;
	int elementsused;
	int seccount;
	double* XG;
	double* u;
	int* indexPerCSS;
	double* sig;
	double* str;
/*** temporaries as pointers ***/
	double* T3Ptr;
	double* sigPtr;
	double* strPtr;
	double* Dsig;
	double* Dstr;

	double* invF;
	double* invFT;
	double* G;
	double* P11q;
	double* P22;
	double* P21;
	int* interSec;
	int* numLayArray;
	double* layProp;
	int* layPropint;
	double* matProp;
	int* matID;
	double* LBase;
	double* T3;
	double* fe1q;
	double* fe2;
	double* fs;
	int* CSScountArray;
//	int* numDOFEI;
	int* dof;
	cl_command_queue* comandQueues;
	size_t* numElemPerCalcSize;
	cl_context* clContextArray;
	cl_device_id* clDevices;
	cl_mem* Tcl;
	cl_mem* xcl;
	cl_mem* Hcl;
	cl_mem* dcl;
	cl_mem* t1cl;
	cl_mem* t2cl;
	cl_mem* Nsigcl;
	cl_mem* Nstrcl;
	cl_mem* Dcl;
	cl_mem* detJ0cl;
	cl_mem* fecl;
	cl_mem* Fcl;
	cl_mem* Kgcl;
	cl_mem* Pcl;
	cl_mem* numLayArraycl;
	cl_mem* indexPerCSScl;
	cl_mem* CSScountArraycl;
	cl_mem* interSeccl;
	cl_mem* XGcl;
	cl_mem* uGcl;

	cl_mem* sigGcl;
	cl_mem* strGcl;
	cl_mem* LBaseGcl;
	cl_mem* layPropcl;
	cl_mem* layPropintcl;
	cl_mem* matIDcl;
	cl_mem* matPropcl;
	cl_mem* xi1cl;
	cl_mem* xi2cl;
	cl_mem* dNdxicl;

	cl_mem* Ktcl;
	cl_mem* invFGcl;
	cl_mem* invFTGcl;
	cl_mem* GGcl;
	cl_mem* P11qcl;
	cl_mem* P22Gcl;
	cl_mem* P21Gcl;
	cl_mem* T3Ecl;
	cl_mem* fe1qcl;
	cl_mem* fe2cl;
	cl_mem* fscl;

	cl_kernel* clKernelFirstPart;
	cl_kernel* clKernelSecondPart;
	cl_kernel* clKernelThirdPart;
	cl_mem* fincl;

	cl_mem* numDOFEIcl;
	cl_mem* dofcl;
	size_t** numElemPerCalc;
	int* numCalcsPerDev;
	int* elemCalcPerDev;
	size_t** localWork;


	///////////////////////////////
	cl_uint clDevicesSize;
};

void constructCLElemQMHS4(struct CLElemQMHS4* cLElemQMHS4);
void destructCLElemQMHS4(struct CLElemQMHS4* cLElemQMHS4);

int calcQMHS4(struct CLElemQMHS4* cLElemQMHS4, double* Kt,double* fin,int* elementsIndex,int* numDOFperindex);
int initCalcQMHS4firstTime(struct CLElemQMHS4* cLElemQMHS4, const int numThreads,const int numKernel);
ght_hash_table_t* setElementsZero(const size_t nElems);
const char* print_cl_errstring(const cl_int err);

#endif /* OPENCLCALLERQMHS4_H_ */
