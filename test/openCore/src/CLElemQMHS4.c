/*
 * CLElemQMHS4.cpp
 *
 *      Author: manfred
 */
#include <string.h>
#include "CL/cl.h"
#include <stdio.h>
//#include <cstdlib>
//#include <fstream>
#include <math.h>
#include <time.h>
#include "utils.h"
#include "CLElemQMHS4.h"
#include "futils.h"
#include "clutils.h"
//#include <boost/filesystem.hpp>
//#include <boost/lexical_cast.hpp>
#include <sys/time.h>

void constructCLElemQMHS4(struct CLElemQMHS4* cLElemQMHS4) {
	/*
	% sampling points:
	xi  = sqrt(1/3);
	xi1 = [xi -xi  -xi   xi];
	xi2 = [xi  xi  -xi  -xi];
	*/
	cLElemQMHS4->xi = sqrt(1.0/((double)3.0));
	cLElemQMHS4->xi1[0]=cLElemQMHS4->xi; cLElemQMHS4->xi1[1]=-1*cLElemQMHS4->xi; cLElemQMHS4->xi1[2]=-1*cLElemQMHS4->xi; cLElemQMHS4->xi1[3]=cLElemQMHS4->xi;
	cLElemQMHS4->xi2[0]=cLElemQMHS4->xi; cLElemQMHS4->xi2[1]=cLElemQMHS4->xi; cLElemQMHS4->xi2[2]=-1*cLElemQMHS4->xi; cLElemQMHS4->xi2[3]=-1*cLElemQMHS4->xi;

	/*
	% local derivatives of shape functions:
	dNdxi(1,:,1:4) = 0.25*[(1+xi2') -(1+xi2') -(1-xi2')  (1-xi2')]';
	dNdxi(2,:,1:4) = 0.25*[(1+xi1')  (1-xi1') -(1-xi1') -(1+xi1')]';
	*/
	for(int i=0;i<4;i++) {
		cLElemQMHS4->dNdxi[0*4*4+0*4+i]=0.25*(1+cLElemQMHS4->xi2[i]);
		cLElemQMHS4->dNdxi[0*4*4+1*4+i]=-0.25*(1+cLElemQMHS4->xi2[i]);
		cLElemQMHS4->dNdxi[0*4*4+2*4+i]=-0.25*(1-cLElemQMHS4->xi2[i]);
		cLElemQMHS4->dNdxi[0*4*4+3*4+i]=0.25*(1-cLElemQMHS4->xi2[i]);

		cLElemQMHS4->dNdxi[1*4*4+0*4+i]=0.25*(1+cLElemQMHS4->xi1[i]);
		cLElemQMHS4->dNdxi[1*4*4+1*4+i]=0.25*(1-cLElemQMHS4->xi1[i]);
		cLElemQMHS4->dNdxi[1*4*4+2*4+i]=-0.25*(1-cLElemQMHS4->xi1[i]);
		cLElemQMHS4->dNdxi[1*4*4+3*4+i]=-0.25*(1+cLElemQMHS4->xi1[i]);

		}

	// set pointers that are freed in constructor to NULL to ensure predictable behaviour in case they are not initialized
	cLElemQMHS4->interSec = NULL;
	cLElemQMHS4->numElemPerCalc = NULL;
	cLElemQMHS4->localWork = NULL;
	// allocated using realloc
	cLElemQMHS4->clDevices = NULL;
}

void destructCLElemQMHS4(struct CLElemQMHS4* cLElemQMHS4) {
	free(cLElemQMHS4->interSec);
	for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++){
		clReleaseMemObject(cLElemQMHS4->Tcl[i]);
		clReleaseMemObject(cLElemQMHS4->xcl[i]);
		clReleaseMemObject(cLElemQMHS4->Hcl[i]);
		clReleaseMemObject(cLElemQMHS4->dcl[i]);
		clReleaseMemObject(cLElemQMHS4->t1cl[i]);
		clReleaseMemObject(cLElemQMHS4->t2cl[i]);
		clReleaseMemObject(cLElemQMHS4->Nsigcl[i]);
		clReleaseMemObject(cLElemQMHS4->Nstrcl[i]);
		clReleaseMemObject(cLElemQMHS4->Dcl[i]);
		clReleaseMemObject(cLElemQMHS4->detJ0cl[i]);
		clReleaseMemObject(cLElemQMHS4->fecl[i]);
		clReleaseMemObject(cLElemQMHS4->Fcl[i]);
		clReleaseMemObject(cLElemQMHS4->Pcl[i]);
		clReleaseMemObject(cLElemQMHS4->Kgcl[i]);
		clReleaseMemObject(cLElemQMHS4->CSScountArraycl[i]);
		clReleaseMemObject(cLElemQMHS4->interSeccl[i]);
		clReleaseMemObject(cLElemQMHS4->LBaseGcl[i]);
		clReleaseMemObject(cLElemQMHS4->numDOFEIcl[i]);
		clReleaseMemObject(cLElemQMHS4->dofcl[i]);
		clReleaseMemObject(cLElemQMHS4->T3Ecl[i]);
		clReleaseMemObject(cLElemQMHS4->Ktcl[i]);
		clReleaseMemObject(cLElemQMHS4->fincl[i]);
		clReleaseMemObject(cLElemQMHS4->XGcl[i]);
		clReleaseMemObject(cLElemQMHS4->uGcl[i]);
		clReleaseMemObject(cLElemQMHS4->sigGcl[i]);
		clReleaseMemObject(cLElemQMHS4->strGcl[i]);
		clReleaseMemObject(cLElemQMHS4->invFGcl[i]);
		clReleaseMemObject(cLElemQMHS4->invFTGcl[i]);
		clReleaseMemObject(cLElemQMHS4->GGcl[i]);
		clReleaseMemObject(cLElemQMHS4->P11qcl[i]);
		clReleaseMemObject(cLElemQMHS4->P22Gcl[i]);
		clReleaseMemObject(cLElemQMHS4->P21Gcl[i]);
		clReleaseMemObject(cLElemQMHS4->fe1qcl[i]);
		clReleaseMemObject(cLElemQMHS4->fe2cl[i]);
		clReleaseMemObject(cLElemQMHS4->fscl[i]);

		free(cLElemQMHS4->numElemPerCalc[i]);
		free(cLElemQMHS4->localWork[i]);

		clReleaseContext(cLElemQMHS4->clContextArray[i]);
		clReleaseCommandQueue(cLElemQMHS4->comandQueues[i]);
		clReleaseKernel(cLElemQMHS4->clKernelFirstPart[i]);
		clReleaseKernel(cLElemQMHS4->clKernelSecondPart[i]);
		clReleaseKernel(cLElemQMHS4->clKernelThirdPart[i]);
		clReleaseMemObject(cLElemQMHS4->numLayArraycl[i]);
		clReleaseMemObject(cLElemQMHS4->indexPerCSScl[i]);
		clReleaseMemObject(cLElemQMHS4->layPropintcl[i]);
		clReleaseMemObject(cLElemQMHS4->matPropcl[i]);
		clReleaseMemObject(cLElemQMHS4->matIDcl[i]);
		clReleaseMemObject(cLElemQMHS4->layPropcl[i]);
		clReleaseMemObject(cLElemQMHS4->xi1cl[i]);
		clReleaseMemObject(cLElemQMHS4->xi2cl[i]);
		clReleaseMemObject(cLElemQMHS4->dNdxicl[i]);
	}

	// to do splitt elements in smaller parts biggest <=10000 elements
	free(cLElemQMHS4->Tcl);
	free(cLElemQMHS4->xcl);
	free(cLElemQMHS4->Hcl);
	free(cLElemQMHS4->dcl);
	free(cLElemQMHS4->t1cl);
	free(cLElemQMHS4->t2cl);
	free(cLElemQMHS4->Nsigcl);
	free(cLElemQMHS4->Nstrcl);
	free(cLElemQMHS4->Dcl);
	free(cLElemQMHS4->detJ0cl);
	free(cLElemQMHS4->fecl);
	free(cLElemQMHS4->Fcl);
	free(cLElemQMHS4->Pcl);
	free(cLElemQMHS4->Kgcl);
	free(cLElemQMHS4->CSScountArraycl);
	free(cLElemQMHS4->interSeccl);
	free(cLElemQMHS4->LBaseGcl);
	free(cLElemQMHS4->numDOFEIcl );
	free(cLElemQMHS4->dofcl);
	free(cLElemQMHS4->T3Ecl );
	free(cLElemQMHS4->Ktcl);
	free(cLElemQMHS4->fincl);
	free(cLElemQMHS4->XGcl);
	free(cLElemQMHS4->uGcl);
	free(cLElemQMHS4->sigGcl );
	free(cLElemQMHS4->strGcl );
	free(cLElemQMHS4->invFGcl);
	free(cLElemQMHS4->invFTGcl);
	free(cLElemQMHS4->GGcl);
	free(cLElemQMHS4->P11qcl);
	free(cLElemQMHS4->P22Gcl);
	free(cLElemQMHS4->P21Gcl);
	free(cLElemQMHS4->fe1qcl);
	free(cLElemQMHS4->fe2cl);
	free(cLElemQMHS4->fscl);

	free(cLElemQMHS4->numLayArraycl);
	free(cLElemQMHS4->indexPerCSScl);
	free(cLElemQMHS4->layPropintcl);
	free(cLElemQMHS4->matPropcl);
	free(cLElemQMHS4->matIDcl);
	free(cLElemQMHS4->layPropcl);
	free(cLElemQMHS4->xi1cl);
	free(cLElemQMHS4->xi2cl);
	free(cLElemQMHS4->dNdxicl);

	free(cLElemQMHS4->sig);
	free(cLElemQMHS4->str);

	free(cLElemQMHS4->clKernelFirstPart);
	free(cLElemQMHS4->clKernelSecondPart);
	free(cLElemQMHS4->clKernelThirdPart);
	free(cLElemQMHS4->clContextArray);
	free(cLElemQMHS4->comandQueues);

	free(cLElemQMHS4->numElemPerCalc);
	free(cLElemQMHS4->localWork);
	free(cLElemQMHS4->numElemPerCalcSize);
	free(cLElemQMHS4->elemCalcPerDev);
	free(cLElemQMHS4->numCalcsPerDev);

	free(cLElemQMHS4->indexPerCSS);
	free(cLElemQMHS4->numLayArray);
	free(cLElemQMHS4->layProp);
	free(cLElemQMHS4->layPropint);
	free(cLElemQMHS4->matProp);
	free(cLElemQMHS4->matID);
	free(cLElemQMHS4->T3);
	free(cLElemQMHS4->CSScountArray);
	free(cLElemQMHS4->dof);
}

ght_hash_table_t* setElementsZero(const size_t nElems) {
	ght_hash_table_t* elements = ght_create(nElems);
	int nodeNumbers = 1;
	// start counting at 1!!!!
	for(size_t i = 1; i <= nElems; ++i) {
		struct Element entry = createElement(i, "QMHS4", &nodeNumbers, 1);
		ght_insert(elements, &entry, sizeof(int), &i);
	}
//		elements[i] = createElement(i, "QMHS4", &nodeNumbers, 1);
	return elements;
}

int errorCodeOut(const cl_int error,const char* ownMessage) {

	if (error != CL_SUCCESS) {
		printf("%s\n%s\n", ownMessage, print_cl_errstring(error));
		return -1;
	}
	return 0;
}

int splitfunction(const int elements,const int maxElements,const int allign,size_t** numElemPerCalc,size_t* numElemPerCalcSize, const int device) {

	int max=maxElements/allign;
	int normMax=max*allign;
	int allignRest=elements%allign;
	int allignNumElements=elements/allign;
	int allignElements=allignNumElements*allign;
	int splittrest;
	int splitt;
//	std::cout << "in int" << std::endl;
	if(allignElements!=0) {
//		std::cout << "in int2" << std::endl;
		if(allignElements>normMax) {						// means maximum of memory is smaller than problem size
			splitt=allignElements/normMax;
			splittrest=allignElements%normMax;
			if(splittrest!=0) {
				splitt++;
			}
			if(allignRest==0) {
				(*numElemPerCalc) = (size_t*)malloc(splitt * sizeof(size_t));
				*numElemPerCalcSize = splitt;
			} else {
				(*numElemPerCalc) = (size_t*)malloc((splitt+1) * sizeof(size_t));
				*numElemPerCalcSize = splitt+1;
			}
			for(int i=0;i<splitt;i++) {
				(*numElemPerCalc)[i]=normMax;
//				std::cout << "numElemPerCalcloop " <<numElemPerCalc[device][i] << std::endl;
			}
			if(splittrest!=0) {
				(*numElemPerCalc)[splitt-1]=splittrest;
			}
		} else {
			// allignElements is smaller or same size as normMax
			if(allignRest==0) {
				(*numElemPerCalc) = (size_t*)malloc(1 * sizeof(size_t));
				*numElemPerCalcSize = 1;
			} else {
				(*numElemPerCalc) = (size_t*)malloc(2 * sizeof(size_t));
				*numElemPerCalcSize = 2;
			}
			(*numElemPerCalc)[0]=allignElements;
//			std::cout << "numElemPerCalc " <<numElemPerCalc[device][0] << std::endl;
		}
	} else {
		(*numElemPerCalc) = (size_t*)malloc(1 * sizeof(size_t));
	}
	return allignRest;
}

int initCalcQMHS4firstTime(struct CLElemQMHS4* cLElemQMHS4,  const int numElems,const int numKernel) {
	struct icoSSData* icoShellSectionsIT;
	char* icoShellSectionsKey;
	ght_iterator_t iterator;

	cl_platform_id* platforms;
	int elementNr;
	cl_int errcode;
	cl_int* err=0;
	int counterNumElem=0;
	int counterNumLay=0;
	int counterLay=0;
	char devName[512];
	char devVend[512];
	char devVers[512];
	char devVersion[512];
	cl_device_id* clDevicesGPU;
	cl_device_id* clDevicesCPU;
	cLElemQMHS4->numShellSections= ght_size(cLElemQMHS4->icoShellSections);
	cLElemQMHS4->sumNumLay=0;
	cLElemQMHS4->elementsused=0;
	cLElemQMHS4->seccount=0;

	for(icoShellSectionsIT = (struct icoSSData*)ght_first(cLElemQMHS4->icoShellSections, &iterator, (const void**)&icoShellSectionsKey);
			icoShellSectionsIT;
			icoShellSectionsIT = (struct icoSSData*)ght_next(cLElemQMHS4->icoShellSections, &iterator, (const void**)&icoShellSectionsKey)) {
//	for( icoShellSectionsIT=cLElemQMHS4->icoShellSections.begin(); icoShellSectionsIT!=cLElemQMHS4->icoShellSections.end(); icoShellSectionsIT++) {
		int numLay= *(int*)ght_get(cLElemQMHS4->issSize, sizeof(char*), icoShellSectionsKey);
//		int numLay=cLElemQMHS4->issSize[(*icoShellSectionsIT).first];
//printf("KEY: %s\n %lx ", icoShellSectionsKey, (unsigned long)cLElemQMHS4->elsets[ icoShellSectionsKey ]);
		int* elementsOfCSS=(int*)ght_get(cLElemQMHS4->elsets, sizeof(char*), icoShellSectionsKey);
		elementNr=elementsOfCSS[0];
/***		if( elements[elementNr].getElementType().compare( "QMHS4")==0) { we only use QMHS4 ***/
		if(1) {
			// not all elements are used for QMHS4 so get only the one that are needed
			cLElemQMHS4->sumNumLay+=numLay;
			cLElemQMHS4->seccount++;
//			elementsused+=elementsOfCSS.size();
		}
	}
	cLElemQMHS4->elementsused=ght_size(cLElemQMHS4->elements);
//	std::cout << "seccout "<<seccount << std::endl;
//	std::cout << "Elements " << elementsused << std::endl;
	cLElemQMHS4->indexPerCSS= (int*)malloc(cLElemQMHS4->seccount * sizeof(int));
	cLElemQMHS4->sig= (double*)malloc(cLElemQMHS4->elementsused*14 * sizeof(double));
	cLElemQMHS4->str= (double*)malloc(cLElemQMHS4->elementsused*20 * sizeof(double));
	cLElemQMHS4->numLayArray= (int*)malloc(cLElemQMHS4->seccount * sizeof(int));
	cLElemQMHS4->layProp= (double*)malloc(cLElemQMHS4->sumNumLay*2 * sizeof(double));
	cLElemQMHS4->layPropint=(int*)malloc(cLElemQMHS4->sumNumLay*2 * sizeof(int));
	cLElemQMHS4->matProp=(double*)malloc(cLElemQMHS4->sumNumLay*2 * sizeof(double));
	cLElemQMHS4->matID=(int*)malloc(cLElemQMHS4->sumNumLay * sizeof(int));
	cLElemQMHS4->T3=(double*)malloc(3*3*4*cLElemQMHS4->elementsused * sizeof(double));
	cLElemQMHS4->CSScountArray=(int*)malloc(cLElemQMHS4->elementsused * sizeof(int));
	cLElemQMHS4->dof=(int*)malloc(cLElemQMHS4->elementsused*24 * sizeof(int));

	for(int i=0;i<24*cLElemQMHS4->elementsused;i++) {
		cLElemQMHS4->dof[i]=0;
	}

	for(icoShellSectionsIT = (struct icoSSData*)ght_first(cLElemQMHS4->icoShellSections, &iterator, (const void**)&icoShellSectionsKey); icoShellSectionsIT;
			icoShellSectionsIT = (struct icoSSData*)ght_next(cLElemQMHS4->icoShellSections, &iterator, (const void**)&icoShellSectionsKey)) {
//	for( icoShellSectionsIT=cLElemQMHS4->icoShellSections.begin(); icoShellSectionsIT!=cLElemQMHS4->icoShellSections.end(); icoShellSectionsIT++) {
		int numLay=*(int*)ght_get(cLElemQMHS4->issSize, sizeof(char*), icoShellSectionsKey);
		int* elementsOfCSS=(int*)ght_get(cLElemQMHS4->elsets, sizeof(char*), icoShellSectionsKey);
//		int* elementsOfCSS=cLElemQMHS4->elsets[ icoShellSectionsKey ];
		elementNr=elementsOfCSS[0];

/***		if( elements[elementNr].getElementType().compare( "QMHS4")==0) { we only use QMHS4 ***/
		if(1) {
			for(int j=0; j<numLay; j++) {
				struct Material* material = (struct Material*)ght_get(cLElemQMHS4->mat, sizeof(char*), icoShellSectionsIT[j].matName);
				cLElemQMHS4->matProp[counterNumLay*2+2*j+0]=material->eModul;
				cLElemQMHS4->matProp[counterNumLay*2+2*j+1]=material->nue;
				cLElemQMHS4->matID[counterNumLay+j]=2;										//number of values for THIS material in matProp; needs if-switch for plastic-material
				cLElemQMHS4->layProp[counterNumLay*2+2*j]= icoShellSectionsIT[j].ug;				//lower bound
				cLElemQMHS4->layProp[counterNumLay*2+2*j+1]= icoShellSectionsIT[j].og;				//upper bound
				cLElemQMHS4->layPropint[counterNumLay*2+2*j]=j;								//matrial ID in array matProp
				cLElemQMHS4->layPropint[counterNumLay*2+2*j+1]= icoShellSectionsIT[j].intPoints;	//number integration points
			}
			cLElemQMHS4->indexPerCSS[counterLay]=counterNumLay;
			cLElemQMHS4->numLayArray[counterLay]=numLay;
			counterLay++;
			counterNumLay+=numLay;
			size_t currElsetSize = *(size_t*)ght_get(cLElemQMHS4->elsetSize, sizeof(char*), icoShellSectionsKey);
			for(unsigned int  i=0; i< currElsetSize; i++) {
//!				int elementNr_=elementsOfCSS[i];
				cLElemQMHS4->CSScountArray[counterNumElem]=counterLay-1;

				for(int k=0; k<4; k++) {
					/*** seems to be unused ***/
					cLElemQMHS4->interSec[counterNumElem*4+k] = 0; //!!!!!cLElemQMHS4->ShInter[elementNr_].i4[k];
				}
				//lBase
//				std::cout << std::endl;

				int adder=0;
				for(int j=0;j<24;j++) {

					if((!((cLElemQMHS4->interSec[counterNumElem*4+0]==0)&&(j==5)))&&
							(!((cLElemQMHS4->interSec[counterNumElem*4+1]==0)&&(j==11)))&&(!((cLElemQMHS4->interSec[counterNumElem*4+2]==0)&&
							(j==17)))&&(!((cLElemQMHS4->interSec[counterNumElem*4+3]==0)&&(j==23)))) {
						cLElemQMHS4->dof[counterNumElem*24+adder]=j;
						adder++;
					}
				}
				counterNumElem++;
			}
		}
	}
//	for(int i=0;i<counterNumElem;i++) {
//		for(int j=0;j<24;j++) {
//			std::cout << dof[i*24+j] << "\t";
//		}
//		std::cout << std::endl;
//	}
	// Initialize OpenCL
	// OpenCL specific variables
	cl_uint nPlatforms;
	errcode = clGetPlatformIDs(0, NULL, &nPlatforms);
	platforms = (cl_platform_id*)malloc(nPlatforms * sizeof(cl_platform_id));
	errcode |= clGetPlatformIDs(nPlatforms, platforms, 0);

	if (errorCodeOut(errcode,"Error getting platforms") == -1)
		return 1;

	cLElemQMHS4->clDevicesSize = 0;
#if 0
	for(unsigned int i=0;i<nPlatforms;i++) {
		cl_uint gdsTmp;
		clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_GPU, 0, NULL, &gdsTmp);
		if (errcode!=CL_DEVICE_NOT_FOUND) {
			if (errorCodeOut(errcode,"Error searching device GPU") == -1)
				return 1;
		}
		if(gdsTmp == 0)
			continue;
		clDevicesGPU = (cl_device_id*)malloc(gdsTmp * sizeof(cl_device_id));
		errcode = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_GPU, gdsTmp, clDevicesGPU, 0);
		if (errorCodeOut(errcode,"Error getting device GPU") == -1)
			return 1;
		if(gdsTmp > 0)
			cLElemQMHS4->clDevices = (cl_device_id*)realloc(cLElemQMHS4->clDevices, (cLElemQMHS4->clDevicesSize+gdsTmp) * sizeof(cl_device_id));

		for(unsigned int j=0;j<gdsTmp;j++) {
			cLElemQMHS4->clDevices[j+cLElemQMHS4->clDevicesSize] = clDevicesGPU[j];
		}
		cLElemQMHS4->clDevicesSize += gdsTmp;
		free(clDevicesGPU);
	}
#endif
	if(cLElemQMHS4->clDevicesSize==0) {
		for(unsigned int i=0;i<nPlatforms;i++) {
			cl_uint cdsTmp;
			clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_CPU, 0, NULL, &cdsTmp);
			if (errcode!=CL_DEVICE_NOT_FOUND) {
				if (errorCodeOut(errcode,"Error searching device GPU") == -1)
					return 1;
			}
			if(cdsTmp == 0)
				continue;
			clDevicesCPU = (cl_device_id*)malloc(cdsTmp * sizeof(cl_device_id));
			errcode = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_CPU, cdsTmp, clDevicesCPU, 0);
			if (errorCodeOut(errcode,"Error searching device CPU") == -1)
				return 1;
			if(cdsTmp > 0)
				cLElemQMHS4->clDevices = (cl_device_id*)realloc(cLElemQMHS4->clDevices, (cLElemQMHS4->clDevicesSize+cdsTmp) * sizeof(cl_device_id));

			for(unsigned int j=0;j<cdsTmp;j++) {
				cLElemQMHS4->clDevices[j+cLElemQMHS4->clDevicesSize] = clDevicesCPU[j];
			}
			cLElemQMHS4->clDevicesSize += cdsTmp;
			free(clDevicesCPU);
		}
	}
	if(cLElemQMHS4->clDevicesSize==0) {
		fprintf(stderr, "No device found\n");
		return 1;
	}

	// release platforms
	free(platforms);

//	if(cLElemQMHS4->clDevicesSize>1) {			// only needed for test
//		cLElemQMHS4->clDevicesSize = 1;
//		clDevices.pop_back();
//		clDevices.erase(clDevices.begin());
//	}

	for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;){
		char extensions[2048];
		errcode =clGetDeviceInfo(cLElemQMHS4->clDevices[i], CL_DEVICE_EXTENSIONS, 2048, &extensions, 0);
		if (!(strstr(extensions, "cl_amd_fp64")||strstr(extensions, "cl_khr_fp64")) || errcode != CL_SUCCESS) {
			// TODO add release
			cl_device_id toremove = cLElemQMHS4->clDevices[i];
			--cLElemQMHS4->clDevicesSize;
			for(unsigned int j = i;i<cLElemQMHS4->clDevicesSize;j++)
				cLElemQMHS4->clDevices[j] = cLElemQMHS4->clDevices[j+1];
			cLElemQMHS4->clDevices[cLElemQMHS4->clDevicesSize] = toremove;
		} else
			++i;
	}
/***	std::vector < cl::Device >::iterator toremoveIT;
	for(toremoveIT=toremove.begin();toremoveIT!=toremove.end();toremoveIT++){
		clDevices.erase(toremoveIT);
	}***/
	if(cLElemQMHS4->clDevicesSize==0) {
		fprintf(stderr, "Devices do not support double precision extension!\n");
		return 1;
	}

	printf("Devices found:\n");
	for(unsigned int i=0;i< cLElemQMHS4->clDevicesSize;i++) {
		errcode =clGetDeviceInfo(cLElemQMHS4->clDevices[i], CL_DEVICE_VERSION, 512, &devVersion, 0);
		errcode|=clGetDeviceInfo(cLElemQMHS4->clDevices[i], CL_DEVICE_NAME, 512, &devName, 0);
		errcode|=clGetDeviceInfo(cLElemQMHS4->clDevices[i], CL_DEVICE_VENDOR, 512, &devVend, 0);
		errcode|=clGetDeviceInfo(cLElemQMHS4->clDevices[i], CL_DRIVER_VERSION, 512, &devVers, 0);
		if (errorCodeOut(errcode,"Error reading device info") == -1)
			return 1;

//		std::cout <<"Max WorkG "<< clDevices[i].getInfo< CL_DEVICE_MAX_WORK_ITEM_SIZES>()[1]<< std::endl;
		printf("Device %i %s %s %s %s\n", i, devVend, devName, devVers, devVersion);
	}
	err= (cl_int*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_int));
	for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++){
		err[i]=0;
	}

	cLElemQMHS4->clContextArray = (cl_context*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_context));

	for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {

//!		clContextArray.push_back(cl::Context(devV,NULL,NULL,NULL,&errcode));
		cLElemQMHS4->clContextArray[i] = clCreateContext(NULL, 1, &cLElemQMHS4->clDevices[i],NULL,NULL,&errcode);
		if (errorCodeOut(errcode,"Error creating Context") == -1)
			return 1;
	}
/*** seems to be unused
	long int* maxElemPerDev=new long int[clDevices.size()]; */
	cLElemQMHS4->comandQueues= (cl_command_queue*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_command_queue));

	for(unsigned int i=0;i< cLElemQMHS4->clDevicesSize;i++) {
		cl_ulong mas = 0, wgs = 0;
		errcode =clGetDeviceInfo(cLElemQMHS4->clDevices[i], CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(size_t), &wgs, 0);
		if (errorCodeOut(errcode,"Error reading the maximal work group size") == -1)
			return 1;
		cLElemQMHS4->comandQueues[i]=clCreateCommandQueue(cLElemQMHS4->clContextArray[i], cLElemQMHS4->clDevices[i], 0, &errcode);
		if (errorCodeOut(errcode,"Error creating ComandQueues") == -1)
			return 1;
		errcode =clGetDeviceInfo(cLElemQMHS4->clDevices[i], CL_DEVICE_MAX_MEM_ALLOC_SIZE, sizeof(cl_ulong), &mas, 0);
		if (errorCodeOut(errcode,"Error reading memory size infomation") == -1)
			return 1;
		if(mas<wgs*50000) {
			printf("Device has too few memory %lu\n", mas);
			return 1;
		}
	}

	int elem=cLElemQMHS4->elementsused/cLElemQMHS4->clDevicesSize;
	int rest=cLElemQMHS4->elementsused%cLElemQMHS4->clDevicesSize;
	cLElemQMHS4->elemCalcPerDev= (int*)malloc(cLElemQMHS4->clDevicesSize * sizeof(int));
	cLElemQMHS4->numCalcsPerDev= (int*)malloc(cLElemQMHS4->clDevicesSize * sizeof(int));

//	elementsCalcPerKernel= (int*)malloc(cLElemQMHS4->clDevicesSize * sizeof(int));
	printf("OpenCL Threads used: %d", numElems);
	int allign;
	if(numElems<64) {
		allign=numElems;
	} else {
		allign=64;
	}
	printf("Opencl local Threads used: %d\n", allign);
	cLElemQMHS4->numElemPerCalc = (size_t**)malloc(cLElemQMHS4->clDevicesSize * sizeof(size_t*));
	cLElemQMHS4->numElemPerCalcSize = (size_t*)malloc(cLElemQMHS4->clDevicesSize * sizeof(size_t));
	cLElemQMHS4->localWork = (size_t**)malloc(cLElemQMHS4->clDevicesSize * sizeof(size_t*));
	for(unsigned int i=0;i< cLElemQMHS4->clDevicesSize;i++) {
//		elementsCalcPerKernel[i]=numElems;
		if(rest!=0) {
			cLElemQMHS4->elemCalcPerDev[i]=elem+1;
			rest--;
		} else {
			cLElemQMHS4->elemCalcPerDev[i]=elem;
		}
//		std::cout << "elemcpd " << elemCalcPerDev[i] << " numElems " << numElems << " align " << allign << std::endl;
		int additional=splitfunction(cLElemQMHS4->elemCalcPerDev[i],numElems,allign,&cLElemQMHS4->numElemPerCalc[i],&cLElemQMHS4->numElemPerCalcSize[i],i);

		cLElemQMHS4->numCalcsPerDev[i]=cLElemQMHS4->numElemPerCalcSize[i];
		cLElemQMHS4->localWork[i] = (size_t*)malloc(cLElemQMHS4->numCalcsPerDev[i] * sizeof(size_t));
		for(int j=0;j<cLElemQMHS4->numCalcsPerDev[i];j++) {
			cLElemQMHS4->localWork[i][j]=allign;
		}
		if(additional!=0) {	// if additional is not 0 there is rest as a result is calculated as last part splitfunction has already made numElemPerCalc one calculation bigger
			cLElemQMHS4->localWork[i][cLElemQMHS4->numCalcsPerDev[i]-1]=additional;
			cLElemQMHS4->numElemPerCalc[i][cLElemQMHS4->numCalcsPerDev[i]-1]=additional;
		}
	}
	// to do splitt elements in smaller parts biggest <=10000 elements
	cLElemQMHS4->Tcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->xcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->Hcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->dcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->t1cl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->t2cl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->Nsigcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->Nstrcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->Dcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->detJ0cl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->fecl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->Fcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->Pcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->Kgcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->CSScountArraycl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->interSeccl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->LBaseGcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->numDOFEIcl  = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->dofcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->T3Ecl  = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->Ktcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->fincl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->XGcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->uGcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->sigGcl  = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->strGcl  = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->invFGcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->invFTGcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->GGcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->P11qcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->P22Gcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->P21Gcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->fe1qcl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->fe2cl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->fscl = (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));



	cLElemQMHS4->numLayArraycl= (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->indexPerCSScl= (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->layPropintcl= (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->matPropcl= (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->matIDcl= (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->layPropcl= (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->xi1cl= (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->xi2cl= (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
	cLElemQMHS4->dNdxicl= (cl_mem*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_mem));
//std::cout << "seccount " <<  seccount<< " " <<sumNumLay << std::endl;

	free(err);
	err= (cl_int*)malloc(35 * sizeof(cl_int));
	for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
		cLElemQMHS4->numLayArraycl[i] 	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*cLElemQMHS4->seccount,		NULL, &err[0]);
		cLElemQMHS4->indexPerCSScl[i] 	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*cLElemQMHS4->seccount,		NULL, &err[1]);
		cLElemQMHS4->layPropintcl[i] 	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*cLElemQMHS4->sumNumLay*2,		NULL, &err[2]);
		cLElemQMHS4->matPropcl[i] 		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*cLElemQMHS4->sumNumLay*2,	NULL, &err[3]);
		cLElemQMHS4->matIDcl[i] 		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*cLElemQMHS4->sumNumLay,		NULL, &err[4]);
		cLElemQMHS4->layPropcl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*cLElemQMHS4->sumNumLay*2,	NULL, &err[5]);
		cLElemQMHS4->xi1cl[i] 			= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*4,							NULL, &err[6]);
		cLElemQMHS4->xi2cl[i]			= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*4,							NULL, &err[7]);
		cLElemQMHS4->dNdxicl[i]			= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*2*4*4,						NULL, &err[8]);

		for(int j=0;j<9;j++) {
			if (errorCodeOut(err[j],"Error creating buffer") == -1)
				return -1;
		}
	}

	for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
		cLElemQMHS4->Tcl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*3*3*4,	NULL, &err[0]);
		cLElemQMHS4->xcl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*3*4,		NULL, &err[1]);
		cLElemQMHS4->Hcl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*3*3*4,	NULL, &err[2]);
		cLElemQMHS4->dcl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*3*4,		NULL, &err[3]);
		cLElemQMHS4->t1cl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*3,		NULL, &err[4]);
		cLElemQMHS4->t2cl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*3,		NULL, &err[5]);
		cLElemQMHS4->Nsigcl[i]	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*8*14*4,	NULL, &err[6]);
		cLElemQMHS4->Nstrcl[i]	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*10*20*4,	NULL, &err[7]);
		cLElemQMHS4->Dcl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*4*3,		NULL, &err[8]);
		cLElemQMHS4->detJ0cl[i]	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0],			NULL, &err[9]);
		cLElemQMHS4->fecl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*20,		NULL, &err[10]);
		cLElemQMHS4->Fcl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*20*14,	NULL, &err[11]);
		cLElemQMHS4->Pcl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*20*20,	NULL, &err[12]);
		cLElemQMHS4->Kgcl[i] 		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*24*24,	NULL, &err[13]);
		cLElemQMHS4->CSScountArraycl[i] = clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*cLElemQMHS4->numElemPerCalc[i][0],		NULL, &err[14]);
		cLElemQMHS4->interSeccl[i]= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*cLElemQMHS4->numElemPerCalc[i][0]*4,			NULL, &err[15]);
		cLElemQMHS4->LBaseGcl[i]	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*3*3*4,	NULL, &err[16]);
		cLElemQMHS4->numDOFEIcl[i]= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*cLElemQMHS4->numElemPerCalc[i][0],				NULL, &err[17]);
		cLElemQMHS4->dofcl[i] 	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*cLElemQMHS4->numElemPerCalc[i][0]*24,			NULL, &err[18]);
		cLElemQMHS4->T3Ecl[i] 	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*3*3*4,	NULL, &err[19]);
		cLElemQMHS4->Ktcl[i] 		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*24*24,	NULL, &err[20]);
		cLElemQMHS4->fincl[i] 	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*24,		NULL, &err[21]);
		cLElemQMHS4->XGcl[i] 		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*4*3,		NULL, &err[22]);
		cLElemQMHS4->uGcl[i] 		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*4*6,		NULL, &err[23]);
		cLElemQMHS4->sigGcl[i] 	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*14,		NULL, &err[24]);
		cLElemQMHS4->strGcl[i] 	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*20,		NULL, &err[25]);
		cLElemQMHS4->invFGcl[i]	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*14*14,	NULL, &err[26]);
		cLElemQMHS4->invFTGcl[i]	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*14*14,	NULL, &err[27]);
		cLElemQMHS4->GGcl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*14*24,	NULL, &err[28]);
		cLElemQMHS4->P11qcl[i]	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*14*14,	NULL, &err[29]);
		cLElemQMHS4->P22Gcl[i]	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*6*6,		NULL, &err[30]);
		cLElemQMHS4->P21Gcl[i]	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*14*6,	NULL, &err[31]);
		cLElemQMHS4->fe1qcl[i]	= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*14,		NULL, &err[32]);
		cLElemQMHS4->fe2cl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*6,		NULL, &err[33]);
		cLElemQMHS4->fscl[i]		= clCreateBuffer(cLElemQMHS4->clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*cLElemQMHS4->numElemPerCalc[i][0]*14,		NULL, &err[34]);

		for(int j=0;j<35;j++) {
			if (errorCodeOut(err[j],"Error creating buffer") == -1){
				return 1;
			}
		}

	}




// load Binaries from openCL Kernel files the order of devises should be the same as by the compilation thats why it should get right binary to devise
/*** run time compilation only, baby! ***
 	size_t* size = (size_t*)malloc(cLElemQMHS4->clDevicesSize * sizeof(size_t));
	char fname[1024];
	cl_int binStatus;
	const unsigned char** binaries = (const unsigned char**)malloc(cLElemQMHS4->clDevicesSize * sizeof(unsigned char*));
//	const unsigned char* binariestemp;
	for(int i=0;i<(int)cLElemQMHS4->clDevicesSize;i++) {
		errcode =clGetDeviceInfo(cLElemQMHS4->clDevices[i], CL_DEVICE_VERSION, 512, &devVersion, 0);
		errcode|=clGetDeviceInfo(cLElemQMHS4->clDevices[i], CL_DEVICE_NAME, 512, &devName, 0);
		errcode|=clGetDeviceInfo(cLElemQMHS4->clDevices[i], CL_DEVICE_VENDOR, 512, &devVend, 0);
		errcode|=clGetDeviceInfo(cLElemQMHS4->clDevices[i], CL_DRIVER_VERSION, 512, &devVers, 0);
		if (errorCodeOut(errcode,"Error reading device info") == -1){
			return 1;
		}
		sprintf(fname, "./Kernel-%s-%s-%s-%s.ocl", devVend, devVersion, devName, devVers);
		std::cout << "Loading file:\n" << fname << std::endl;
		FILE* file = fopen(fname, "r");
		if (!file) {
			std::cout << "Binary kernel not found!\n Create kernel binary first!\n";
			return 1;
		}

		  // get length of file:
		fseek(file, 0, SEEK_END);
		size[i] = ftell(file);
		fseek(file, 0, SEEK_SET);

//		std::cout << "FILE size: " << size[i] << std::endl;
		binaries[i] = (unsigned char*)malloc(size[i] * sizeof(unsigned char));
		fread((void*)binaries[i], sizeof(unsigned char), size[i], file);
		fclose(file);
	}

	cLElemQMHS4->clKernelFirstPart = (cl_kernel*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_kernel));
	cLElemQMHS4->clKernelSecondPart = (cl_kernel*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_kernel));
	cLElemQMHS4->clKernelThirdPart = (cl_kernel*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_kernel));

	for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
//		binariestemp = binaries[i];
		cl_program clProgram = clCreateProgramWithBinary(cLElemQMHS4->clContextArray[i],1,&cLElemQMHS4->clDevices[i],&size[i],&binaries[i],&binStatus, &errcode);
		if (errorCodeOut(errcode,"Error creating program") == -1)
			return 1;

		if (errorCodeOut(binStatus,"Error creating program status") == -1)
			return 1;

		errcode = clBuildProgram(clProgram, 1, &cLElemQMHS4->clDevices[i], "", NULL, NULL);
		if (errorCodeOut(errcode,"Error building program") == -1) {
			if (errcode != CL_SUCCESS) {
				char str[4096];
				errcode  = clGetProgramBuildInfo(clProgram, cLElemQMHS4->clDevices[i], CL_PROGRAM_BUILD_LOG, 4096, &str, NULL);
				std::cout << " BUILD LOG Device " << i <<std::endl;
				std::cout << str << std::endl;
				errcode |= clGetProgramBuildInfo(clProgram, cLElemQMHS4->clDevices[i], CL_PROGRAM_BUILD_OPTIONS, 4096, &str, NULL);
				std::cout << " BUILD Options used:\n";
				std::cout << str << std::endl;
				if (errorCodeOut(errcode,"Error reading build info") == -1)
					return 1;
			}
			return 1;
		}
		cLElemQMHS4->clKernelFirstPart[i] = clCreateKernel(clProgram, "firstPart", &errcode);
		if (errorCodeOut(errcode,"Error creating Kernel firstPart") == -1)
			return 1;
		cLElemQMHS4->clKernelSecondPart[i] = clCreateKernel(clProgram, "secondPart", &errcode);
		if (errorCodeOut(errcode,"Error creating Kernel secondPart") == -1)
			return 1;
		cLElemQMHS4->clKernelThirdPart[i] = clCreateKernel(clProgram, "thirdPart", &errcode);
		if (errorCodeOut(errcode,"Error creating Kernel thirdPart") == -1)
			return 1;

		clReleaseProgram(clProgram);
//		free(binaries[i]);
	}
	free(size);
	free(binaries);

	/*********** loading source code *****************/
	size_t len;
	const char *code = readFile("ClKernelQMHS4.cl", &len);
	cLElemQMHS4->clKernelFirstPart = (cl_kernel*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_kernel));
	cLElemQMHS4->clKernelSecondPart = (cl_kernel*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_kernel));
	cLElemQMHS4->clKernelThirdPart = (cl_kernel*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_kernel));
	for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
#pragma insieme kernelFile "bin/ClKernelQMHS4.clg"
		cl_program clProgram = clCreateProgramWithSource(cLElemQMHS4->clContextArray[i], 1, &code, &len, &errcode);
		if (errorCodeOut(errcode,"Error creating OpenCL Program") == -1)
			return 1;
		char charStr[4096] = "-O0";

		errcode = clBuildProgram(clProgram, 1u, &cLElemQMHS4->clDevices[i], charStr, NULL, NULL);
		if (errorCodeOut(errcode,"Error building program") == -1) {
	//		if (errcode == CL_BUILD_PROGRAM_FAILURE) {
				clGetProgramBuildInfo(clProgram, cLElemQMHS4->clDevices[i], CL_PROGRAM_BUILD_LOG, 4096, &charStr, 0);
				printf(" BUILD LOG Device %d\n", i);
				printf("%s\n", charStr);
				clGetProgramBuildInfo(clProgram, cLElemQMHS4->clDevices[i], CL_PROGRAM_BUILD_OPTIONS, 4096, &charStr, 0);
				printf(" BUILD Options used:\n%s\n", charStr);
	//		}
			return 1;
		}

		cLElemQMHS4->clKernelFirstPart[i] = clCreateKernel(clProgram, "firstPart", &errcode);
		if (errorCodeOut(errcode,"Error creating Kernel firstPart") == -1)
			return 1;
		cLElemQMHS4->clKernelSecondPart[i] = clCreateKernel(clProgram, "secondPart", &errcode);
		if (errorCodeOut(errcode,"Error creating Kernel secondPart") == -1)
			return 1;
		cLElemQMHS4->clKernelThirdPart[i] = clCreateKernel(clProgram, "thirdPart", &errcode);
		if (errorCodeOut(errcode,"Error creating Kernel thirdPart") == -1)
			return 1;
	}

	cl_event* event= (cl_event*)malloc(9 * sizeof(cl_event));
	for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
		err[0] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->xi1cl[i], 			CL_FALSE, 0, sizeof(double)*4,cLElemQMHS4->xi1,
				0,	NULL, &event[0]);
		err[1] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->xi2cl[i], 			CL_FALSE, 0, sizeof(double)*4,cLElemQMHS4->xi2,
				0,	NULL, &event[1]);
		err[2] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->dNdxicl[i], 		CL_FALSE, 0, sizeof(double)*2*4*4,cLElemQMHS4->dNdxi,
				0,	NULL, &event[2]);
		err[3] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->layPropcl[i],		CL_FALSE, 0, sizeof(double)*cLElemQMHS4->sumNumLay*2,
				cLElemQMHS4->layProp, 0, NULL, &event[3]);
		err[4] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->layPropintcl[i], 	CL_FALSE, 0, sizeof(int)*cLElemQMHS4->sumNumLay*2,
				cLElemQMHS4->layPropint, 0, NULL, &event[4]);
		err[5] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->matPropcl[i], 		CL_FALSE, 0, sizeof(double)*cLElemQMHS4->sumNumLay*2,
				cLElemQMHS4->matProp, 0, NULL, &event[5]);
		err[6] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->matIDcl[i], 		CL_FALSE, 0, sizeof(int)*cLElemQMHS4->sumNumLay,
				cLElemQMHS4->matID,   	0,	NULL, &event[6]);
		err[7] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->indexPerCSScl[i], 	CL_FALSE, 0, sizeof(int)*cLElemQMHS4->seccount,
				cLElemQMHS4->indexPerCSS,  0, 	NULL, &event[7]);
		err[8] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->numLayArraycl[i], 	CL_FALSE, 0, sizeof(int)*cLElemQMHS4->seccount,
				cLElemQMHS4->numLayArray,  0, 	NULL, &event[8]);
		for(int j=0;j<9;j++) {
			if (errorCodeOut(err[j],"Error writing data to buffers") == -1) {
				return 1;
			}
		}
		// over all events wait till transfer finished
//		for(int j=0;j<9;j++) {
			errcode = clEnqueueWaitForEvents(cLElemQMHS4->comandQueues[i], 9, event);
			if (errorCodeOut(errcode,"Error while writing data to buffers") == -1)
				return 1;
//		}
	}
	free(err);
	free(event);

	//************************************************************************
//	free(cLElemQMHS4->clContextArray);
	//************************************************************************
	return 0;
}


int calcQMHS4(struct CLElemQMHS4* cLElemQMHS4, double* Kt,double* fin,int* elementsIndex,int* numDOFperindex) {
	struct icoSSData* icoShellSectionsIT;
	char* icoShellSectionsKey;
	ght_iterator_t iterator;

	//size_t dataBytes;
	cl_int errcode;
	cl_int err[40];
	// OpenCL device memory
	struct timeval start, end;
	cl_event* events;
	cl_event* events2;
	cl_event* events3;
//	std::vector<cl_event> events4;
	cl_event* event = (cl_event*)malloc(34 * sizeof(cl_event));
	int counterNumElem=0;
	int elementNr;
	int globalNr=0;
/***	size_t indices[6]; Not needed
	int ierr = 0;
	double a[6];
	double b[6]; ***/
//				std::cout << "CSScoutnArray "<< CSScountArray[counterNumElem] << " " << counterNumElem << std::endl;
	for(icoShellSectionsIT = (struct icoSSData*)ght_first(cLElemQMHS4->icoShellSections, &iterator, (const void**)&icoShellSectionsKey);
			icoShellSectionsIT;
			icoShellSectionsIT = (struct icoSSData*)ght_next(cLElemQMHS4->icoShellSections, &iterator, (const void**)&icoShellSectionsKey)) {
//	for( icoShellSectionsIT=cLElemQMHS4->icoShellSections.begin(); icoShellSectionsIT!=cLElemQMHS4->icoShellSections.end(); icoShellSectionsIT++) {
		int* elementsOfCSS=*(int**)ght_get(cLElemQMHS4->elsets, sizeof(char*), icoShellSectionsKey);
//		int* elementsOfCSS=cLElemQMHS4->elsets[ icoShellSectionsKey ];
		elementNr=elementsOfCSS[0];

/***		if( elements[elementNr].getElementType().compare( "QMHS4")==0) { we only use QMHS4 ***/
		if(1) {
			size_t currElsetSize = *(size_t*)ght_get(cLElemQMHS4->elsetSize, sizeof(char*), icoShellSectionsKey);
			for(unsigned int  i=0; i< currElsetSize; i++) {
				int elementNr_=elementsOfCSS[i];
/*** XG is done via set
				for (int j = 0; j < 4; j++) {
					globalNr = elements[elementNr_].getGlobalNrOfNodeNr(j);
					XG[counterNumElem*4*3+ 3 * j + 0] = nodes[globalNr].getX();
					XG[counterNumElem*4*3+ 3 * j + 1] = nodes[globalNr].getY();
					XG[counterNumElem*4*3+ 3 * j + 2] = nodes[globalNr].getZ();
//					std::cout << XG[counterNumElem*4*3+ 3 * j + 2] << "\t";
					for(int k=0; k<6; k++) {
						indices[k]=6*globalNr + k;
					}

					CHKERRQ( ierr);***/
/** u already done via set
					ierr=VecGetValues( DuG, 6, indices, b);
					CHKERRQ( ierr);

					for(int k=0; k<6; k++){
						// u is needed in the calculation but not un and Du
						u[counterNumElem*6*4+6*j+k]=a[k]+b[k];
//						std::cout << b[k] << "\t";
					}

				}
 lBase already done via set
				//lBase
				std::map<std::string, std::map<int, NodeBases> >::const_iterator fit = 
						LBaseG->find(elements[elementNr_].getAVLSetName());
				assert(fit != LBaseG->end());

				const std::map< int, NodeBases>& tempNB = fit->second;
				std::map< int, NodeBases>::const_iterator tempNBIT;

				for(int j=0; j<4; j++) {
					tempNBIT=tempNB.find( elements[elementNr_].getGlobalNrOfNodeNr(j) );

					for(int k=0;k<3;k++) {
						LBase[counterNumElem*4*3*3+j*3*3+3*k+0] = tempNBIT->second.v1[k];
						LBase[counterNumElem*4*3*3+j*3*3+3*k+1] = tempNBIT->second.v2[k];
						LBase[counterNumElem*4*3*3+j*3*3+3*k+2] = tempNBIT->second.v3[k];
					}
				}**/
				// ***** sig str Dsig Dstr *****
				for(int j=0; j<14; j++) {
//!					sig[counterNumElem*14+j] = (*sigG)[elementNr_][j] + (*DsigG)[elementNr_][j];
					cLElemQMHS4->sig[counterNumElem*14+j] = cLElemQMHS4->sigPtr[(elementNr_-1)*14+j] + cLElemQMHS4->Dsig[(elementNr_-1)*14+j];
//					std::cout << sig[counterNumElem*14+j] << "\t";
				}
//				std::cout << std::endl;
				for(int j=0; j<20; j++) {
//!					str[counterNumElem*20+j]=(*strG)[elementNr_][j] + (*DstrG)[elementNr_][j];
					cLElemQMHS4->str[counterNumElem*20+j]=cLElemQMHS4->strPtr[(elementNr_-1)*20+j] + cLElemQMHS4->Dstr[(elementNr_-1)*20+j];
//					std::cout << str[counterNumElem*20+j] << "\t";
				}
				numDOFperindex[counterNumElem]=20;
				for(int j=0;j<4;j++) {
					numDOFperindex[counterNumElem]+=cLElemQMHS4->interSec[counterNumElem*4+j];
				}
				counterNumElem++;
			}
		}
	}

#ifndef WIN32
	gettimeofday(&start, 0);
#endif
	int* countElemCalc= (int*)malloc(cLElemQMHS4->clDevicesSize*4 * sizeof(int));
	for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize*4;i++){
		countElemCalc[i]=0;

	}
	int* startValPerDev= (int*)malloc(cLElemQMHS4->clDevicesSize * sizeof(int));
	startValPerDev[0]=0;
	for(unsigned int i=1;i<cLElemQMHS4->clDevicesSize;i++) {
		startValPerDev[i]=startValPerDev[i-1]+cLElemQMHS4->numCalcsPerDev[i-1];
		countElemCalc[i*4+2]=startValPerDev[i];
		countElemCalc[i*4+3]=countElemCalc[(i-1)*4+3]+cLElemQMHS4->elemCalcPerDev[i-1];
	}
	int calculated=0;
//	cl_ulong sumLoadTime=0;
//	cl_ulong sumSecondPart=0;
//	cl_ulong sumThirdPart=0;
//	cl_ulong memtransfered=0;
//	cl_ulong wholeUpTransTime=0;
//	cl_ulong wholeUpMemTrans=0;
//	cl_ulong wholeDownTransTime=0;
//	cl_ulong wholeDownMemTrans=0;

	while(!calculated) {
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			countElemCalc[i*4]=countElemCalc[i*4+2];
			countElemCalc[i*4+1]=countElemCalc[i*4+3];
		}
//		sumLoadTime=0;
//		memtransfered=0;
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			if(countElemCalc[i*4]<cLElemQMHS4->numCalcsPerDev[i]+startValPerDev[i]){
//				std::cout << "here " << countElemCalc[i*4+1]<<" "<< i<< std::endl;
				int l=countElemCalc[i*4]-startValPerDev[i];
				err[1] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->XGcl[i], 	CL_FALSE, 0, sizeof(double)*
						cLElemQMHS4->numElemPerCalc[i][l]*4*3,		&cLElemQMHS4->XG[countElemCalc[i*4+1]*4*3],			0,	NULL, &event[1]);
				err[2] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->uGcl[i], 	CL_FALSE, 0, sizeof(double)*
						cLElemQMHS4->numElemPerCalc[i][l]*4*6,		&cLElemQMHS4->u[countElemCalc[i*4+1]*4*6],			0,	NULL, &event[2]);
				err[3] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->CSScountArraycl[i],CL_FALSE, 0, sizeof(int)*
						cLElemQMHS4->numElemPerCalc[i][l],			&cLElemQMHS4->CSScountArray[countElemCalc[i*4+1]], 0,NULL, &event[3]);
				err[4] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->sigGcl[i], 	CL_FALSE, 0, sizeof(double)*
						cLElemQMHS4->numElemPerCalc[i][l]*14,		&cLElemQMHS4->sig[countElemCalc[i*4+1]*14],		0,	NULL, &event[4]);
				err[5] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->dofcl[i], 	CL_FALSE, 0, sizeof(int)*
						cLElemQMHS4->numElemPerCalc[i][l]*24,		&cLElemQMHS4->dof[countElemCalc[i*4+1]*24],  	0,	NULL, &event[5]);
				err[6] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->strGcl[i], 	CL_FALSE, 0, sizeof(double)*
						cLElemQMHS4->numElemPerCalc[i][l]*20,		&cLElemQMHS4->str[countElemCalc[i*4+1]*20],		0,	NULL, &event[6]);
				err[7] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->numDOFEIcl[i],CL_FALSE, 0, sizeof(int)*
						cLElemQMHS4->numElemPerCalc[i][l],			&numDOFperindex[countElemCalc[i*4+1]],0,NULL, &event[7]);
				err[8] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->interSeccl[i],CL_FALSE, 0, sizeof(int)*
						cLElemQMHS4->numElemPerCalc[i][l]*4,		&cLElemQMHS4->interSec[countElemCalc[i*4+1]*4],  0,	NULL, &event[8]);
				err[9] = clEnqueueWriteBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->LBaseGcl[i],	CL_FALSE, 0, sizeof(double)*
						cLElemQMHS4->numElemPerCalc[i][l]*3*3*4,	&cLElemQMHS4->LBase[countElemCalc[i*4+1]*3*3*4],  	0,	NULL, &event[9]);

				for(int j=1;j<10;j++) {
					if (errorCodeOut(err[j],"Error writing data to buffers") == -1) {
						return 1;
					}
				}
				// over all events wait till transfer finished
//					for(int j=1;j<10;j++) {
//						errcode = event[j].wait();
//						if (errorCodeOut(errcode,"Error while writing data to buffers") == -1)
//							return 1;
//					}

//					for(int j=1;j<10;j++) {
//						cl_ulong start=event[j].getProfilingInfo<CL_PROFILING_COMMAND_START>(&errcode);
//						if (errorCodeOut(errcode,"Error while getting profiling data") == -1)
//							return 1;
//						cl_ulong end=event[j].getProfilingInfo<CL_PROFILING_COMMAND_END>(&errcode);
//						if (errorCodeOut(errcode,"Error while getting profiling data") == -1)
//							return 1;
//						sumLoadTime+=end-start;
//	//					std::cout << "sumLoadTime " << sumLoadTime << std::endl;
//	//				std::cout<< "time needed XG " << end-start << " ns " << ((double)sizeof(double)*cLElemQMHS4->numElemPerCalc[i][l]*4*3/(double)((double)(end-start)/(double)1000000)) << " byte/s" << std::endl;
//					}
//					memtransfered+=sizeof(double)*cLElemQMHS4->numElemPerCalc[i][l]*(4*3+4*6+14+20+3*3*4)+sizeof(int)*cLElemQMHS4->numElemPerCalc[i][l]*(1+24+1+4);

				countElemCalc[i*4]++;
				countElemCalc[i*4+1]+=cLElemQMHS4->numElemPerCalc[i][l];
			}
		}
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			errcode=clFinish(cLElemQMHS4->comandQueues[i]);
//			std::cout << "comandQueue " << i << std::endl;
			if (errorCodeOut(errcode,"Error while wait for all comandQues to finish") == -1) {
				return 1;
			}
		}
	//	std::cout<<"Time Upload "<<sumLoadTime/1000000<< " msec Upload speed " << ((double)memtransfered/(double)((double)(sumLoadTime)/(double)1000000000)/((double)1024*1024)) << " Mbyte/s" << std::endl;
//		wholeUpTransTime+=sumLoadTime;
//		wholeUpMemTrans+=memtransfered;

	//	firstPart(int elementsused,__global double* X,__global double* u,__global int* interSec,__global double* LBase,__global double* T3,__global double* xi1,__global double* xi2,__global double* T,__global double* x
	//							,__global double* H,__global double* d,__global double* t1,__global double* t2,__global double* Nsig,__global double* Nstr,__global double* D,__global double* detJ0)
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			countElemCalc[i*4]=countElemCalc[i*4+2];
		}

		events = (cl_event*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_event));
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			if(countElemCalc[i*4]<cLElemQMHS4->numCalcsPerDev[i]+startValPerDev[i]){
				// function parameters
//				std::cout << "First Part "<<countElemCalc[i*4] << std::endl;
				int l=countElemCalc[i*4]-startValPerDev[i];
//				std::cout << "First Part "<<l << std::endl;
				cl_uint sizeArg = cLElemQMHS4->numElemPerCalc[i][l];
				err[0]  = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 0,  sizeof(cl_uint),(void *) &sizeArg);
				err[1]  = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 1,  sizeof(cl_mem), (void *) &cLElemQMHS4->XGcl[i]);
				err[2]  = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 2,  sizeof(cl_mem), (void *) &cLElemQMHS4->uGcl[i]);
				err[3]  = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 3,  sizeof(cl_mem), (void *) &cLElemQMHS4->interSeccl[i]);
				err[4]  = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 4,  sizeof(cl_mem), (void *) &cLElemQMHS4->LBaseGcl[i]);
				err[5]  = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 5,  sizeof(cl_mem), (void *) &cLElemQMHS4->T3Ecl[i]);
				err[6]  = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 6,  sizeof(cl_mem), (void *) &cLElemQMHS4->xi1cl[i]);
				err[7]  = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 7,  sizeof(cl_mem), (void *) &cLElemQMHS4->xi2cl[i]);
				err[8]  = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 8,  sizeof(cl_mem), (void *) &cLElemQMHS4->Tcl[i]);
				err[9]  = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 9,  sizeof(cl_mem), (void *) &cLElemQMHS4->xcl[i]);
				err[10] = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 10, sizeof(cl_mem), (void *) &cLElemQMHS4->Hcl[i]);
				err[11] = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 11, sizeof(cl_mem), (void *) &cLElemQMHS4->dcl[i]);
				err[12] = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 12, sizeof(cl_mem), (void *) &cLElemQMHS4->t1cl[i]);
				err[13] = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 13, sizeof(cl_mem), (void *) &cLElemQMHS4->t2cl[i]);
				err[14] = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 14, sizeof(cl_mem), (void *) &cLElemQMHS4->Nsigcl[i]);
				err[15] = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 15, sizeof(cl_mem), (void *) &cLElemQMHS4->Nstrcl[i]);
				err[16] = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 16, sizeof(cl_mem), (void *) &cLElemQMHS4->Dcl[i]);
				err[17] = clSetKernelArg(cLElemQMHS4->clKernelFirstPart[i], 17, sizeof(cl_mem), (void *) &cLElemQMHS4->detJ0cl[i]);
				countElemCalc[i*4]++;
				for(int j=0;j<18;j++) {
					if (errorCodeOut(err[j],"Error settig arguments for Kernel firstPart") == -1) {
						printf("at %d elem %d\n", i, j);
						return 1;
					}
				}
			//there should be as much comandQueues as contextes
				cl_event ev;
				errcode = clEnqueueNDRangeKernel(cLElemQMHS4->comandQueues[i], cLElemQMHS4->clKernelFirstPart[i], 1, NULL,
						&cLElemQMHS4->numElemPerCalc[i][l], &cLElemQMHS4->localWork[i][l], 0, NULL, &ev);
				if (errorCodeOut(errcode,"Error starting calculation Kernel firstPart") == -1)
					return 1;
				errcode = clFlush(cLElemQMHS4->comandQueues[i]);
				if (errorCodeOut(errcode,"Error Flush Kernel firstPart") == -1)
					return 1;
				events[i] = ev;
			}
		}
/*		std::cout<< "finish1 " << std::endl;
			for(unsigned int i=0;i<comandQueues.size();i++) {
				errcode=comandQueues[i].finish();
	//			std::cout << "comandQueue " << i << std::endl;
				if (errorCodeOut(errcode,"Error while wait for all comandQues to finish") == -1) {
					return 1;
				}
			}
*/
//		for(int j=0;j<events.size();j++) {
//			cl_ulong start=events[j].getProfilingInfo<CL_PROFILING_COMMAND_START>(&errcode);
//			if (errorCodeOut(errcode,"Error while getting profiling data") == -1)
//				return 1;
//			cl_ulong end=events[j].getProfilingInfo<CL_PROFILING_COMMAND_END>(&errcode);
//			if (errorCodeOut(errcode,"Error while getting profiling data") == -1)
//				return 1;
//			sumLoadTime+=end-start;
//	//					std::cout << "sumLoadTime " << sumLoadTime << std::endl;
//	//				std::cout<< "time needed XG " << end-start << " ns " << ((double)sizeof(double)*cLElemQMHS4->numElemPerCalc[i][l]*4*3/(double)((double)(end-start)/(double)1000000)) << " byte/s" << std::endl;
//		}


	//		firstPart+=end_firstPart-end_upload;
	//	std::vector<cl::Event>::iterator eventsIT;
	//	for(eventsIT=events.begin();eventsIT!=events.end();eventsIT++) {
	//		errcode = (*eventsIT).wait();
	//		if (errorCodeOut(errcode,"Error calculating Kernel firstPart") == -1)
	//			return 1;
	//	}
	/*	void secondPart(int elementsused,__global double* X,__global double* u,__global double* x,__global double* d,__global double* T3,__global double* xi1,__global double* xi2,__global double* dNdxi,__global double* T
			,__global double* H,__global double* t1,__global double* t2,__global double* Nsig,__global double* Nstr,__global double* D,__global double* detJ0,__global double* sig,__global double* str,__global int* layPropint,__global double* layProp
			,__global double* matProp,__global int* numLayArray,__global double* G,__global double* fs,__global int* numDOFEI,__global int* dof,__global int* CSScountArray,__global int* indexPerCSScl,__global double* fe,__global double* F
			,__global double* P,__global double* Kg) {

	*/
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			countElemCalc[i*4]=countElemCalc[i*4+2];
		}
		events2 = (cl_event*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_event));
	//	std::cout << "Second Part" << std::endl;
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			if(countElemCalc[i*4]<cLElemQMHS4->numCalcsPerDev[i]+startValPerDev[i]){
				int l=countElemCalc[i*4]-startValPerDev[i];

				cl_uint sizeArg = cLElemQMHS4->numElemPerCalc[i][l]; // size_t has invalid size for an argument
				err[0]  = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i],  0, sizeof(cl_uint),(void *) &sizeArg);
				err[1]  = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i],  1, sizeof(cl_mem), (void *) &cLElemQMHS4->XGcl[i]);
				err[2]  = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i],  2, sizeof(cl_mem), (void *) &cLElemQMHS4->uGcl[i]);
				err[3]  = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i],  3, sizeof(cl_mem), (void *) &cLElemQMHS4->xcl[i]);
				err[4]  = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i],  4, sizeof(cl_mem), (void *) &cLElemQMHS4->dcl[i]);
				err[5]  = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i],  5, sizeof(cl_mem), (void *) &cLElemQMHS4->T3Ecl[i]);
				err[6]  = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i],  6, sizeof(cl_mem), (void *) &cLElemQMHS4->xi1cl[i]);
				err[7]  = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i],  7, sizeof(cl_mem), (void *) &cLElemQMHS4->xi2cl[i]);
				err[8]  = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i],  8, sizeof(cl_mem), (void *) &cLElemQMHS4->dNdxicl[i]);
				err[9]  = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i],  9, sizeof(cl_mem), (void *) &cLElemQMHS4->Tcl[i]);
				err[10] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 10, sizeof(cl_mem), (void *) &cLElemQMHS4->Hcl[i]);
				err[11] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 11, sizeof(cl_mem), (void *) &cLElemQMHS4->t1cl[i]);
				err[12] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 12, sizeof(cl_mem), (void *) &cLElemQMHS4->t2cl[i]);
				err[13] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 13, sizeof(cl_mem), (void *) &cLElemQMHS4->Nsigcl[i]);
				err[14] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 14, sizeof(cl_mem), (void *) &cLElemQMHS4->Nstrcl[i]);
				err[15] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 15, sizeof(cl_mem), (void *) &cLElemQMHS4->Dcl[i]);
				err[16] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 16, sizeof(cl_mem), (void *) &cLElemQMHS4->detJ0cl[i]);
				err[17] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 17, sizeof(cl_mem), (void *) &cLElemQMHS4->sigGcl[i]);
				err[18] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 18, sizeof(cl_mem), (void *) &cLElemQMHS4->strGcl[i]);
				err[19] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 19, sizeof(cl_mem), (void *) &cLElemQMHS4->layPropintcl[i]);
				err[20] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 20, sizeof(cl_mem), (void *) &cLElemQMHS4->layPropcl[i]);
				err[21] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 21, sizeof(cl_mem), (void *) &cLElemQMHS4->matPropcl[i]);
				err[22] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 22, sizeof(cl_mem), (void *) &cLElemQMHS4->numLayArraycl[i]);
				err[23] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 23, sizeof(cl_mem), (void *) &cLElemQMHS4->GGcl[i]);
				err[24] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 24, sizeof(cl_mem), (void *) &cLElemQMHS4->fscl[i]);
				err[25] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 25, sizeof(cl_mem), (void *) &cLElemQMHS4->numDOFEIcl[i]);
				err[26] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 26, sizeof(cl_mem), (void *) &cLElemQMHS4->dofcl[i]);
				err[27] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 27, sizeof(cl_mem), (void *) &cLElemQMHS4->CSScountArraycl[i] );
				err[28] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 28, sizeof(cl_mem), (void *) &cLElemQMHS4->indexPerCSScl[i]);
				err[29] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 29, sizeof(cl_mem), (void *) &cLElemQMHS4->fecl[i]);
				err[30] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 30, sizeof(cl_mem), (void *) &cLElemQMHS4->Fcl[i]);
				err[31] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 31, sizeof(cl_mem), (void *) &cLElemQMHS4->Pcl[i]);
				err[32] = clSetKernelArg(cLElemQMHS4->clKernelSecondPart[i], 32, sizeof(cl_mem), (void *) &cLElemQMHS4->Kgcl[i]);
				countElemCalc[i*4]++;
				for(int j=0;j<33;j++) {
					if (errorCodeOut(err[j],"Error settig arguments for Kernel secondPart") == -1) {
						printf("at %d Elem %d\n", i, j);
						return 1;
					}
				}
				cl_event ev;
				errcode = clEnqueueNDRangeKernel(cLElemQMHS4->comandQueues[i], cLElemQMHS4->clKernelSecondPart[i], 1, NULL,
						&cLElemQMHS4->numElemPerCalc[i][l], &cLElemQMHS4->localWork[i][l], 1, &events[i], &ev);
				if (errorCodeOut(errcode,"Error starting calculation Kernel secondPart") == -1) {
					printf("commandQueue %d\n", i);
					return 1;
				}
				errcode = clFlush(cLElemQMHS4->comandQueues[i]);
				if (errorCodeOut(errcode,"Error Flush Kernel secondPart") == -1)
					return 1;
				events2[i] = ev;
			}
		}
		free(events);
	//	std::cout<< "finish2 " << std::endl;
//			for(unsigned int i=0;i<comandQueues.size();i++) {
//				errcode=comandQueues[i].finish();
//	//			std::cout << "comandQueue " << i << std::endl;
//				if (errorCodeOut(errcode,"Error while wait for all comandQues to finish") == -1) {
//					return 1;
//				}
//			}
//			for(int j=0;j<events2.size();j++) {
//				cl_ulong start=events2[j].getProfilingInfo<CL_PROFILING_COMMAND_START>(&errcode);
//				if (errorCodeOut(errcode,"Error while getting profiling data") == -1)
//					return 1;
//				cl_ulong end=events2[j].getProfilingInfo<CL_PROFILING_COMMAND_END>(&errcode);
//				if (errorCodeOut(errcode,"Error while getting profiling data") == -1)
//					return 1;
//				sumSecondPart+=end-start;
//		//					std::cout << "sumLoadTime " << sumLoadTime << std::endl;
//		//				std::cout<< "time needed XG " << end-start << " ns " << ((double)sizeof(double)*cLElemQMHS4->numElemPerCalc[i][l]*4*3/(double)((double)(end-start)/(double)1000000)) << " byte/s" << std::endl;
//			}
	//	for(eventsIT=events2.begin();eventsIT!=events2.end();eventsIT++) {
	//		errcode = (*eventsIT).wait();
	//		if (errorCodeOut(errcode,"Error calculating Kernel secondPart") == -1)
	//			return 1;
	//	}


	/*
	 * __kernel void thirdPart(int const elementsused,__global double* fe,__global double* fs,__global double* fe2,__global double* fem,__global double* P,__global double* F,__global double* P22,__global double* P21,__global double* Pm,__global double const * sig,__global double const * str,
							__global double* invF,__global double* invFT,__global double* G,__global double* Kg,__global int const * numDOFEI,__global double* Kt,__global double* fin) {

	 *
	 */
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			countElemCalc[i*4]=countElemCalc[i*4+2];
		}
	//	std::cout << "third Part" << std::endl;
		events3 = (cl_event*)malloc(cLElemQMHS4->clDevicesSize * sizeof(cl_event));
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			if(countElemCalc[i*4]<cLElemQMHS4->numCalcsPerDev[i]+startValPerDev[i]){
				int l=countElemCalc[i*4]-startValPerDev[i];
				err[0]  = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 0,  sizeof(cl_mem), (void *) &cLElemQMHS4->fecl[i]);
				err[1]  = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 1,  sizeof(cl_mem), (void *) &cLElemQMHS4->fscl[i]);
				err[2]  = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 2,  sizeof(cl_mem), (void *) &cLElemQMHS4->fe2cl[i]);
				err[3]  = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 3,  sizeof(cl_mem), (void *) &cLElemQMHS4->fe1qcl[i]);
				err[4]  = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 4,  sizeof(cl_mem), (void *) &cLElemQMHS4->Pcl[i]);
				err[5]  = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 5,  sizeof(cl_mem), (void *) &cLElemQMHS4->Fcl[i]);
				err[6]  = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 6,  sizeof(cl_mem), (void *) &cLElemQMHS4->P22Gcl[i]);
				err[7]  = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 7,  sizeof(cl_mem), (void *) &cLElemQMHS4->P21Gcl[i]);
				err[8]  = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 8,  sizeof(cl_mem), (void *) &cLElemQMHS4->P11qcl[i]);
				err[9]  = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 9,  sizeof(cl_mem), (void *) &cLElemQMHS4->sigGcl[i]);
				err[10] = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 10, sizeof(cl_mem), (void *) &cLElemQMHS4->strGcl[i]);
				err[11] = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 11, sizeof(cl_mem), (void *) &cLElemQMHS4->invFGcl[i]);
				err[12] = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 12, sizeof(cl_mem), (void *) &cLElemQMHS4->invFTGcl[i]);
				err[13] = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 13, sizeof(cl_mem), (void *) &cLElemQMHS4->GGcl[i]);
				err[14] = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 14, sizeof(cl_mem), (void *) &cLElemQMHS4->Kgcl[i]);
				err[15] = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 15, sizeof(cl_mem), (void *) &cLElemQMHS4->numDOFEIcl[i]);
				err[16] = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 16, sizeof(cl_mem), (void *) &cLElemQMHS4->Ktcl[i]);
				err[17] = clSetKernelArg(cLElemQMHS4->clKernelThirdPart[i], 17, sizeof(cl_mem), (void *) &cLElemQMHS4->fincl[i]);
				countElemCalc[i*4]++;
				for(int j=0;j<18;j++) {
					if (errorCodeOut(err[j],"Error setting arguments for Kernel thirdPart") == -1)
						return 1;
				}
				cl_event ev;
				errcode = clEnqueueNDRangeKernel(cLElemQMHS4->comandQueues[i], cLElemQMHS4->clKernelThirdPart[i], 1, NULL,
						&cLElemQMHS4->numElemPerCalc[i][l], &cLElemQMHS4->localWork[i][l], 1, &events2[i], &ev);
				if (errorCodeOut(errcode,"Error starting calculation Kernel thirdPart") == -1)
					return 1;
				errcode = clFlush(cLElemQMHS4->comandQueues[i]);
				if (errorCodeOut(errcode,"Error Flush Kernel thirdPart") == -1)
					return 1;
				events3[i] = ev;
			}
		}
		free(events2);
	//	std::cout<< "finish4 " << std::endl;
//			for(unsigned int i=0;i<comandQueues.size();i++) {
//				errcode=comandQueues[i].finish();
//	//			std::cout << "comandQueue " << i << std::endl;
//				if (errorCodeOut(errcode,"Error while wait for all comandQues to finish") == -1) {
//					return 1;
//				}
//			}
//		for(int j=0;j<events3.size();j++) {
//			cl_ulong start=events3[j].getProfilingInfo<CL_PROFILING_COMMAND_START>(&errcode);
//			if (errorCodeOut(errcode,"Error while getting profiling data") == -1)
//				return 1;
//			cl_ulong end=events3[j].getProfilingInfo<CL_PROFILING_COMMAND_END>(&errcode);
//			if (errorCodeOut(errcode,"Error while getting profiling data") == -1)
//				return 1;
//			sumThirdPart+=end-start;
	//					std::cout << "sumLoadTime " << sumLoadTime << std::endl;
	//				std::cout<< "time needed XG " << end-start << " ns " << ((double)sizeof(double)*cLElemQMHS4->numElemPerCalc[i][l]*4*3/(double)((double)(end-start)/(double)1000000)) << " byte/s" << std::endl;
//		}
	//	for(eventsIT=events4.begin();eventsIT!=events4.end();eventsIT++) {
	//		errcode = (*eventsIT).wait();
	//		if (errorCodeOut(errcode,"Error calculating Kernel thirdPart") == -1)
	//			return 1;
	//	}
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			countElemCalc[i*4]=countElemCalc[i*4+2];
			countElemCalc[i*4+1]=countElemCalc[i*4+3];
		}


//		sumLoadTime=0;
//		memtransfered=0;
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			if(countElemCalc[i*4]<cLElemQMHS4->numCalcsPerDev[i]+startValPerDev[i]){
			// Reading out the calculated values
				int l=countElemCalc[i*4]-startValPerDev[i];
//				if(i == 0 && k == 0)
//std::cout<< "save " <<countElemCalc[i*4+1]<< " "<< cLElemQMHS4->numElemPerCalc[i][l]<< " " << countElemCalc[i*4+1]<< std::endl;
				err[0]  = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->Ktcl[i], 	CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*24*24* sizeof(double), 	(void*)&Kt[countElemCalc[i*4+1]*24*24],
						1, &events3[i], &event[0]);
				err[1]  = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->fincl[i], 	CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*24* sizeof(double), 		(void*)&fin[countElemCalc[i*4+1]*24],
						1, &events3[i], &event[1]);
				err[2]  = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->invFGcl[i], CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*14*14*sizeof(double), 	(void*)&cLElemQMHS4->invF[countElemCalc[i*4+1]*14*14],
						1, &events3[i], &event[2]);
				err[3]  = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->invFTGcl[i],CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*14*14* sizeof(double), 	(void*)&cLElemQMHS4->invFT[countElemCalc[i*4+1]*14*14],
						1, &events3[i], &event[3]);
				err[4]  = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->GGcl[i], 	CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*14*24* sizeof(double), 	(void*)&cLElemQMHS4->G[countElemCalc[i*4+1]*14*24],
						1, &events3[i], &event[4]);
				err[5]  = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->P11qcl[i], 	CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*14*14* sizeof(double), 	(void*)&cLElemQMHS4->P11q[countElemCalc[i*4+1]*14*14],
						1, &events3[i], &event[5]);
				err[6]  = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->P22Gcl[i], 	CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*6*6* sizeof(double), 		(void*)&cLElemQMHS4->P22[countElemCalc[i*4+1]*6*6],
						1, &events3[i], &event[6]);
				err[7]  = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->P21Gcl[i], 	CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*14*6* sizeof(double), 	(void*)&cLElemQMHS4->P21[countElemCalc[i*4+1]*14*6],
						1, &events3[i], &event[7]);
				err[8]  = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->T3Ecl[i],	CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*3*3*4* sizeof(double), 	(void*)&cLElemQMHS4->T3[countElemCalc[i*4+1]*3*3*4],
						1, &events3[i], &event[8]);
				err[9]  = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->fe1qcl[i], 	CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*14* sizeof(double), 		(void*)&cLElemQMHS4->fe1q[countElemCalc[i*4+1]*14],
						1, &events3[i], &event[9]);
				err[10] = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->fe2cl[i], 	CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*6* sizeof(double), 		(void*)&cLElemQMHS4->fe2[(countElemCalc[i*4+1])*6],
						1, &events3[i], &event[10]);
				err[11] = clEnqueueReadBuffer(cLElemQMHS4->comandQueues[i], cLElemQMHS4->fscl[i], 	CL_FALSE, 0,
						cLElemQMHS4->numElemPerCalc[i][l]*14* sizeof(double), 		(void*)&cLElemQMHS4->fs[countElemCalc[i*4+1]*14],
						1, &events3[i], &event[11]);
				for(int j=0;j<12;j++) {
					if (errorCodeOut(err[j],"Error reading result from Buffer") == -1)
						return 1;
				}

//					for(int j=0;j<12;j++) {
//						errcode = event[j].wait();
//						if (errorCodeOut(errcode,"Error while reading result from Buffer") == -1)
//							return 1;
//					}
//					for(int j=0;j<12;j++) {
//						cl_ulong start=event[j].getProfilingInfo<CL_PROFILING_COMMAND_START>(&errcode);
//						if (errorCodeOut(errcode,"Error while getting profiling data") == -1)
//							return 1;
//						if (errorCodeOut(errcode,"Error while getting profiling data") == -1)
//							return 1;
//	//					std::cout <<"start " << start << std::endl;
//	//					std::cout << "end " << end << std::endl;
//						sumLoadTime+=end-start;
//	//					std::cout << "sumLoadTime "<< j << "\t" << sumLoadTime << std::endl;
//
//	//				std::cout<< "time needed XG " << end-start << " ns " << ((double)sizeof(double)*cLElemQMHS4->numElemPerCalc[i][l]*4*3/(double)((double)(end-start)/(double)1000000)) << " byte/s" << std::endl;
//					}
//					memtransfered+=sizeof(double)*cLElemQMHS4->numElemPerCalc[i][l]*(24*24+24+14*14*3+14*24+6*6+14*6+3*3*4+14*2+6);
				countElemCalc[i*4]++;
				countElemCalc[i*4+1]+=cLElemQMHS4->numElemPerCalc[i][l];
			} else
				printf("NOT %d %d\n", countElemCalc[i*4], cLElemQMHS4->numCalcsPerDev[i]+startValPerDev[i]);
		}
		free(events3);
	//	std::cout <<"memTransf " <<memtransfered/(1024*1024) << " Mb"<< std::endl;
	//	std::cout <<"sumLoadTime " <<sumLoadTime/(1000000) << " ms"<< std::endl;
	//	std::cout<<"Time Download "<<sumLoadTime/1000000<< " msec Download speed " << ((double)memtransfered/(double)((double)(sumLoadTime)/(double)1000000000)/((double)1024*1024)) << " Mbyte/s" << std::endl;
//		wholeDownTransTime+=sumLoadTime;
//		wholeDownMemTrans+=memtransfered;
	//	std::cout<< "finish " << std::endl;
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
	//		std::cout << "comandQueue " << i << std::endl;
			errcode=clFinish(cLElemQMHS4->comandQueues[i]);

			if (errorCodeOut(errcode,"Error while wait for all comandQues to finish") == -1) {
				return 1;
			}
		}

		calculated=1;
		for(unsigned int i=0;i<cLElemQMHS4->clDevicesSize;i++) {
			countElemCalc[i*4+2]=countElemCalc[i*4];
			countElemCalc[i*4+3]=countElemCalc[i*4+1];

			if((countElemCalc[i*4]<(cLElemQMHS4->numCalcsPerDev[i]+startValPerDev[i]))) {
				calculated=0;
			}
		}
	}
#ifndef WIN32
//	std::cout<<"Time firstPart "<< sumLoadTime/1000000<< " msec " << std::endl;
//	std::cout<<"Time secondPart "<< sumSecondPart/1000000<< " msec " << std::endl;
//	std::cout<<"Time tirdPart "<< sumThirdPart/1000000<< " msec " << std::endl;
	gettimeofday(&end, 0);
//	std::cout<<"Time whole Upload "<<(double)wholeUpTransTime/(double)1000000<< " msec Download speed " << ((double)wholeUpMemTrans/(double)((double)(wholeUpTransTime)/(double)1000000000)/((double)1024*1024)) << " Mbyte/s" << std::endl;
//	std::cout<<"Time whole Download "<<(double)wholeDownTransTime/(double)1000000<< " msec Download speed " << ((double)wholeDownMemTrans/(double)((double)(wholeDownTransTime)/(double)1000000000)/((double)1024*1024)) << " Mbyte/s" << std::endl;
#endif

	free(event);
	free(countElemCalc);
	free(startValPerDev);
	long sec=end.tv_sec-start.tv_sec;
	long millisec=(sec*1000+end.tv_usec/1000)-start.tv_usec/1000;
	printf("\nTime openCL device %ld msec \n", millisec);
	counterNumElem=0;
	size_t elemcounter=0;
	for(icoShellSectionsIT = (struct icoSSData*)ght_first(cLElemQMHS4->icoShellSections, &iterator, (const void**)&icoShellSectionsKey);
			icoShellSectionsIT;
			icoShellSectionsIT = (struct icoSSData*)ght_next(cLElemQMHS4->icoShellSections, &iterator, (const void**)&icoShellSectionsKey)) {
//	for( icoShellSectionsIT=cLElemQMHS4->icoShellSections.begin(); icoShellSectionsIT!=cLElemQMHS4->icoShellSections.end(); icoShellSectionsIT++) {
		int* elementsOfCSS=(int*)ght_get(cLElemQMHS4->elsets, sizeof(char*), icoShellSectionsKey);
//		int* elementsOfCSS=cLElemQMHS4->elsets[ icoShellSectionsKey ];
		elementNr=elementsOfCSS[0];
/***		if( elements[elementNr].getElementType().compare( "QMHS4")==0) { we only use QMHS4 ***/
		if(1) {
			size_t currElsetSize = *(size_t*)ght_get(cLElemQMHS4->elsetSize, sizeof(char*), icoShellSectionsKey);
printf("cureElsetSize: %lu\n", currElsetSize);
			for(unsigned int i=0; i< currElsetSize; i++) {
//printf("accessing : %u\n", i);
				int elementNr_=elementsOfCSS[i];
//				for(int j=0;j<numDOFEI[counterNumElem];j++) {
//					for(int r=0;r<numDOFEI[counterNumElem];r++) {
//						allKte[elementNr_].kte[j][r]=Kt[counterNumElem*24*24+j*numDOFEI[counterNumElem]+r];
////					std::cout <<std::setprecision (8) <<allKte[elementNr_].kte[j][r] << "\t";
//					}
////				std::cout << std::endl;
////					std::cout <<std::setprecision (5) <<fin[counterNumElem*24+j] << "\t";
//					allKte[elementNr_].rhs[j]=fin[counterNumElem*24+j];
//				}
//				std::cout  <<std::endl;
				elementsIndex[elemcounter+i]=elementNr_;
//!				(*invFG)[elementNr_].arrayCopyToMat(&invF[counterNumElem*14*14],14,14);
//!				(*invFTG)[elementNr_].arrayCopyToMat(&invFT[counterNumElem*14*14],14,14);
//!				(*P11qG)[elementNr_].arrayCopyToMat(&P11q[counterNumElem*14*14],14,14);
//!				(*GG)[elementNr_].arrayCopyToMat(&G[counterNumElem*14*24],14,numDOFperindex[counterNumElem]);
//!				(*P21G)[elementNr_].arrayCopyToMat(&P21[counterNumElem*14*6],14,6);
//!				(*fe1qG)[elementNr_].arrayCopyToVec(&fe1q[counterNumElem*14],14);
//!				(*fsG)[elementNr_].arrayCopyToVec(&fs[counterNumElem*14],14);
//!				(*P22G)[elementNr_].arrayCopyToMat(&P22[counterNumElem*6*6],6,6);
//!				(*fe2G)[elementNr_].arrayCopyToVec(&fe2[counterNumElem*6],6);

				for(int l=0;l<4;l++) {
//					globalNr=elements[elementNr_].getGlobalNrOfNodeNr( l);
					globalNr=cLElemQMHS4->nrOfOrder[counterNumElem * 4 + l];
//					if( nodes[globalNr].getLocalDOF()==5) { always true
						for(int k=0;k<3;k++) {
							for(int j=0;j<2;j++) {
//								(*T3E)[globalNr].set(k,j, T3[counterNumElem*3*3*4+k*3*4+j*4+l]);
								cLElemQMHS4->T3Ptr[globalNr*3*2 + k + j*3] = cLElemQMHS4->T3[counterNumElem*3*3*4+k*3*4+j*4+l];
							}
						}
//					}
				}
				counterNumElem++;
			}
			elemcounter+=*(size_t*)ght_get(cLElemQMHS4->elsetSize, sizeof(char*), icoShellSectionsKey);
		}
	}

	return 0;
}

const char* print_cl_errstring(const cl_int err) {
		switch (err) {
		case CL_SUCCESS:
			return ("Success!"); break;
		case CL_DEVICE_NOT_FOUND:
			return ("Device not found."); break;
		case CL_DEVICE_NOT_AVAILABLE:
			return ("Device not available"); break;
		case CL_COMPILER_NOT_AVAILABLE:
			return ("Compiler not available"); break;
		case CL_MEM_OBJECT_ALLOCATION_FAILURE:
			return ("Memory object allocation failure"); break;
		case CL_OUT_OF_RESOURCES:
			return ("Out of resources"); break;
		case CL_OUT_OF_HOST_MEMORY:
			return ("Out of host memory"); break;
		case CL_PROFILING_INFO_NOT_AVAILABLE:
			return ("Profiling information not available"); break;
		case CL_MEM_COPY_OVERLAP:
			return ("Memory copy overlap"); break;
		case CL_IMAGE_FORMAT_MISMATCH:
			return ("Image format mismatch"); break;
		case CL_IMAGE_FORMAT_NOT_SUPPORTED:
			return ("Image format not supported"); break;
		case CL_BUILD_PROGRAM_FAILURE:
			return ("Program build failure"); break;
		case CL_MAP_FAILURE:
			return ("Map failure"); break;
		case CL_INVALID_VALUE:
			return ("Invalid value"); break;
		case CL_INVALID_DEVICE_TYPE:
			return ("Invalid device type"); break;
		case CL_INVALID_PLATFORM:
			return ("Invalid platform"); break;
		case CL_INVALID_DEVICE:
			return ("Invalid device"); break;
		case CL_INVALID_CONTEXT:
			return ("Invalid context"); break;
		case CL_INVALID_QUEUE_PROPERTIES:
			return ("Invalid queue properties"); break;
		case CL_INVALID_COMMAND_QUEUE:
			return ("Invalid command queue"); break;
		case CL_INVALID_HOST_PTR:
			return ("Invalid host pointer"); break;
		case CL_INVALID_MEM_OBJECT:
			return ("Invalid memory object"); break;
		case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:
			return ("Invalid image format descriptor"); break;
		case CL_INVALID_IMAGE_SIZE:
			return ("Invalid image size"); break;
		case CL_INVALID_SAMPLER:
			return ("Invalid sampler"); break;
		case CL_INVALID_BINARY:
			return ("Invalid binary"); break;
		case CL_INVALID_BUILD_OPTIONS:
			return ("Invalid build options"); break;
		case CL_INVALID_PROGRAM:
			return ("Invalid program"); break;
		case CL_INVALID_PROGRAM_EXECUTABLE:
			return ("Invalid program executable"); break;
		case CL_INVALID_KERNEL_NAME:
			return ("Invalid kernel name"); break;
		case CL_INVALID_KERNEL_DEFINITION:
			return ("Invalid kernel definition"); break;
		case CL_INVALID_KERNEL:
			return ("Invalid kernel"); break;
		case CL_INVALID_ARG_INDEX:
			return ("Invalid argument index"); break;
		case CL_INVALID_ARG_VALUE:
			return ("Invalid argument value"); break;
		case CL_INVALID_ARG_SIZE:
			return ("Invalid argument size"); break;
		case CL_INVALID_KERNEL_ARGS:
			return ("Invalid kernel arguments"); break;
		case CL_INVALID_WORK_DIMENSION:
			return ("Invalid work dimension"); break;
		case CL_INVALID_WORK_GROUP_SIZE:
			return ("Invalid work group size"); break;
		case CL_INVALID_WORK_ITEM_SIZE:
			return ("Invalid work item size"); break;
		case CL_INVALID_GLOBAL_OFFSET:
			return ("Invalid global offset"); break;
		case CL_INVALID_EVENT_WAIT_LIST:
			return ("Invalid event wait list"); break;
		case CL_INVALID_EVENT:
			return ("Invalid event"); break;
		case CL_INVALID_OPERATION:
			return ("Invalid operation"); break;
		case CL_INVALID_GL_OBJECT:
			return ("Invalid OpenGL object"); break;
		case CL_INVALID_BUFFER_SIZE:
			return ("Invalid buffer size"); break;
		case CL_INVALID_MIP_LEVEL:
			return ("Invalid mip-map level"); break;
		default:
			return ("Unknown error");
		}
	}
