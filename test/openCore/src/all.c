#include <stdio.h>
#include <string.h>
#include <sys/time.h>

#include <math.h>
#include <CL/cl.h>

#include "ght_hash_table.h"
#include "config.h"
#include "utils.h"
#include "Material.h"
#include "Element.h"
#include "clutils.h"
#include "futils.h"
#include "loadCheckSave.h"


int main()  {
    double* sigPtr = 0;
    double* strPtr = 0;
    double* Dsig = 0;
    double* Dstr = 0;
    double* invF = 0;
    double* invFT = 0;
    double* G = 0;
    double* P11q = 0;
    double* P22 = 0;
    double* P21 = 0;
    double* T3Ptr = 0;
    double* fe1q = 0;
    double* fe2 = 0;
    double* fs = 0;
    double* u = 0;
    double* LBase = 0;
    double* XG = 0;
    size_t* nrOfOrder = 0;
    ght_hash_table_t* icoShellSections = 0; //new std::map<const char*, icoSSData* >
    ght_hash_table_t* elsets = 0; //new std::map<const char*, int* >;
    ght_hash_table_t* issSize = 0;
    ght_hash_table_t* elsetSize = 0;
    ght_hash_table_t* mat = 0;

    int* interSec = 0;
    ght_hash_table_t* elements = 0;
	int elast = 0;

	unsigned int numShellSections;
	int sumNumLay;
	size_t elementsused;
	int seccount;
	int* indexPerCSS;
	double* sig;
	double* str;
    double* T3 = 0;
/*** temporaries as pointers ***/
	int* numLayArray;
	double* layProp;
	int* layPropint;
	double* matProp;
	int* matID;
	int* CSScountArray;
//	int* numDOFEI;
	int* dof;
	cl_command_queue* comandQueues;
	size_t* numElemPerCalcSize;
	cl_context* clContextArray;
	cl_device_id* clDevices = NULL;
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
	size_t** numElemPerCalc = NULL;
	size_t* numCalcsPerDev;
	size_t* elemCalcPerDev;
	size_t** localWork = NULL;


	///////////////////////////////
	size_t clDevicesSize;



//    constructCLElemQMHS4(&cLElemQMHS4);
	// sampling points:
	double xi;
	double xi1[4];
	double xi2[4];
	double dNdxi[2*4*4];
	xi = sqrt(1.0/((double)3.0));
	xi1[0]=xi; xi1[1]=-1*xi; xi1[2]=-1*xi; xi1[3]=xi;
	xi2[0]=xi; xi2[1]=xi; xi2[2]=-1*xi; xi2[3]=-1*xi;

	//local derivatives of shape functions:
	for(int i=0;i<4;i++) {
		dNdxi[0*4*4+0*4+i]=0.25*(1+xi2[i]);
		dNdxi[0*4*4+1*4+i]=-0.25*(1+xi2[i]);
		dNdxi[0*4*4+2*4+i]=-0.25*(1-xi2[i]);
		dNdxi[0*4*4+3*4+i]=0.25*(1-xi2[i]);

		dNdxi[1*4*4+0*4+i]=0.25*(1+xi1[i]);
		dNdxi[1*4*4+1*4+i]=0.25*(1-xi1[i]);
		dNdxi[1*4*4+2*4+i]=-0.25*(1-xi1[i]);
		dNdxi[1*4*4+3*4+i]=-0.25*(1+xi1[i]);

		}

    // set up path to files
    char path[256];

#ifdef WIN32
    sprintf(path, "C:/Users/klois/uni/OpenCore/save/");
#else
    sprintf(path, "/home/klaus/dpsnfs/save/");
#endif
    char filenameAddition[128];
    sprintf(filenameAddition, "before-1-0.5-");

    printf("Reading input data from files\n");
    size_t vertices = 0;
    size_t elems = setAll(path, filenameAddition, &sigPtr, &strPtr, &Dsig, &Dstr, &invF, &invFT, &G, &P11q, &P22, &P21, &T3Ptr, &fe1q, &fe2, &fs,
            &u, &LBase, &XG, &nrOfOrder, &icoShellSections, &elsets, &issSize, &elsetSize, &mat, &interSec, &elements, &elast, &vertices);

    printf("Calling initCalcQMHS4firstTime\n");
//    if(initCalcQMHS4firstTime(&cLElemQMHS4, 1024, 1) != 0) {
//        printf("initCalcQMHS4firstTime FAILED!\n");
//        return -1;
//    }
	const int numElems = 1024;
//	const int numKernel = 1;
	{
		struct icoSSData* icoShellSectionsIT;
		void* icoShellSectionsKey;
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
		numShellSections= ght_size(icoShellSections);
		sumNumLay=0;
		elementsused=0;
		seccount=0;

		for(icoShellSectionsIT = (struct icoSSData*)ght_first(icoShellSections, &iterator, (const void**)&icoShellSectionsKey);
				icoShellSectionsIT;
				icoShellSectionsIT = (struct icoSSData*)ght_next(icoShellSections, &iterator, (const void**)&icoShellSectionsKey)) {
	//	for( icoShellSectionsIT=icoShellSections.begin(); icoShellSectionsIT!=icoShellSections.end(); icoShellSectionsIT++) {
			int numLay= *(int*)ght_get(issSize, sizeof(char*), icoShellSectionsKey);
	//		int numLay=issSize[(*icoShellSectionsIT).first];
	//printf("KEY: %s\n %lx ", icoShellSectionsKey, (unsigned long)elsets[ icoShellSectionsKey ]);
			int* elementsOfCSS=(int*)ght_get(elsets, sizeof(char*), icoShellSectionsKey);
			elementNr=elementsOfCSS[0];
	/***		if( elements[elementNr].getElementType().compare( "QMHS4")==0) { we only use QMHS4 ***/
			if(1) {
				// not all elements are used for QMHS4 so get only the one that are needed
				sumNumLay+=numLay;
				seccount++;
	//			elementsused+=elementsOfCSS.size();
			}
		}
		elementsused=ght_size(elements);
	//	std::cout << "seccout "<<seccount << std::endl;
	//	std::cout << "Elements " << elementsused << std::endl;
		indexPerCSS= (int*)malloc(seccount * sizeof(int));
		sig= (double*)malloc(elementsused*14 * sizeof(double));
		str= (double*)malloc(elementsused*20 * sizeof(double));
		numLayArray= (int*)malloc(seccount * sizeof(int));
		layProp= (double*)malloc(sumNumLay*2 * sizeof(double));
		layPropint=(int*)malloc(sumNumLay*2 * sizeof(int));
		matProp=(double*)malloc(sumNumLay*2 * sizeof(double));
		matID=(int*)malloc(sumNumLay * sizeof(int));
		T3=(double*)malloc(3*3*4*elementsused * sizeof(double));
		CSScountArray=(int*)malloc(elementsused * sizeof(int));
		dof=(int*)malloc(elementsused*24 * sizeof(int));

		for(int i=0;i<24*elementsused;i++) {
			dof[i]=0;
		}

		for(icoShellSectionsIT = (struct icoSSData*)ght_first(icoShellSections, &iterator, (const void**)&icoShellSectionsKey);
				icoShellSectionsIT;
				icoShellSectionsIT = (struct icoSSData*)ght_next(icoShellSections, &iterator, (const void**)&icoShellSectionsKey)) {
	//	for( icoShellSectionsIT=icoShellSections.begin(); icoShellSectionsIT!=icoShellSections.end(); icoShellSectionsIT++) {
			int numLay=*(int*)ght_get(issSize, sizeof(char*), icoShellSectionsKey);
			int* elementsOfCSS=(int*)ght_get(elsets, sizeof(char*), icoShellSectionsKey);
	//		int* elementsOfCSS=elsets[ icoShellSectionsKey ];
			elementNr=elementsOfCSS[0];

	/***		if( elements[elementNr].getElementType().compare( "QMHS4")==0) { we only use QMHS4 ***/
			if(1) {
				for(int j=0; j<numLay; j++) {
					struct Material* material = (struct Material*)ght_get(mat, sizeof(char*), icoShellSectionsIT[j].matName);
					matProp[counterNumLay*2+2*j+0]=material->eModul;
					matProp[counterNumLay*2+2*j+1]=material->nue;
					matID[counterNumLay+j]=2;										//number of values for THIS material in matProp; needs if-switch for plastic-material
					layProp[counterNumLay*2+2*j]= icoShellSectionsIT[j].ug;				//lower bound
					layProp[counterNumLay*2+2*j+1]= icoShellSectionsIT[j].og;				//upper bound
					layPropint[counterNumLay*2+2*j]=j;								//matrial ID in array matProp
					layPropint[counterNumLay*2+2*j+1]= icoShellSectionsIT[j].intPoints;	//number integration points
				}
				indexPerCSS[counterLay]=counterNumLay;
				numLayArray[counterLay]=numLay;
				counterLay++;
				counterNumLay+=numLay;
				size_t currElsetSize = *(size_t*)ght_get(elsetSize, sizeof(char*), icoShellSectionsKey);
				for(unsigned int  i=0; i< currElsetSize; i++) {
	//!				int elementNr_=elementsOfCSS[i];
					CSScountArray[counterNumElem]=counterLay-1;

					for(int k=0; k<4; k++) {
						/*** seems to be unused ***/
						interSec[counterNumElem*4+k] = 0; //!!!!!ShInter[elementNr_].i4[k];
					}
					//lBase
	//				std::cout << std::endl;

					int adder=0;
					for(int j=0;j<24;j++) {

						if((!((interSec[counterNumElem*4+0]==0)&&(j==5)))&&
								(!((interSec[counterNumElem*4+1]==0)&&(j==11)))&&(!((interSec[counterNumElem*4+2]==0)&&
								(j==17)))&&(!((interSec[counterNumElem*4+3]==0)&&(j==23)))) {
							dof[counterNumElem*24+adder]=j;
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

		clDevicesSize = 0;
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
				clDevices = (cl_device_id*)realloc(clDevices, (clDevicesSize+gdsTmp) * sizeof(cl_device_id));

			for(unsigned int j=0;j<gdsTmp;j++) {
				clDevices[j+clDevicesSize] = clDevicesGPU[j];
			}
			clDevicesSize += gdsTmp;
			free(clDevicesGPU);
		}
	#endif
		if(clDevicesSize==0) {
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
					clDevices = (cl_device_id*)realloc(clDevices, (clDevicesSize+cdsTmp) * sizeof(cl_device_id));

				for(unsigned int j=0;j<cdsTmp;j++) {
					clDevices[j+clDevicesSize] = clDevicesCPU[j];
				}
				clDevicesSize += cdsTmp;
				free(clDevicesCPU);
			}
		}
		if(clDevicesSize==0) {
			printf("No device found\n");
			return 1;
		}

		// release platforms
		free(platforms);

	//	if(clDevicesSize>1) {			// only needed for test
	//		clDevicesSize = 1;
	//		clDevices.pop_back();
	//		clDevices.erase(clDevices.begin());
	//	}

		for(unsigned int i=0;i<clDevicesSize;){
			char extensions[2048];
			errcode =clGetDeviceInfo(clDevices[i], CL_DEVICE_EXTENSIONS, 2048, &extensions, 0);
			if (!(strstr(extensions, "cl_amd_fp64")||strstr(extensions, "cl_khr_fp64")) || errcode != CL_SUCCESS) {
				// TODO add release
				cl_device_id toremove = clDevices[i];
				--clDevicesSize;
				for(unsigned int j = i;i<clDevicesSize;j++)
					clDevices[j] = clDevices[j+1];
				clDevices[clDevicesSize] = toremove;
			} else
				++i;
		}
	/***	std::vector < cl::Device >::iterator toremoveIT;
		for(toremoveIT=toremove.begin();toremoveIT!=toremove.end();toremoveIT++){
			clDevices.erase(toremoveIT);
		}***/
		if(clDevicesSize==0) {
			printf("Devices do not support double precision extension!\n");
			return 1;
		}

		printf("Devices found:\n");
		for(unsigned int i=0;i< clDevicesSize;i++) {
			errcode =clGetDeviceInfo(clDevices[i], CL_DEVICE_VERSION, 512, &devVersion, 0);
			errcode|=clGetDeviceInfo(clDevices[i], CL_DEVICE_NAME, 512, &devName, 0);
			errcode|=clGetDeviceInfo(clDevices[i], CL_DEVICE_VENDOR, 512, &devVend, 0);
			errcode|=clGetDeviceInfo(clDevices[i], CL_DRIVER_VERSION, 512, &devVers, 0);
			if (errorCodeOut(errcode,"Error reading device info") == -1)
				return 1;

	//		std::cout <<"Max WorkG "<< clDevices[i].getInfo< CL_DEVICE_MAX_WORK_ITEM_SIZES>()[1]<< std::endl;
			printf("Device %i %s %s %s %s\n", i, devVend, devName, devVers, devVersion);
		}
		err= (cl_int*)malloc(clDevicesSize * sizeof(cl_int));
		for(unsigned int i=0;i<clDevicesSize;i++){
			err[i]=0;
		}

		clContextArray = (cl_context*)malloc(clDevicesSize * sizeof(cl_context));

		for(unsigned int i=0;i<clDevicesSize;i++) {

	//!		clContextArray.push_back(cl::Context(devV,NULL,NULL,NULL,&errcode));
			clContextArray[i] = clCreateContext(NULL, 1, &clDevices[i],NULL,NULL,&errcode);
			if (errorCodeOut(errcode,"Error creating Context") == -1)
				return 1;
		}
	/*** seems to be unused
		long int* maxElemPerDev=new long int[clDevices.size()]; */
		comandQueues= (cl_command_queue*)malloc(clDevicesSize * sizeof(cl_command_queue));

		for(unsigned int i=0;i< clDevicesSize;i++) {
			cl_ulong mas = 0, wgs = 0;
			errcode =clGetDeviceInfo(clDevices[i], CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof(size_t), &wgs, 0);
			if (errorCodeOut(errcode,"Error reading the maximal work group size") == -1)
				return 1;
			comandQueues[i]=clCreateCommandQueue(clContextArray[i], clDevices[i], 0, &errcode);
			if (errorCodeOut(errcode,"Error creating ComandQueues") == -1)
				return 1;
			errcode =clGetDeviceInfo(clDevices[i], CL_DEVICE_MAX_MEM_ALLOC_SIZE, sizeof(cl_ulong), &mas, 0);
			if (errorCodeOut(errcode,"Error reading memory size infomation") == -1)
				return 1;
			if(mas<wgs*50000) {
				printf("Device has too few memory %lu\n", mas);
				return 1;
			}
		}

		size_t elem=elementsused/clDevicesSize;
		size_t rest=elementsused%clDevicesSize;
		elemCalcPerDev= (size_t*)malloc(clDevicesSize * sizeof(size_t));
		numCalcsPerDev= (size_t*)malloc(clDevicesSize * sizeof(size_t));

	//	elementsCalcPerKernel= (int*)malloc(clDevicesSize * sizeof(int));
		printf("OpenCL Threads used: %d", numElems);
		int allign;
		if(numElems<64) {
			allign=numElems;
		} else {
			allign=64;
		}
		printf("Opencl local Threads used: %d\n", allign);
		numElemPerCalc = (size_t**)malloc(clDevicesSize * sizeof(size_t*));
		numElemPerCalcSize = (size_t*)malloc(clDevicesSize * sizeof(size_t));
		localWork = (size_t**)malloc(clDevicesSize * sizeof(size_t*));
		for(unsigned int i=0;i< clDevicesSize;i++) {
	//		elementsCalcPerKernel[i]=numElems;
			if(rest!=0) {
				elemCalcPerDev[i]=elem+1;
				rest--;
			} else {
				elemCalcPerDev[i]=elem;
			}
	//		std::cout << "elemcpd " << elemCalcPerDev[i] << " numElems " << numElems << " align " << allign << std::endl;
			int additional=splitfunction(elemCalcPerDev[i],numElems,allign,&numElemPerCalc[i],&numElemPerCalcSize[i],i);

			numCalcsPerDev[i]=numElemPerCalcSize[i];
			localWork[i] = (size_t*)malloc(numCalcsPerDev[i] * sizeof(size_t));
			for(int j=0;j<numCalcsPerDev[i];j++) {
				localWork[i][j]=allign;
			}
			if(additional!=0) {	// if additional is not 0 there is rest as a result is calculated as last part splitfunction has already made numElemPerCalc one calculation bigger
				localWork[i][numCalcsPerDev[i]-1]=additional;
				numElemPerCalc[i][numCalcsPerDev[i]-1]=additional;
			}
		}
		// to do splitt elements in smaller parts biggest <=10000 elements
		Tcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		xcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		Hcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		dcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		t1cl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		t2cl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		Nsigcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		Nstrcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		Dcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		detJ0cl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		fecl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		Fcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		Pcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		Kgcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		CSScountArraycl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		interSeccl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		LBaseGcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		numDOFEIcl  = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		dofcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		T3Ecl  = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		Ktcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		fincl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		XGcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		uGcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		sigGcl  = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		strGcl  = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		invFGcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		invFTGcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		GGcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		P11qcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		P22Gcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		P21Gcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		fe1qcl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		fe2cl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		fscl = (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));



		numLayArraycl= (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		indexPerCSScl= (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		layPropintcl= (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		matPropcl= (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		matIDcl= (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		layPropcl= (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		xi1cl= (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		xi2cl= (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
		dNdxicl= (cl_mem*)malloc(clDevicesSize * sizeof(cl_mem));
	//std::cout << "seccount " <<  seccount<< " " <<sumNumLay << std::endl;

		free(err);
		err= (cl_int*)malloc(35 * sizeof(cl_int));
		for(unsigned int i=0;i<clDevicesSize;i++) {
			numLayArraycl[i] 	= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*seccount,		NULL, &err[0]);
			indexPerCSScl[i] 	= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*seccount,		NULL, &err[1]);
			layPropintcl[i] 	= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*sumNumLay*2,		NULL, &err[2]);
			matPropcl[i] 		= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*sumNumLay*2,	NULL, &err[3]);
			matIDcl[i] 		= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*sumNumLay,		NULL, &err[4]);
			layPropcl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*sumNumLay*2,	NULL, &err[5]);
			xi1cl[i] 			= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*4,							NULL, &err[6]);
			xi2cl[i]			= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*4,							NULL, &err[7]);
			dNdxicl[i]			= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*2*4*4,						NULL, &err[8]);

			for(int j=0;j<9;j++) {
				if (errorCodeOut(err[j],"Error creating buffer") == -1)
					return -1;
			}
		}

		for(unsigned int i=0;i<clDevicesSize;i++) {
			Tcl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*3*3*4,	NULL, &err[0]);
			xcl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*3*4,		NULL, &err[1]);
			Hcl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*3*3*4,	NULL, &err[2]);
			dcl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*3*4,		NULL, &err[3]);
			t1cl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*3,		NULL, &err[4]);
			t2cl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*3,		NULL, &err[5]);
			Nsigcl[i]	= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*8*14*4,	NULL, &err[6]);
			Nstrcl[i]	= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*10*20*4,	NULL, &err[7]);
			Dcl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*4*3,		NULL, &err[8]);
			detJ0cl[i]	= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0],			NULL, &err[9]);
			fecl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*20,		NULL, &err[10]);
			Fcl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*20*14,	NULL, &err[11]);
			Pcl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*20*20,	NULL, &err[12]);
			Kgcl[i] 		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*24*24,	NULL, &err[13]);
			CSScountArraycl[i] = clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*numElemPerCalc[i][0],		NULL, &err[14]);
			interSeccl[i]= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*numElemPerCalc[i][0]*4,			NULL, &err[15]);
			LBaseGcl[i]	= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*numElemPerCalc[i][0]*3*3*4,	NULL, &err[16]);
			numDOFEIcl[i]= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*numElemPerCalc[i][0],				NULL, &err[17]);
			dofcl[i] 	= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(int)*numElemPerCalc[i][0]*24,			NULL, &err[18]);
			T3Ecl[i] 	= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*3*3*4,	NULL, &err[19]);
			Ktcl[i] 		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*24*24,	NULL, &err[20]);
			fincl[i] 	= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*24,		NULL, &err[21]);
			XGcl[i] 		= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*numElemPerCalc[i][0]*4*3,		NULL, &err[22]);
			uGcl[i] 		= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*numElemPerCalc[i][0]*4*6,		NULL, &err[23]);
			sigGcl[i] 	= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*numElemPerCalc[i][0]*14,		NULL, &err[24]);
			strGcl[i] 	= clCreateBuffer(clContextArray[i], CL_MEM_READ_ONLY, sizeof(double)*numElemPerCalc[i][0]*20,		NULL, &err[25]);
			invFGcl[i]	= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*14*14,	NULL, &err[26]);
			invFTGcl[i]	= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*14*14,	NULL, &err[27]);
			GGcl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*14*24,	NULL, &err[28]);
			P11qcl[i]	= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*14*14,	NULL, &err[29]);
			P22Gcl[i]	= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*6*6,		NULL, &err[30]);
			P21Gcl[i]	= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*14*6,	NULL, &err[31]);
			fe1qcl[i]	= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*14,		NULL, &err[32]);
			fe2cl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*6,		NULL, &err[33]);
			fscl[i]		= clCreateBuffer(clContextArray[i], CL_MEM_READ_WRITE, sizeof(double)*numElemPerCalc[i][0]*14,		NULL, &err[34]);

			for(int j=0;j<35;j++) {
				if (errorCodeOut(err[j],"Error creating buffer") == -1){
					return 1;
				}
			}

		}




	// load Binaries from openCL Kernel files the order of devises should be the same as by the compilation thats why it should get right binary to devise
	/*** run time compilation only, baby! ***
	 	size_t* size = (size_t*)malloc(clDevicesSize * sizeof(size_t));
		char fname[1024];
		cl_int binStatus;
		const unsigned char** binaries = (const unsigned char**)malloc(clDevicesSize * sizeof(unsigned char*));
	//	const unsigned char* binariestemp;
		for(int i=0;i<(int)clDevicesSize;i++) {
			errcode =clGetDeviceInfo(clDevices[i], CL_DEVICE_VERSION, 512, &devVersion, 0);
			errcode|=clGetDeviceInfo(clDevices[i], CL_DEVICE_NAME, 512, &devName, 0);
			errcode|=clGetDeviceInfo(clDevices[i], CL_DEVICE_VENDOR, 512, &devVend, 0);
			errcode|=clGetDeviceInfo(clDevices[i], CL_DRIVER_VERSION, 512, &devVers, 0);
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

		clKernelFirstPart = (cl_kernel*)malloc(clDevicesSize * sizeof(cl_kernel));
		clKernelSecondPart = (cl_kernel*)malloc(clDevicesSize * sizeof(cl_kernel));
		clKernelThirdPart = (cl_kernel*)malloc(clDevicesSize * sizeof(cl_kernel));

		for(unsigned int i=0;i<clDevicesSize;i++) {
	//		binariestemp = binaries[i];
			cl_program clProgram = clCreateProgramWithBinary(clContextArray[i],1,&clDevices[i],&size[i],&binaries[i],&binStatus, &errcode);
			if (errorCodeOut(errcode,"Error creating program") == -1)
				return 1;

			if (errorCodeOut(binStatus,"Error creating program status") == -1)
				return 1;

			errcode = clBuildProgram(clProgram, 1, &clDevices[i], "", NULL, NULL);
			if (errorCodeOut(errcode,"Error building program") == -1) {
				if (errcode != CL_SUCCESS) {
					char str[4096];
					errcode  = clGetProgramBuildInfo(clProgram, clDevices[i], CL_PROGRAM_BUILD_LOG, 4096, &str, NULL);
					std::cout << " BUILD LOG Device " << i <<std::endl;
					std::cout << str << std::endl;
					errcode |= clGetProgramBuildInfo(clProgram, clDevices[i], CL_PROGRAM_BUILD_OPTIONS, 4096, &str, NULL);
					std::cout << " BUILD Options used:\n";
					std::cout << str << std::endl;
					if (errorCodeOut(errcode,"Error reading build info") == -1)
						return 1;
				}
				return 1;
			}
			clKernelFirstPart[i] = clCreateKernel(clProgram, "firstPart", &errcode);
			if (errorCodeOut(errcode,"Error creating Kernel firstPart") == -1)
				return 1;
			clKernelSecondPart[i] = clCreateKernel(clProgram, "secondPart", &errcode);
			if (errorCodeOut(errcode,"Error creating Kernel secondPart") == -1)
				return 1;
			clKernelThirdPart[i] = clCreateKernel(clProgram, "thirdPart", &errcode);
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
		clKernelFirstPart = (cl_kernel*)malloc(clDevicesSize * sizeof(cl_kernel));
		clKernelSecondPart = (cl_kernel*)malloc(clDevicesSize * sizeof(cl_kernel));
		clKernelThirdPart = (cl_kernel*)malloc(clDevicesSize * sizeof(cl_kernel));
		for(unsigned int i=0;i<clDevicesSize;i++) {
			cl_program clProgram;
	#pragma insieme kernelFile "ClKernelQMHS4.cl"
			clProgram = clCreateProgramWithSource(clContextArray[i], 1, &code, &len, &errcode);
			if (errorCodeOut(errcode,"Error creating OpenCL Program") == -1)
				return 1;
			char charStr[4096] = "-O0";

			errcode = clBuildProgram(clProgram, 1u, &clDevices[i], charStr, NULL, NULL);
			if (errorCodeOut(errcode,"Error building program") == -1) {
		//		if (errcode == CL_BUILD_PROGRAM_FAILURE) {
					clGetProgramBuildInfo(clProgram, clDevices[i], CL_PROGRAM_BUILD_LOG, 4096, &charStr, 0);
					printf(" BUILD LOG Device %d\n", i);
					printf("%s\n", charStr);
					clGetProgramBuildInfo(clProgram, clDevices[i], CL_PROGRAM_BUILD_OPTIONS, 4096, &charStr, 0);
					printf(" BUILD Options used:\n%s\n", charStr);
		//		}
				return 1;
			}

			clKernelFirstPart[i] = clCreateKernel(clProgram, "firstPart", &errcode);
			if (errorCodeOut(errcode,"Error creating Kernel firstPart") == -1)
				return 1;
			clKernelSecondPart[i] = clCreateKernel(clProgram, "secondPart", &errcode);
			if (errorCodeOut(errcode,"Error creating Kernel secondPart") == -1)
				return 1;
			clKernelThirdPart[i] = clCreateKernel(clProgram, "thirdPart", &errcode);
			if (errorCodeOut(errcode,"Error creating Kernel thirdPart") == -1)
				return 1;
		}

		cl_event* event= (cl_event*)malloc(9 * sizeof(cl_event));
		for(unsigned int i=0;i<clDevicesSize;i++) {
			err[0] = clEnqueueWriteBuffer(comandQueues[i], xi1cl[i], 			CL_FALSE, 0, sizeof(double)*4,xi1,
					0,	NULL, &event[0]);
			err[1] = clEnqueueWriteBuffer(comandQueues[i], xi2cl[i], 			CL_FALSE, 0, sizeof(double)*4,xi2,
					0,	NULL, &event[1]);
			err[2] = clEnqueueWriteBuffer(comandQueues[i], dNdxicl[i], 		CL_FALSE, 0, sizeof(double)*2*4*4,dNdxi,
					0,	NULL, &event[2]);
			err[3] = clEnqueueWriteBuffer(comandQueues[i], layPropcl[i],		CL_FALSE, 0, sizeof(double)*sumNumLay*2,
					layProp, 0, NULL, &event[3]);
			err[4] = clEnqueueWriteBuffer(comandQueues[i], layPropintcl[i], 	CL_FALSE, 0, sizeof(int)*sumNumLay*2,
					layPropint, 0, NULL, &event[4]);
			err[5] = clEnqueueWriteBuffer(comandQueues[i], matPropcl[i], 		CL_FALSE, 0, sizeof(double)*sumNumLay*2,
					matProp, 0, NULL, &event[5]);
			err[6] = clEnqueueWriteBuffer(comandQueues[i], matIDcl[i], 		CL_FALSE, 0, sizeof(int)*sumNumLay,
					matID,   	0,	NULL, &event[6]);
			err[7] = clEnqueueWriteBuffer(comandQueues[i], indexPerCSScl[i], 	CL_FALSE, 0, sizeof(int)*seccount,
					indexPerCSS,  0, 	NULL, &event[7]);
			err[8] = clEnqueueWriteBuffer(comandQueues[i], numLayArraycl[i], 	CL_FALSE, 0, sizeof(int)*seccount,
					numLayArray,  0, 	NULL, &event[8]);
			for(int j=0;j<9;j++) {
				if (errorCodeOut(err[j],"Error writing data to buffers") == -1) {
					return 1;
				}
			}
			// over all events wait till transfer finished
	//		for(int j=0;j<9;j++) {
				errcode = clEnqueueWaitForEvents(comandQueues[i], 9, event);
				if (errorCodeOut(errcode,"Error while writing data to buffers") == -1)
					return 1;
	//		}
		}
		free(err);
		free(event);

		//************************************************************************
	//	free(clContextArray);
		//************************************************************************
	}

    double* Kt = (double*)malloc(24 * 24 * elems * sizeof(double));
    double* fin = (double*)malloc(24 * elems * sizeof(double));
    int* elementsIndex = (int*)malloc(elems * sizeof(int));
    int* numDOFperindex = (int*)malloc(elems * sizeof(int));

    printf("Calling calcQMHS4\n");
//    if(calcQMHS4(&cLElemQMHS4, Kt, fin, elementsIndex, numDOFperindex)) {
//        printf("calcQMHS4 FAILED!\n");
//        return -1;
//    }
	{
		struct icoSSData* icoShellSectionsIT;
		void* icoShellSectionsKey;
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
		for(icoShellSectionsIT = (struct icoSSData*)ght_first(icoShellSections, &iterator, (const void**)&icoShellSectionsKey);
				icoShellSectionsIT;
				icoShellSectionsIT = (struct icoSSData*)ght_next(icoShellSections, &iterator, (const void**)&icoShellSectionsKey)) {
	//	for( icoShellSectionsIT=icoShellSections.begin(); icoShellSectionsIT!=icoShellSections.end(); icoShellSectionsIT++) {
			int* elementsOfCSS=*(int**)ght_get(elsets, sizeof(char*), icoShellSectionsKey);
	//		int* elementsOfCSS=elsets[ icoShellSectionsKey ];
			elementNr=elementsOfCSS[0];

	/***		if( elements[elementNr].getElementType().compare( "QMHS4")==0) { we only use QMHS4 ***/
			if(1) {
				size_t currElsetSize = *(size_t*)ght_get(elsetSize, sizeof(char*), icoShellSectionsKey);
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
						sig[counterNumElem*14+j] = sigPtr[(elementNr_-1)*14+j] + Dsig[(elementNr_-1)*14+j];
	//					std::cout << sig[counterNumElem*14+j] << "\t";
					}
	//				std::cout << std::endl;
					for(int j=0; j<20; j++) {
	//!					str[counterNumElem*20+j]=(*strG)[elementNr_][j] + (*DstrG)[elementNr_][j];
						str[counterNumElem*20+j]=strPtr[(elementNr_-1)*20+j] + Dstr[(elementNr_-1)*20+j];
	//					std::cout << str[counterNumElem*20+j] << "\t";
					}
					numDOFperindex[counterNumElem]=20;
					for(int j=0;j<4;j++) {
						numDOFperindex[counterNumElem]+=interSec[counterNumElem*4+j];
					}
					counterNumElem++;
				}
			}
		}

	#ifndef WIN32
		gettimeofday(&start, 0);
	#endif
		size_t* countElemCalc= (size_t*)malloc(clDevicesSize*4 * sizeof(size_t));
		for(unsigned int i=0;i<clDevicesSize*4;i++){
			countElemCalc[i]=0;

		}
		int* startValPerDev= (int*)malloc(clDevicesSize * sizeof(int));
		startValPerDev[0]=0;
		for(unsigned int i=1;i<clDevicesSize;i++) {
			startValPerDev[i]=startValPerDev[i-1]+numCalcsPerDev[i-1];
			countElemCalc[i*4+2]=startValPerDev[i];
			countElemCalc[i*4+3]=countElemCalc[(i-1)*4+3]+elemCalcPerDev[i-1];
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
			for(unsigned int i=0;i<clDevicesSize;i++) {
				countElemCalc[i*4]=countElemCalc[i*4+2];
				countElemCalc[i*4+1]=countElemCalc[i*4+3];
			}
	//		sumLoadTime=0;
	//		memtransfered=0;
			for(unsigned int i=0;i<clDevicesSize;i++) {
				if(countElemCalc[i*4]<numCalcsPerDev[i]+startValPerDev[i]){
	//				std::cout << "here " << countElemCalc[i*4+1]<<" "<< i<< std::endl;
					int l=countElemCalc[i*4]-startValPerDev[i];
					err[1] = clEnqueueWriteBuffer(comandQueues[i], XGcl[i], 	CL_FALSE, 0, sizeof(double)*
							numElemPerCalc[i][l]*4*3,		&XG[countElemCalc[i*4+1]*4*3],			0,	NULL, &event[1]);
					err[2] = clEnqueueWriteBuffer(comandQueues[i], uGcl[i], 	CL_FALSE, 0, sizeof(double)*
							numElemPerCalc[i][l]*4*6,		&u[countElemCalc[i*4+1]*4*6],			0,	NULL, &event[2]);
					err[3] = clEnqueueWriteBuffer(comandQueues[i], CSScountArraycl[i],CL_FALSE, 0, sizeof(int)*
							numElemPerCalc[i][l],			&CSScountArray[countElemCalc[i*4+1]], 0,NULL, &event[3]);
					err[4] = clEnqueueWriteBuffer(comandQueues[i], sigGcl[i], 	CL_FALSE, 0, sizeof(double)*
							numElemPerCalc[i][l]*14,		&sig[countElemCalc[i*4+1]*14],		0,	NULL, &event[4]);
					err[5] = clEnqueueWriteBuffer(comandQueues[i], dofcl[i], 	CL_FALSE, 0, sizeof(int)*
							numElemPerCalc[i][l]*24,		&dof[countElemCalc[i*4+1]*24],  	0,	NULL, &event[5]);
					err[6] = clEnqueueWriteBuffer(comandQueues[i], strGcl[i], 	CL_FALSE, 0, sizeof(double)*
							numElemPerCalc[i][l]*20,		&str[countElemCalc[i*4+1]*20],		0,	NULL, &event[6]);
					err[7] = clEnqueueWriteBuffer(comandQueues[i], numDOFEIcl[i],CL_FALSE, 0, sizeof(int)*
							numElemPerCalc[i][l],			&numDOFperindex[countElemCalc[i*4+1]],0,NULL, &event[7]);
					err[8] = clEnqueueWriteBuffer(comandQueues[i], interSeccl[i],CL_FALSE, 0, sizeof(int)*
							numElemPerCalc[i][l]*4,		&interSec[countElemCalc[i*4+1]*4],  0,	NULL, &event[8]);
					err[9] = clEnqueueWriteBuffer(comandQueues[i], LBaseGcl[i],	CL_FALSE, 0, sizeof(double)*
							numElemPerCalc[i][l]*3*3*4,	&LBase[countElemCalc[i*4+1]*3*3*4],  	0,	NULL, &event[9]);

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
	//	//				std::cout<< "time needed XG " << end-start << " ns " << ((double)sizeof(double)*numElemPerCalc[i][l]*4*3/(double)((double)(end-start)/(double)1000000)) << " byte/s" << std::endl;
	//					}
	//					memtransfered+=sizeof(double)*numElemPerCalc[i][l]*(4*3+4*6+14+20+3*3*4)+sizeof(int)*numElemPerCalc[i][l]*(1+24+1+4);

					countElemCalc[i*4]++;
					countElemCalc[i*4+1]+=numElemPerCalc[i][l];
				}
			}
			for(unsigned int i=0;i<clDevicesSize;i++) {
				errcode=clFinish(comandQueues[i]);
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
			for(unsigned int i=0;i<clDevicesSize;i++) {
				countElemCalc[i*4]=countElemCalc[i*4+2];
			}

			events = (cl_event*)malloc(clDevicesSize * sizeof(cl_event));
			for(unsigned int i=0;i<clDevicesSize;i++) {
				if(countElemCalc[i*4]<numCalcsPerDev[i]+startValPerDev[i]){
					// function parameters
	//				std::cout << "First Part "<<countElemCalc[i*4] << std::endl;
					int l=countElemCalc[i*4]-startValPerDev[i];
	//				std::cout << "First Part "<<l << std::endl;
					cl_uint sizeArg = numElemPerCalc[i][l];
					err[0]  = clSetKernelArg(clKernelFirstPart[i], 0,  sizeof(cl_uint),(void *) &sizeArg);
					err[1]  = clSetKernelArg(clKernelFirstPart[i], 1,  sizeof(cl_mem), (void *) &XGcl[i]);
					err[2]  = clSetKernelArg(clKernelFirstPart[i], 2,  sizeof(cl_mem), (void *) &uGcl[i]);
					err[3]  = clSetKernelArg(clKernelFirstPart[i], 3,  sizeof(cl_mem), (void *) &interSeccl[i]);
					err[4]  = clSetKernelArg(clKernelFirstPart[i], 4,  sizeof(cl_mem), (void *) &LBaseGcl[i]);
					err[5]  = clSetKernelArg(clKernelFirstPart[i], 5,  sizeof(cl_mem), (void *) &T3Ecl[i]);
					err[6]  = clSetKernelArg(clKernelFirstPart[i], 6,  sizeof(cl_mem), (void *) &xi1cl[i]);
					err[7]  = clSetKernelArg(clKernelFirstPart[i], 7,  sizeof(cl_mem), (void *) &xi2cl[i]);
					err[8]  = clSetKernelArg(clKernelFirstPart[i], 8,  sizeof(cl_mem), (void *) &Tcl[i]);
					err[9]  = clSetKernelArg(clKernelFirstPart[i], 9,  sizeof(cl_mem), (void *) &xcl[i]);
					err[10] = clSetKernelArg(clKernelFirstPart[i], 10, sizeof(cl_mem), (void *) &Hcl[i]);
					err[11] = clSetKernelArg(clKernelFirstPart[i], 11, sizeof(cl_mem), (void *) &dcl[i]);
					err[12] = clSetKernelArg(clKernelFirstPart[i], 12, sizeof(cl_mem), (void *) &t1cl[i]);
					err[13] = clSetKernelArg(clKernelFirstPart[i], 13, sizeof(cl_mem), (void *) &t2cl[i]);
					err[14] = clSetKernelArg(clKernelFirstPart[i], 14, sizeof(cl_mem), (void *) &Nsigcl[i]);
					err[15] = clSetKernelArg(clKernelFirstPart[i], 15, sizeof(cl_mem), (void *) &Nstrcl[i]);
					err[16] = clSetKernelArg(clKernelFirstPart[i], 16, sizeof(cl_mem), (void *) &Dcl[i]);
					err[17] = clSetKernelArg(clKernelFirstPart[i], 17, sizeof(cl_mem), (void *) &detJ0cl[i]);
					countElemCalc[i*4]++;
					for(int j=0;j<18;j++) {
						if (errorCodeOut(err[j],"Error settig arguments for Kernel firstPart") == -1) {
							printf("at %d elem %d\n", i, j);
							return 1;
						}
					}
				//there should be as much comandQueues as contextes
					cl_event ev;
					errcode = clEnqueueNDRangeKernel(comandQueues[i], clKernelFirstPart[i], 1, NULL,
							&numElemPerCalc[i][l], &localWork[i][l], 0, NULL, &ev);
					if (errorCodeOut(errcode,"Error starting calculation Kernel firstPart") == -1)
						return 1;
					errcode = clFlush(comandQueues[i]);
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
	//	//				std::cout<< "time needed XG " << end-start << " ns " << ((double)sizeof(double)*numElemPerCalc[i][l]*4*3/(double)((double)(end-start)/(double)1000000)) << " byte/s" << std::endl;
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
			for(unsigned int i=0;i<clDevicesSize;i++) {
				countElemCalc[i*4]=countElemCalc[i*4+2];
			}
			events2 = (cl_event*)malloc(clDevicesSize * sizeof(cl_event));
		//	std::cout << "Second Part" << std::endl;
			for(unsigned int i=0;i<clDevicesSize;i++) {
				if(countElemCalc[i*4]<numCalcsPerDev[i]+startValPerDev[i]){
					int l=countElemCalc[i*4]-startValPerDev[i];

					cl_uint sizeArg = numElemPerCalc[i][l]; // size_t has invalid size for an argument
					err[0]  = clSetKernelArg(clKernelSecondPart[i],  0, sizeof(cl_uint),(void *) &sizeArg);
					err[1]  = clSetKernelArg(clKernelSecondPart[i],  1, sizeof(cl_mem), (void *) &XGcl[i]);
					err[2]  = clSetKernelArg(clKernelSecondPart[i],  2, sizeof(cl_mem), (void *) &uGcl[i]);
					err[3]  = clSetKernelArg(clKernelSecondPart[i],  3, sizeof(cl_mem), (void *) &xcl[i]);
					err[4]  = clSetKernelArg(clKernelSecondPart[i],  4, sizeof(cl_mem), (void *) &dcl[i]);
					err[5]  = clSetKernelArg(clKernelSecondPart[i],  5, sizeof(cl_mem), (void *) &T3Ecl[i]);
					err[6]  = clSetKernelArg(clKernelSecondPart[i],  6, sizeof(cl_mem), (void *) &xi1cl[i]);
					err[7]  = clSetKernelArg(clKernelSecondPart[i],  7, sizeof(cl_mem), (void *) &xi2cl[i]);
					err[8]  = clSetKernelArg(clKernelSecondPart[i],  8, sizeof(cl_mem), (void *) &dNdxicl[i]);
					err[9]  = clSetKernelArg(clKernelSecondPart[i],  9, sizeof(cl_mem), (void *) &Tcl[i]);
					err[10] = clSetKernelArg(clKernelSecondPart[i], 10, sizeof(cl_mem), (void *) &Hcl[i]);
					err[11] = clSetKernelArg(clKernelSecondPart[i], 11, sizeof(cl_mem), (void *) &t1cl[i]);
					err[12] = clSetKernelArg(clKernelSecondPart[i], 12, sizeof(cl_mem), (void *) &t2cl[i]);
					err[13] = clSetKernelArg(clKernelSecondPart[i], 13, sizeof(cl_mem), (void *) &Nsigcl[i]);
					err[14] = clSetKernelArg(clKernelSecondPart[i], 14, sizeof(cl_mem), (void *) &Nstrcl[i]);
					err[15] = clSetKernelArg(clKernelSecondPart[i], 15, sizeof(cl_mem), (void *) &Dcl[i]);
					err[16] = clSetKernelArg(clKernelSecondPart[i], 16, sizeof(cl_mem), (void *) &detJ0cl[i]);
					err[17] = clSetKernelArg(clKernelSecondPart[i], 17, sizeof(cl_mem), (void *) &sigGcl[i]);
					err[18] = clSetKernelArg(clKernelSecondPart[i], 18, sizeof(cl_mem), (void *) &strGcl[i]);
					err[19] = clSetKernelArg(clKernelSecondPart[i], 19, sizeof(cl_mem), (void *) &layPropintcl[i]);
					err[20] = clSetKernelArg(clKernelSecondPart[i], 20, sizeof(cl_mem), (void *) &layPropcl[i]);
					err[21] = clSetKernelArg(clKernelSecondPart[i], 21, sizeof(cl_mem), (void *) &matPropcl[i]);
					err[22] = clSetKernelArg(clKernelSecondPart[i], 22, sizeof(cl_mem), (void *) &numLayArraycl[i]);
					err[23] = clSetKernelArg(clKernelSecondPart[i], 23, sizeof(cl_mem), (void *) &GGcl[i]);
					err[24] = clSetKernelArg(clKernelSecondPart[i], 24, sizeof(cl_mem), (void *) &fscl[i]);
					err[25] = clSetKernelArg(clKernelSecondPart[i], 25, sizeof(cl_mem), (void *) &numDOFEIcl[i]);
					err[26] = clSetKernelArg(clKernelSecondPart[i], 26, sizeof(cl_mem), (void *) &dofcl[i]);
					err[27] = clSetKernelArg(clKernelSecondPart[i], 27, sizeof(cl_mem), (void *) &CSScountArraycl[i] );
					err[28] = clSetKernelArg(clKernelSecondPart[i], 28, sizeof(cl_mem), (void *) &indexPerCSScl[i]);
					err[29] = clSetKernelArg(clKernelSecondPart[i], 29, sizeof(cl_mem), (void *) &fecl[i]);
					err[30] = clSetKernelArg(clKernelSecondPart[i], 30, sizeof(cl_mem), (void *) &Fcl[i]);
					err[31] = clSetKernelArg(clKernelSecondPart[i], 31, sizeof(cl_mem), (void *) &Pcl[i]);
					err[32] = clSetKernelArg(clKernelSecondPart[i], 32, sizeof(cl_mem), (void *) &Kgcl[i]);
					countElemCalc[i*4]++;
					for(int j=0;j<33;j++) {
						if (errorCodeOut(err[j],"Error settig arguments for Kernel secondPart") == -1) {
							printf("at %d Elem %d\n", i, j);
							return 1;
						}
					}
					cl_event ev;
					errcode = clEnqueueNDRangeKernel(comandQueues[i], clKernelSecondPart[i], 1, NULL,
							&numElemPerCalc[i][l], &localWork[i][l], 1, &events[i], &ev);
					if (errorCodeOut(errcode,"Error starting calculation Kernel secondPart") == -1) {
						printf("commandQueue %d\n", i);
						return 1;
					}
					errcode = clFlush(comandQueues[i]);
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
	//		//				std::cout<< "time needed XG " << end-start << " ns " << ((double)sizeof(double)*numElemPerCalc[i][l]*4*3/(double)((double)(end-start)/(double)1000000)) << " byte/s" << std::endl;
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
			for(unsigned int i=0;i<clDevicesSize;i++) {
				countElemCalc[i*4]=countElemCalc[i*4+2];
			}
		//	std::cout << "third Part" << std::endl;
			events3 = (cl_event*)malloc(clDevicesSize * sizeof(cl_event));
			for(unsigned int i=0;i<clDevicesSize;i++) {
				if(countElemCalc[i*4]<numCalcsPerDev[i]+startValPerDev[i]){
					int l=countElemCalc[i*4]-startValPerDev[i];
					err[0]  = clSetKernelArg(clKernelThirdPart[i], 0,  sizeof(cl_mem), (void *) &fecl[i]);
					err[1]  = clSetKernelArg(clKernelThirdPart[i], 1,  sizeof(cl_mem), (void *) &fscl[i]);
					err[2]  = clSetKernelArg(clKernelThirdPart[i], 2,  sizeof(cl_mem), (void *) &fe2cl[i]);
					err[3]  = clSetKernelArg(clKernelThirdPart[i], 3,  sizeof(cl_mem), (void *) &fe1qcl[i]);
					err[4]  = clSetKernelArg(clKernelThirdPart[i], 4,  sizeof(cl_mem), (void *) &Pcl[i]);
					err[5]  = clSetKernelArg(clKernelThirdPart[i], 5,  sizeof(cl_mem), (void *) &Fcl[i]);
					err[6]  = clSetKernelArg(clKernelThirdPart[i], 6,  sizeof(cl_mem), (void *) &P22Gcl[i]);
					err[7]  = clSetKernelArg(clKernelThirdPart[i], 7,  sizeof(cl_mem), (void *) &P21Gcl[i]);
					err[8]  = clSetKernelArg(clKernelThirdPart[i], 8,  sizeof(cl_mem), (void *) &P11qcl[i]);
					err[9]  = clSetKernelArg(clKernelThirdPart[i], 9,  sizeof(cl_mem), (void *) &sigGcl[i]);
					err[10] = clSetKernelArg(clKernelThirdPart[i], 10, sizeof(cl_mem), (void *) &strGcl[i]);
					err[11] = clSetKernelArg(clKernelThirdPart[i], 11, sizeof(cl_mem), (void *) &invFGcl[i]);
					err[12] = clSetKernelArg(clKernelThirdPart[i], 12, sizeof(cl_mem), (void *) &invFTGcl[i]);
					err[13] = clSetKernelArg(clKernelThirdPart[i], 13, sizeof(cl_mem), (void *) &GGcl[i]);
					err[14] = clSetKernelArg(clKernelThirdPart[i], 14, sizeof(cl_mem), (void *) &Kgcl[i]);
					err[15] = clSetKernelArg(clKernelThirdPart[i], 15, sizeof(cl_mem), (void *) &numDOFEIcl[i]);
					err[16] = clSetKernelArg(clKernelThirdPart[i], 16, sizeof(cl_mem), (void *) &Ktcl[i]);
					err[17] = clSetKernelArg(clKernelThirdPart[i], 17, sizeof(cl_mem), (void *) &fincl[i]);
					countElemCalc[i*4]++;
					for(int j=0;j<18;j++) {
						if (errorCodeOut(err[j],"Error setting arguments for Kernel thirdPart") == -1)
							return 1;
					}
					cl_event ev;
					errcode = clEnqueueNDRangeKernel(comandQueues[i], clKernelThirdPart[i], 1, NULL,
							&numElemPerCalc[i][l], &localWork[i][l], 1, &events2[i], &ev);
					if (errorCodeOut(errcode,"Error starting calculation Kernel thirdPart") == -1)
						return 1;
					errcode = clFlush(comandQueues[i]);
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
		//				std::cout<< "time needed XG " << end-start << " ns " << ((double)sizeof(double)*numElemPerCalc[i][l]*4*3/(double)((double)(end-start)/(double)1000000)) << " byte/s" << std::endl;
	//		}
		//	for(eventsIT=events4.begin();eventsIT!=events4.end();eventsIT++) {
		//		errcode = (*eventsIT).wait();
		//		if (errorCodeOut(errcode,"Error calculating Kernel thirdPart") == -1)
		//			return 1;
		//	}
			for(unsigned int i=0;i<clDevicesSize;i++) {
				countElemCalc[i*4]=countElemCalc[i*4+2];
				countElemCalc[i*4+1]=countElemCalc[i*4+3];
			}


	//		sumLoadTime=0;
	//		memtransfered=0;
			for(unsigned int i=0;i<clDevicesSize;i++) {
				if(countElemCalc[i*4]<numCalcsPerDev[i]+startValPerDev[i]){
				// Reading out the calculated values
					int l=countElemCalc[i*4]-startValPerDev[i];
	//				if(i == 0 && k == 0)
	//std::cout<< "save " <<countElemCalc[i*4+1]<< " "<< numElemPerCalc[i][l]<< " " << countElemCalc[i*4+1]<< std::endl;
					err[0]  = clEnqueueReadBuffer(comandQueues[i], Ktcl[i], 	CL_FALSE, 0,
							numElemPerCalc[i][l]*24*24* sizeof(double), 	(void*)&Kt[countElemCalc[i*4+1]*24*24],
							1, &events3[i], &event[0]);
					err[1]  = clEnqueueReadBuffer(comandQueues[i], fincl[i], 	CL_FALSE, 0,
							numElemPerCalc[i][l]*24* sizeof(double), 		(void*)&fin[countElemCalc[i*4+1]*24],
							1, &events3[i], &event[1]);
					err[2]  = clEnqueueReadBuffer(comandQueues[i], invFGcl[i], CL_FALSE, 0,
							numElemPerCalc[i][l]*14*14*sizeof(double), 	(void*)&invF[countElemCalc[i*4+1]*14*14],
							1, &events3[i], &event[2]);
					err[3]  = clEnqueueReadBuffer(comandQueues[i], invFTGcl[i],CL_FALSE, 0,
							numElemPerCalc[i][l]*14*14* sizeof(double), 	(void*)&invFT[countElemCalc[i*4+1]*14*14],
							1, &events3[i], &event[3]);
					err[4]  = clEnqueueReadBuffer(comandQueues[i], GGcl[i], 	CL_FALSE, 0,
							numElemPerCalc[i][l]*14*24* sizeof(double), 	(void*)&G[countElemCalc[i*4+1]*14*24],
							1, &events3[i], &event[4]);
					err[5]  = clEnqueueReadBuffer(comandQueues[i], P11qcl[i], 	CL_FALSE, 0,
							numElemPerCalc[i][l]*14*14* sizeof(double), 	(void*)&P11q[countElemCalc[i*4+1]*14*14],
							1, &events3[i], &event[5]);
					err[6]  = clEnqueueReadBuffer(comandQueues[i], P22Gcl[i], 	CL_FALSE, 0,
							numElemPerCalc[i][l]*6*6* sizeof(double), 		(void*)&P22[countElemCalc[i*4+1]*6*6],
							1, &events3[i], &event[6]);
					err[7]  = clEnqueueReadBuffer(comandQueues[i], P21Gcl[i], 	CL_FALSE, 0,
							numElemPerCalc[i][l]*14*6* sizeof(double), 	(void*)&P21[countElemCalc[i*4+1]*14*6],
							1, &events3[i], &event[7]);
					err[8]  = clEnqueueReadBuffer(comandQueues[i], T3Ecl[i],	CL_FALSE, 0,
							numElemPerCalc[i][l]*3*3*4* sizeof(double), 	(void*)&T3[countElemCalc[i*4+1]*3*3*4],
							1, &events3[i], &event[8]);
					err[9]  = clEnqueueReadBuffer(comandQueues[i], fe1qcl[i], 	CL_FALSE, 0,
							numElemPerCalc[i][l]*14* sizeof(double), 		(void*)&fe1q[countElemCalc[i*4+1]*14],
							1, &events3[i], &event[9]);
					err[10] = clEnqueueReadBuffer(comandQueues[i], fe2cl[i], 	CL_FALSE, 0,
							numElemPerCalc[i][l]*6* sizeof(double), 		(void*)&fe2[(countElemCalc[i*4+1])*6],
							1, &events3[i], &event[10]);
					err[11] = clEnqueueReadBuffer(comandQueues[i], fscl[i], 	CL_FALSE, 0,
							numElemPerCalc[i][l]*14* sizeof(double), 		(void*)&fs[countElemCalc[i*4+1]*14],
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
	//	//				std::cout<< "time needed XG " << end-start << " ns " << ((double)sizeof(double)*numElemPerCalc[i][l]*4*3/(double)((double)(end-start)/(double)1000000)) << " byte/s" << std::endl;
	//					}
	//					memtransfered+=sizeof(double)*numElemPerCalc[i][l]*(24*24+24+14*14*3+14*24+6*6+14*6+3*3*4+14*2+6);
					countElemCalc[i*4]++;
					countElemCalc[i*4+1]+=numElemPerCalc[i][l];
				} else
					printf("NOT %ld %ld\n", countElemCalc[i*4], numCalcsPerDev[i]+startValPerDev[i]);
			}
			free(events3);
		//	std::cout <<"memTransf " <<memtransfered/(1024*1024) << " Mb"<< std::endl;
		//	std::cout <<"sumLoadTime " <<sumLoadTime/(1000000) << " ms"<< std::endl;
		//	std::cout<<"Time Download "<<sumLoadTime/1000000<< " msec Download speed " << ((double)memtransfered/(double)((double)(sumLoadTime)/(double)1000000000)/((double)1024*1024)) << " Mbyte/s" << std::endl;
	//		wholeDownTransTime+=sumLoadTime;
	//		wholeDownMemTrans+=memtransfered;
		//	std::cout<< "finish " << std::endl;
			for(unsigned int i=0;i<clDevicesSize;i++) {
		//		std::cout << "comandQueue " << i << std::endl;
				errcode=clFinish(comandQueues[i]);

				if (errorCodeOut(errcode,"Error while wait for all comandQues to finish") == -1) {
					return 1;
				}
			}

			calculated=1;
			for(unsigned int i=0;i<clDevicesSize;i++) {
				countElemCalc[i*4+2]=countElemCalc[i*4];
				countElemCalc[i*4+3]=countElemCalc[i*4+1];

				if((countElemCalc[i*4]<(numCalcsPerDev[i]+startValPerDev[i]))) {
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
		long millisec=(sec*1000lu+end.tv_usec/1000lu)-start.tv_usec/1000lu;
		printf("\nTime openCL device %ld msec \n", millisec);
		counterNumElem=0;
		size_t elemcounter=0;
		for(icoShellSectionsIT = (struct icoSSData*)ght_first(icoShellSections, &iterator, (const void**)&icoShellSectionsKey);
				icoShellSectionsIT;
				icoShellSectionsIT = (struct icoSSData*)ght_next(icoShellSections, &iterator, (const void**)&icoShellSectionsKey)) {
	//	for( icoShellSectionsIT=icoShellSections.begin(); icoShellSectionsIT!=icoShellSections.end(); icoShellSectionsIT++) {
			int* elementsOfCSS=(int*)ght_get(elsets, sizeof(char*), icoShellSectionsKey);
	//		int* elementsOfCSS=elsets[ icoShellSectionsKey ];
			elementNr=elementsOfCSS[0];
	/***		if( elements[elementNr].getElementType().compare( "QMHS4")==0) { we only use QMHS4 ***/
			if(1) {
				size_t currElsetSize = *(size_t*)ght_get(elsetSize, sizeof(char*), icoShellSectionsKey);
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

					for(int l=0;l<4;l++) {
	//					globalNr=elements[elementNr_].getGlobalNrOfNodeNr( l);
						globalNr=nrOfOrder[counterNumElem * 4 + l];
	//					if( nodes[globalNr].getLocalDOF()==5) { always true
							for(int k=0;k<3;k++) {
								for(int j=0;j<2;j++) {
	//								(*T3E)[globalNr].set(k,j, T3[counterNumElem*3*3*4+k*3*4+j*4+l]);
									T3Ptr[globalNr*3*2 + k + j*3] = T3[counterNumElem*3*3*4+k*3*4+j*4+l];
								}
							}
	//					}
					}
					counterNumElem++;
				}
				elemcounter+=*(size_t*)ght_get(elsetSize, sizeof(char*), icoShellSectionsKey);
			}
		}
	}

    printf("Checking result\n");
//    sprintf(path, "/home/klaus/dpsnfs/save/");
    sprintf(filenameAddition, "after-1-0.5-");
    if(checkResult(path, filenameAddition, sigPtr, strPtr, Dsig, Dstr, invF, invFT, G, P11q, P22, P21, T3Ptr, fe1q, fe2, fs, elems) == 0)
        printf("\tResult is correct\n");

    printf("Writing output files\n");
//    sprintf(path, "/home/klaus/dpsnfs/save/gc/");
    sprintf(filenameAddition, "gc/c-");
    saveAll(path, filenameAddition, sigPtr, strPtr, Dsig, Dstr, invF, invFT, G, P11q, P22, P21, T3Ptr, fe1q, fe2, fs, elems, vertices);

//    destructCLElemQMHS4(&cLElemQMHS4);
	{
		free(interSec);
		for(unsigned int i=0;i<clDevicesSize;i++){
			clReleaseMemObject(Tcl[i]);
			clReleaseMemObject(xcl[i]);
			clReleaseMemObject(Hcl[i]);
			clReleaseMemObject(dcl[i]);
			clReleaseMemObject(t1cl[i]);
			clReleaseMemObject(t2cl[i]);
			clReleaseMemObject(Nsigcl[i]);
			clReleaseMemObject(Nstrcl[i]);
			clReleaseMemObject(Dcl[i]);
			clReleaseMemObject(detJ0cl[i]);
			clReleaseMemObject(fecl[i]);
			clReleaseMemObject(Fcl[i]);
			clReleaseMemObject(Pcl[i]);
			clReleaseMemObject(Kgcl[i]);
			clReleaseMemObject(CSScountArraycl[i]);
			clReleaseMemObject(interSeccl[i]);
			clReleaseMemObject(LBaseGcl[i]);
			clReleaseMemObject(numDOFEIcl[i]);
			clReleaseMemObject(dofcl[i]);
			clReleaseMemObject(T3Ecl[i]);
			clReleaseMemObject(Ktcl[i]);
			clReleaseMemObject(fincl[i]);
			clReleaseMemObject(XGcl[i]);
			clReleaseMemObject(uGcl[i]);
			clReleaseMemObject(sigGcl[i]);
			clReleaseMemObject(strGcl[i]);
			clReleaseMemObject(invFGcl[i]);
			clReleaseMemObject(invFTGcl[i]);
			clReleaseMemObject(GGcl[i]);
			clReleaseMemObject(P11qcl[i]);
			clReleaseMemObject(P22Gcl[i]);
			clReleaseMemObject(P21Gcl[i]);
			clReleaseMemObject(fe1qcl[i]);
			clReleaseMemObject(fe2cl[i]);
			clReleaseMemObject(fscl[i]);

			free(numElemPerCalc[i]);
			free(localWork[i]);

			clReleaseContext(clContextArray[i]);
			clReleaseCommandQueue(comandQueues[i]);
			clReleaseKernel(clKernelFirstPart[i]);
			clReleaseKernel(clKernelSecondPart[i]);
			clReleaseKernel(clKernelThirdPart[i]);
			clReleaseMemObject(numLayArraycl[i]);
			clReleaseMemObject(indexPerCSScl[i]);
			clReleaseMemObject(layPropintcl[i]);
			clReleaseMemObject(matPropcl[i]);
			clReleaseMemObject(matIDcl[i]);
			clReleaseMemObject(layPropcl[i]);
			clReleaseMemObject(xi1cl[i]);
			clReleaseMemObject(xi2cl[i]);
			clReleaseMemObject(dNdxicl[i]);
		}

		// to do splitt elements in smaller parts biggest <=10000 elements
		free(Tcl);
		free(xcl);
		free(Hcl);
		free(dcl);
		free(t1cl);
		free(t2cl);
		free(Nsigcl);
		free(Nstrcl);
		free(Dcl);
		free(detJ0cl);
		free(fecl);
		free(Fcl);
		free(Pcl);
		free(Kgcl);
		free(CSScountArraycl);
		free(interSeccl);
		free(LBaseGcl);
		free(numDOFEIcl );
		free(dofcl);
		free(T3Ecl );
		free(Ktcl);
		free(fincl);
		free(XGcl);
		free(uGcl);
		free(sigGcl );
		free(strGcl );
		free(invFGcl);
		free(invFTGcl);
		free(GGcl);
		free(P11qcl);
		free(P22Gcl);
		free(P21Gcl);
		free(fe1qcl);
		free(fe2cl);
		free(fscl);

		free(numLayArraycl);
		free(indexPerCSScl);
		free(layPropintcl);
		free(matPropcl);
		free(matIDcl);
		free(layPropcl);
		free(xi1cl);
		free(xi2cl);
		free(dNdxicl);

		free(sig);
		free(str);

		free(clKernelFirstPart);
		free(clKernelSecondPart);
		free(clKernelThirdPart);
		free(clContextArray);
		free(comandQueues);

		free(numElemPerCalc);
		free(localWork);
		free(numElemPerCalcSize);
		free(elemCalcPerDev);
		free(numCalcsPerDev);

		free(indexPerCSS);
		free(numLayArray);
		free(layProp);
		free(layPropint);
		free(matProp);
		free(matID);
		free(T3);
		free(CSScountArray);
		free(dof);
	}

    free( sigPtr );
    free( strPtr );
    free( Dsig );
    free( Dstr );
    free( invF );
    free( invFT );
    free( G );
    free( P11q );
    free( P22 );
    free( P21 );
    free( T3Ptr );
    free( fe1q );
    free( fe2 );
    free( fs );
    free( u );
    free( LBase );
    free( XG );
    free( nrOfOrder );

    free(Kt);
    free(fin);
    free(elementsIndex);
    free(numDOFperindex);

    // take care of correctly removing double pointers
    size_t** tmpPtr = 0;
    void* tmpKey = 0;
    ght_iterator_t iterator;
    for(tmpPtr = ght_first(elsets, &iterator, (const void**)&tmpKey); tmpPtr;
            tmpPtr = ght_next(elsets, &iterator, (const void**)&tmpKey)) {
        free(*tmpPtr);
        free(tmpPtr);
    }
    ght_finalize(icoShellSections);
    ght_finalize(elsets);
    ght_finalize(issSize);
    ght_finalize(elsetSize);
    ght_finalize(mat);

    return 0;
}
