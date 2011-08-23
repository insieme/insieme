/*
 * openClKernel.cl
 *
 *  Created on: 04.02.2011
 *      Author: manfred
 */

// only for eclpipse for syntax highlighting
#ifndef __OPENCL_VERSION__
/* Define out keywords causing errors */
#define __kernel
#define __global
#define __local
#define __private
#endif
#ifdef cl_khr_fp64
#pragma OPENCL EXTENSION cl_khr_fp64 : enable
#endif
#ifdef cl_amd_fp64
#pragma OPENCL EXTENSION cl_amd_fp64 : enable
//#pragma OPENCL EXTENSION cl_amd_printf : enable
#endif

#ifdef INSIEME
#include "ocl_device.h"
#endif

double mySin(double val);
double myCos(double val);
void invMatr(__private double* A, int a,__global double* B);
void sampWeight(__global int* layProb, int numLay, int wLay,__private double* w3,__private double* xi3);
void matrixMult(__private double* A, int a, int ab,__private double* B, int b,__private double* C);
void matrixMult1(__global double* A, int a, int ab,__global double* B, int b,__private double* C);
void matrixMult2(__private double* A, int a, int ab,__global double* B, int b,__private double* C);
void matrixMult3(__private double* A, int a, int ab,__global double* B, int b,__global double* C);
void matrixMult4(__private double* A, int a, int ab,__private double* B, int b,__global double* C);
void secDirVec(__global double const * rotVec, int rotStart,__global double* dirVec, int dir_k,__private double* h,__private double* M);
void zeros(double* toZ, uint numel);
void transMat(int ind,__global double const * u,__private double* R,__global double* H,int i);
double norm(__private double* A, int sizeA);
//double det(double* A, int x,double* Atemp);
void crossP(double* A,double* B, __private double *C);
void crossPG(__global double* A,__global double* B,__private double *C);
void elastC( double E, double nu, double h,__private double* C);
void transformationMatrices(__global double const * X,__global double const * u,__global double* T3,__global double* x,__global double const * lBase,__global double* H,__global double* d,__global int const * interSec,__global double* T);
void localCoOrdinateSystem(__global double const * X,__global double* t1,__global double* t2);
double interpolationConstantsOfStressAndStrains(__global double const * X,__global double* Nsig,__global double* Nstr,__global double* t1,__global double* t2,__global double const * xi1,__global double const * xi2);
void staticCondensation(__global double* F,__global double* invF,__global double* invFT,__global double* Pm,__global double* G,__global double* Kg,__global double const * sig,__global double* fem,__global double* fs,__global double* Kt,int const numDOFEI,__global double* fin);
void modifyVectorsAndOutputMatrices(__global double* fe,__global double* fs,__global double* fe2,__global double* fem,__global double* P,__global double* F,__global double* P22,__global double* P21,__global double* Pm,__global double const * sig,__global double const * str);
void assembleMatricesAndVectors(int i,int const numDOFEI,double* C,__global double* Nsig,double* NstrI,__global double* F,__global double* P,__global double* G,
		__global double* fs,__global double* fe,double detJ,double* strG,double* B,__global int const * dof,double* S);
void geometricStiffnessMatrix(int i,int const numDOFEI,__global double const * u,__global double* Nsig,__global double const * sig,double* dNdt,double* dxdt,double* fac,double* invJ,
		double* dxdxiAP,double* facQ1,double* facQ2,__global double* Kg,__global int const * dof,__global double* H,__global double* T3,__global double* T,__global double* d,double detJ);
void thicknessIntegration(int i,double* C,double* S,__global double const * str,int* indMat,double* NstrI,__global int const * layPropint,__global double const * layProp,__global double const * matProp,int const numLay);
void strainDisplacementMatrix(int i,double* dNdt,double* dxdt,double* dddt,__global double* T,double* B,double* invJ,double* fac,double* dAP,double* dxdxiAP);
double jacobiGlobalDerivatives(int i,__global double const * X,__global double* x,__global double* d,__global double* D,__global double* t1,__global double* t2,double* invJ,double* dNdt,double* dddt,double* dxdt,double* strG,double* fac,double* gAP,__global double const * xi1,__global double const * xi2,__global double const * dNdxi);
void assumedNaturalStrains(__global double* D,__global double* d,__global double const * X,__global double* x,double* dAP,double* dxdxiAP,double* gAP);
void zerosG(__global double* toZ,uint numel);
__kernel void firstPart(unsigned int const elementsused,__global double const * X,__global double const * u,__global int const * interSec,__global double const * LBase,__global double* T3,__global double const* xi1,__global double const * xi2,__global double* T,__global double* x
						,__global double* H,__global double* d,__global double* t1,__global double* t2,__global double* Nsig,__global double* Nstr,__global double* D,__global double* detJ0);
__kernel void secondPart(unsigned int const elementsused,__global double const * X,__global double const * u,__global double* x,__global double* d,__global double* T3,__global double const * xi1,__global double const * xi2,__global double const * dNdxi,__global double* T
		,__global double* H,__global double* t1,__global double* t2,__global double* Nsig,__global double* Nstr,__global double* D,__global double* detJ0,__global double const * sig,__global double const * str,__global int const * layPropint,__global double const* layProp
		,__global double const * matProp,__global int const * numLayArray,__global double* G,__global double* fs,__global int const * numDOFEI,__global int const * dof,__global int const * CSScountArray,__global int const * indexPerCSScl,__global double* fe,__global double* F
		,__global double* P,__global double* Kg);
__kernel void thirdPart(__global double* fe,__global double* fs,__global double* fe2,__global double* fem,__global double* P,__global double* F,__global double* P22,__global double* P21,__global double* Pm,__global double const * sig,__global double const * str,
						__global double* invF,__global double* invFT,__global double* G,__global double* Kg,__global int const * numDOFEI,__global double* Kt,__global double* fin);
//void printMat3DG(__global double* Mat,int a,int b,int c);
//void printMat3D(double* Mat,int a,int b,int c);
//void printMat(double* Mat,int a,int b);
void areaIntegration(__global double const * u,__global double const * X,__global double* x,__global double* d,__global double* D,__global double* t1,__global double* t2,double* gAP,double* dAP,double* dxdxiAP,
		__global double* T,__global double* H,__global double* T3,__global double* Nsig,__global double* Nstr,double detJ0,__global double const * sig,__global double const * str,__global int const * layPropint,__global double const * layProp,__global double const * matProp,int const numLay,
		__global double* F,__global double* P,__global double* G,__global double* fs,__global double* fe,__global double* Kg,__global double const * xi1,__global double const * xi2,__global double const * dNdxi,int const numDOFEI,__global int const * dof);


#pragma insieme mark
__kernel //__attribute__((vec_type_hint(double)))
//__attribute__((work_group_size_hint(64, 1, 1)))
void firstPart(unsigned int const elementsused,__global double const * X,__global double const * u,__global int const * interSec,__global double const * LBase,__global double* T3,__global double const* xi1,__global double const * xi2,__global double* T,__global double* x
						,__global double* H,__global double* d,__global double* t1,__global double* t2,__global double* Nsig,__global double* Nstr,__global double* D,__global double* detJ0) {


	uint localElempos = get_global_id(0);
	zerosG(&d[localElempos*4*3], 3 * 4);
//	zerosG(&T[localElempos*3*3*4], 3 * 3 * 4);
	zerosG(&t1[localElempos *3],3);
	zerosG(&t2[localElempos *3],3);
	zerosG(&D[localElempos*4*3],4*3);
	zerosG(&x[localElempos * 4 * 3],4*3);

	transformationMatrices(&X[localElempos * 4 * 3], &u[localElempos*24],&T3[localElempos*3*3*4], &x[localElempos * 4 * 3], &LBase[localElempos*4*3*3],&H[localElempos*3*3*4], &d[localElempos*4*3], &interSec[localElempos*4], &T[localElempos*3*3*4]);
//	printMat3DG(&T3[localElempos*3*3*4],3,3,4);
	localCoOrdinateSystem(&X[localElempos * 4 * 3],&t1[localElempos *3], &t2[localElempos *3]);

	detJ0[localElempos] = interpolationConstantsOfStressAndStrains(&X[localElempos * 4 * 3], &Nsig[localElempos*8 * 14 * 4], &Nstr[localElempos*10 * 20 * 4], &t1[localElempos *3],&t2[localElempos *3], xi1, xi2);

//			/*
//			 % --- DIRECTOR VECTOR (undeformed) ---
//			 for i=1:4,D(i,:) = lBase(:,3,i)';end
//			 % ---
//			 */
//
	for (int j = 0; j < 4; j++) {
		for (int k = 0; k < 3; k++) {
			D[localElempos*4*3 + j*3 + k] = LBase[localElempos*4*3*3 + 3*3*j + k*3 + 2];
		}
	}

}

#pragma insieme mark
__kernel //__attribute__((vec_type_hint(double)))
//__attribute__((work_group_size_hint(64, 1, 1)))
void secondPart(unsigned int const elementsused,__global double const * X,__global double const * u,__global double* x,__global double* d,__global double* T3,__global double const * xi1,__global double const * xi2,__global double const * dNdxi,__global double* T
		,__global double* H,__global double* t1,__global double* t2,__global double* Nsig,__global double* Nstr,__global double* D,__global double* detJ0,__global double const * sig,__global double const * str,__global int const * layPropint,__global double const* layProp
		,__global double const * matProp,__global int const * numLayArray,__global double* G,__global double* fs,__global int const * numDOFEI,__global int const * dof,__global int const * CSScountArray,__global int const * indexPerCSScl,__global double* fe,__global double* F
		,__global double* P,__global double* Kg) {

	double dxdxiAP[4 * 3];
	double dAP[4 * 3];
	double gAP[4 * 1];
	uint css;
	uint indexMatLay;

	uint localElempos = get_global_id(0);

	css=CSScountArray[localElempos];
	indexMatLay=indexPerCSScl[css];
	zerosG(&P[localElempos*20*20],20*20);
	zerosG(&F[localElempos*20*14],20*14);
	zerosG(&fe[localElempos*20],20*1);
	zerosG(&Kg[localElempos*24*24],24*24);
	zerosG(&fs[localElempos*14], 14 * 1);
	zeros(gAP,4*1);
	zeros(dAP,4*3);
	zeros(dxdxiAP,4*3);
	zerosG(&G[localElempos*14*24],14*24);
	assumedNaturalStrains(&D[localElempos*4*3],&d[localElempos*4*3],&X[localElempos * 4 * 3],&x[localElempos * 4 * 3],dAP,dxdxiAP,gAP);

	areaIntegration(&u[localElempos*24],&X[localElempos * 4 * 3],&x[localElempos * 4 * 3],&d[localElempos*4*3],&D[localElempos*4*3],&t1[localElempos *3],&t2[localElempos *3],
					gAP,dAP,dxdxiAP,&T[localElempos*3*3*4],&H[localElempos*3*3*4],&T3[localElempos*3*3*4],&Nsig[localElempos*8 * 14 * 4],&Nstr[localElempos*10 * 20 * 4],
					detJ0[localElempos],&sig[localElempos*14],&str[localElempos*20],&layPropint[indexMatLay],&layProp[indexMatLay],&matProp[indexMatLay],numLayArray[css],
					&F[localElempos*20*14],&P[localElempos*20*20],&G[localElempos*14*24],&fs[localElempos*14],&fe[localElempos*20],&Kg[localElempos*24*24],xi1,xi2,dNdxi,numDOFEI[localElempos],&dof[localElempos*24]);
//	if(get_global_id(0)==0) {
//			printMat3DG(G,14,24,1);
//		}
}

#pragma insieme mark
__kernel //__attribute__((vec_type_hint(double)))
//__attribute__((work_group_size_hint(64, 1, 1)))
void thirdPart(__global double* fe,__global double* fs,__global double* fe2,__global double* fem,__global double* P,__global double* F,__global double* P22,__global double* P21,__global double* Pm,__global double const * sig,__global double const * str,
						__global double* invF,__global double* invFT,__global double* G,__global double* Kg,__global int const * numDOFEI,__global double* Kt,__global double* fin) {

	uint localElempos = get_global_id(0);
	zerosG(&fem[localElempos*14], 14 * 1);
	zerosG(&P22[localElempos*6*6],6*6);
	zerosG(&P21[localElempos*14*6],14*6);
	zerosG(&Pm[localElempos*14*14],14*14);
	zerosG(&fe2[localElempos*6], 6 * 1);
	zerosG(&Kt[localElempos*24*24],24*24);
	zerosG(&fin[localElempos*24],24);
	modifyVectorsAndOutputMatrices(&fe[localElempos*20],&fs[localElempos*14],&fe2[localElempos*6],&fem[localElempos*14],&P[localElempos*20*20],&F[localElempos*20*14],&P22[localElempos*6*6],&P21[localElempos*14*6],&Pm[localElempos*14*14],&sig[localElempos*14],&str[localElempos*20]);

	staticCondensation(&F[localElempos*20*14],&invF[localElempos*14*14],&invFT[localElempos*14*14],&Pm[localElempos*14*14],&G[localElempos*14*24],&Kg[localElempos*24*24],&sig[localElempos*14],&fem[localElempos*14],&fs[localElempos*14],&Kt[localElempos*24*24],numDOFEI[localElempos],&fin[localElempos*24]);

}




void areaIntegration(__global double const * u,__global double const * X,__global double* x,__global double* d,__global double* D,__global double* t1,__global double* t2,double* gAP,double* dAP,double* dxdxiAP,
		__global double* T,__global double* H,__global double* T3,__global double* Nsig,__global double* Nstr,double detJ0,__global double const * sig,__global double const * str,__global int const * layPropint,__global double const * layProp,__global double const * matProp,int const numLay,
		__global double* F,__global double* P,__global double* G,__global double* fs,__global double* fe,__global double* Kg,__global double const * xi1,__global double const * xi2,__global double const * dNdxi,int const numDOFEI,__global int const * dof) {
	double NstrI[10 * 20]={0};
	double invJ[4]={0};
	double dNdt[2 * 4]={0};
	double dddt[2 * 3]={0};
	double dxdt[2 * 3]={0};
	double B[8 * 24]={0};
	double fac[4]={0};
	double C[10 * 10];
	double S[10 * 1];
	double facQ1[4 * 4]={0};
	double facQ2[4 * 4]={0};
	double strG[8*1]={0};
	int indMat[2] = {0,1};
	double detJ=0;
	for (int i = 0; i < 4; i++) {

		detJ=jacobiGlobalDerivatives(i,X,x,d,D,t1,t2,invJ,dNdt,dddt,dxdt,strG,fac,gAP,xi1,xi2,dNdxi);

		strainDisplacementMatrix(i,dNdt,dxdt,dddt,T,B,invJ,fac,dAP,dxdxiAP);

		/*
		 *
		 % --- STRAIN INTERPOLATION ---
		 NstrI = Nstr(:,:,i);
		 NstrI(1:6,15:18) = detJ0/detJ*NstrI(1:6,15:18);
		 % ---
		 *
		 */
		for (int k = 0; k < 10; k++) {
			for (int l = 0; l < 20; l++) {
				NstrI[k * 20 + l] = Nstr[k * 20 * 4 + l * 4 + i];
			}
		}
		for (int k = 0; k < 6; k++) {
			for (int l = 14; l < 18; l++)
				NstrI[k * 20 + l] = detJ0 / detJ * NstrI[k * 20 + l];
		}
//		printf("%f\n",detJ);
//		if(get_global_id(0)==0){
//			printMat3D(NstrI,10,20,1);
//		}
		thicknessIntegration(i,C,S,str,indMat,NstrI,layPropint,layProp,matProp,numLay);

		geometricStiffnessMatrix(i,numDOFEI,u,Nsig,sig,dNdt,dxdt,fac,invJ,dxdxiAP,facQ1,facQ2,Kg,dof,H,T3,T,d,detJ);

		assembleMatricesAndVectors(i,numDOFEI,C,Nsig,NstrI,F,P,G,fs,fe,detJ,strG,B,dof,S);

	}
}


void staticCondensation(__global double* F,__global double* invF,__global double* invFT,__global double* Pm,__global double* G,__global double* Kg,__global double const * sig,__global double* fem,__global double* fs,__global double* Kt,int const numDOFEI,__global double* fin) {
	/*
		 *
		 % --- STATIC CONDENSATION ---
		 invF  = F(1:14,1:14)^(-1);
		 invFT = F(1:14,1:14)'^(-1);
		 Ph  = invF*Pm*invFT;
		 Kt  = G'*Ph*G + Kg;
		 fin = G'*(sig + Ph*fs + invF*fem);
		 % ---
		 */
	double Ph[14*14]={0};
	double tempG[14*24]={0};

	{
		double tempF[14 * 14];

		for (int i = 0; i < 14; i++) {
			for (int k = 0; k < 14; k++) {
				tempF[i * 14 + k] = F[i * 14 + k];
			}
		}

		invMatr(tempF, 14, invF);

		for (int i = 0; i < 14; i++) {
			for (int k = 0; k < 14; k++) {
				tempF[i * 14 + k] = F[k * 14 + i];
			}
		}

		invMatr(tempF, 14, invFT);

		matrixMult1(invF, 14, 14, Pm, 14, tempF);

		matrixMult2(tempF, 14, 14, invFT,14, Ph);

		for (int i = 0; i < 14; i++) {
			for (int k = 0; k < numDOFEI; k++) {
				tempG[k * 14 + i] = G[i * numDOFEI + k];
			}
		}
	}
	{
		double tempGM[24*14];

		matrixMult(tempG, numDOFEI, 14, Ph, 14, tempGM);

		matrixMult3(tempGM, numDOFEI, 14, G, numDOFEI, Kt);
	}
	for (int i = 0; i < numDOFEI; i++) {
		for (int k = 0; k < numDOFEI; k++) {
			Kt[i * numDOFEI + k] = Kt[i * numDOFEI + k] + Kg[i * numDOFEI + k];
		}
	}
	{
		double tempfin[14]={0};
		for (int i = 0; i < 14; i++) {
			double value = 0;
			double value1 = 0;
			for (int k = 0; k < 14; k++) {
				value += Ph[i * 14 + k] * fs[k];
				value1 += invF[i * 14 + k] * fem[k];
			}
			tempfin[i] = sig[i] + value + value1;
		}
		matrixMult4(tempG, numDOFEI, 14, tempfin, 1, fin);
	}
}

void modifyVectorsAndOutputMatrices(__global double* fe,__global double* fs,__global double* fe2,__global double* fem,__global double* P,__global double* F,__global double* P22,__global double* P21,__global double* Pm,__global double const * sig,__global double const * str) {
	/*
	 * % *************************************************************************
	 % modify vectors:
	 fe = fe - F*sig;    fs = fs - F'*str;

	 % output:
	 invP22 = P(15:20,15:20)^(-1);
	 Pm  = P(1:14,1:14) - P(1:14,15:20)*invP22*P(15:20,1:14);
	 P22 = P(15:20,15:20)^(-1);
	 P21 = P(15:20,1:14);
	 fem = fe(1:14) - P(1:14,15:20)*invP22*fe(15:20);
	 fe2 = fe(15:20);
	 */
	for (int i = 0; i < 20; i++) {
		double value = 0;
		for (int k = 0; k < 14; k++) {
			value += F[i * 14 + k] * sig[k];
		}
		fe[i] = fe[i] - value;
		if (i > 13) {
			fe2[i - 14] = fe[i];
		}
	}
	for (int i = 0; i < 14; i++) {
		double value = 0;
		for (int k = 0; k < 20; k++) {
			value += F[k * 14 + i] * str[k];
		}
		fs[i] = fs[i] - value;
	}
	{
		double Ptemp[6 * 6];
		for (int i = 0; i < 6; i++) {
			for (int k = 0; k < 6; k++) {
				Ptemp[i * 6 + k] = P[(i + 14) * 20 + (k + 14)];
			}
		}

		invMatr(Ptemp, 6, P22); // inversion of Matrix
	}
	{
		double Ptemp2[14 * 6];
		for (int i = 0; i < 14; i++) {
			for (int k = 0; k < 6; k++) {
				double value = 0;
				for (int l = 0; l < 6; l++) {
					value += P[i * 20 + (l + 14)] * P22[l * 6 + k];
				}
				Ptemp2[i * 6 + k] = value;
			}
		}

		for (int i = 0; i < 14; i++) {
			for (int k = 0; k < 14; k++) {
				double value = 0;
				for (int l = 0; l < 6; l++) {
					value += Ptemp2[k * 6 + l] * P[(l + 14) * 20 + i];
				}
				Pm[i * 14 + k] = P[i * 20 + k] - value;
			}
		}

		for (int i = 0; i < 14; i++) {
			double value = 0;
			for (int k = 0; k < 6; k++) {
				value += Ptemp2[i * 6 + k] * fe[k + 14];
			}
			fem[i] = fe[i] - value;
		}
	}
	for (int i = 0; i < 6; i++) {
		for (int k = 0; k < 14; k++) {
			P21[i * 14 + k] = P[(i + 14) * 20 + k];
		}
	}
//	if(get_local_id(0)==0){
//		printMat3D(Ptemp2,14,6,1);
//		printf("\n");
//	}
}

void assembleMatricesAndVectors(int i,int const numDOFEI,double* C,__global double* Nsig,double* NstrI,__global double* F,__global double* P,__global double* G,
		__global double* fs,__global double* fe,double detJ,double* strG,double* B,__global int const * dof,double* S) {

	/*
	 * % --- ASSEMBLE MATRICES AND VECTORS ---
	 P = P + NstrI'*C*NstrI*detJ;
	 F = F + NstrI'*[Nsig(:,:,i);zeros(2,14)]*detJ;
	 G = G + Nsig(:,:,i)'*B(:,dof)*detJ;

	 fe = fe + NstrI'*S*detJ;
	 fs = fs + Nsig(:,:,i)'*strG*detJ;
	 */
	{
		double tempP[20 * 10];

		for (int k = 0; k < 20; k++) {
			for (int l = 0; l < 14; l++) {
				double value = 0;
				double value1 = 0;
				for (int s = 0; s < 10; s++) {
					if (l < 10) {
						value += NstrI[s * 20 + k] * C[s * 10 + l];
					}
					if (s < 8) {
						value1 += NstrI[s * 20 + k] * Nsig[s * 14 * 4 + l * 4+ i];
					}
				}
				if (l < 10) {
					tempP[k * 10 + l] = value;
				}
				F[k * 14 + l] = F[k * 14 + l] + value1 * detJ;
			}
		}
		for (int k = 0; k < 20; k++) {
			for (int l = 0; l < 20; l++) {
				double value = 0;
				for (int s = 0; s < 10; s++) {
					value += tempP[k * 10 + s] * NstrI[s * 20 + l];
				}
				P[k * 20 + l] += detJ * value;
			}
		}
	}
//	printMat3DG(Nsig,8,14,4);
	for (int j = 0; j < 14; j++) {
		for (int l = 0; l < numDOFEI; l++) {
			double value = 0;
			for (int s = 0; s < 8; s++) {
				value += Nsig[s * 14 * 4 + j * 4 + i]* B[s * 24 + dof[l]];
			}
			G[j * numDOFEI + l] += value * detJ;
		}
	}
	for (int k = 0; k < 20; k++) {
		double value = 0;
		double value1 = 0;
		for (int l = 0; l < 10; l++) {
			value += NstrI[l * 20 + k] * S[l];
			if ((l < 8) && (k < 14)) {
				value1 += Nsig[l * 14 * 4 + k * 4 + i] * strG[l];
			}
		}
		fe[k] = fe[k] + value * detJ;
		if (k < 14) {
			fs[k] += value1 * detJ;
		}
	}
}

void geometricStiffnessMatrix(int i,int const numDOFEI,__global double const * u,__global double* Nsig,__global double const * sig,double* dNdt,double* dxdt,double* fac,double* invJ,
		double* dxdxiAP,double* facQ1,double* facQ2,__global double* Kg,__global int const * dof,__global double* H,__global double* T3,__global double* T,__global double* d,double detJ) {
	__private double sigSP[8]={0};
	__private double facQR[4 * 3];
	double facN[4 * 4];
	double facM[4 * 4];

/*% --- GEOMETRIC STIFFNESS MATRIX ---
 kg(:,:) = 0;
 % stresses at sampling point:
 sigSP = Nsig(:,:,i)*sig;
 */

for (int j = 0; j < 8; j++) {
	sigSP[j] = 0;
	for (int k = 0; k < 14; k++) {
		sigSP[j] += Nsig[j * 14 * 4 + k * 4 + i] * sig[k];
	}
}

/*
 *% force factors:
 facN = sigSP(1)*dNdt(1,:)'*dNdt(1,:) + sigSP(2)*dNdt(2,:)'*dNdt(2,:) + sigSP(3)*(dNdt(1,:)'*dNdt(2,:)+dNdt(2,:)'*dNdt(1,:));
 facM = sigSP(4)*dNdt(1,:)'*dNdt(1,:) + sigSP(5)*dNdt(2,:)'*dNdt(2,:) + sigSP(6)*(dNdt(1,:)'*dNdt(2,:)+dNdt(2,:)'*dNdt(1,:));
 */


for (int j = 0; j < 4; j++) {
	for (int k = 0; k < 4; k++) {
		facN[j * 4 + k] = sigSP[0] * dNdt[0 * 4 + j] * dNdt[0 * 4 + k]+ sigSP[1] * dNdt[1 * 4 + j] * dNdt[1 * 4 + k]+ sigSP[2] * (dNdt[0 * 4 + j] * dNdt[1 * 4 + k]	+ dNdt[1 * 4 + j] * dNdt[0 * 4 + k]);
		facM[j * 4 + k] = sigSP[3] * dNdt[0 * 4 + j] * dNdt[0 * 4 + k]+ sigSP[4] * dNdt[1 * 4 + j] * dNdt[1 * 4 + k]+ sigSP[5] * (dNdt[0 * 4 + j] * dNdt[1 * 4 + k]	+ dNdt[1 * 4 + j] * dNdt[0 * 4 + k]);
	}
}
/*
 *     % transverse shear forces:
 q = invJ'*sigSP(7:8);
 %
 facQ1(1,:) = 0.5*q(1)*fac3;  facQ1(2,:) =-0.5*q(1)*fac3;
 facQ1(3,:) =-0.5*q(1)*fac4;  facQ1(4,:) = 0.5*q(1)*fac4;
 facQ2(1,:) = 0.5*q(2)*fac1;  facQ2(2,:) = 0.5*q(2)*fac2;
 facQ2(3,:) =-0.5*q(2)*fac2;  facQ2(4,:) =-0.5*q(2)*fac1;
 %
 facQR(1,:) = 2*facQ1(1,1)*dxdxiAP(1,:) + 2*facQ2(1,1)*dxdxiAP(4,:);
 facQR(2,:) = 2*facQ1(1,1)*dxdxiAP(1,:) + 2*facQ2(2,1)*dxdxiAP(2,:);
 facQR(3,:) = 2*facQ1(4,1)*dxdxiAP(3,:) + 2*facQ2(2,1)*dxdxiAP(2,:);
 facQR(4,:) = 2*facQ1(4,1)*dxdxiAP(3,:) + 2*facQ2(1,1)*dxdxiAP(4,:);
 %
 */
{
	double q[2];
	q[0] = invJ[0] * sigSP[6] + invJ[2] * sigSP[7];
	q[1] = invJ[1] * sigSP[6] + invJ[3] * sigSP[7];
	for (int j = 0; j < 4; j++) {
		facQ1[0 * 4 + j] = 0.5 * q[0] * fac[2];
		facQ1[1 * 4 + j] = -0.5 * q[0] * fac[2];
		facQ1[2 * 4 + j] = -0.5 * q[0] * fac[3];
		facQ1[3 * 4 + j] = 0.5 * q[0] * fac[3];
		facQ2[0 * 4 + j] = 0.5 * q[1] * fac[0];
		facQ2[1 * 4 + j] = 0.5 * q[1] * fac[1];
		facQ2[2 * 4 + j] = -0.5 * q[1] * fac[1];
		facQ2[3 * 4 + j] = -0.5 * q[1] * fac[0];
	}
	for (int j = 0; j < 3; j++) {
		facQR[0 * 3 + j] = 2 * facQ1[0] * dxdxiAP[0 * 3 + j] + 2 * facQ2[0]	* dxdxiAP[3 * 3 + j];
		facQR[1 * 3 + j] = 2 * facQ1[0] * dxdxiAP[0 * 3 + j] + 2 * facQ2[1	* 4 + 0] * dxdxiAP[1 * 3 + j];
		facQR[2 * 3 + j] = 2 * facQ1[3 * 4 + 0] * dxdxiAP[2 * 3 + j] + 2* facQ2[2] * dxdxiAP[1 * 3 + j];
		facQR[3 * 3 + j] = 2 * facQ1[3 * 4 + 0] * dxdxiAP[2 * 3 + j] + 2* facQ2[0] * dxdxiAP[3 * 3 + j];
	}
}
/*

 % identity matrices for geometric stiffness matrix:
 IG1 = [1  1  0  0 ; 1  1  0  0 ; 0  0  1  1 ; 0  0  1  1];
 IG2 = [1  0  0  1 ; 0  1  1  0 ; 0  1  1  0 ; 1  0  0  1];
 % ---

 for j=1:4   % loop over rows
 indJ = j*6-5:j*6;
 for k=1:4   % loop over columns
 indK = k*6-5:k*6;
 % displacements - displacements:
 kg(indJ(1:3),indK(1:3)) = facN(j,k)*I3;
 % displacements - rotations:
 facQ = 0;
 if IG1(j,k)
 facQ = facQ + facQ1(j,k);
 end
 if IG2(j,k)
 facQ = facQ + facQ2(j,k);
 end
 kg(indJ(1:3),indK(4:6)) = (facM(j,k)+facQ)*T(:,:,k);
 kg(indK(4:6),indJ(1:3)) = kg(indJ(1:3),indK(4:6))';
 % rotations - rotations:
 if j==k
 ind = j*6-2:j*6;
 h = sigSP(4)*dNdt(1,k)*dxdt(1,:)' + sigSP(5)*dNdt(2,k)*dxdt(2,:)' + sigSP(6)*(dNdt(2,k)*dxdt(1,:)'+dNdt(1,k)*dxdt(2,:)') + facQR(k,:)';
 M = secDirVec(u(ind),d(:,k),h);
 kg(indK(4:6),indK(4:6)) = T3(:,:,k)'*H(:,:,k)'*M*H(:,:,k)*T3(:,:,k);
 end
 end
 end
 */
{
	double h[3];
	int IG1[4 * 4] = { 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1 };
	int IG2[4 * 4] = { 1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1 };
	double M[3 * 3];
	double kg[24 * 24]={0};
	double tempMat[3 * 3];
	for (int j = 0; j < 4; j++) {
		int indJ = j * 6;
		for (int k = 0; k < 4; k++) {
			double facQ = 0;
			int indK = k * 6;
			for(int r=indJ;r<indJ+3;r++) {
				for(int l=indK;l<indK+3;l++) {
					// to create I3 wich is eye(3)
					kg[r*24+l]=((r-indJ)==(l-indK)) ? facN[j*4+k]:0;
//					if((r-indJ)==(l-indK)) {
//						kg[r*24+l]=facN[j*4+k];
//					} else
//					{
//						kg[r*24+l]=0;
//					}
				}
			}
			if (IG1[j * 4 + k] == 1) {
				facQ = facQ + facQ1[j * 4 + k];
			}
			if (IG2[j * 4 + k] == 1) {
				facQ = facQ + facQ2[j * 4 + k];
			}
			for (int r = indJ; r < indJ + 3; r++) {
				for (int l = indK+3; l < indK + 6; l++) {
//					printf("%d %d %d\n",(r - indJ),(l - indK-3),(r - indJ) * 3 * 4 + (l - indK-3) * 4 + k);
					kg[r * 24 + l] = (facM[j * 4 + k] + facQ)* T[(r - indJ) * 3 * 4 + (l - indK-3) * 4 + k];
					tempMat[(r - indJ) * 3 + (l - indK-3)] = kg[r * 24 + l];

				}
			}
			for (int r = indK + 3; r < indK + 6; r++) {
				for (int l = indJ; l < indJ + 3; l++) {
					kg[r * 24 + l] = tempMat[(l - indJ) * 3 + (r - indK-3)];
				}
			}

			if (j == k) {
				int indg = j * 6 + 3;
				for (int r = 0; r < 3; r++) {
					h[r] = sigSP[3] * dNdt[0 * 4 + k] * dxdt[0 * 3 + r]+ sigSP[4] * dNdt[1 * 4 + k] * dxdt[1 * 3 + r]+ sigSP[5] * (dNdt[1 * 4 + k] * dxdt[0 * 3 + r]+ dNdt[0 * 4 + k] * dxdt[1 * 3 + r])+ facQR[k * 3 + r];

				}


				secDirVec(u, indg, d, k, h, M);

				for (int r = 0; r < 3; r++) {
					for (int e = 0; e < 3; e++) {
						double value = 0;
						for (int l = 0; l < 3; l++) {
							value += T3[l * 3 * 4 + r * 4 + k] * H[e * 3* 4 + l * 4 + k];

						}
						tempMat[r * 3 + e] = value;
					}
				}
				for (int r = 0; r < 3; r++) {
					for (int e = 0; e < 3; e++) {
						double value = 0;
						for (int l = 0; l < 3; l++) {
							value += tempMat[r * 3 + l] * M[l * 3 + e];

						}
						kg[(r + indK+3) * 24 + (e + indK+3)] = value;
					}
				}
				for (int r = 0; r < 3; r++) {
					for (int e = 0; e < 3; e++) {
						double value = 0;
						for (int l = 0; l < 3; l++) {
							value += kg[(r + indK+3) * 24 + (l + indK+3)] * H[l* 3 * 4 + e * 4 + k];
						}
						tempMat[r * 3 + e] = value;
					}
				}
				for (int r = 0; r < 3; r++) {
					for (int e = 0; e < 3; e++) {
						double value = 0;
						for (int l = 0; l < 3; l++) {
							value += tempMat[r * 3 + l] * T3[l * 3 * 4 + e* 4 + k];

						}
						kg[(r + indK+3) * 24 + (e + indK+3)] = value;
					}
				}
			}
		}
	}
//			if(get_global_id(0)==0) {
//				printf("\n");
//				printMat3DG(T3,3,3,4);
//				printf("\n");
//			}
//if(get_global_id(0)==0) {
//	printf("\n");
//	printMat(dNdt,2,4);
//	printf("\n");
//}


/*
		 * %
		 Kg = Kg + kg(dof,dof)*detJ;
		 % ---
		 */
		for (int k = 0; k < numDOFEI; k++) {
			for (int l = 0; l < numDOFEI; l++) {
				Kg[k * numDOFEI + l] += detJ*kg[dof[k] * 24+ dof[l]];
			}
		}
	}
}

void thicknessIntegration(int i,double* C,double* S,__global double const * str,int* indMat,double* NstrI,__global int const * layPropint,__global double const * layProp,__global double const * matProp,int const numLay) {


	/*
		 *  % --- THICKNESS INTEGRATION ---
		 S(:) = 0;
		 C(:) = 0;
		 for j=1:numLay
		 Clay(:) = 0;
		 Slay(:) = 0;
		 dh = abs(layProp(1,2)-layProp(1,3));
		 % elastic:
		 if elast
		 if matID(layProp(j,1),1)==1
		 Clay = elastC(matProp(indMat(layProp(j,1))+1),matProp(indMat(layProp(j,1)+1)),dh);
		 Slay = Clay*NstrI*str;
		 else
		 for k=1:layProp(j,4)
		 % strains at layer
		 elE = AE{j}(:,:,k)*NstrI*str;
		 [Sk,Ck] = matElastic(finE,elE,eye(3),matID(layProp(j,1),1),matProp(indMat(layProp(j,1))+1:indMat(layProp(j,1)+1)));
		 % integration
		 Slay = Slay + AE{j}(:,:,k)'*Sk*w3{j}(k)*dh;
		 Clay = Clay + AE{j}(:,:,k)'*Ck*AE{j}(:,:,k)*w3{j}(k)*dh;
		 end
		 end
		 % plastic:
		 else
		 %             for j=1:5
		 %                 % strains at layer
		 %                 elE = AE(:,:,j)*NstrI*str;
		 %                 [Sj,Cj,DepsPL(:,i,j),Dan(i,j)] = matPlastic(FE,elE,[t1,t2,t3],matNum,varargin{1}(:,:,i,j)',varargin{2}(:,i,j));
		 %                 % integration
		 %                 S = S + AE(:,:,j)'*Sj*w3(j)*0.5*FE.h;
		 %             	C = C + AE(:,:,j)'*Cj*AE(:,:,j)*w3(j)*0.5*FE.h;
		 %             end
		 end
		 S = S + Slay;
		 C = C + Clay;
		 end
		 */
	double Slay[10 * 1]={0};
	double Clay[10 * 10]={0};

	zeros(C, 10 * 10);
	zeros(S, 10 * 1);
	for (int j = 0; j < numLay; j++) {
		zeros(Clay, 10 * 10);
		zeros(Slay, 10 * 1);
		double dh = fabs(layProp[0] - layProp[1]);
		// future use of elast
		elastC(matProp[indMat[layPropint[j*2]]],matProp[indMat[layPropint[j*2]+1]], dh, Clay);
		for (int k = 0; k < 10; k++) {
			Slay[k] = 0;
			for (int l = 0; l < 10; l++) {
				double value = 0;
				for (int r = 0; r < 20; r++) {
					value += NstrI[l * 20 + r] * str[r];
				}
				Slay[k] += Clay[k * 10 + l] * value;
			}
		}
		for (int l = 0; l < 10; l++) {
			S[l] = S[l] + Slay[l];
			for (int k = 0; k < 10; k++) {
				C[l * 10 + k] += Clay[l * 10 + k];
			}
		}

	}

	/*
	 % shear correction:
	 C(1:6,7:8) = C(1:6,7:8)*5/6;
	 C(7:8,1:8) = C(7:8,1:8)*5/6;
	 S(7:8)     = S(7:8)*5/6;
	 % ---
	*/

	for (int j = 0; j < 8; j++) {
		if (j < 6) {
			C[j * 10 + 6] *= (double)5 / (double)6;
			C[j * 10 + 7] *= (double)5 / (double)6;
		}
		C[6 * 10 + j] *= (double)5 / (double)6;
		C[7 * 10 + j] *= (double)5 / (double)6;
	}
	S[6] *= (double)5 / (double)6;
	S[7] *= (double)5 / (double)6;

}


void strainDisplacementMatrix(int i,double* dNdt,double* dxdt,double* dddt,__global double* T,double* B,double* invJ,double* fac,double* dAP,double* dxdxiAP) {
	/*
	 % --- STRAIN DISPLACEMENT MATRIX ---
	 B(:) = 0;
	 for j=1:4
	 % membrane and bending strains:
	 % displacements:
	 indU = j*6-5:j*6-3;
	 B(1,indU) = dNdt(1,j)*dxdt(1,:);
	 B(2,indU) = dNdt(2,j)*dxdt(2,:);
	 B(3,indU) = dNdt(1,j)*dxdt(2,:) + dNdt(2,j)*dxdt(1,:);
	 B(4,indU) = dNdt(1,j)*dddt(1,:);
	 B(5,indU) = dNdt(2,j)*dddt(2,:);
	 B(6,indU) = dNdt(2,j)*dddt(1,:) + dNdt(1,j)*dddt(2,:);
	 % rotations:
	 indR = j*6-2:j*6;
	 B(4,indR) = dNdt(1,j)*dxdt(1,:)*T(:,:,j);
	 B(5,indR) = dNdt(2,j)*dxdt(2,:)*T(:,:,j);
	 B(6,indR) = (dNdt(1,j)*dxdt(2,:) + dNdt(2,j)*dxdt(1,:))*T(:,:,j);
	 end
	 */

	zeros(B, 8 * 24);
	for (int j = 0; j < 4; j++) {
		for (int k = 0; k < 3; k++) {
			int indU = j * 6 + k;
			B[0 * 24 + indU] = dNdt[0 * 4 + j] * dxdt[0 * 3 + k];
			B[1 * 24 + indU] = dNdt[1 * 4 + j] * dxdt[1 * 3 + k];
			B[2 * 24 + indU] = dNdt[0 * 4 + j] * dxdt[1 * 3 + k] + dNdt[1* 4 + j] * dxdt[0 * 3 + k];
			B[3 * 24 + indU] = dNdt[0 * 4 + j] * dddt[0 * 3 + k];
			B[4 * 24 + indU] = dNdt[1 * 4 + j] * dddt[1 * 3 + k];
			B[5 * 24 + indU] = dNdt[1 * 4 + j] * dddt[0 * 3 + k] + dNdt[0* 4 + j] * dddt[1 * 3 + k];
			int indR = j * 6 + 3 + k;
			double value = 0;
			double value1 = 0;
			double value2 = 0;
			for (int l = 0; l < 3; l++) {
				value += dxdt[0 * 3 + l] * T[l * 3 * 4 + k * 4 + j];
				value1 += dxdt[1 * 3 + l] * T[l * 3 * 4 + k * 4 + j];
				value2 += (dNdt[0 * 4 + j] * dxdt[1 * 3 + l] + dNdt[1 * 4+ j] * dxdt[0 * 3 + l]) * T[l * 3 * 4 + k * 4 + j];
			}
			B[3 * 24 + indR] = dNdt[0 * 4 + j] * value;
			B[4 * 24 + indR] = dNdt[1 * 4 + j] * value1;
			B[5 * 24 + indR] = value2;

		}

	}

	/*
	 *   % transverse shear strains:
	 fac1 = 0.25*(1+xi1(i)); fac2 = 0.25*(1-xi1(i));
	 fac3 = 0.25*(1+xi2(i)); fac4 = 0.25*(1-xi2(i));
	 dgdxi = [ fac3*dAP(1,:) fac3*dxdxiAP(1,:)*T(:,:,1) -fac3*dAP(1,:) fac3*dxdxiAP(1,:)*T(:,:,2) ...
	 -fac4*dAP(3,:) fac4*dxdxiAP(3,:)*T(:,:,3)  fac4*dAP(3,:) fac4*dxdxiAP(3,:)*T(:,:,4);
	 fac1*dAP(4,:) fac1*dxdxiAP(4,:)*T(:,:,1)  fac2*dAP(2,:) fac2*dxdxiAP(2,:)*T(:,:,2) ...
	 -fac2*dAP(2,:) fac2*dxdxiAP(2,:)*T(:,:,3) -fac1*dAP(4,:) fac1*dxdxiAP(4,:)*T(:,:,4)];
	 B(7:8,:) = invJ*dgdxi;
	 % ---
	 */

	{
		double dgdxi[2 * 24];
		for (int k = 0; k < 3; k++) {
			dgdxi[0 * 24 + k] = fac[2] * dAP[0 * 3 + k];
			dgdxi[0 * 24 + 6 + k] = -1 * fac[2] * dAP[0 * 3 + k];
			dgdxi[0 * 24 + 12 + k] = -1 * fac[3] * dAP[2 * 3 + k];
			dgdxi[0 * 24 + 18 + k] = fac[3] * dAP[2 * 3 + k];

			dgdxi[1 * 24 + k] = fac[0] * dAP[3 * 3 + k];
			dgdxi[1 * 24 + 6 + k] = fac[1] * dAP[1 * 3 + k];
			dgdxi[1 * 24 + 12 + k] = -1 * fac[1] * dAP[1 * 3 + k];
			dgdxi[1 * 24 + 18 + k] = -1 * fac[0] * dAP[3 * 3 + k];

			dgdxi[0 * 24 + 3 + k] = 0;
			dgdxi[0 * 24 + 9 + k] = 0;
			dgdxi[0 * 24 + 15 + k] = 0;
			dgdxi[0 * 24 + 21 + k] = 0;

			dgdxi[1 * 24 + 3 + k] = 0;
			dgdxi[1 * 24 + 9 + k] = 0;
			dgdxi[1 * 24 + 15 + k] = 0;
			dgdxi[1 * 24 + 21 + k] = 0;

			for (int l = 0; l < 3; l++) {
//				printf("%d\n",l * 3 * 4 + k* 4 + 3);
				dgdxi[0 * 24 + 3 + k] += dxdxiAP[0 * 3 + l] * T[l * 3 * 4 + k* 4 + 0];
				dgdxi[0 * 24 + 9 + k] += dxdxiAP[0 * 3 + l] * T[l * 3 * 4 + k* 4 + 1];
				dgdxi[0 * 24 + 15 + k] += dxdxiAP[2 * 3 + l] * T[l * 3 * 4 + k* 4 + 2];
				dgdxi[0 * 24 + 21 + k] += dxdxiAP[2 * 3 + l] * T[l * 3 * 4 + k* 4 + 3];
				dgdxi[1 * 24 + 3 + k] += dxdxiAP[3 * 3 + l] * T[l * 3 * 4 + k* 4 + 0];
				dgdxi[1 * 24 + 9 + k] += dxdxiAP[1 * 3 + l] * T[l * 3 * 4 + k* 4 + 1];
				dgdxi[1 * 24 + 15 + k] += dxdxiAP[1 * 3 + l] * T[l * 3 * 4 + k* 4 + 2];
				dgdxi[1 * 24 + 21 + k] += dxdxiAP[3 * 3 + l] * T[l * 3 * 4 + k* 4 + 3];
			}

			dgdxi[0 * 24 + 3 + k] *= fac[2];
			dgdxi[0 * 24 + 9 + k] *= fac[2];
			dgdxi[0 * 24 + 15 + k] *= fac[3];
			dgdxi[0 * 24 + 21 + k] *= fac[3];

			dgdxi[1 * 24 + 3 + k] *= fac[0];
			dgdxi[1 * 24 + 9 + k] *= fac[1];
			dgdxi[1 * 24 + 15 + k] *= fac[1];
			dgdxi[1 * 24 + 21 + k] *= fac[0];
		}
		for (int k = 0; k < 24; k++) {
			B[6 * 24 + k] = invJ[0 * 2 + 0] * dgdxi[0 * 24 + k] + invJ[0 * 2+ 1] * dgdxi[1 * 24 + k];
			B[7 * 24 + k] = invJ[1 * 2 + 0] * dgdxi[0 * 24 + k] + invJ[1 * 2+ 1] * dgdxi[1 * 24 + k];

		}
	}
}


double jacobiGlobalDerivatives(int i,__global double const * X,__global double* x,__global double* d,__global double* D,__global double* t1,__global double* t2,double* invJ,double* dNdt,double* dddt,double* dxdt,double* strG,double* fac,double* gAP,__global double const * xi1,__global double const * xi2,__global double const * dNdxi) {
	/*

		 % *** A R E A  I N T E G R A T I O N **************************************
		 for i=1:4
		 % --- JACOBI MATRIX ---
		 % local derivative of position vector
		 dXdxi = zeros(2,3);
		 for j=1:4
		 dXdxi = dXdxi + dNdxi(:,j,i)*X(j,:);
		 end
		 % jacobi
		 J = [dXdxi(1,:)*t1  dXdxi(1,:)*t2;dXdxi(2,:)*t1  dXdxi(2,:)*t2];
		 detJ = det(J);
		 % ---
		 */



	double detJ;
	{
		double J[4]={0};
		{
			double dXdxi[2 * 3]={0};
			for (int j = 0; j < 4; j++) {
				for (int k = 0; k < 2; k++) {
					for (int l = 0; l < 3; l++) {
						dXdxi[k * 3 + l] = dXdxi[k * 3 + l] + dNdxi[k * 4 * 4 + j* 4 + i] * X[j * 3 + l];
					}
				}
			}
			for (int k = 0; k < 3; k++) {
				J[0] += dXdxi[0 * 3 + k] * t1[k];
				J[1] += dXdxi[0 * 3 + k] * t2[k];
				J[2] += dXdxi[1 * 3 + k] * t1[k];
				J[3] += dXdxi[1 * 3 + k] * t2[k];
			}
		}
		detJ = J[0] * J[3] - J[1] * J[2];

		fac[0] = 0.25 * (1 + xi1[i]);
		fac[1] = 0.25 * (1 - xi1[i]);
		fac[2] = 0.25 * (1 + xi2[i]);
		fac[3] = 0.25 * (1 - xi2[i]);
		/*
		 *  % --- GLOBAL DERIVATIVES ---
		 % shape functions:
		 invJ = J^(-1);
		 dNdt[2*4]
		 dNdt = invJ*dNdxi(:,:,i);
		 % position vector (undeformed):
		 dXdt = dNdt*X;
		 % position vector:
		 dxdt = dNdt*x;
		 % director vector (undeformed):
		 dDdt = dNdt*D;
		 % director vector:
		 dddt = dNdt*d';
		 % ---
		 */
		invJ[0] = (double)1 / detJ * J[3];
		invJ[1] = (double)1 / detJ * (-1) * J[1];
		invJ[2] = (double)1 / detJ * (-1) * J[2];
		invJ[3] = (double)1 / detJ * J[0];
	}
	zeros(dNdt, 2 * 4);
	for (int k = 0; k < 4; k++) {
		for (int l = 0; l < 2; l++) {
			dNdt[0 * 4 + k] += invJ[0 * 2 + l] * dNdxi[l * 4 * 4 + k * 4+ i];
			dNdt[1 * 4 + k] += invJ[1 * 2 + l] * dNdxi[l * 4 * 4 + k * 4+ i];
		}
	}
	{
		double dDdt[2 * 3];
		double dXdt[2 * 3];
		for (int k = 0; k < 3; k++) {
		for (int j = 0; j < 2; j++) {
			double value = 0;
			double value1 = 0;
			double value2 = 0;
			double value3 = 0;
			for (int l = 0; l < 4; l++) {
				value += dNdt[j * 4 + l] * X[l * 3 + k];
				value1 += dNdt[j * 4 + l] * x[l * 3 + k];
				value2 += dNdt[j * 4 + l] * D[l * 3 + k];
				//d[3*4]; must be transposed
				value3 += dNdt[j * 4 + l] * d[k * 4 + l];
			}
			dXdt[j * 3 + k] = value;
			dxdt[j * 3 + k] = value1;
			dDdt[j * 3 + k] = value2;
			dddt[j * 3 + k] = value3;

		}
	}


	/*
	 * strG vektor(8,1);
	 *  % --- SHELL STRAINS ---
	 % directors at sampling point:
	 strG(1) = 0.5*(dxdt(1,:)*dxdt(1,:)' - dXdt(1,:)*dXdt(1,:)');
	 strG(2) = 0.5*(dxdt(2,:)*dxdt(2,:)' - dXdt(2,:)*dXdt(2,:)');
	 strG(3) = dxdt(1,:)*dxdt(2,:)' - dXdt(1,:)*dXdt(2,:)';
	 strG(4) = dxdt(1,:)*dddt(1,:)' - dXdt(1,:)*dDdt(1,:)';
	 strG(5) = dxdt(2,:)*dddt(2,:)' - dXdt(2,:)*dDdt(2,:)';
	 strG(6) = dxdt(1,:)*dddt(2,:)' + dxdt(2,:)*dddt(1,:)' - dXdt(1,:)*dDdt(2,:)' - dXdt(2,:)*dDdt(1,:)';
	 g(1) = 2*(fac3*gAP(1) + fac4*gAP(3));
	 g(2) = 2*(fac1*gAP(4) + fac2*gAP(2));
	 strG(7:8) = invJ*g';
	 % ---
	 */
	zeros(strG,8*1);
	for (int k = 0; k < 3; k++) {

		strG[0] += 0.5 * (dxdt[0 * 3 + k] * dxdt[0 * 3 + k]- dXdt[0 * 3 + k] * dXdt[0 * 3 + k]);
		strG[1] += 0.5 * (dxdt[1 * 3 + k] * dxdt[1 * 3 + k]- dXdt[1 * 3 + k] * dXdt[1 * 3 + k]);
		strG[2] += dxdt[0 * 3 + k] * dxdt[1 * 3 + k] - dXdt[0 * 3 + k]* dXdt[1 * 3 + k];
		strG[3] += dxdt[0 * 3 + k] * dddt[0 * 3 + k] - dXdt[0 * 3 + k]* dDdt[0 * 3 + k];
		strG[4] += dxdt[1 * 3 + k] * dddt[1 * 3 + k] - dXdt[1 * 3 + k]* dDdt[1 * 3 + k];
		strG[5] += dxdt[0 * 3 + k] * dddt[1 * 3 + k] + dxdt[1 * 3 + k]* dddt[0 * 3 + k] - dXdt[0 * 3 + k] * dDdt[1 * 3 + k]- dXdt[1 * 3 + k] * dDdt[0 * 3 + k];
	}
	}
	{
		double g[2];

		g[0] = 2 * (fac[2] * gAP[0] + fac[3] * gAP[2]);
		g[1] = 2 * (fac[0] * gAP[3] + fac[1] * gAP[1]);
		strG[6] = invJ[0 * 2 + 0] * g[0] + invJ[0 * 2 + 1] * g[1];
		strG[7] = invJ[1 * 2 + 0] * g[0] + invJ[1 * 2 + 1] * g[1];
	}
	return detJ;
}
void assumedNaturalStrains(__global double* D,__global double* d,__global double const * X,__global double* x,double* dAP,double* dxdxiAP,double* gAP){

	/*
	 * % deformed directors at allocation points
	 dAP(1,:) = 0.5*(d(:,1) + d(:,2))'; % Point A
	 dAP(2,:) = 0.5*(d(:,2) + d(:,3))'; % Point B
	 dAP(3,:) = 0.5*(d(:,3) + d(:,4))'; % Point C
	 dAP(4,:) = 0.5*(d(:,1) + d(:,4))'; % Point D
	 *
	 */

	for (int i = 0; i < 3; i++) {
		dAP[0 * 3 + i] = 0.5 * (d[4 * i + 0] + d[4 * i + 1]);
		dAP[1 * 3 + i] = 0.5 * (d[4 * i + 1] + d[4 * i + 2]);
		dAP[2 * 3 + i] = 0.5 * (d[4 * i + 2] + d[4 * i + 3]);
		dAP[3 * 3 + i] = 0.5 * (d[4 * i + 0] + d[4 * i + 3]);
	}


	/*
	 *
	 % derivative of position vector
	 dxdxiAP(1,:) = 0.5*(x(1,:) - x(2,:));
	 dxdxiAP(2,:) = 0.5*(x(2,:) - x(3,:));
	 dxdxiAP(3,:) = 0.5*(x(4,:) - x(3,:));
	 dxdxiAP(4,:) = 0.5*(x(1,:) - x(4,:));
	 *
	 */
	for (int i = 0; i < 3; i++) {
		dxdxiAP[0 * 3 + i] = 0.5 * (x[0 * 3 + i] - x[1 * 3 + i]);
		dxdxiAP[1 * 3 + i] = 0.5 * (x[1 * 3 + i] - x[2 * 3 + i]);
		dxdxiAP[2 * 3 + i] = 0.5 * (x[3 * 3 + i] - x[2 * 3 + i]);
		dxdxiAP[3 * 3 + i] = 0.5 * (x[0 * 3 + i] - x[3 * 3 + i]);
	}
	/*
	 * % --- ASSUMED NATURAL STRAINS ---
	 % undeformed directors at allocation points
	 DAP(1,:) = 0.5*(D(1,:) + D(2,:))'; % Point A
	 DAP(2,:) = 0.5*(D(2,:) + D(3,:))'; % Point B
	 DAP(3,:) = 0.5*(D(3,:) + D(4,:))'; % Point C
	 DAP(4,:) = 0.5*(D(1,:) + D(4,:))'; % Point D
	 */
	{
		double DAP[4 * 3]={0};
		double dXdxiAP[4 * 3]={0};
		for (int i = 0; i < 3; i++) {
			DAP[3 * 0 + i] = 0.5 * (D[3 * 0 + i] + D[3 * 1 + i]);
			DAP[3 * 1 + i] = 0.5 * (D[3 * 1 + i] + D[3 * 2 + i]);
			DAP[3 * 2 + i] = 0.5 * (D[3 * 2 + i] + D[3 * 3 + i]);
			DAP[3 * 3 + i] = 0.5 * (D[3 * 0 + i] + D[3 * 3 + i]);

		}
		/*
		 * % derivative of position vector
		 dXdxiAP(1,:) = 0.5*(X(1,:) - X(2,:));
		 dXdxiAP(2,:) = 0.5*(X(2,:) - X(3,:));
		 dXdxiAP(3,:) = 0.5*(X(4,:) - X(3,:));
		 dXdxiAP(4,:) = 0.5*(X(1,:) - X(4,:));
		 */
		for (int i = 0; i < 3; i++) {
			dXdxiAP[0 * 3 + i] = 0.5 * (X[0 * 3 + i] - X[1 * 3 + i]);
			dXdxiAP[1 * 3 + i] = 0.5 * (X[1 * 3 + i] - X[2 * 3 + i]);
			dXdxiAP[2 * 3 + i] = 0.5 * (X[3 * 3 + i] - X[2 * 3 + i]);
			dXdxiAP[3 * 3 + i] = 0.5 * (X[0 * 3 + i] - X[3 * 3 + i]);
		}
		/*
		 *gAP  = zeros(4,1);
		 % transverse strains:
		 for i=1:4
		 gAP(i) = dxdxiAP(i,:)*dAP(i,:)' - dXdxiAP(i,:)*DAP(i,:)';
		 end*/
		for (int i = 0; i < 4; i++) {
			double value = 0;
			for (int k = 0; k < 3; k++) {
				value += dxdxiAP[i * 3 + k] * dAP[i * 3 + k] - dXdxiAP[i * 3 + k]* DAP[i * 3 + k];
			}
			gAP[i] = value;
		}
	}
}

double interpolationConstantsOfStressAndStrains(__global double const * X,__global double* Nsig,__global double* Nstr,__global double* t1,__global double* t2,__global double const * xi1,__global double const * xi2) {
	/*
	 % --- INTERPOLATION CONSTANTS OF STRESSES AND STRAINS ---	//
	 Gxi1_0 = 0.25*(X(1,:) - X(2,:) - X(3,:) + X(4,:));
	 Gxi2_0 = 0.25*(X(1,:) + X(2,:) - X(3,:) - X(4,:));
	 G_1    = 0.25*(X(1,:) - X(2,:) + X(3,:) - X(4,:));
	 j0 = norm(cross(Gxi1_0,Gxi2_0));
	 j1 = t3'*cross(Gxi1_0,G_1)';
	 j2 = t3'*cross(G_1,Gxi2_0)';
	 %
	 A = 12*j0;	xi1q = j1/A;   xi2q = j2/A;
	 */
	double J0[2 * 2]={0};
	double xi1q=0, xi2q=0;
	double detJ0=0;
	{
		double G_1[3]={0};
		double Gxi1_0[3]={0};
		double Gxi2_0[3]={0};
		for (int i = 0; i < 3; i++) {
			Gxi1_0[i] = 0.25 * (X[0 * 3 + i] - X[1 * 3 + i] - X[2 * 3 + i] + X[3* 3 + i]);
			Gxi2_0[i] = 0.25 * (X[0 * 3 + i] + X[1 * 3 + i] - X[2 * 3 + i] - X[3* 3 + i]);
			G_1[i] = 0.25 * (X[0 * 3 + i] - X[1 * 3 + i] + X[2 * 3 + i] - X[3 * 3+ i]);
		}

		// cross product of two vectors wiht length 3
		{
			double t3[3]={0};
			double tempCross[3]={0};
			double j0=0, j1=0, j2=0;
			crossPG(t1, t2, t3);
			zerosG(Nsig,8*14*4);
			zerosG(Nstr,10*20*4);
			crossP(Gxi1_0, Gxi2_0, tempCross);
			zeros(J0,4);
			j0 = norm(tempCross, 3);
			crossP(Gxi1_0, G_1, tempCross);
			j1 = t3[0] * tempCross[0] + t3[1] * tempCross[1] + t3[2] * tempCross[2];
			crossP(G_1, Gxi2_0, tempCross);
			j2 = t3[0] * tempCross[0] + t3[1] * tempCross[1] + t3[2] * tempCross[2];

			/*
			 * A = 12*j0;	xi1q = j1/A;   xi2q = j2/A;
			 */

			xi1q = j1 / (12 * j0);
			xi2q = j2 / (12 * j0);
		}
		/*
		 % --- JACOBI MATRIX AT CENTER ---
		 J0 = [Gxi1_0*t1  Gxi1_0*t2;Gxi2_0*t1  Gxi2_0*t2];
		 detJ0 = det(J0);
		 % ---
		 */

		//zeros(J0,4);

		for (int i = 0; i < 3; i++) {
			J0[0]+=Gxi1_0[i]*t1[i];
			J0[1]+=Gxi1_0[i]*t2[i];
			J0[2]+=Gxi2_0[i]*t1[i];
			J0[3]+=Gxi2_0[i]*t2[i];

		}

	}

	detJ0=J0[0]*J0[3]-J0[1]*J0[2];

	/*
	 * % --- PREASSIGN INTERPOLATION MATRICES ---
	 * I4 = zeros(3,3,4);
	 * for i=1:4,I4(:,:,i) = eye(3);end
	 * % STRESSES:
	 NsigM(1,1,:) = J0(1,1)*J0(1,1)*(xi2-xi2q);  NsigM(1,2,:) = J0(2,1)*J0(2,1)*(xi1-xi1q);
	 NsigM(2,1,:) = J0(1,2)*J0(1,2)*(xi2-xi2q);  NsigM(2,2,:) = J0(2,2)*J0(2,2)*(xi1-xi1q);
	 NsigM(3,1,:) = J0(1,1)*J0(1,2)*(xi2-xi2q);  NsigM(3,2,:) = J0(2,1)*J0(2,2)*(xi1-xi1q);
	 NsigS(1,1,:) = J0(1,1)*(xi2-xi2q);  NsigS(1,2,:) = J0(1,2)*(xi1-xi1q);
	 NsigS(2,1,:) = J0(2,1)*(xi2-xi2q);  NsigS(2,2,:) = J0(2,2)*(xi1-xi1q);
	 Nsig(1:3,9:10,:) = NsigM;  Nsig(4:6,11:12,:) = NsigM;  Nsig(7:8,13:14,:) = NsigS;
	 Nsig(1:3,1:3,:) = I4;  Nsig(4:6,4:6,:) = I4;  Nsig(7:8,7:8,:) = I4(1:2,1:2,:);
	 */
	{
		double NsigS[2 * 2 * 4]={0};
		double NsigM[3 * 2 * 4]={0};
		double I4[3 * 3 * 4]={0};
		for (int i = 0; i < 4; i++) {
			for (int k = 0; k < 3; k++) {
				I4[k * 3 * 4 + k * 4 + i] = 1;
			}
		}
		for (int i = 0; i < 4; i++) {
			NsigM[0 * 2 * 4 + 0 * 4 + i] = J0[0]*J0[0] * (xi2[i] - xi2q);
			NsigM[0 * 2 * 4 + 1 * 4 + i] = J0[1 * 2 + 0]*J0[1 * 2 + 0] * (xi1[i]	- xi1q);
			NsigM[1 * 2 * 4 + 0 * 4 + i] = J0[0 * 2  + 1]*J0[0 * 2  + 1] * (xi2[i]- xi2q);
			NsigM[1 * 2 * 4 + 1 * 4 + i] = J0[1 * 2 + 1]*J0[1 * 2 + 1] * (xi1[i]	- xi1q);
			NsigM[2 * 2 * 4 + 0 * 4 + i] = J0[0] * J0[0 * 2  + 1] * (xi2[i]- xi2q);
			NsigM[2 * 2 * 4 + 1 * 4 + i] = J0[1 * 2 + 0] * J0[1 * 2 + 1]* (xi1[i] - xi1q);
			//NsigS = zeros(2,2,4);
			NsigS[0 + i] = J0[0] * (xi2[i] - xi2q);
			NsigS[4 + i] = J0[0 * 2 + 1] * (xi1[i] - xi1q);
			NsigS[8 + i] = J0[1 * 2 + 0] * (xi2[i] - xi2q);
			NsigS[12 + i] = J0[1 * 2 + 1] * (xi1[i] - xi1q);
			//Nsig  = zeros(8,14,4);
			for (int k = 0; k < 3; k++) {
				for (int l = 0; l < 2; l++) {
					Nsig[14 * 4 * (k + 0) + 4 * (l + 8) + i] = NsigM[k * 2 * 4 + l* 4 + i];
					Nsig[14 * 4 * (k + 3) + 4 * (l + 10) + i] = NsigM[k * 2 * 4 + l	* 4 + i];

				}
				for (int l = 0; l < 3; l++) {
					Nsig[14 * 4 * (k + 0) + 4 * (l + 0) + i] = I4[k * 3 * 4 + l * 4	+ i];
					Nsig[14 * 4 * (k + 3) + 4 * (l + 3) + i] = I4[k * 3 * 4 + l * 4	+ i];
				}
			}
			for (int k = 0; k < 2; k++) {
				for (int l = 0; l < 2; l++) {
					Nsig[14 * 4 * (k + 6) + 4 * (l + 12) + i] = NsigS[k * 2 * 4 + l	* 4 + i];
					Nsig[14 * 4 * (k + 6) + 4 * (l + 6) + i] = I4[k * 3 * 4 + l * 4	+ i];
				}
			}

		}
	}
	/*
	 * Nstr  = zeros(10,20,4);
	 * % first part:
	 Nstr(1:8,1:14,:) = Nsig;
	 Nstr([3,6],9:14,:)  = Nstr([3,6],9:14,:)*2;
	 */
	for (int i = 0; i < 8; i++) {
		for (int k = 0; k < 14; k++) {
			for (int l = 0; l < 4; l++) {
				int factor=(((i == 2) || (i == 5)) && ((k > 7) && (k < 14))) ? 2:1;
				Nstr[20 * 4 * i + 4 * k + l] = factor*Nsig[i * 14 * 4 + k * 4 + l];
//				Nstr[20 * 4 * i + 4 * k + l] = Nsig[i * 14 * 4 + k * 4 + l];
//				if (((i == 2) || (i == 5)) && ((k > 7) && (k < 14))) {
//					Nstr[20 * 4 * i + 4 * k + l] *= 2;
//				}
			}
		}
	}
	/*
	 * Tsig0 = zeros(3,3);
	 * % second part:
	 Tsig0(1,:) = [J0(1,1)*J0(1,1)  J0(2,1)*J0(2,1)  2*J0(1,1)*J0(2,1)];
	 Tsig0(2,:) = [J0(1,2)*J0(1,2)  J0(2,2)*J0(2,2)  2*J0(1,2)*J0(2,2)];
	 Tsig0(3,:) = [J0(1,1)*J0(1,2)  J0(2,1)*J0(2,2)  J0(1,1)*J0(2,2)+J0(1,2)*J0(2,1)];
	 invT = Tsig0'^(-1);
	 M2(1,1,:) = xi1;     M2(2,2,:) = xi2;
	 for i=1:4
	 Nstr(1:3,15:16,i)  = invT*M2(:,:,i);
	 Nstr(4:6,17:18,i)  = Nstr(1:3,15:16,i);
	 Nstr(9:10,19:20,i) = eye(2);
	 end
	 % ---
	 */
	{
		double Tsig0[3*3]={0};
		double invT[3*3]={0};
		Tsig0[0] = pown(J0[0], 2);
		Tsig0[1] = pown(J0[1 * 2 + 0], 2);
		Tsig0[2] = 2 * J0[0] * J0[1 * 2 + 0];
		Tsig0[3] = pown(J0[1], 2);
		Tsig0[4] = pown(J0[1 * 2 + 1], 2);
		Tsig0[5] = 2 * J0[1] * J0[1 *2 + 1];
		Tsig0[6] = J0[0] * J0[1];
		Tsig0[7] = J0[1 * 2 + 0] * J0[1 * 2 + 1];
		Tsig0[8] = J0[0] * J0[1 * 2 + 1] + J0[1] * J0[1 * 2 + 0];

		double detT = (double)1 / (Tsig0[0] * (Tsig0[4] * Tsig0[8] - Tsig0[7] * Tsig0[5])
				+ Tsig0[3] * (Tsig0[7] * Tsig0[2] - Tsig0[8] * Tsig0[1]) + Tsig0[6]
				* (Tsig0[1] * Tsig0[5] - Tsig0[4] * Tsig0[2]));
		invT[0] = detT * (Tsig0[4] * Tsig0[8] - Tsig0[5] * Tsig0[7]);
		invT[1] = detT * (Tsig0[6] * Tsig0[5] - Tsig0[3] * Tsig0[8]);
		invT[2] = detT * (Tsig0[3] * Tsig0[7] - Tsig0[6] * Tsig0[4]);
		invT[3] = detT * (Tsig0[7] * Tsig0[2] - Tsig0[1] * Tsig0[8]);
		invT[4] = detT * (Tsig0[0] * Tsig0[8] - Tsig0[6] * Tsig0[2]);
		invT[5] = detT * (Tsig0[6] * Tsig0[1] - Tsig0[0] * Tsig0[7]);
		invT[6] = detT * (Tsig0[1] * Tsig0[5] - Tsig0[4] * Tsig0[2]);
		invT[7] = detT * (Tsig0[3] * Tsig0[2] - Tsig0[0] * Tsig0[5]);
		invT[8] = detT * (Tsig0[0] * Tsig0[4] - Tsig0[3] * Tsig0[1]);

		{
			double M2[3*2*4]={0};
			for (int i = 0; i < 4; i++) {
				M2[0 + i] = xi1[i];
				M2[1 * 2 * 4 + 1 * 4 + i] = xi2[i];
			}

			for (int k = 0; k < 3; k++) {
				for (int l = 0; l < 2; l++) {
					for (int i = 0; i < 4; i++) {
						double value = 0;
						for (int j = 0; j < 3; j++) {
							value += invT[k * 3 + j] * M2[j * 2 * 4 + l * 4 + i];
						}

						Nstr[k * 20 * 4 + (l + 14) * 4 + i] = value;
						Nstr[(k + 3) * 20 * 4 + (l + 16) * 4 + i] = value;
						Nstr[8 * 20 * 4 + 18 * 4 + i] = 1;
						Nstr[8 * 20 * 4 + 19 * 4 + i] = 0;
						Nstr[9 * 20 * 4 + 18 * 4 + i] = 0;
						Nstr[9 * 20 * 4 + 19 * 4 + i] = 1;
					}
				}
			}
		}
	}
	return detJ0;
}



void localCoOrdinateSystem(__global double const * X,__global double* t1,__global double* t2){

	/*
	 % --- LOCAL CO-ORDINATE SYSTEM ---
	 d1 = X(1,:) - X(3,:);   d1 = d1/norm(d1);
	 d2 = X(4,:) - X(2,:);   d2 = d2/norm(d2);

	 t1 = d1 + d2;   t1 = t1'/norm(t1);
	 t2 = d1 - d2;   t2 = t2'/norm(t2);
	 t3 = cross(t1,t2);											// cross product of two vectors
	 % ---*/
	double d1[3]={0};
	double d2[3]={0};
	double d1temp = 0;
	double d2temp = 0;

	for (int i = 0; i < 3; i++) {
		d1[i] = X[0 * 3 + i] - X[2 * 3 + i];
		d1temp = d1temp + d1[i]*d1[i];
		d2[i] = X[3 * 3 + i] - X[1 * 3 + i];
		d2temp = d2temp + d2[i]*d2[i];
	}
	d1temp = sqrt(d1temp);
	d2temp = sqrt(d2temp);
	for (int i = 0; i < 3; i++) {
		d1[i] = d1[i] / d1temp;
		d2[i] = d2[i] / d2temp;
	}
	d1temp = 0;
	d2temp = 0;
	for (int i = 0; i < 3; i++) {
		t1[i] = d1[i] + d2[i];
		d1temp = d1temp + t1[i]*t1[i];
		t2[i] = d1[i] - d2[i];
		d2temp = d2temp + t2[i]*t2[i];
	}
	d1temp = sqrt(d1temp);
	d2temp = sqrt(d2temp);
	// t1 and t2 should be transposed but that is not done here
	for (int i = 0; i < 3; i++) {
		t1[i] = t1[i] / d1temp;
		t2[i] = t2[i] / d2temp;
	}


}

void transformationMatrices(__global double const * X,__global double const * u,__global double* T3,__global double* x,__global double const * lBase,__global double* H,__global double* d,__global int const * interSec,__global double* T) {
	/*	% --- TRANSFORMATION MATRICES ---
	 for i=1:4
	 ind = i*6-2:i*6;
	 % transformation matrices:
	 [R,H(:,:,i)] = transMat(u(ind));
	 % deformed base:
	 dBase = R*lBase(:,:,i);
	 % director vector:
	 d(:,i) = dBase(:,3);
	 % transformation matrix (local rotations):
	 if interSec(i)
	 T3(:,:,i) = eye(3);
	 else
	 T3(:,:,i) = [dBase(:,1:2) zeros(3,1)];
	 end
	 % virtual axial vector to virtual director vector
	 W = zeros(3,3);
	 W(1,2) = -d(3,i); W(1,3) = d(2,i); W(2,3) = -d(1,i);
	 W = W - W';
	 % transformation matrix:
	 T(:,:,i) = W'*H(:,:,i)*T3(:,:,i);
	 end
	 */

	zerosG(H,3*3*4);
	zerosG(T3,3*3*4);
	zerosG(T,3*3*4);
	//x = X + [u(1:3)';u(7:9)';u(13:15)';u(19:21)';];

	for(int i=0;i<3;i++) {
		x[0*3+i]=X[0*3+i]+u[i];
		x[1*3+i]=X[1*3+i]+u[i+6];
		x[2*3+i]=X[2*3+i]+u[i+12];
		x[3*3+i]=X[3*3+i]+u[i+18];
	}


	//% lBase    ... local base                   [3x3x4]
	{
		double R[9]={0};
		double dBase[9]={0};
		double HT3[9]={0};
		double W[3 * 3]={0};
		for (int i = 0; i < 4; i++) {
			int ind = i * 6 + 3;

			transMat(ind, u, R, H,i);
	//		printMat(R,3,3);
	//		printf("\n");
			for (int k = 0; k < 3; k++) {
				for (int l = 0; l < 3; l++) {
					double value = 0;
					for (int r = 0; r < 3; r++) {
						value += R[k*3 + r] * lBase[i*3*3 + 3*r + l];
					}
					dBase[k * 3 + l] = value;
				}
			}

			for (int k = 0; k < 3; k++) {
				d[k * 4 + i] = dBase[3 * k + 2];
			}
			//T3 = zeros(3,3,4);

			if (interSec[i] == 1) {
				for (int k = 0; k < 3; k++) {
					for (int l = 0; l < 3; l++) {
						if (k == l) {
							//T3(:,:,i) = eye(3);
							T3[3 * 4 * k + 4 * l + i] = 1;
						}
					}

				}
			} else {
				//T3(:,:,i) = [dBase(:,1:2) zeros(3,1)];
				for (int k = 0; k < 3; k++) {
					for (int l = 0; l < 2; l++) {
						T3[3 * 4 * k + 4 * l + i] = dBase[k * 3 + l];
					}
				}
			}

		/*
		 W = zeros(3,3);
		 W(1,2) = -d(3,i); W(1,3) = d(2,i); W(2,3) = -d(1,i);
		 W = W - W';
		 % transformation matrix:
		 T(:,:,i) = W'*H(:,:,i)*T3(:,:,i);
		 */


			//W is transposed used thats why transposed calculated
			W[0 * 3 + 1] = -1*d[2 * 4 + i];
			W[0 * 3 + 2] = d[1 * 4 + i];
			W[1 * 3 + 2] = -1*d[0 * 4 + i];

			W[1 * 3 + 0] = d[2 * 4 + i];
			W[2 * 3 + 0] =  -1 *d[1 * 4 + i];
			W[2 * 3 + 1] = d[0 * 4 + i];

			for (int k = 0; k < 3; k++) {
				for (int l = 0; l < 3; l++) {
					double value = 0;
					for (int r = 0; r < 3; r++) {
						value += H[k*3*4 + r*4 + i] * T3[r*3*4 + l*4 + i];
					}
					HT3[3*k + l] = value;
				}
			}
	//		printMat(HT3,3,3);
			for (int k = 0; k < 3; k++) {
				for (int l = 0; l < 3; l++) {
					double value = 0;
					for (int r = 0; r < 3; r++) {
						value += W[r*3 + k] * HT3[r*3 + l];
					}
					T[3*4*k + l*4 + i] = value;
				}
			}
		}
	}
}


/*
 * matrix B=A^(-1)
 * A[a x a]
 * Gauss Jordan algorithm
 */
void invMatr(__private double* A, int a, __global double* B) {
// turm B into I
	zerosG(B,a*a);

	for (int i = 0; i < a; i++) {
		B[i * a + i] = 1;
	}

	for (int i = 0; i < a; i++) {
		if (A[i * a + i] == 0) {
			double maxval=0;
			int kmax=0;
			for (int k = i + 1; k < a; k++) {
				if ((A[k * a + i] > maxval)||(A[k * a + i]<maxval)) {
					maxval=A[k*a+i];
					kmax=k;
				}
			}
			for (int l = 0; l < a; l++) {
				double tempv = A[kmax * a + l];
				A[kmax * a + l] = A[i * a + l];
				A[i * a + l] = tempv;
				tempv = B[kmax * a + l];
				B[kmax * a + l] = B[i * a + l];
				B[i * a + l] = tempv;
			}
		}
		{
			double div = A[i * a + i];

			for (int k = 0; k < a; k++) {
				A[i * a + k] = A[i * a + k] / div;
				B[i * a + k] = B[i * a + k] / div;
			}

			for (int k = 0; k < a; k++) {
				div = A[k * a + i]/A[i*a+i];
				if(k!=i) {
					for (int l = 0; l < a; l++) {
						if(l>=i) {
							A[k * a + l] -= A[i * a + l] * div;
						}
						B[k * a + l] -= B[i * a + l] * div;

					}
				}
			}
		}
	}
}
/*
 * matrixMult
 * A[a x ab]
 * B[ab x b]
 * C[a x b]
 */
void matrixMult(double* A, int a, int ab,double* B, int b, double* C) {

	for (int i = 0; i < a; i++) {
		for (int k = 0; k < b; k++) {
			C[i * b + k] = 0;
			for (int l = 0; l < ab; l++) {
				C[i * b + k] += A[i * ab + l] * B[l * b + k];
			}
		}
	}
}
void matrixMult1(__global double* A, int a, int ab,__global double* B, int b, double* C) {

	for (int i = 0; i < a; i++) {
		for (int k = 0; k < b; k++) {
			C[i * b + k] = 0;
			for (int l = 0; l < ab; l++) {
				C[i * b + k] += A[i * ab + l] * B[l * b + k];
			}
		}
	}
}
void matrixMult2(double* A, int a, int ab,__global double* B, int b, double* C) {

	for (int i = 0; i < a; i++) {
		for (int k = 0; k < b; k++) {
			C[i * b + k] = 0;
			for (int l = 0; l < ab; l++) {
				C[i * b + k] += A[i * ab + l] * B[l * b + k];
			}
		}
	}
}
void matrixMult3(double* A, int a, int ab,__global double* B, int b,__global double* C) {

	for (int i = 0; i < a; i++) {
		for (int k = 0; k < b; k++) {
			C[i * b + k] = 0;
			for (int l = 0; l < ab; l++) {
				C[i * b + k] += A[i * ab + l] * B[l * b + k];
			}
		}
	}
}
void matrixMult4(double* A, int a, int ab,double* B, int b,__global double* C) {

	for (int i = 0; i < a; i++) {
		for (int k = 0; k < b; k++) {
			C[i * b + k] = 0;
			for (int l = 0; l < ab; l++) {
				C[i * b + k] += A[i * ab + l] * B[l * b + k];
			}
		}
	}
}
/*
 * % second variation of director vector
 function M = secDirVec(rotVec,dirVec,h)

 % coefficients:
 magO = norm(rotVec);
 if magO<1E-4strainDisplacementMatrix
 c3   = 1/6*(1+1/60*magO^2);
 c10q = 1/6*(1+1/30*magO^2);
 c11  =-1/360*(1+1/21*magO^2);
 else
 c3   = (magO*sin(magO) + 2*(cos(magO)-1))/(magO^2*(cos(magO)-1));
 c10q = (sin(magO)-magO)/(2*magO*(cos(magO)-1));
 c11  = (4*(cos(magO)-1)+magO^2+magO*sin(magO))/(2*magO^4*(cos(magO)-1));
 end
 b(1) = dirVec(2)*h(3) - dirVec(3)*h(2);
 b(2) = dirVec(3)*h(1) - dirVec(1)*h(3);
 b(3) = dirVec(1)*h(2) - dirVec(2)*h(1);
 c10 = c10q*(b*rotVec) - dirVec'*h;

 t = -c3*b' + c11*(b*rotVec)*rotVec;
 M = 0.5*(dirVec*h' + h*dirVec') + 0.5*(t*rotVec' + rotVec*t') + c10*eye(3);
 */
void secDirVec(__global double const * rotVec, int rotStart,__global double* dirVec, int dir_k,__private double* h, __private double* M) {

	double c3;
	double c10q;
	double c11;

	double t[3];
	double c10 = 0;

	{
		double mag0 = sqrt(pown(rotVec[rotStart], 2)+ pown(rotVec[rotStart + 1], 2) + pown(rotVec[rotStart + 2],2));
		if (mag0 <  (double)1 /  (double)10000) {
			c3 =  (double)1 /  (double)6 * (1 +  (double)1 /  (double)60 * pown(mag0, 2));
			c10q =  (double)1 /  (double)6 * (1 +  (double)1 /  (double)30 * pown(mag0, 2));
			c11 =  (double)-1 /  (double)360 * (1 +  (double)1 /  (double)21 * pown(mag0, 2));
		} else {
			c3 = (mag0 *mySin(mag0) + 2*(myCos(mag0) - 1)) / (pown(mag0, 2)*(myCos(mag0) - 1));
			c10q = ((double)mySin(mag0) - mag0) / (2 * mag0 * ((double)myCos(mag0) - 1));
			c11 = (4 * ((double)myCos(mag0) - 1) + pown(mag0, 2) + mag0 * (double)mySin(mag0)) / (2* pown(mag0, 4) * ((double)myCos(mag0) - 1));
		}
	}
	{
		double b[3];
		double brotScalar=0;
		b[0] = dirVec[1 * 4 + dir_k] * h[2] - dirVec[2 * 4 + dir_k] * h[1];
		b[1] = dirVec[2 * 4 + dir_k] * h[0] - dirVec[0 * 4 + dir_k] * h[2];
		b[2] = dirVec[0 * 4 + dir_k] * h[1] - dirVec[1 * 4 + dir_k] * h[0];
		for(int i=0;i<3;i++) {
			brotScalar+=b[i]*rotVec[rotStart+i];
		}
		for (int i = 0; i < 3; i++) {
			c10 += c10q * (b[i] * rotVec[rotStart+i]) - dirVec[i * 4 + dir_k]* h[i];
			t[i] = -1 * c3 * b[i] + c11 * (brotScalar) * rotVec[rotStart+i];

		}
	}
	for (int i = 0; i < 3; i++) {
		for (int k = 0; k < 3; k++) {
			M[i * 3 + k] = 0.5 * (dirVec[i * 4 + dir_k] * h[k] + h[i]* dirVec[k * 4 + dir_k]) + 0.5 * (t[i] * rotVec[rotStart+ k] + rotVec[rotStart + i] * t[k]);
			if (k == i) {
				M[i * 3 + k] += c10;
			}
		}
	}

}

/*
 * % elastic material matrix:
 function C = elastC(E,nu,h)

 C  = zeros(10,10);
 c1 = nu/(1-nu);
 c2 = (1-2*nu)/(2*(1-nu));
 c3 = h^3/12;

 C(1,2)  = c1*h;
 C(1,9)  = c1*h;    C(2,9)  = c1*h;
 C(4,5)  = c3*c1;   C(4,10) = c3*c1;
 C(5,10) = c3*c1;
 C = C + C';
 C(1,1)  = h;       C(2,2)   = h;
 C(3,3)  = c2*h;    C(4,4)   = c3;
 C(5,5)  = c3;      C(6,6)   = c2*c3;
 C(7,7)  = c2*h;    C(8,8)   = c2*h;
 C(9,9)  = h;       C(10,10) = c3;
 C = E*(1-nu)/((1+nu)*(1-2*nu))*C;

 *
 */
void elastC( double E, double nu, double h,__private double* C) {
	zeros(C, 10*10);
	double c1 = nu / (1 - nu);
	double c2 = (1 - 2 * nu) / (2 * (1 - nu));
	double c3 = pown(h, 3 )/ (double) 12;
	double cmul = E * (1 - nu) / ((1 + nu) * (1 - 2 * nu));
	C[0 * 10 + 1] = cmul * c1 * h;
	C[0 * 10 + 8] = cmul * c1 * h;
	C[1 * 10 + 8] = cmul * c1 * h;
	C[3 * 10 + 4] = cmul * c3 * c1;
	C[3 * 10 + 9] = cmul * c3 * c1;
	C[4 * 10 + 9] = cmul * c3 * c1;
	// transposed
	C[1 * 10 + 0] = cmul * c1 * h;
	C[8 * 10 + 0] = cmul * c1 * h;
	C[8 * 10 + 1] = cmul * c1 * h;
	C[4 * 10 + 3] = cmul * c3 * c1;
	C[9 * 10 + 3] = cmul * c3 * c1;
	C[9 * 10 + 4] = cmul * c3 * c1;
	C[0 * 10 + 0] = cmul * h;
	C[1 * 10 + 1] = cmul * h;
	C[2 * 10 + 2] = cmul * c2 * h;
	C[3 * 10 + 3] = cmul * c3;
	C[4 * 10 + 4] = cmul * c3;
	C[5 * 10 + 5] = cmul * c2 * c3;
	C[6 * 10 + 6] = cmul * c2 * h;
	C[7 * 10 + 7] = cmul * c2 * h;
	C[8 * 10 + 8] = cmul * h;
	C[9 * 10 + 9] = cmul * c3;
}

/*
 * Sets all values of input array to 0
 */
void zeros(double* toZ,uint numel) {

	for (uint i = 0; i < numel; i++) {
		toZ[i] = 0;
	}

}

/*
 * Sets all values of input array to 0
 */
void zerosG(__global double* toZ,uint numel) {

	for (uint i = 0; i < numel; i++) {
		toZ[i] = 0;
	}

}

/*Crossproduct for vectors of size 3
 *
 */
void crossP(double* A,double* B,__private double *C) {

	C[0] = A[1] * B[2] - A[2] * B[1];
	C[1] = A[2] * B[0] - A[0] * B[2];
	C[2] = A[0] * B[1] - A[1] * B[0];

}
void crossPG(__global double* A,__global double* B,__private double *C) {

	C[0] = A[1] * B[2] - A[2] * B[1];
	C[1] = A[2] * B[0] - A[0] * B[2];
	C[2] = A[0] * B[1] - A[1] * B[0];

}
/*
 * calculate normalization with euclid
 */
double norm(__private double* A, int sizeA) {

	double retVal = 0;
	for (uint i = 0; i < sizeA; i++) {
		retVal = retVal + A[i]*A[i];
	}
	return sqrt(retVal);

}

/*
 % transformation matrices
 function [R,H] = transMat(rotVec)
 % R ... rotation matrix (Rodrigues' formula)
 % H ... rotation vector to axial vector

 O = zeros(3,3);
 O(1,2) = -rotVec(3); O(1,3) = rotVec(2); O(2,3) = -rotVec(1);
 O = O - O';

 magO = norm(rotVec);
 if magO<1E-4
 fac1 = 1;
 fac2 = 0.5;
 fac3 = 1/6;
 else
 fac1 = sin(magO)/magO;
 fac2 = (1-cos(magO))/magO^2;
 fac3 = (magO - sin(magO))/magO^3;
 end
 R = eye(3) + fac1*O + fac2*O^2;
 H = eye(3) + fac2*O + fac3*O^2;

 */
void transMat(int ind,__global double const * u,__private double* R,__global double* H,int s) {

	double O[3 * 3]={0};
	double O2[3 * 3]={0};
	double fac1;
	double fac2;
	double fac3;
	{
		double rotVec[3];
		rotVec[0] = u[ind];
		rotVec[1] = u[ind+1];
		rotVec[2] = u[ind+2];

		O[0 * 3 + 1] = -1 * rotVec[2];
		O[0 * 3 + 2] = rotVec[1];
		O[1 * 3 + 2] = -1 * rotVec[0];

		O[1 * 3 + 0] = rotVec[2];
		O[2 * 3 + 0] = -1 * rotVec[1];
		O[2 * 3 + 1] = rotVec[0];
	//	if(get_global_id(0)==0) {
	//		printf("%d",ind);
	//		printf("\n");
	//	}
		{
			double magO = norm(rotVec,3);
			if (magO < (double)1 / (double) 10000) {
				fac1 = 1;
				fac2 = 0.5;
				fac3 = (double)1 / (double)6;
			} else {
				fac1 = mySin(magO) / magO;
				fac2 = (1 - myCos(magO)) / (magO*magO);
				fac3 = (magO - mySin(magO)) / pown(magO, 3);
			}
		}
	}
	matrixMult(O,3,3,O,3,O2);

	for (int i = 0; i < 3; i++) {
		for (int j = 0; j < 3; j++) {
			if (i == j)  {
				R[i*3+j] = 1 + fac1 * O[i*3+j] + fac2 * O2[i*3+j];
				H[3*4*i+4*j+s] = 1 + fac2 * O[i*3+j] + fac3 * O2[i*3+j];
			} else {
				R[i*3+j] = fac1 * O[i*3+j] + fac2 * O2[i*3+j];
				H[3*4*i+4*j+s] = fac2 * O[i*3+j] + fac3 * O2[i*3+j];
			}
		}
	}
//	if(get_global_id(0)==0)
//		printMat(R,3,3);
}

double mySin(double val) {

	return sin(val);

}
double myCos(double val) {

	return cos(val);

}
//
//void printMat(double* Mat,int a,int b) {
//
//	for(int i=0;i<a;i++) {
//		for(int j=0;j<b;j++) {
//			printf("%f\t",Mat[i*b+j]);
//		}
//		printf("\n");
//	}
//	printf("\n");
//}
//
//void printMat3D(double* Mat,int a,int b,int c) {
//
//	for(int k=0;k<c;k++) {
//		for(int i=0;i<a;i++) {
//			for(int j=0;j<b;j++) {
//				printf("%f\t",Mat[i*b*c+j*c+k]);
//			}
//			printf("\n");
//		}
//		printf("\n");
//	}
//}
//void printMat3DG(__global double* Mat,int a,int b,int c) {
//	for(int k=0;k<c;k++) {
//		for(int i=0;i<a;i++) {
//			for(int j=0;j<b;j++) {
//				printf("%f\t",Mat[i*b*c+j*c+k]);
//			}
//			printf("\n");
//		}
//		printf("\n");
//	}
//}

/*
 % sampling points and weights for thickness integration:
 function [xi3,w3] = sampWeight(n,a,b)

 if isequal(n,2)
 xi3 = [a  b];
 w3  = [0.5  0.5];
 elseif isequal(n,3)
 xi3 = linspace(a,b,3);
 w3  = [1/6  4/6  1/6];
 elseif isequal(n,4)
 xi3 = linspace(a,b,4);
 w3  = [1/8 3/8  3/8  1/8];
 elseif isequal(n,5)
 xi3 = linspace(a,b,5);
 w3  = [7/90 32/90 12/90 32/90 7/90];
 elseif isequal(n,6)
 xi3 = linspace(a,b,6);
 w3  = [19/288 75/288 50/288 50/288 75/288 19/288];
 elseif isequal(n,7)
 xi3 = linspace(a,b,7);
 w3  = [41/840 216/840 27/840 272/840 27/840 216/840 41/840];
 end
 */
/*void sampWeight(__global int* layProb, int numLay, int wLay,__private double* w3,__private double* xi3) {
	int i,k = 0;
	for (i = 0; i < numLay; i++) {
		double a = layProb[i * wLay + 2];
		double b = layProb[i * wLay + 3];
		double n = layProb[i * wLay + 4];
		if (n == 2) {
			xi3[i * 7] = a;
			xi3[i * 7 + 1] = b;
			w3[0] = 0.5;
			w3[1] = 0.5;
		} else if (n == 3) {
			// calculating value exactly in between of a b
			double vB = (b - a) / 2;
			xi3[i * 7] = a;
			xi3[i * 7 + 1] = vB + a;
			xi3[i * 7 + 2] = b;
			w3[0] = 1 / 6;
			w3[1] = 4 / 6;
			w3[2] = 1 / 6;
		} else if (n == 4) {
			double vB = (b - a) / 3;
			xi3[i * 7] = a;
			xi3[i * 7 + 1] = vB + a;
			xi3[i * 7 + 2] = vB * 2 + a;
			xi3[i * 7 + 3] = b;
			w3[0] = 1 / 8;
			w3[1] = 3 / 8;
			w3[2] = 3 / 8;
			w3[3] = 1 / 8;
		} else if (n == 5) {
			double vB = (b - a) / 4;
			for (k = 0; k < 5; k++) {
				xi3[i * 7 + k] = vB * k + a;
			}
			w3[0] = 7 / 90;
			w3[1] = 32 / 90;
			w3[2] = 12 / 90;
			w3[3] = 32 / 90;
			w3[4] = 7 / 90;

		} else if (n == 6) {
			double vB = (b - a) / 5;
			for (k = 0; k < 6; k++) {
				xi3[i * 7 + k] = vB * k + a;
			}
			w3[0] = 19 / 288;
			w3[1] = 75 / 288;
			w3[2] = 50 / 288;
			w3[3] = 50 / 288;
			w3[4] = 75 / 288;
			w3[5] = 19 / 288;

		} else if (n == 7) {
			double vB = (b - a) / 6;
			for (k = 0; k < 7; k++) {
				xi3[i * 7 + k] = vB * k + a;
			}

			w3[0] = 41 / 840;
			w3[1] = 216 / 840;
			w3[2] = 27 / 840;
			w3[3] = 272 / 840;
			w3[4] = 27 / 840;
			w3[5] = 216 / 840;
			w3[6] = 41 / 840;

		}
	}

	*/
