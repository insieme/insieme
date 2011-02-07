/*
 * FETask.cpp
 *
 * FiniteElementTask
 *
 *  Created on: 2.08.2010
 *      Author: clemens
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

//constructors + destructor
FETask::FETask() {
	minX=0;
	maxX=0;
	minY=0;
	maxY=0;
	minZ=0;
	maxZ=0;

	renumbered=false;
	success=false;

	maxIter=MAX_ITERATIONS;
	Utol=TOL_U;
	Ftol=TOL_F;

	nrCPUs=1;
}

FETask::~FETask() {
}

//------------------------------------------------------------
//static methods
string FETask::intToStr( int nr) {
	stringstream ssOut;

	ssOut << nr;
	return ssOut.str();
}

string FETask::doubleToStr( double nr) {
	stringstream ssOut;

	ssOut << nr;
	return ssOut.str();
}

void FETask::appendToLogFile( string message) {
	ofstream file;

	file.open( lfPathName.c_str(), ios::app);

	file << message;

	file.close();
}

//------------------------------------------------------------
//public methods
//get- + set-methods
void FETask::setLogFileName( std::string lfName) {
	lfPathName=lfName;
}

string FETask::getLogFileName() {
	return lfPathName;
}

void FETask::setStartTime( time_t start_) {
	rawTimeStart=start_;
}

time_t FETask::getStartTime() {
	return rawTimeStart;
}

void FETask::setInterimTime( time_t interim_) {
	rawTimeInterim=interim_;
}

time_t FETask::getInterimTime() {
	return rawTimeInterim;
}

void FETask::setHeading( std::string text) {
	heading=text;
}

string FETask::getHeading() {
	return heading;
}

int FETask::getNrOfNodes() {
	numNode=nodes.size();
	return numNode;
}

int FETask::getNrOfNsets() {
	return nsets.size();
}

void FETask::setDOFPerNode( int dof) {
	numDOFN=dof;
}

int FETask::getDOFPerNode() {
	return numDOFN;
}

void FETask::setNrOfNodesPerElement( int nr) {
	numNodeE=nr;
}

int FETask::getNrOfNodesPerElement() {
	return numNodeE;
}

double FETask::getLambda() {
	return Lambda;
}

void FETask::setMaxIterations( int nr) {
	maxIter=nr;
}

int FETask::getMaxIterations() {
	return maxIter;
}

void FETask::setUTol( double val) {
	Utol=val;
}

double FETask::getUTol() {
	return Utol;
}

void FETask::setFTol( double val) {
	Ftol=val;
}
double FETask::getFTol() {
	return Ftol;
}

void FETask::setNrOfMaxIterations( int nr) {
	maxIter=nr;
}

int FETask::getNrOfMaxIterations() {
	return maxIter;
}

int FETask::getNrOfElements() {
	numElement=elements.size();
	return numElement;
}

int FETask::getNrOfMaterials() {
	return materials.size();
}

int FETask::getNrOfElsets() {
	return elsets.size();
}

int FETask::getNrOfSteps() {
	numStep=steps.size();
	return numStep;
}

int FETask::getNrOfSolidSections() {
	return solidSections.size();
}

vector<int>& FETask::getNset( string name) {
	nsetsIT=nsets.find(name);

	vector<int>& tmpNSet=(*nsetsIT).second;

	return tmpNSet;
}

Step& FETask::getStepNr( int nr) {
	int i;
	int stepMaxNr=steps.size();

	if( (nr<0) && (nr>=stepMaxNr)) {
		nr=0;
	}

	for( i=0, stepsIT=steps.begin(); i<nr; stepsIT++) {
		i++;
	}

	Step &tmpStep=(*stepsIT).second;

	return tmpStep;
}

int FETask::getNrOfBoundaries() {
	return boundaryConditions.size();
}

double FETask::getMaxX() {
	return maxX;
}

double FETask::getMinX() {
	return minX;
}

double FETask::getMaxY() {
	return maxY;
}

double FETask::getMinY() {
	return minY;
}

double FETask::getMaxZ() {
	return maxZ;
}

double FETask::getMinZ() {
	return minZ;
}

vector< Node>& FETask::getNodes() {
	vector< Node>& tmpMap=nodes;

	return tmpMap;
}

vector< Node>& FETask::getNodesU() {
	vector< Node>& tmpMap=nodesU;

	return tmpMap;
}

map< int, Element>& FETask::getElements() {
	map< int, Element>& tmpMap=elements;

	return tmpMap;
}

bool FETask::getRenumbered() {
	return renumbered;
}

bool FETask::getSuccess() {
	return success;
}

int  FETask::getOmpNrThreads() {
	return nrCPUs;
}

void  FETask::setOmpNrThreads( int nr) {
	nrCPUs=nr;
}


//------------------------------------------------------------
//add-methods
int FETask::addNode( int nodeNr, double x, double y, double z) {
	int errorNr=0;
	int newNr;

	Node tmpNode( nodeNr, x, y, z);

	nodeReferencesIT=nodeReferences.find( nodeNr);

	if( nodeReferencesIT==nodeReferences.end()) {
		newNr=nodes.size();
		nodeReferences[nodeNr]=newNr;

		//cout << nodeNr << "---" << newNr << endl;

		tmpNode.setNodeNr( newNr);
		nodes.push_back( tmpNode);

		if( x>maxX) {
			maxX=x;
		}
		if( x<minX) {
			minX=x;
		}
		if( y>maxY) {
			maxY=y;
		}
		if( y<minY) {
			minY=y;
		}
		if( z>maxZ) {
			maxZ=z;
		}
		if( z<minZ) {
			minZ=z;
		}
	}
	else {
		errorNr=NODE_EXISTS;
	}

	return errorNr;
}

int FETask::addNset( string &nsetName, vector< int> &nodeNrsOfNset) {
	int errorNr=0;

	nsetsIT=nsets.find( nsetName);

	if( nsetsIT==nsets.end()) {
		nsets[nsetName]=nodeNrsOfNset;
	}
	else {
		errorNr=NSET_EXISTS;
	}

	return errorNr;
}

int FETask::addElement(int nodeNr, string &elementType, vector<int> &nodeNrsOfElement) {
	int errorNr=0;

	Element tmpElement( nodeNr, elementType, nodeNrsOfElement);

	elementsIT=elements.find( nodeNr);

	if( elementsIT==elements.end()) {
		elements[nodeNr]=tmpElement;
	}
	else {
		errorNr=ELEMENT_EXISTS;
	}

	return errorNr;
}

int FETask::addElset( string &elsetName, vector< int> &nodeNrsOfElset) {
	int errorNr=0;

	elsetsIT=elsets.find( elsetName);

	if( elsetsIT==elsets.end()) {
		elsets[elsetName]=nodeNrsOfElset;
	}
	else {
		errorNr=ELSET_EXISTS;
	}

	return errorNr;
}

int FETask::addMaterial( Material material) {
	int errorNr=0;
	string materialName=material.getMaterialName();

	materialsIT=materials.find( materialName);

	if( materialsIT==materials.end()) {
		materials[materialName]=material;
	}
	else {
		errorNr=MATERIAL_EXISTS;
	}

	return errorNr;
}

int FETask::addStep( Step analysisStep) {
	int errorNr=0;
	string stepName=analysisStep.getStepName();

	stepsIT=steps.find( stepName);

	if( stepsIT==steps.end()) {
		steps[stepName]=analysisStep;
	}
	else {
		errorNr=STEP_EXISTS;
	}

	return errorNr;
}

int FETask::addSolidSection( string elsetName, string materialName) {
	int errorNr=0;

	solidSectionsIT=solidSections.find( elsetName);

	if( solidSectionsIT==solidSections.end()) {
		solidSections[elsetName]=materialName;
	}
	else {
		errorNr=SOLIDSECTION_EXISTS;
	}

	return errorNr;
}

void FETask::addBoundaryCondition( int nr, int direction_) {
	BoundaryCondition tmpBoundary;

	tmpBoundary.nodeNr=nr;
	tmpBoundary.direction=direction_;

	boundaryConditions.push_back( tmpBoundary);
}

//------------------------------------------------------------
void FETask::renumberNodeNrsElements() {
	int oldNr;

	renumbered=true;

	//replace nodeNrs in elements
	for( elementsIT=elements.begin(); elementsIT!=elements.end(); elementsIT++) {
		vector<int> &nodesOfElement=(*elementsIT).second.getNodeNrsOfElement();

		//cout << "e " << (*elementsIT).first << "--" << nodesOfElement.size() << endl;

//		for( int i=0; i<(int) nodesOfElement.size(); i++) {
//			cout << nodesOfElement[i] << "\t";
//		}

//		cout << endl;

		for( int i=0; i<(int) nodesOfElement.size(); i++) {
			oldNr=nodesOfElement[i];
			nodesOfElement[i]=nodeReferences[oldNr];
//			cout << nodesOfElement[i] << "\t";
		}
	}
}

//------------------------------------------------------------
void FETask::setInitialTemperature() {
	int nodeNr, origNodeNr;

	for( initialNodeTemperaturesIT=initialNodeTemperatures.begin(); initialNodeTemperaturesIT!=initialNodeTemperatures.end(); initialNodeTemperaturesIT++) {
		origNodeNr=(*initialNodeTemperaturesIT).first;
		nodeNr=nodeReferences[origNodeNr];			//renumber node-numbers

		nodes[nodeNr].setInitalTemperature( (*initialNodeTemperaturesIT).second);
		nodes[nodeNr].setTemperature( (*initialNodeTemperaturesIT).second);
	}
}

//------------------------------------------------------------
void FETask::renumberNodeNrsCLoads( vector<CLoad> &cloadsOfStep) {
	int oldNr;
	vector<CLoad>::iterator cloadsOfStepIT;

	//replace nodeNrs in CLoads

	//cout << "cloads" << endl;

	for( cloadsOfStepIT=cloadsOfStep.begin(); cloadsOfStepIT!=cloadsOfStep.end(); cloadsOfStepIT++) {
		oldNr=(*cloadsOfStepIT).nodeNr;
		//cout << oldNr << "\t";
	}

	cout << endl;

	for( cloadsOfStepIT=cloadsOfStep.begin(); cloadsOfStepIT!=cloadsOfStep.end(); cloadsOfStepIT++) {
		oldNr=(*cloadsOfStepIT).nodeNr;
		(*cloadsOfStepIT).nodeNr=nodeReferences[oldNr];
		//cout << nodeReferences[oldNr] << "\t";
	}

	cout << endl;
}

//------------------------------------------------------------
int FETask::getNrOfInitialTemperatureNodes() {
	return initialNodeTemperatures.size();
}

//------------------------------------------------------------
void FETask::setInitialTemperatureNodes( map< int, double> &initialTemps) {
	initialNodeTemperatures=initialTemps;
}

//------------------------------------------------------------
//------------------------------------------------------------
string FETask::getStepName() {
	stepsIT=steps.begin();
	Step &currentStep=(*stepsIT).second;

	return currentStep.getStepName();
}

double FETask::getLoadFactor() {
	stepsIT=steps.begin();
	Step &currentStep=(*stepsIT).second;

	return currentStep.getLoadFactor();
}

double FETask::getLoadFactorInc() {
	stepsIT=steps.begin();
	Step &currentStep=(*stepsIT).second;

	return currentStep.getLoadFactorInc();
}

void FETask::mergeBoundaryConditions() {
	stepsIT=steps.begin();
	Step &currentStep=(*stepsIT).second;

	currentStep.addGeneralBoundaryConditions( boundaryConditions );
	currentStep.renumberNodeNrsBoundaryConditions( nodeReferences);

	vector<BoundaryCondition> &currentBC=currentStep.getBoundaryConditionsOfStep();
	vector<BoundaryCondition>::iterator currentBCIT;

	PetscInt nr=currentBC.size();

	PetscMalloc( nr*sizeof(PetscInt), &rows);
	PetscMalloc( nr*sizeof(PetscScalar), &values);

	int i=0;

	for( currentBCIT=currentBC.begin(); currentBCIT!=currentBC.end(); currentBCIT++) {
		rows[i]=(*currentBCIT).nodeNr*numDOFN + (*currentBCIT).direction - 1;
		//cout << rows[i] << endl;
		values[i]=0;

		i++;
	}
}

void FETask::applyBoundaryConditions( Mat &K_, Vec &f_) {
	PetscErrorCode ierr;

	PetscInt nr=getNrOfBoundaries();

	ierr=MatZeroRows( K_, nr, rows, 1); CHKERRV( ierr);
	ierr=VecSetValues( f_, nr, rows, values, INSERT_VALUES); CHKERRV( ierr);
}

void FETask::applyCLoads( Vec &f_) {
	PetscInt position;
	PetscErrorCode ierr;

	stepsIT=steps.begin();
	Step &currentStep=(*stepsIT).second;

	vector<CLoad> &cloadsOfStep=currentStep.getCLoads();
	renumberNodeNrsCLoads( cloadsOfStep);							//renumber cloads

	vector<CLoad>::iterator cloadsOfStepIT;

	for( cloadsOfStepIT=cloadsOfStep.begin(); cloadsOfStepIT!=cloadsOfStep.end(); cloadsOfStepIT++) {
		position=(*cloadsOfStepIT).nodeNr*numDOFN + (*cloadsOfStepIT).directionOfForce - 1;

		//cout << "CL " << (*cloadsOfStepIT).nodeNr << "-" << (*cloadsOfStepIT).directionOfForce << "-"<< position<<endl;

		ierr=VecSetValue(f_, position, (*cloadsOfStepIT).force, INSERT_VALUES); CHKERRV( ierr);
	}
}

void FETask::assembleStiffnessMatrix( Vec &du_, Vec &ut_, Mat &Kt, Vec &fin) {
	int nodeNr, elementNr, i, j, k, l, m, rpos, cpos;
	PetscErrorCode ierr;

	vector<int>::iterator elementsOfCSSIT;

	struct tm *timeinfo;
	string logMessage;

	map< int, elData> allKte[getOmpNrThreads()];
	map< int, elData>::iterator allKteIT;

	//loop solid sections
	for( solidSectionsIT=solidSections.begin(); solidSectionsIT!=solidSections.end(); solidSectionsIT++) {
		vector<int> &elementsOfCSS=elsets[ (*solidSectionsIT).first];		//get elements of current solidsection (string as index)

		//cout << endl << elementsOfCSS.size() << endl;

		omp_set_num_threads( getOmpNrThreads());

		#pragma omp parallel for
		for( i=0; i<(int) elementsOfCSS.size(); i++) {
			int elementNr_=elementsOfCSS[i];

			if( elements[elementNr_].getElementType().compare( "HC8A8E9")==0) {
				//cout << "\r" << elementNr_;
				ElemHC8A8E9 oneElement( elements[elementNr_], nodes, du_, ut_, materials[ (*solidSectionsIT).second], allKte[omp_get_thread_num()]);
			}
		}
	}

	time( &rawTimeEnd);
	timeinfo=localtime( &rawTimeEnd);

	logMessage="generating element matrices t=";
	logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeInterim)).c_str());
	logMessage.append( "s\n");

	cout << logMessage;
	FETask::appendToLogFile( logMessage);

	rawTimeInterim=rawTimeEnd;

	//assemble gobal stiffnessmatrix
	for( m=0; m<getOmpNrThreads(); m++) {
		//cout << m << "-" << allKte[m].size() << endl;
		PetscPrintf( PETSC_COMM_WORLD, "%d/%d\t", m+1, getOmpNrThreads());
		//cout << m+1 <<"/" << getOmpNrThreads() << "   ";

		for( allKteIT=allKte[m].begin();  allKteIT!=allKte[m].end(); allKteIT++) {
			elementNr=(*allKteIT).first;

			//cout << "\r" << elementNr;
			//cout << elementNr << endl;

			for( i=0; i<24; i+=3) {
				nodeNr=elements[elementNr].getGlobalNrOfNodeNr( (int) i/3);
				rpos=numDOFN*nodeNr;

				for( j=0; j<24; j+=3) {
					nodeNr=elements[elementNr].getGlobalNrOfNodeNr( (int) j/3);
					cpos=numDOFN*nodeNr;

					//cout << rpos << " -- " << cpos << endl;

					for( k=0; k<3; k++) {
						for( l=0; l<3; l++) {
							ierr=MatSetValue( Kt, rpos+k, cpos+l, (*allKteIT).second.kte[i+k][j+l], ADD_VALUES); CHKERRV( ierr);
							//cout << (*allKteIT).second.kte[i+k][j+l] << " ";
						}
						//cout << endl;
					}
				}

				ierr=VecSetValue( fin, rpos  , (*allKteIT).second.rhs[i]  , ADD_VALUES); CHKERRV( ierr);
				ierr=VecSetValue( fin, rpos+1, (*allKteIT).second.rhs[i+1], ADD_VALUES); CHKERRV( ierr);
				ierr=VecSetValue( fin, rpos+2, (*allKteIT).second.rhs[i+2], ADD_VALUES); CHKERRV( ierr);
			}
		}

		allKte[m].clear();
	}

	time( &rawTimeEnd);
	timeinfo=localtime( &rawTimeEnd);

	logMessage="\nfilling global matrix t=";
	logMessage.append( FETask::doubleToStr( difftime( rawTimeEnd, rawTimeInterim)).c_str());
	logMessage.append( "s\n");

	PetscPrintf( PETSC_COMM_WORLD, "%s\n", logMessage.c_str());
	//cout << logMessage;
	FETask::appendToLogFile( logMessage);

	rawTimeInterim=rawTimeEnd;
}
