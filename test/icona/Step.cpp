/*
 * Step.cpp
 *
 *  Created on: 19.08.2010
 *      Author: clemens
 */

#include <string>
#include <vector>
#include <map>
#include <iostream>

using namespace std;

#include "Step.h"

//constructor + destructor
Step::Step() {
	parsing=false;
}

Step::~Step() {
}

//------------------------------------------------------------
//get- + set-methods
void Step::setParsing( bool val) {
	parsing=val;

	if( parsing) {
		lamInc=1;
		lamEnd=1;
		cLoads.clear();
		boundaryConditions.clear();
	}
}

bool Step::getParsing() {
	return parsing;
}

void Step::setStepName( std::string name) {
	stepName=name;
}

string Step::getStepName() {
	return stepName;
}

void Step::setLoadFactor( double factor) {
	lamEnd=factor;
}

double Step::getLoadFactor() {
	return lamEnd;
}

void Step::setLoadFactorInc( double factor) {
	lamInc=factor;
}

double Step::getLoadFactorInc() {
	return lamInc;
}

int Step::getNrOfCLoads() {
	return cLoads.size();
}

void Step::addCLoad( int nr, int direction, double force_) {
	CLoad tmpCLoad;

	tmpCLoad.nodeNr=nr;
	tmpCLoad.directionOfForce=direction;
	tmpCLoad.force=force_;

	cLoads.push_back( tmpCLoad);
}

vector<CLoad>& Step::getCLoads() {
	vector<CLoad>&  tmpCloads=cLoads;

	return tmpCloads;
}

int Step::getNrOfBoundaries() {
	return boundaryConditions.size();
}

void Step::addBoundaryCondition( int nr, int direction_) {
	BoundaryCondition tmpBoundary;

	tmpBoundary.nodeNr=nr;
	tmpBoundary.direction=direction_;

	boundaryConditions.push_back( tmpBoundary);
}

void Step::addGeneralBoundaryConditions( vector<BoundaryCondition> &boundCond) {
	for( boundaryConditionsIT=boundCond.begin(); boundaryConditionsIT!=boundCond.end(); boundaryConditionsIT++) {
		boundaryConditions.push_back( (*boundaryConditionsIT));
		//cout << 	(*boundaryConditionsIT).nodeNr << "--" << (*boundaryConditionsIT).direction << endl;
	}
}

vector<BoundaryCondition>& Step::getBoundaryConditionsOfStep() {
	vector<BoundaryCondition> &tmpBCs=boundaryConditions;

	return tmpBCs;
}

void Step::renumberNodeNrsBoundaryConditions( map< int, int> &nodeReferences) {
	int oldNr;
	vector<BoundaryCondition>::iterator boundCondIT;

	//cout << "boundaries" << endl;

//	for( boundaryConditionsIT=boundaryConditions.begin(); boundaryConditionsIT!=boundaryConditions.end(); boundaryConditionsIT++) {
//		oldNr=(*boundaryConditionsIT).nodeNr;
//		cout << oldNr << "\t";
//	}

	//cout << endl;

	for( boundaryConditionsIT=boundaryConditions.begin(); boundaryConditionsIT!=boundaryConditions.end(); boundaryConditionsIT++) {
		oldNr=(*boundaryConditionsIT).nodeNr;
		(*boundaryConditionsIT).nodeNr=nodeReferences[oldNr];
		//cout << nodeReferences[oldNr] << "\t";
	}

	//cout << endl;
}
