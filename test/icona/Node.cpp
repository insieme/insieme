/*
 * Node.cpp
 *
 * class for handling 3D-nodes
 *
 *  Created on: 21.01.2010
 *  Modivied:   02.08.2010
 *
 *      Author: clemens
 */

#include <iostream>
#include <vector>
#include <cmath>
#include <string>

using namespace std;

#include "Node.h"

//contructors + destructor
Node::Node() {
}

Node::Node( int nr, double x, double y, double z) {
	origNodeNr=nr;

	nodeCoords[0]=x;
	nodeCoords[1]=y;
	nodeCoords[2]=z;

	initialTemperature=0;
	temperature=0;
}

Node::Node( int nr, double coords[3]) {
	origNodeNr=nr;
	nodeNr=nr;

	for( int i=0; i<3; i++) {
		nodeCoords[i]=coords[i];
	}

	initialTemperature=0;
	temperature=0;
}

//destructor
Node::~Node() {
}

//public methods
int Node::getNodeNr() {
	return nodeNr;
}

void Node::setNodeNr( int nr) {
	nodeNr=nr;
}

int Node::getOrigNodeNr() {
	return origNodeNr;
}

void Node::setOrigNodeNr( int nr) {
	origNodeNr=nr;
}

string Node::getNodeLabel() {
	return nodeLabel;
}

void Node::setNodeLabel( string label) {
	nodeLabel = label;
}

double Node::getTemperature() {
	return temperature;
}

void Node::setTemperature( double temp) {
	temperature=temp;
}

double Node::getInitialTemperature() {
	return initialTemperature;
}

void Node::setInitalTemperature( double temp) {
	initialTemperature=temp;
}

double Node::getX() {
	return nodeCoords[0];
}

double Node::getY() {
	return nodeCoords[1];
}

double Node::getZ() {
	return nodeCoords[2];
}

void Node::add( double x_, double y_, double z_) {
	nodeCoords[0]+=x_;
	nodeCoords[1]+=y_;
	nodeCoords[2]+=z_;
}

void Node::print() {
	cout << "Node " << getOrigNodeNr() << "(" << getNodeNr() << ")";

	for( int i=0; i<3; i++) {
		cout << "\t" << nodeCoords[i];
	}

	cout << "\t - " << getTemperature() << "" << getInitialTemperature() << endl;
}

//static methods
Node& Node::nodeBetween( Node &A, Node &B, double scalar) {
	double s1, s2;

	if( (scalar<0) || (scalar>1)) {
		scalar=0;
	}

	s1=scalar;
	s2=1-s1;

	Node N( 0, s2*A.nodeCoords[0]+s1*B.nodeCoords[0], s2*A.nodeCoords[1]+s1*B.nodeCoords[1], s2*A.nodeCoords[2]+s1*B.nodeCoords[2]);

	Node &tmpNode=N;

	return tmpNode;;
}

double Node::distanceAtoB( Node &A, Node &B) {
	int i;
	double l=0;

	Node C=AminusB( A, B);

	for( i=0; i<2; i++) {
		l+=pow( C.nodeCoords[i], 2);
	}

	return sqrt( l);
}

Node& Node::AplusB( Node &A, Node &B) {
	Node N( 0, A.nodeCoords[0]+B.nodeCoords[0], A.nodeCoords[1]+B.nodeCoords[1], A.nodeCoords[2]+B.nodeCoords[2]);

	Node &tmpNode=N;

	return tmpNode;;
}

Node& Node::AminusB( Node &A, Node &B) {
	Node N( 0, A.nodeCoords[0]-B.nodeCoords[0], A.nodeCoords[1]-B.nodeCoords[1], A.nodeCoords[2]-B.nodeCoords[2]);

	Node &tmpNode=N;

	return tmpNode;;
}

Node& Node::sTimesA( Node::Node &A, double scalar) {
	Node N( 0, scalar*A.nodeCoords[0], scalar*A.nodeCoords[1], scalar*A.nodeCoords[2]);

	Node &tmpNode=N;

	return tmpNode;
}

Node& Node::AcrossB( Node &A, Node &B) {
	double x=A.getY()*B.getZ()-A.getZ()*B.getY();

	double y=-(A.getX()*B.getZ()-A.getZ()*B.getX());

	double z=A.getX()*B.getY()-A.getY()*B.getX();

	Node N( 0, x, y, z);
	Node &tmpNode=N;

	return tmpNode;
}
