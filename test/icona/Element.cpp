/*
 * Element.cpp
 *
 *  Created on: 28.01.2010
 *      Author: clemens
 */

#include <string>
#include <vector>

using namespace std;

#include "config.h"
#include "Element.h"

//constructors + destructor
Element::Element() {
}

Element::Element( int elemNr, string elemType, vector<int> &nodeNrs) {
	elementNr=elemNr;
	elementType=elemType;
	nodeNrsOfElement=nodeNrs;
}

Element::~Element() {
}

//public methods
int Element::getElementNr() {
	return elementNr;
}

string Element::getElementType() {
	return elementType;
}

int Element::getNrOfNodes() {
	return nodeNrsOfElement.size();
}

int Element::getGlobalNrOfNodeNr( int nr) {
//	if(( nr<0) && ( nr >= getNrOfNodes())) {
//		nr=0;
//	}

	return nodeNrsOfElement[nr];
}

vector<int>& Element::getNodeNrsOfElement() {
	vector<int> &tmpNodes=nodeNrsOfElement;

	return tmpNodes;
}
