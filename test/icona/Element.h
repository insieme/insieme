/*
 * Element.h
 *
 *  Created on: 28.01.2010
 *      Author: clemens
 */

#ifndef ELEMENT_H_
#define ELEMENT_H_

class Element {
public:
	Element();
	Element( int elemNr, std::string elemType, std::vector<int> &nodeNrs);
	virtual ~Element();

	//public methods
	int getElementNr();
	std::string getElementType();
	int getNrOfNodes();
	int getGlobalNrOfNodeNr( int nr);
	std::vector<int> &getNodeNrsOfElement();

private:
	int elementNr;
	std::string elementType;

	std::vector<int> nodeNrsOfElement;
};

#endif /* ELEMENT_H_ */
