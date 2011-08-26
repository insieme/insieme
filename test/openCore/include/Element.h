//
// Element.h
//----------------------------------------

#ifndef ELEMENT_H_
#define ELEMENT_H_

/** \class Element
 * \brief class storing various data and informations on an single element
 *
 * \li element number
 * \li element type
 * \li list of numbers of nodes that define this element
 * \li name of AVLSet (Elset) the element is part of
 */
struct Element {
	int elementNr;
	const char* elementType;

	const int* nodeNrsOfElement;
	size_t nodeNrsSize;
	const char* AVLSetName;
};

struct Element createElement(int elemNr, const char* elemType, const int* nodeNrs, const size_t nodeNrsSize_);

/** returns local number 0-(n-1) of node of given global number */
int getLocalElementNumberNode(struct Element* element, const int nr);
#endif /* ELEMENT_H_ */
