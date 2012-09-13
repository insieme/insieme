//
// Element.cpp
//----------------------------------------
#include <stdio.h>
#include "Element.h"

struct Element createElement(int elemNr, const char* elemType, const int* nodeNrs, const size_t nodeNrsSize_) {
	struct Element element;
	element.elementNr = elemNr;
	element.elementType = elemType;
	element.nodeNrsOfElement = nodeNrs;
	element.nodeNrsSize = nodeNrsSize_;
	return element;
}

int getLocalElementNumberNode(struct Element* element, const int nr) {
	for(size_t i = 0; i < element->nodeNrsSize; ++i)
		if(element->nodeNrsOfElement[i] == nr)
			return i;

	return -1;
}
