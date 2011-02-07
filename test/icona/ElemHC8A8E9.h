/*
 * ElemHC8A8E9.h
 *
 *  Created on: 25.06.2010
 *      Author: clemens
 */

#ifndef ELEMHC8A8E9_H_
#define ELEMHC8A8E9_H_

#include "petsc.h"

struct elData {
	double kte[24][24];
	double rhs[24];
};

class ElemHC8A8E9 {
public:
	ElemHC8A8E9();
	ElemHC8A8E9( Element::Element &element, std::vector<Node::Node> &nodes,  Vec &du_, Vec &ut_,Material::Material &material, std::map< int, elData> &allKte);
	virtual ~ElemHC8A8E9();

	double rhs[24];	//arma::Col<double> Fine;		//better static???
	double kte[24][24];	//arma::mat Kte[24][24];
	double energy;
};

#endif /* ELEMHC8A8E9_H_ */
