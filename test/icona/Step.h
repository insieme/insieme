/*
 * Step.h
 *
 *  Created on: 19.08.2010
 *      Author: clemens
 */

#ifndef STEP_H_
#define STEP_H_

struct CLoad {
	int nodeNr;
	int directionOfForce;
	double force;
};

struct BoundaryCondition {
	int nodeNr;
	int direction;
};

class Step {
public:
	//constructor + destructor
	Step();
	virtual ~Step();

	//get- + set-methods
	void setParsing( bool val);
	bool getParsing();
	void setStepName( std::string name);
	std::string getStepName();
	void setLoadFactor( double factor);
	double getLoadFactor();
	void setLoadFactorInc( double factor);
	double getLoadFactorInc();
	int getNrOfCLoads();
	void addCLoad( int nr, int direction, double force);
	std::vector<CLoad> &getCLoads();
	int getNrOfBoundaries();
	void addBoundaryCondition( int nr, int direction_);
	void addGeneralBoundaryConditions( std::vector<BoundaryCondition> &boundCond);
	std::vector<BoundaryCondition> &getBoundaryConditionsOfStep();
	void renumberNodeNrsBoundaryConditions( std::map< int, int> &nodeReferences);

private:
	bool parsing;
	std::string stepName;

	double lamInc, lamEnd;
	std::vector<CLoad> cLoads;
	std::vector<BoundaryCondition> boundaryConditions;
	std::vector<BoundaryCondition>::iterator boundaryConditionsIT;
};

#endif /* STEP_H_ */
