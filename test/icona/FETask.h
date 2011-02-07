/*
 * FETask.h
 *
 * FiniteElementTask
 *
 *  Created on: 2.08.2010
 *      Author: clemens
 */

#ifndef FETASK_H_
#define FETASK_H_

#include "petsc.h"

//----------------------------------------
class FETask {
public:
	//constructors
	FETask();
	virtual ~FETask();

	//public methods

	//get- set-methods
	void setLogFileName( std::string lfName);
	std::string getLogFileName();
	void setStartTime( time_t start_);
	time_t getStartTime();
	void setInterimTime( time_t interim_);
	time_t getInterimTime();
	void setHeading( std::string text);
	std::string getHeading();
	int getNrOfNodes();
	int getNrOfNsets();
	void setDOFPerNode( int dof);
	int getDOFPerNode();
	int getNrOfElements();
	void setNrOfNodesPerElement( int nr);
	int getNrOfNodesPerElement();
	double getLambda();
	void setMaxIterations( int nr);
	int getMaxIterations();
	void setUTol( double val);
	double getUTol();
	void setFTol( double val);
	double getFTol();
	void setNrOfMaxIterations( int nr);
	int getNrOfMaxIterations();
	int getNrOfMaterials();
	int getNrOfElsets();
	int getNrOfSteps();
	int getNrOfSolidSections();
	int getNrOfInitialTemperatureNodes();
	void setInitialTemperatureNodes( std::map< int, double> &initialTemps);
	std::vector<int>& getNset( std::string name);
	Step::Step& getStepNr( int nr);
	int getNrOfBoundaries();
	double getMaxX();
	double getMinX();
	double getMaxY();
	double getMinY();
	double getMaxZ();
	double getMinZ();
	std::vector< Node::Node>& getNodes();
	std::vector< Node::Node>& getNodesU();
	std::map< int, Element::Element>& getElements();
	bool getRenumbered();
	bool getSuccess();
	double getLoadFactor();
	double getLoadFactorInc();
	std::string getStepName();

	int getOmpNrThreads();
	void setOmpNrThreads( int nr);

	//add-methods
	int addNode( int nodeNr, double x, double y, double z);
	int addNset( std::string &nsetName, std::vector< int> &nodeNrsOfNset);
	int addElement( int nodeNr, std::string &elementType, std::vector< int> &nodeNrsOfElement);
	int addElset( std::string &elsetName, std::vector< int> &nodeNrsOfElset);
	int addMaterial( Material::Material material);
	int addStep( Step::Step analysisStep);
	int addSolidSection( std::string elsetName, std::string materialName);
	void addBoundaryCondition( int nr, int direction);

	//misc
	void applyBoundaryConditions( Mat &Kt_, Vec &f_);
	void applyCLoads( Vec &f_);
	void assembleStiffnessMatrix( Vec &du_, Vec &ut_, Mat &Kt, Vec &fin);
	void mergeBoundaryConditions();

	void renumberNodeNrsElements();
	void setInitialTemperature();
	void renumberNodeNrsCLoads( std::vector<CLoad> &cloadsOfStep);
	void appendToLogFile( std::string message);

	//static
	static std::string intToStr( int nr);
	static std::string doubleToStr( double nr);

	PetscInt *rows;
	PetscScalar *values;

private:
	bool renumbered;
	bool success;
	std::string lfPathName;

	time_t rawTimeStart;
	time_t rawTimeInterim;
	time_t rawTimeEnd;

	//modell-data
	std::string heading;							//job-name
	std::vector< Node::Node> nodes;
	std::vector< Node::Node> nodesU;
	std::vector< Node::Node>::iterator nodesIT;
	std::map< int, int> nodeReferences;
	std::map< int, int>::iterator nodeReferencesIT;
	std::map< std::string, std::vector<int> > nsets;
	std::map< std::string, std::vector<int> >::iterator nsetsIT;
	std::map< int, Element::Element> elements;
	std::map< int, Element::Element>::iterator elementsIT;
	std::map< std::string, Material::Material> materials;
	std::map< std::string, Material::Material>::iterator materialsIT;
	std::map< std::string, std::vector<int> > elsets;
	std::map< std::string, std::vector<int> >::iterator elsetsIT;
	std::map< std::string, Step::Step> steps;
	std::map< std::string, Step::Step>::iterator stepsIT;
	std::map< std::string, std::string> solidSections;
	std::map< std::string, std::string>::iterator solidSectionsIT;
	std::vector<BoundaryCondition> boundaryConditions;
	std::map< int, double> initialNodeTemperatures;
	std::map< int, double>::iterator initialNodeTemperaturesIT;

	double minX, maxX, minY, maxY, minZ, maxZ;

	//analysis-vars,
	int numStep;				//number of steps
	int numNode;				//number of nodes
	int numElement;				//number if elements
	int numDOFN;				//number of DOF per node
	int numDOF;					//number of DOF
	int numNodeE;				//number of nodes per element
	double lamInc;				//load increment
	double lamEnd;				//maximum load
	double Lambda;				//load
	int maxIter;				//maximum iterations
	double Utol;				//displacement tolerance
	double Ftol;				//force tolerance

	int newtonIterStatus;		//status of iteration
	int iterations;				//number of iterations

	int nrCPUs;					//number of CPUs used (OpenMP)

	//private methods

};

#endif /* FETASK_H_ */
