/*
 * Node.h
 *
 *  Created on: 21.01.2010
 *      Author: clemens
 */

#ifndef NODE_H_
#define NODE_H_

class Node {
public:
	//constructors
	Node();
	Node( int nr, double x, double y, double z);
	Node( int nr, double coords[3]);

	//destructor
	virtual ~Node();

	//public variables
	double nodeCoords[3];

	//public methods
	int getNodeNr();
	void setNodeNr( int nr);

	int getOrigNodeNr();
	void setOrigNodeNr( int nr);

	double getTemperature();
	void setTemperature( double temp);

	double getInitialTemperature();
	void setInitalTemperature( double temp);

	std::string getNodeLabel();
	void setNodeLabel( std::string label);

	double getX();
	double getY();
	double getZ();

	void print();

	void add( double x_, double y_, double z_);

	//static methods
	static Node::Node &nodeBetween( Node::Node &A, Node::Node &B, double scalar);
	static double distanceAtoB( Node::Node &A, Node::Node &B);
	static Node::Node &AplusB( Node::Node &A, Node::Node &B);
	static Node::Node &AminusB( Node::Node &A, Node::Node &B);
	static Node::Node &sTimesA( Node::Node &A, double scalar);
	static Node::Node &AcrossB( Node::Node &A, Node::Node &B);

private:
	std::string nodeLabel;
	int nodeNr;
	int origNodeNr;

	double temperature;
	double initialTemperature;
};

#endif /* NODE_H_ */
