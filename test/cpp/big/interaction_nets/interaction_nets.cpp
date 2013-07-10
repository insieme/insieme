
#include <iostream>
#include <fstream>
#include <utility>
#include <cassert>
#include <set>
#include <string>
#include <queue>
#include <sstream>
#include <atomic>

#define MAX_PORTS 3

#ifndef N
#define N 700
#endif

using namespace std;

// some type aliases
typedef char Symbol;

class Cell;

// a type forming half a wire connection
struct Port {
	Cell* cell;
	unsigned port;
	Port(Cell* cell = 0, unsigned port = 0)
	 : cell(cell), port(port) {}

	operator Cell*() const { return cell; }
};

/**
 * An abstract base class for all cells.
 */
class Cell {
	Symbol symbol;
	unsigned numPorts;
	Port ports[MAX_PORTS];
public:
	Cell(Symbol symbol, unsigned numPorts) 
		: symbol(symbol), numPorts(numPorts) { 
		assert(numPorts > 0 && numPorts <= MAX_PORTS);
		for(int i=0; i<numPorts; i++) {
			ports[i] = Port();
		}
	}

	virtual ~Cell() {};

	Symbol getSymbol() const { return symbol; }

	unsigned getNumPorts() const { return numPorts; }

	const Port& getPrinciplePort() const {
		return getPort(0);
	}

	void setPrinciplePort(const Port& wire) {
		setPort(0, wire);	
	}

	const Port& getPort(unsigned index) const {
		assert(index < numPorts);
		return ports[index];
	}

	void setPort(unsigned index, const Port& wire) {
		assert(index < numPorts);
		ports[index] = wire;
	}
};


void link(Cell* a, unsigned portA, Cell* b, unsigned portB) {
	a->setPort(portA, Port(b, portB));
	b->setPort(portB, Port(a, portA));
}

void link(Cell* a, unsigned portA, const Port& portB) {
	link(a, portA, portB.cell, portB.port);
}

void link(const Port& portA, Cell* b, unsigned portB) {
	link(portA.cell, portA.port, b, portB);
}

void link(const Port& a, const Port& b) {
	link(a.cell, a.port, b);
}


// ------ Special Cell Nodes ---------


struct End : public Cell {
	End() : Cell('#',1) {}
};

struct Zero : public Cell {
	Zero() : Cell('0',1) {}
};

struct Succ : public Cell {
	Succ() : Cell('s',2) {}
};

struct Add : public Cell {
	Add() : Cell('+',3) {}
};

struct Mul : public Cell {
	Mul() : Cell('*',3) {}
};

struct Eraser : public Cell {
	Eraser() : Cell('e', 1) {}
};

struct Duplicator : public Cell {
	Duplicator() : Cell('d', 3) {}
};


// ------- Net Construction Operations -------

Cell* toNet(unsigned value) {

	// terminal case
	if (value == 0) return new Zero();

	// other cases
	Cell* inner = toNet(value - 1);
	Cell* succ = new Succ();
	link(succ, 1, inner, 0);
	return succ;
}

int toValue(Cell* net) {
	// compute value
	int ret = 0;
	while(net->getSymbol() != '0') {
		if(net->getSymbol() != 's') return -1;
		ret++;
		net = net->getPort(1);
	}
	return ret;
}

Port add(Port a, Port b) {
	Cell* res = new Add();
	link(res, 0, a);
	link(res, 2, b);
	return Port(res, 1);
}

Port mul(Port a, Port b) {
	Cell* res = new Mul();
	link(res, 0, a);
	link(res, 2, b);
	return Port(res, 1);
}

Cell* end(Port a) {
	Cell* res = new End();
	link(res, 0, a);
	return res;
}

// ------- Computation Operation --------

void compute(Cell* net);


// other utilities
void destroy(const Cell* net);
void plotGraph(const Cell* cell, const string& filename = "net.dot");

int main() {

	// create input network
	Cell* n = end(mul(toNet(N), toNet(N)));

	// conduct computation
	compute(n);

	int res = toValue(n->getPort(0));
	cout << "Result: " << res << "\n"; 
	cout << "Verification: " << (N*N==res?"OK":"ERR") << "\n";

	// clean up remaining network
	destroy(n);

	return 0;
}


// -----------------------------------------------------------------------


set<const Cell*> getClosure(const Cell* cell) {
	set<const Cell*> res;

	vector<const Cell*> open;
	open.push_back(cell);

	while(!open.empty()) {
		// get top element
		const Cell* cur = open.back();
		open.pop_back();

		if (!cur) continue;

		// add current cell to resulting set
		bool newElement = res.insert(cur).second;
		if (!newElement) continue;

		// add connected nodes to open set
		for(int i=0; i<cur->getNumPorts(); i++) {
			open.push_back(cur->getPort(i));
		}
	}

	return res;
}

/*
void plotGraph(const Cell* cell, const string& filename) {

	// step 0: open file
	ofstream file;
	file.open(filename);

	// step 1: get set of all cells (convex closure)
	set<const Cell*> cells = getClosure(cell);

	// step 2: print dot header
	file << "digraph test {\n";

	// step 3: print node and edge description
	for(const Cell* cur : cells) {
		// add node description
		file << "\tn" << (cur) << " [label=\"" << cur->getSymbol() << "\"];\n";

		// add ports
		for(int i=0; i<cur->getNumPorts(); i++) {
			const Cell* other = cur->getPort(i).cell;

			if (other) {
				file << "\tn" << (cur) << " -> n" << (other) 
					<< " [label=\"\"" << ((i==0)?", color=\"blue\"":"") << "];\n";
			}
		}
	}

	// step 4: finish description
	file << "}\n";

	// step 5: close file
	file.close();
}
*/

void destroy(const Cell* net) {
	typedef set<const Cell*>::const_iterator iter;

	// step 1: get all cells
	set<const Cell*> cells = getClosure(net);

	// step 2: destroy them
	for(iter it = cells.begin(); it != cells.end(); ++it) {
		delete *it;
	}
}

namespace {

	bool isCut(const Cell* a, const Cell* b) {
		return a->getPrinciplePort().cell == b && b->getPrinciplePort().cell == a;
	}

}


void compute(Cell* net) {
	typedef set<const Cell*>::const_iterator iter;

	// step 1: get all cells in the net
	set<const Cell*> cells = getClosure(net);

	// step 2: get all connected principle ports
	queue<Cell*> cuts;
	for(iter it = cells.begin(); it != cells.end(); ++it) {
		const Cell* cur = *it;
		const Port& port = cur->getPrinciplePort();
		if (cur < port.cell && isCut(cur, port.cell)) {
			cuts.push(const_cast<Cell*>(cur));
		}
	}

//std::cout << "Found " << cuts.size() << " cut(s).\n";

	// step 3: run processing
	int counter = 0;
	while(!cuts.empty()) {

//std::cout << "Queue-Length: " << cuts.size() << "\n";
	
		Cell* cut = cuts.front();
		cuts.pop();

		Cell* a = cut;
		Cell* b = cut->getPrinciplePort().cell;

		// ensure proper order
		if (a->getSymbol() < b->getSymbol()) {
			Cell* h = a; a = b; b = h;
		}
		
/*
		// debugging:
		stringstream file;
		file << "step" << counter++ << ".dot";
		plotGraph(net, file.str());
		std::cout << "Step: " << counter << " - Processing: " << a->getSymbol() << " vs. " << b->getSymbol() << "\n";
*/
		switch(a->getSymbol()) {
		case '0': {
			switch(b->getSymbol()) {
			case '+': {
				// implement the 0+ rule
				Port x = b->getPort(1);
				Port y = b->getPort(2);
				link(x, y);
				delete a;
				delete b;

				// check whether this is producing a cut
				if (isCut(x,y)) cuts.push(x);
				break;
			}
			case '*': {
				// implement s* rule
				Port x = b->getPort(1);
				Port y = b->getPort(2);

				// create a new cell
				Cell* e = new Eraser();

				// alter wirering
				link(a, 0, x);
				link(e, 0, y);
				
				// eliminate nodes
				delete b;

				// check for new cuts
				if (isCut(a,x)) cuts.push(x);
				if (isCut(e,y)) cuts.push(y);
				break;
			}
			case 'd': {
				// implement the 0+ rule
				Port x = b->getPort(1);
				Port y = b->getPort(2);

				// creat new cell
				Cell* n = new Zero();

				// update links
				link(a, 0, b, 2);
				link(n, 0, b, 1);

				// remove old cell
				delete b;

				// check whether this is producing a cut
				if (isCut(a,y)) cuts.push(y);
				if (isCut(n,x)) cuts.push(x);
				break;
			}
			default: break;
			}
			break;
		}
		case 's': {
			switch(b->getSymbol()) {
			case '+': {
				// implement the s+ rule
				Port x = a->getPort(1);
				Port y = b->getPort(1);

				// alter wirering
				link(x, b, 0);
				link(y, a, 0);
				link(a,1,b,1);

				// check for new cuts
				if (isCut(x,b)) cuts.push(x);
				if (isCut(y,a)) cuts.push(y);
				break;
			}
			case '*': {
				// implement s* rule
				Port x = a->getPort(1);
				Port y = b->getPort(1);
				Port z = b->getPort(2);

				// create new cells
				Cell* p = new Add();
				Cell* d = new Duplicator();

				// alter wirering
				link(b, 0, x);
				link(b, 1, p, 0);
				link(b, 2, d, 1);

				link(p, 1, y);
				link(p, 2, d, 2);

				link(d, 0, z);
				
				// eliminate nodes
				delete a;

				// check for new cuts
				if (isCut(x,b)) cuts.push(x);
				if (isCut(d,z)) cuts.push(d);
				break;
			}
			case 'd': {
				// implement the sd rule
				Port x = a->getPort(1);
				Port y = b->getPort(1);
				Port z = b->getPort(2);

				// crate new cells
				Cell* s = new Succ();

				// alter wirering
				link(b, 0, x);
				link(b, 1, s, 1);
				link(b, 2, a, 1);

				link(s, 0, z);
				link(a, 0, y);

				// check for new cuts
				if (isCut(x,b)) cuts.push(x);
				if (isCut(y,a)) cuts.push(y);
				if (isCut(z,s)) cuts.push(z);
				break;
			}
			case 'e': {
				// implement the sd rule
				Port x = a->getPort(1);

				// alter wirering
				link(b, 0, x);

				// delete old cell
				delete a;

				// check for new cuts
				if (isCut(x,b)) cuts.push(x);
				break;
			}
			default: break;
			}
			break;
		}
		case '+': break;
		case 'd': {
			switch(b->getSymbol()) {
			case '0': {
				// implement the 0+ rule
				Port x = a->getPort(1);
				Port y = a->getPort(2);

				// creat new cell
				Cell* n = new Zero();

				// update links
				link(b, 0, x);
				link(n, 0, y);

				// remove old cell
				delete a;

				// check whether this is producing a cut
				if (isCut(b,x)) cuts.push(x);
				if (isCut(n,y)) cuts.push(y);
				break;
			}
			default: break;
			}
			break;
		}
		case 'e': {
			switch(b->getSymbol()) {
			case '0': {
				// just delete cells
				delete a;
				delete b;
				break;
			}
			default: break;
			}
			break;
		}
		default: break;
		}

	}

/*
	// debugging:
	stringstream file;
	file << "step" << counter++ << ".dot";
	plotGraph(net, file.str());
*/
}

