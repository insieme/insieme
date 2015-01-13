/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 * regarding third party software licenses.
 */

#pragma once 

#include <boost/utility.hpp>
#include <vector>
#include <utility>
//#include "insieme/iwir/iwir_condition_builder.h"
//#include "insieme/iwir/iwir_condition_ast.h"

namespace iwir {
namespace ast {
	//forward decls
	class NodeManager;
	class Node;
	struct Port;
	struct Link;
	struct Task;
	struct AtomicTask;
	struct BlockScope;
	struct IfTask;
	struct ForTask;
	struct ForEachTask;
	struct ParallelForTask;
	struct ParallelForEachTask;
	struct WhileTask;
	struct LoopCounter;
	struct Property;
	struct Constraint;
	struct TaskType;
	struct Type;
	struct Condition;
}

namespace condition_ast {

	struct op_or  {};
	struct op_and {};
	struct op_neq {};
	struct op_eq {};
	struct op_gt {};
	struct op_gte {};
	struct op_lt {};
	struct op_lte {};
	struct op_not {};

	struct port { 
		port() : name(), p(nullptr) {}
		port(const string& n) : name(n), p(nullptr) {}
		port(const string& n, iwir::ast::Port* p) : name(n), p(p) {}
		string name; 
		iwir::ast::Port* p;
	};

	template <typename tag> struct binop;
	template <typename tag> struct unop;

	typedef boost::variant<
		int,
		double,
		bool,
		std::string,
		port, 
		//iwir::ast::Port*, 
		boost::recursive_wrapper<binop<op_or >>,
		boost::recursive_wrapper<binop<op_and>>,
		boost::recursive_wrapper<binop<op_neq>>, 
		boost::recursive_wrapper<binop<op_eq >>, 
		boost::recursive_wrapper<binop<op_gt >>, 
		boost::recursive_wrapper<binop<op_gte>>, 
		boost::recursive_wrapper<binop<op_lt >>, 
		boost::recursive_wrapper<binop<op_lte>>, 
		boost::recursive_wrapper< unop<op_not>> 
		> ConditionExpr;

	template <typename tag> struct binop 
	{ 
		explicit binop(const ConditionExpr& l, const ConditionExpr& r) : oper1(l), oper2(r) { }
		ConditionExpr oper1, oper2; 
	};

	template <typename tag> struct unop  
	{ 
		explicit unop(const ConditionExpr& o) : oper1(o) { }
		ConditionExpr oper1; 
	};

} //condition_ast end

namespace ast {

using std::vector;
using std::string;
using std::pair;


enum NodeType { NT_Links, NT_Tasks, NT_Ports, NT_Properties, NT_Constraints, NT_Property, NT_Constraint, NT_TaskType, NT_Type, NT_Condition, NT_Port, NT_Link, NT_AtomicTask, NT_BlockScope, NT_IfTask, NT_ForTask, NT_ForEachTask, NT_ParallelForTask, NT_ParallelForEachTask, NT_WhileTask, NT_LoopCounter};

class Node : public insieme::utils::Printable {
	NodeManager* manager; 
	const NodeType nodeType;

	public:
	Node(NodeType nodeType) : nodeType(nodeType) {};
	virtual ~Node() {}
	NodeType getNodeType() const { return nodeType; }
	void setManager(NodeManager* mgr) { manager = mgr; }
	NodeManager* getManager() const { 
		assert(manager); 
		return manager; 
	}
	
	std::ostream& printTo(std::ostream& out) const { 
		return out; 
	}
};


template<typename T> 
struct Group : public Node {
	vector<T*> elements;
	Group(NodeType nodeType) : Node(nodeType) {};

	typedef typename std::vector<T*>::const_iterator const_iterator;

	const_iterator begin() const { return elements.cbegin();}
	const_iterator end() const { return elements.cend();}
};

struct Links : public Group<Link> {
	Links() : Group(NT_Links) {}
}; 
struct Tasks : public Group<Task> {
	Tasks() : Group(NT_Tasks) {}
};
enum PortsKind { PK_InputPorts=0, PK_OutputPorts, PK_LoopElements, PK_LoopPorts, PK_UnionPorts};
struct Ports : public Group<Port> {
	PortsKind portsKind;
	Ports(PortsKind portsKind) : 
		Group(NT_Ports), portsKind(portsKind) {}
};
struct Properties : public Group<Property> {
	Properties() : Group(NT_Properties) {}
};    
struct Constraints : public Group<Constraint> {
	Constraints() : Group(NT_Constraints) {}
};    

struct Property :  public Node {
	string name;
	string value;
	Property(const string& name, const string& value) : 
		Node(NT_Property), name(name), value(value) {}
};

struct Constraint : public Node {
	string name;
	string value;
	Constraint(const string& name, const string& value) : 
		Node(NT_Constraint), name(name), value(value) {}
};
struct TaskType : public Node {
	string type;
	TaskType(const string& type) : Node(NT_TaskType), type(type) {}
};
struct Type : public Node {
	string type;
	Type(const string& type) : Node(NT_Type), type(type) {}
};
struct Condition : public Node {
	iwir::condition_ast::ConditionExpr condition;
	Task* parentTask;
	Condition(const iwir::condition_ast::ConditionExpr& condition, Task* parentTask) : 
		Node(NT_Condition), condition(condition), parentTask(parentTask) {}
};

enum PortKind {PK_Basic=0, PK_LoopPort, PK_LoopElement, PK_UnionPort, PK_LoopCounter};

struct Port : public Node {
	string name;
	Type* type;
	PortKind kind;

	Task* parentTask;
	bool isInput;
	bool isOutput;

	Properties* properties;
	Constraints* constraints;

	Port(const string& name, Type* type, Task* parentTask, PortKind kind = PK_Basic) : 
		Node(NT_Port), name(name), type(type), kind(kind), parentTask(parentTask), properties(nullptr), constraints(nullptr) {};

	std::ostream& printTo(std::ostream& out) const;
	
};

struct Task : public Node {
	string name;

	Ports* inputPorts;
	Ports* outputPorts;
	Task* parentTask;
	bool isTopLevel;

	Properties* properties;
	Constraints* constraints;

	Task(const string& name, NodeType nodeType, Task* parentTask=nullptr, bool isTopLevel=false) : 
		Node(nodeType), name(name), parentTask(parentTask), isTopLevel(isTopLevel), properties(nullptr), constraints(nullptr){};

	std::ostream& printTo(std::ostream& out) const {
		out << "task[" << name << "]";
		return out;
	}
};

std::ostream& Port::printTo(std::ostream& out) const {
		out << "port[" << parentTask->name << ":" << name << "]";
		return out;
}
struct Link : public Node {
	Task* fromTask;
	Task* toTask;
	Port* from;
	Port* to;
	Task* parentTask;
	bool isDataLink;
	
	Link(Task* fromTask, Task* toTask, Task* parentTask) : 
		Node(NT_Link), fromTask(fromTask), toTask(toTask), from(nullptr), to(nullptr), parentTask(parentTask), isDataLink(false) {}

	Link(Task* fromTask, Task* toTask, Port* from, Port* to, Task* parentTask) : 
		Node(NT_Link), fromTask(fromTask), toTask(toTask), from(from), to(to), parentTask(parentTask), isDataLink(true) {}

	std::ostream& printTo(std::ostream& out) const { 
			out << "link[" << fromTask->name << "\\" << from->name<<"->"<<toTask->name << "\\" << to->name <<"]";
			return out;
		}
};

struct AtomicTask : public Task {
	TaskType* type;
	AtomicTask(const string& name, TaskType* type, Task* parentTask) : 
		Task(name, NT_AtomicTask, parentTask), type(type) {};
};

struct BlockScope : public Task {
	Tasks* body;
	Links* links;

	BlockScope(const string& name, Task* parentTask) : 
		Task(name, NT_BlockScope, parentTask) {}
};

struct IfTask : public Task { 
	Condition* condition;
	Tasks* thenBody;	
	Tasks* elseBody;	
	Links* links;
	bool hasElse;
	
	IfTask(const string& name, Task* parentTask) : 
		Task(name, NT_IfTask, parentTask), hasElse(false) {}
};

struct LoopCounter : public Node {
	Task* parentTask;
	Port* port;
	int value;
	bool hasValue;

	LoopCounter(Task* parentTask, Port* port) : 
		Node(NT_LoopCounter), parentTask(parentTask), port(port), value(0), hasValue(false) {}

	LoopCounter(Task* parentTask, int value) : 
		Node(NT_LoopCounter), parentTask(parentTask), port(nullptr), value(value), hasValue(true) {}

};

struct WhileTask : public Task {
	Ports* loopPorts;
	Ports* unionPorts;
	Condition* condition;
	Tasks* body;
	Links* links;

	WhileTask(const string& name, Task* parentTask) : 
		Task(name, NT_WhileTask, parentTask), 
		loopPorts(nullptr), 
		unionPorts(nullptr), 
		condition(nullptr), 
		body(nullptr), 
		links(nullptr) 
	{};
};

struct ForTask : public Task {
	Ports* loopPorts;
	Ports* unionPorts;
	Tasks* body;
	Links* links;
	LoopCounter* counter;
	LoopCounter* fromCounter;
	LoopCounter* toCounter;
	LoopCounter* stepCounter;
	
	ForTask(const string& name, Task* parentTask) : 
		Task(name, NT_ForTask, parentTask), 
		loopPorts(nullptr), 
		unionPorts(nullptr), 
		body(nullptr), 
		links(nullptr),
		counter(nullptr), fromCounter(nullptr), toCounter(nullptr), stepCounter(nullptr)
	{};
};

struct ParallelForTask : public Task {
	Tasks* body;
	Links* links;
	LoopCounter* counter;
	LoopCounter* fromCounter;
	LoopCounter* toCounter;
	LoopCounter* stepCounter;

	ParallelForTask(const string& name, Task* parentTask) : 
		Task(name, NT_ParallelForTask, parentTask), 
		body(nullptr), 
		links(nullptr),
		counter(nullptr), fromCounter(nullptr), toCounter(nullptr), stepCounter(nullptr)
	{};
};

struct ForEachTask : public Task {
	Ports* loopPorts;
	Ports* unionPorts;
	Ports* loopElements;
	Tasks* body;
	Links* links;
	
	ForEachTask(const string& name, Task* parentTask) : 
		Task(name, NT_ForEachTask, parentTask),
		loopPorts(nullptr), 
		unionPorts(nullptr), 
		loopElements(nullptr), 
		body(nullptr), 
		links(nullptr) 
	{};
};

struct ParallelForEachTask : public Task {
	Ports* loopElements;
	Tasks* body;
	Links* links;
	
	ParallelForEachTask(const string& name, Task* parentTask) : 
		Task(name, NT_ParallelForEachTask, parentTask),
		loopElements(nullptr), 
		body(nullptr), 
		links(nullptr) 
	{};
};

typedef std::shared_ptr<NodeManager> SharedNodeManager;

class NodeManager : private boost::noncopyable {

	vector<Node*> nodes;
	public:

	NodeManager() {}
	~NodeManager() { 
		for(auto n : nodes) { delete n; } 
	}

	template<typename T, typename ... Args>
	T* create(Args ... args) {
		T* res = new T(args...);
		res->setManager(this);
		nodes.push_back(res);

		return res;
	}
};

} // namespace ast end 
} // namespace iwir end
