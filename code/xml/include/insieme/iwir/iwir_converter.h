/**
 * Copyright (c) 2002-2014 Distributed and Parallel Systems Group,
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
#include "insieme/utils/logging.h"
#include "insieme/core/ir_builder.h"

#include "insieme/iwir/iwir_ast.h"
#include "insieme/iwir/iwir_builder.h"

#include "insieme/utils/graph_utils.h"
//#include "boost/config.hpp"
//#include <boost/utility.hpp>
//#include <boost/graph/adjacency_list.hpp>
//#include <boost/graph/topological_sort.hpp>
//#include <boost/graph/depth_first_search.hpp>
//#include <boost/graph/dijkstra_shortest_paths.hpp>
//#include <boost/graph/visitors.hpp>
//#include <boost/graph/graphviz.hpp>


#include <utility>
#include <map>
#include <set>
#include <list>
#include <string>
#include <functional>
#include <algorithm>

namespace iwir {

using namespace iwir::ast;
using namespace std;

class IWIRConverter {
	insieme::core::NodeManager& irMgr;

	//per task with links
	struct ConversionContext : public boost::noncopyable , insieme::utils::Printable {
		//TODO BlockScope channel map	-- ((BlockScope*, Port*) : IR_Channel)
		//typedef map<pair<BlockScope*, Port*>, IR_channelType> BlockScopeChannelMap;
		//map of channels for ports used in blockscope
		typedef map<pair<BlockScope*, Port*>, string> BlockScopeChannelMap;
		BlockScopeChannelMap bsChannelMap;

		//TODO Task var map				-- ((Task*, Port*) : IR_Variable)
		//typedef map<pair<Task*, Port*>, IR_VariableType> TaskVarMap;
		//map of used variable per task/port
		typedef map<pair<Task*, Port*>, string> TaskVarMap;
		TaskVarMap taskVarMap;
		
		//TODO Decl var map				-- ( (Parent)Task* : IR_DeclStmt)
		//typedef map<Task*, list<IR_DeclStmt>> DeclVarMap;
		//TODO Decl var map				-- ( (Parent)Task* : list< (Task*, Port*))
		//typedef map<Task*, list<pair<Task*, Port*>>> DeclVarMap;
		
		//map of declared variable per task
		//TODO string should be IRVariable
		typedef map<Task*, list<string>> DeclVarMap;
		DeclVarMap declMap;

		//TODO map of links and the linking ir stmt
		map<Link*, string> linkStmtMap;

		std::ostream& printTo(std::ostream& out) const { 
			out << "ConversionContext( \n";
			out << "taskVars \t [ " << taskVarMap << "]\n"; 
			out << "decls \t [ " << declMap << "]\n";
			out << "linkStmts \t [ " << linkStmtMap << "]\n";
			out << ")\n";
			return out;
		}
	};

	typedef map<pair<Task*, Port*>, string> VarMap;
	VarMap varMap;

	//Graph 
	//	-- Vertex { Task* , Enum { begin, end, atomic} }
	//	-- Edge { Task*, Task*, Property <Port* from, Port* to> }

public:
	IWIRConverter(insieme::core::NodeManager& irMgr) : irMgr(irMgr) {}

	void buildTaskGraphInsieme(const IWIRBuilder& ib) {
		auto tasks =  ib.getTaskCache();
		auto ports =  ib.getPortCache();

		insieme::utils::graph::Graph<Task*> graph;

		for(auto t : tasks) {
			auto name = t.first;
			Task* task = t.second;

			Links* links = nullptr; 
			switch(task->getNodeType()) {
				case NT_BlockScope:
					links = static_cast<BlockScope*>(task)->links;
					break;
				case NT_IfTask:
					links = static_cast<IfTask*>(task)->links;
					break;
				case NT_WhileTask:
					links = static_cast<WhileTask*>(task)->links;
					break;
				case NT_ForTask:
					links = static_cast<ForTask*>(task)->links;
					break;
				case NT_ForEachTask:
					links = static_cast<ForEachTask*>(task)->links;
					break;
				case NT_ParallelForTask:
					links = static_cast<ParallelForTask*>(task)->links;
					break;
				case NT_ParallelForEachTask:
					links = static_cast<ParallelForEachTask*>(task)->links;
					break;
				case NT_AtomicTask:
					//has no links!
					VLOG(2) << graph.addVertex(task);
					continue;

				default:
					assert(false);
			}

			for(Link* l : links->elements) {
				Task* f = l->fromTask;
				Task* t = l->toTask;

				VLOG(2) << task->name << " : link from " << f->name << " to "<< t->name;
				VLOG(2) << graph.addVertex(f);
				VLOG(2) << graph.addVertex(t);
				VLOG(2) << graph.addEdge(f,t);
			}
		}

		graph.printGraphViz( cout, [](std::ostream& o, const Task* t) { o<<t->name;});
	}

	enum VertexKind {begin=0, end, atomic};
	struct Vertex { string taskName; VertexKind kind; }; 
	struct Edge { string fromPort; string toPort; };

	typedef boost::adjacency_list < 
		boost::listS, boost::vecS, boost::directedS, 
		Vertex, Edge
		> Graph;
	typedef Graph::vertex_descriptor VertexID;
	typedef Graph::edge_descriptor EdgeID;

	template<class Name>
	class VertexWriter {
		Name name;
		public:
			VertexWriter(Name name) : name(name) {}
			template<class VertexOrEdge>
			void operator()(std::ostream& out, const VertexOrEdge& v) const {
				switch(name[v].kind) {
					case VertexKind::begin: 
						out << "[label=" << "\"" <<  name[v].taskName << "_begin" << "\"" << "]";
						break;
					case VertexKind::end: 
						out << "[label=" << "\"" <<  name[v].taskName << "_end" << "\"" << "]";
						break;
					case VertexKind::atomic: 
						out << "[label=" << "\"" <<  name[v].taskName << "\"" <<  "]";
						break;
				}
			}
	};
	template<class Name>
	class EdgeWriter {
		Name name;
		public:
			EdgeWriter(Name name) : name(name) {}
			template<class VertexOrEdge>
			void operator()(std::ostream& out, const VertexOrEdge& v) const {
				out << "[label=" << "\"" <<  name[v].fromPort << "\\n to \\n" << name[v].toPort <<  "\"" << "]";
			}
	};

	void buildTaskGraph(const IWIRBuilder& ib) {
		auto tasks =  ib.getTaskCache();
		auto ports =  ib.getPortCache();

		//parentTask needs begin/end entry
		set<Link*> edgeVisited;
		set<Task*> vertexVisited;
		map<Task*, VertexID> vertexBegin;
		map<Task*, VertexID> vertexEnd;
		map<Task*, VertexID> vertexAtomic;

		Graph g;

		typedef map<string, Task*> TaskMap;

		function<void (Task*)> fillGraph = [&](Task* task) {
			VLOG(2) << task->name;
			Links* links = nullptr; 
			switch(task->getNodeType()) {
				case NT_BlockScope:
					
					links = static_cast<BlockScope*>(task)->links;
					break;
				case NT_IfTask:
					links = static_cast<IfTask*>(task)->links;
					break;
				case NT_WhileTask:
					links = static_cast<WhileTask*>(task)->links;
					break;
				case NT_ForTask:
					links = static_cast<ForTask*>(task)->links;
					break;
				case NT_ForEachTask:
					links = static_cast<ForEachTask*>(task)->links;
					break;
				case NT_ParallelForTask:
					links = static_cast<ParallelForTask*>(task)->links;
					break;
				case NT_ParallelForEachTask:
					links = static_cast<ParallelForEachTask*>(task)->links;
					break;
				case NT_AtomicTask:
					{
						//has no links!
						auto fit = vertexVisited.find( task);
						if(fit == vertexVisited.end()) {
							auto vid = boost::add_vertex(g);
							g[vid].taskName = task->name;
							g[vid].kind= VertexKind::atomic;
							vertexVisited.insert( task );
							vertexAtomic.insert( {task, vid} );
							VLOG(2) << "atomic task " << task->name << " added to graph";
						}
					}
					break;

				default:
					assert(false);
			}

			if(links) {
				auto fit = vertexVisited.find(task);
				if(fit == vertexVisited.end()) {
					auto vidB = boost::add_vertex(g);
					g[vidB].taskName = task->name;
					g[vidB].kind = VertexKind::begin;
					vertexVisited.insert( task );
					vertexBegin.insert( {task, vidB} );

					auto vidE = boost::add_vertex(g);
					g[vidE].taskName = task->name;
					g[vidE].kind = VertexKind::end;
					vertexEnd.insert( {task, vidE} );
					VLOG(2) << "begin task " << task->name << " added to graph";
					VLOG(2) << "end task " << task->name << " added to graph";
				}

				for(Link* l : links->elements) {
					Task* f = l->fromTask;
					Task* t = l->toTask;

					VertexID vidFrom;
					VertexID vidTo;

					VLOG(2) << "linkTask from: " << f->name << " == " << l->parentTask->name;
					VLOG(2) << "linkTask to: " << t->name << " == " << l->parentTask->name;

					if(f == l->parentTask && t == l->parentTask) {
						{
							auto fit = vertexVisited.find(f);
							if(fit == vertexVisited.end()) {
								fillGraph(f);
							} 
							{
								auto fit = vertexBegin.find(f);
								assert(fit != vertexBegin.end());
								vidFrom = fit->second;
							}
						}
						{
							auto fit = vertexVisited.find(t);
							if(fit == vertexVisited.end()) {
								fillGraph(t);
							} 
							{
								auto fit = vertexEnd.find(t);
								assert(fit != vertexEnd.end());
								vidTo = fit->second;
							}
						}
						
					} else if(f == l->parentTask && t != l->parentTask) {
						auto fit = vertexVisited.find(f);
						if(fit == vertexVisited.end()) {
							fillGraph(f);
						}
						{
							auto fit = vertexBegin.find(f);
							assert(fit != vertexBegin.end());
							vidFrom = fit->second;
						}

						if(vertexVisited.find(t) == vertexVisited.end()) {
							fillGraph(t);
						}
						if(t->getNodeType() == NT_AtomicTask) {
							vidTo = vertexAtomic.find(t)->second;
						} else {
							vidTo = vertexBegin.find(t)->second;
						}

					} else if(f != l->parentTask && t == l->parentTask) {
						if(vertexVisited.find(f) == vertexVisited.end()) {
							fillGraph(f);
						}
						if(f->getNodeType() == NT_AtomicTask) {
							vidFrom = vertexAtomic.find(f)->second;
						} else {
							vidFrom = vertexEnd.find(f)->second;
						}

						auto fit = vertexVisited.find(t);
						if(fit == vertexVisited.end()) {
							fillGraph(t);
						}
						{
							auto fit = vertexEnd.find(t);
							assert(fit != vertexEnd.end());
							vidTo= fit->second;
						}
					}  else {
						VLOG(2) << "eeeh";
						if(vertexVisited.find(f) == vertexVisited.end()) {
							fillGraph(f);
						}
						if(f->getNodeType() == NT_AtomicTask) {
							vidFrom = vertexAtomic.find(f)->second;
						} else {
							vidFrom = vertexEnd.find(f)->second;
						}

						if(vertexVisited.find(t) == vertexVisited.end()) {
							fillGraph(t);
						}
						if(t->getNodeType() == NT_AtomicTask) {
							vidTo = vertexAtomic.find(t)->second;
						} else {
							vidTo = vertexBegin.find(t)->second;
						}
					}

					if(edgeVisited.find(l) == edgeVisited.end()) {
						EdgeID edge;
						bool ok;
						boost::tie(edge, ok) =  boost::add_edge(vidFrom,vidTo, g);
						edgeVisited.insert( l );
						
						if(ok) {
							g[edge].fromPort = l->from->name;
							g[edge].toPort = l->to->name;
						}
						VLOG(2) << task->name << " : link from " << f->name << " to "<< t->name;
					}
				}
			}
		};

		/*
		for(auto t : tasks) {
			auto name = t.first;
			Task* task = t.second;
		}
		*/
		fillGraph(ib.getTopLevel());

		VertexWriter<Graph> vw(g);
		EdgeWriter<Graph> ew(g);
		// write the dot file
		std::ofstream dotfile (ib.getWFName()+"out.dot");
		boost::write_graphviz (dotfile, g, 
			//	boost::make_label_writer(boost::get(&Vertex::taskName, g))
				vw,  ew
				//boost::make_label_writer(boost::get(&Edge::fromPort, g))
			/*,
			boost::make_edge_writer(
				boost::get(&Edge::fromPort, g),
				boost::get(&Edge::toPort, g)
				)
				*/
			);
	}

	insieme::core::NodePtr convertIwirToIr(const IWIRBuilder& ib) {
		auto tasks =  ib.getTaskCache();
		auto ports =  ib.getPortCache();

		VLOG(2) << "tasks: " << tasks;
		VLOG(2) << "ports: " << ports;
	
		ConversionContext context;
		convert(ib.getTopLevel(), context);
		VLOG(2) << context;

		buildTaskGraph(ib);
		return insieme::core::NodePtr();
	}
	
private:

	class LinkCollector {
		Tasks* body;
		Links* links;

		vector<Link*> linksToBody;
		vector<Link*> linksFromBody;
		vector<Link*> linksInBody;
		vector<Link*> linksOutsideBody;
		vector<Task*> taskOrder;
		map<Task*, vector<Link*>> taskLinkMap;

		public:
		LinkCollector(Tasks* body, Links* links) : body(body), links(links) {
			collectLinksToBody();
			collectLinksFromBody();
			collectLinksInBody();

			topoSort();
		}

		vector<Link*> getLinksToBody() {
			if(linksToBody.empty()) {
				collectLinksToBody();
			}
			return linksToBody; 
		}

		vector<Link*> getLinksFromBody() { 
			if(linksFromBody.empty()) {
				collectLinksFromBody();
			} 
			return linksFromBody; 
		}
		vector<Link*> getLinksInBody() {
			if(linksInBody.empty()) {
				collectLinksInBody();
			}
			return linksInBody; 
		}
		
		vector<Link*> getLinksOutsideBody() {
			if(linksOutsideBody.empty()) {
				collectLinksOutsideBody();
			}
			return linksOutsideBody; 
		}
		
		vector<Task*> getTaskOrder() {
			if(taskOrder.empty()) {
				topoSort(); 
			} 
			return taskOrder; 
		} 

		map<Task*, vector<Link*>> getTaskLinkMap() {
			if(taskLinkMap.empty()) {
				topoSort(); 
			}
			return taskLinkMap; 
		} 

		void printToDotFile(string dotFile) {
			VertexWriter<Graph> vw(graph);
			EdgeWriter<Graph> ew(graph);
			std::ofstream dotfile (dotFile);
			boost::write_graphviz (dotfile, graph, vw, ew);
		}

		private:

		typedef boost::adjacency_list <boost::listS, boost::vecS, boost::directedS, Task*, Link*> Graph;
		Graph graph;
		typedef Graph::vertex_descriptor VertexID;
		typedef Graph::edge_descriptor EdgeID;	
		map<Task*, VertexID> insertedTasks;

		template<class Name>
		class VertexWriter {
			Name name;
			public:
			VertexWriter(Name name) : name(name) {}
			template<class VertexOrEdge>
			void operator()(std::ostream& out, const VertexOrEdge& v) const {
				out << "[label=" << "\"" <<  name[v]->name << "\"" <<  "]";
			}
		};
		template<class Name>
		class EdgeWriter{
			Name name;
			public:
			EdgeWriter(Name name) : name(name) {}
			template<class VertexOrEdge>
			void operator()(std::ostream& out, const VertexOrEdge& v) const {
				out << "[label=" << "\"" <<  name[v]->from->name<< "\\n to \\n" << name[v]->to->name<<  "\"" << "]";
			}
		};

		bool isLinkToBody(Link* link) { return (link->fromTask == link->parentTask) && (link->toTask != link->parentTask); }
		bool isLinkFromBody(Link* link) { return (link->toTask == link->parentTask) && (link->fromTask != link->parentTask); }
		
		void sortLinksToBody(Link* link) {
			if( isLinkToBody(link) && 
				(std::find(body->begin(), body->end(), link->toTask) != body->end()))
			{
				linksToBody.push_back(link);
			}
		}
		
		void sortLinksFromBody(Link* link) { 
			if( isLinkFromBody(link) && 
				(std::find(body->begin(), body->end(), link->fromTask) != body->end()))
			{
				linksFromBody.push_back(link);
			}
		}

		void sortLinksInBody(Link* link) {
			if( (std::find(body->begin(), body->end(), link->fromTask) != body->end()) &&
				(std::find(body->begin(), body->end(), link->toTask) != body->end()))
			{
				linksInBody.push_back(link);
			}
		}

		void sortLinksOutsideBody(Link* link) {
			if( (std::find(body->begin(), body->end(), link->fromTask) == body->end()) &&
				(std::find(body->begin(), body->end(), link->toTask) == body->end()))
			{
				linksOutsideBody.push_back(link);
			}
		}
		void collectLinksToBody() { for(auto it = links->begin(), end = links->end(); it!=end; it++) { sortLinksToBody(*it); } }
		void collectLinksFromBody() { for(auto it = links->begin(), end = links->end(); it!=end; it++) { sortLinksFromBody(*it); } }
		void collectLinksInBody() { for(auto it = links->begin(), end = links->end(); it!=end; it++) { sortLinksInBody(*it); } }
		void collectLinksOutsideBody() { for(auto it = links->begin(), end = links->end(); it!=end; it++) { sortLinksOutsideBody(*it); } }

		VertexID addVertex(Task* cur, Graph& g) {
			//check if task already inserted
			auto res = insertedTasks.insert( { cur, VertexID() } ) ;
			if(!res.second) {
				return res.first->second;
			}

			VertexID v = boost::add_vertex(g);
			//add vertex to graph
			g[v] = cur;
			//add task as inserted
			insertedTasks[cur] = v;

			return v;
		}

		void addEdge(Link* cur, Graph& g) {
			VertexID u = addVertex(cur->fromTask,g);
			VertexID v = addVertex(cur->toTask,g);
			EdgeID e;
			bool ok;
			boost::tie(e,ok) = boost::add_edge(u, v, g);
			if(ok) { g[e] = cur; }
		}

		void fillGraph() {
			for(auto l : linksInBody) {
				addEdge(l, graph);
			}
		}

		void topoSort() {
			fillGraph();
			typedef	boost::graph_traits<Graph>::out_edge_iterator outedge_iter;
			outedge_iter oei, oei_end;

			vector<VertexID> vertexOrder;
			topological_sort(graph, std::back_inserter(vertexOrder));

			for(auto it = vertexOrder.rbegin(), end = vertexOrder.rend(); it!=end; it++) {
				Task* task = graph[*it];
				taskOrder.push_back(task);

				boost::tie(oei, oei_end) = boost::out_edges(*it, graph);
				//check if vertex has outgoing links
				for(;oei!=oei_end; oei++) {
					Link* link = graph[*oei];
					taskLinkMap[task].push_back(link);
				}
			}
		}
	};

	void convert(Node* node, ConversionContext& context) {
		if(!node) return;

		switch(node->getNodeType()) {
			#define DISPATCH(name) case NT_ ## name: convert ## name ( static_cast<name*>(node), context); break;
			DISPATCH(Links)
			DISPATCH(Link)

			DISPATCH(Ports)
			DISPATCH(Port)

			DISPATCH(Properties)
			DISPATCH(Property)

			DISPATCH(Constraints)
			DISPATCH(Constraint)

			DISPATCH(TaskType)
			DISPATCH(Type)

			DISPATCH(Tasks)

			DISPATCH(AtomicTask)
			DISPATCH(BlockScope)
			DISPATCH(IfTask)
			DISPATCH(ForTask)
			DISPATCH(ForEachTask)
			DISPATCH(ParallelForTask)
			DISPATCH(ParallelForEachTask)
			DISPATCH(WhileTask)

			DISPATCH(Condition)
			DISPATCH(LoopCounter)
			#undef DISPATCH
		}
	}

	//TODO add CONVERT_TASKS?
	//TODO add CONVERT_PORTS?
	//TODO add CONVERT_LINKS?
	//
	#define CONVERT(name) void convert ## name( name* node)
	#define CONVERTER(name) void convert ## name( name* node, ConversionContext& context)

	CONVERTER(Links) {
		VLOG(2) << "links ";
		for(Link* n : node->elements) {
			convert(n, context);
		}
	}
	
	CONVERTER(Link) { 
		VLOG(2) << "link";
		//TODO lookup port-variable
		auto vFrom = varMap.find( {node->fromTask, node->from} );
		auto vTo = varMap.find( {node->toTask, node->to} );

		VLOG(2) << node->fromTask->name << " " << node->from->name;
		VLOG(2) << node->toTask->name << " " << node->to->name;
		
		assert(vFrom != varMap.end());
		assert(vTo != varMap.end());

		convert(node->from, context);
		convert(node->to, context);

		//TODO generate linking statment in IR
		string link = "link " + vFrom->second + " to " + vTo->second;
		VLOG(2) << link;
		context.linkStmtMap[node] = link;
	}

	CONVERTER(Ports) {
		VLOG(2) << "Ports";
		switch(node->portsKind) {
			case PK_InputPorts:
				VLOG(2) << "\tInputPorts";
				break;

			case PK_OutputPorts:
				VLOG(2) << "\tOutputPorts";
				break;

			case PK_LoopElements:
				VLOG(2) << "\tLoopElements";
				break;

			case PK_LoopPorts:
				VLOG(2) << "\tLoopPorts";
				break;

			case PK_UnionPorts:
				VLOG(2) << "\tUnionPorts";
				break;

			default: assert(false && "Wrong PortsKind");

		}
		for(Port* n : node->elements) {
			convert(n, context);
		}
	}

	CONVERTER(Port) {
		VLOG(2) << "port: " << node->parentTask->name << "\\" << node->name;
		convert(node->type, context);

		switch(node->kind) {
			case PK_Basic:
				VLOG(2) << "basic";
				break;
			case PK_LoopPort:
				VLOG(2) << "loopPort";
				break;
			case PK_LoopElement:
				VLOG(2) << "loopElement";
				break;
			case PK_UnionPort:
				VLOG(2) << "unionPort";
				break;
			default: 
				assert(false && "Wrong PortKind");
		}
		VLOG(2) << "isInput: " << (node->isInput ? "true" : "false" );
		VLOG(2) << "isOutput: " << (node->isOutput ? "true" : "false" );

		if(varMap.find({node->parentTask, node}) == varMap.end()) {
			pair<Task*,Port*> key = { node->parentTask, node };
			string value = "port_" + node->parentTask->name + "\\" + node->name;
			
			context.taskVarMap.insert( {key, value});
			
			//IRVariable for Task/Port
			varMap.insert( {key, value} ) ;

			//DeclStmt for IRVariable per Port
			context.declMap[node->parentTask].push_back(value);
		}
	}

	CONVERTER(Tasks) {
		VLOG(2) << "Tasks";
		for(Task* n : node->elements) {
			convert(n, context);
		}
	}

	CONVERTER(AtomicTask) { 
		VLOG(2) << "AtomicTask : " << node->name << ":" << node->type->type;
		convert(node->type, context);

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		//TODO turn into annotations
		convert(node->properties, context);
		//TODO turn into annotations
		convert(node->constraints, context);

		//TODO
		//literal for atomic task, with arguments from input/output ports
		//variables for ips and ops
		//generictype for tasktype?
		//
		//decls
		//links-in
		//() => {
		// task(taskType, taskLiteral, ip..., op...);
		//}
		//links-out
	}

	CONVERTER(BlockScope) { 
		VLOG(2) << "BlockScope : " << node->name;

		ConversionContext innerContext;
		convert(node->body, innerContext);
		VLOG(2) << innerContext;

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		convert(node->links, context);

		//TODO turn into annotations
		convert(node->properties, context);
		//TODO turn into annotations
		convert(node->constraints, context);

		//decls
		//links-in
		//() => {
		//	channels-from-children
		//	channels-from-links
		//	
		//	body[parallelTasks] 
		//			{
		//		->	start:	read-links and write to taskPorts
		//			decls-task
		//			links-in-task
		//			taskBody
		//			finish: write-links from taskPorts
		//			links-out-task
		//			}
		//
		//	fillChannels
		//	readChannels
		//}
		//links-out
	
		LinkCollector linkCollector(node->body,node->links); 

		VLOG(2) << "To body";
		for(auto l : linkCollector.getLinksToBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "From body";
		for(auto l : linkCollector.getLinksFromBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "In body";
		for(auto l : linkCollector.getLinksInBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "Outside body";
		for(auto l : linkCollector.getLinksOutsideBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "TaskOrder";
		for(auto t : linkCollector.getTaskOrder()) { 
			VLOG(2) << "\t" << *t;
		}
		
		VLOG(2) << "TaskLinkMap";
		for(auto tl : linkCollector.getTaskLinkMap()) { 
			VLOG(2) << "\tTask: " << *(tl.first);
			for(auto l : (tl.second)) {
				VLOG(2) << "\t " << *l;
			}
		}

		linkCollector.printToDotFile(node->name+".dot");
	}

	CONVERTER(IfTask) { 
		VLOG(2) << "IfTask : " << node->name;

		ConversionContext thenContext;
		convert(node->thenBody, thenContext);
		VLOG(2) << thenContext;

		ConversionContext elseContext;
		if(node->hasElse) {
			convert(node->elseBody, elseContext);
			VLOG(2) << elseContext;
		}

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		convert(node->condition, context);

		convert(node->links, context);

		//TODO turn into annotations
		convert(node->properties, context);
		//TODO turn into annotations
		convert(node->constraints, context);

		//decls
		//links-in
		//() => {
		//	decls-then -- decls in thenContext
		//	decls-else -- decls in elseContext
		//
		//	if( condition ) {
		//		then-links-in (all links with from:parentTask and to:Task elementOf thenBody)
		//		thenBody	(graph with tasks from thenBody and links with from:task elementOf thenBody and to: task elementOf thenBody)
		//					(topoSort graph) 
		//		then-links-out	(all links with from:Task elementOf thenBody and to:parentTask)
		//	} else {
		//		else-links-in (all links with from:parentTask and to:Task elementOf elseBody)
		//		elseBody	(graph with tasks from elseBody and links with from:task elementOf thenBody and to: task elementOf thenBody)
		//					(topoSort graph) 
		//		else-links-out	(all links with from:Task elementOf elseBody and to:parentTask )
		//	}
		//}
		//links-out
		//

		//DECLS
		VLOG(2) << "Declarations:"; 
		if(node->hasElse) {
			for_each(node->thenBody->begin(),node->thenBody->end(), 
				[&](Task* task){
					for(auto d : thenContext.declMap[task]) {
						VLOG(2) << "\t" << d;
					}
				}
			);
			 
			for_each(node->elseBody->begin(),node->elseBody->end(), 
				[&](Task* task){
					for(auto d : elseContext.declMap[task]) {
						VLOG(2) << "\t" << d;
					}
				}
			);
		} else {
			for_each(node->thenBody->begin(),node->thenBody->end(), 
				[&](Task* task){
					for(auto d : thenContext.declMap[task]) {
						VLOG(2) << "\t" << d;
					}
				}
			);		
		}

		//LINKS
		if(node->hasElse) {
			//Then Body
			{
				LinkCollector linkCollector(node->thenBody,node->links); 

				VLOG(2) << "To thenBody";
				for(auto l : linkCollector.getLinksToBody()) { 
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << context.linkStmtMap[l];
				}

				VLOG(2) << "From thenBody";
				for(auto l : linkCollector.getLinksFromBody()) {
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << context.linkStmtMap[l];
				}

				VLOG(2) << "In thenBody";
				for(auto l : linkCollector.getLinksInBody()) {
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << context.linkStmtMap[l];
				}

				VLOG(2) << "Outside thenBody";
				for(auto l : linkCollector.getLinksOutsideBody()) {
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << context.linkStmtMap[l];
				}

				VLOG(2) << "TaskOrder";
				for(auto t : linkCollector.getTaskOrder()) { 
					VLOG(2) << "\t" << *t;
				}
				
				VLOG(2) << "TaskLinkMap";
				for(auto tl : linkCollector.getTaskLinkMap()) { 
					VLOG(2) << "\tTask: " << *(tl.first);
					for(auto l : (tl.second)) {
						VLOG(2) << "\t " << *l;
					}
				}

				linkCollector.printToDotFile(node->name+"_then.dot");

			}
			//Else Body
			{
				LinkCollector linkCollector(node->elseBody,node->links); 

				VLOG(2) << "To elseBody";
				for(auto l : linkCollector.getLinksToBody()) { VLOG(2) << "\t" << *l; }
				VLOG(2) << "From elseBody";
				for(auto l : linkCollector.getLinksFromBody()) { VLOG(2) << "\t" << *l; }
				VLOG(2) << "In elseBody";
				for(auto l : linkCollector.getLinksInBody()) { VLOG(2) << "\t" << *l; }

				VLOG(2) << "Outside elseBody";
				for(auto l : linkCollector.getLinksOutsideBody()) { VLOG(2) << "\t" << *l; }

				VLOG(2) << "TaskOrder";
				for(auto t : linkCollector.getTaskOrder()) { 
					VLOG(2) << "\t" << *t;
				}
				
				VLOG(2) << "TaskLinkMap";
				for(auto tl : linkCollector.getTaskLinkMap()) { 
					VLOG(2) << "\tTask: " << *(tl.first);
					for(auto l : (tl.second)) {
						VLOG(2) << "\t " << *l;
					}
				}

				linkCollector.printToDotFile(node->name+"_else.dot");
			}
		} else {
			//then body
			{
				LinkCollector linkCollector(node->thenBody,node->links); 

				VLOG(2) << "To thenBody";
				for(auto l : linkCollector.getLinksToBody()) { VLOG(2) << "\t" << *l; }
				VLOG(2) << "From thenBody";
				for(auto l : linkCollector.getLinksFromBody()) { VLOG(2) << "\t" << *l; }
				VLOG(2) << "In thenBody";
				for(auto l : linkCollector.getLinksInBody()) { VLOG(2) << "\t" << *l; }

				VLOG(2) << "Outside thenBody";
				for(auto l : linkCollector.getLinksOutsideBody()) { VLOG(2) << "\t" << *l; }

				VLOG(2) << "TaskOrder";
				for(auto t : linkCollector.getTaskOrder()) { 
					VLOG(2) << "\t" << *t;
				}
				
				VLOG(2) << "TaskLinkMap";
				for(auto tl : linkCollector.getTaskLinkMap()) { 
					VLOG(2) << "\tTask: " << *(tl.first);
					for(auto l : (tl.second)) {
						VLOG(2) << "\t " << *l;
					}
				}

				linkCollector.printToDotFile(node->name+"_then.dot");
			}
			{
				//some links need because of no-else-body
				// no else body -> link[ifTask/in->ifTask/out] 
				//				-> link.fromTask==parentTask &&link.toTask==parentTask
				vector<Link*> noElseLinks;
				auto isNoElseLink = [&](Link* link) { 
					return (link->fromTask==link->parentTask && link->toTask==link->parentTask); 
				};
				auto collectNoElseLinks = [&](Link* link) { 
					if(isNoElseLink(link)){ 
						noElseLinks.push_back(link);
					} 
				};
				for_each(node->links->begin(), node->links->end(), collectNoElseLinks);

				VLOG(2) << "noElseLinks:";
				for(auto l : noElseLinks) {
					VLOG(2) << "\t" << *l;
				}
			}
		}
	}

	CONVERTER(WhileTask) {
		VLOG(2) << "WhileTask " << node->name;
		
		ConversionContext innerContext;
		convert(node->body, innerContext);
		VLOG(2) << innerContext;

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);
		convert(node->condition, context);

		convert(node->loopPorts, context);	
		convert(node->unionPorts, context);	

		convert(node->links, context);

		//TODO turn into annotations
		convert(node->properties, context);
		//TODO turn into annotations
		convert(node->constraints, context);


		//decls
		//
		//() => {
		//	decls-body -- decls in innerContext
		//	while( condition ) {
		//		links-to-body
		//		body
		//		links-from-body
		//		loop-links[loopPorts]
		//		loop-links[unionPorts]
		//	}
		//}

		LinkCollector linkCollector(node->body,node->links); 

		VLOG(2) << "To body";
		for(auto l : linkCollector.getLinksToBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "From body";
		for(auto l : linkCollector.getLinksFromBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "In body";
		for(auto l : linkCollector.getLinksInBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "Outside body";
		for(auto l : linkCollector.getLinksOutsideBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "TaskOrder";
		for(auto t : linkCollector.getTaskOrder()) { 
			VLOG(2) << "\t" << *t;
		}
		
		VLOG(2) << "TaskLinkMap";
		for(auto tl : linkCollector.getTaskLinkMap()) { 
			VLOG(2) << "\tTask: " << *(tl.first);
			for(auto l : (tl.second)) {
				VLOG(2) << "\t " << *l;
			}
		}

		linkCollector.printToDotFile(node->name+".dot");

	}

	CONVERTER(ForTask) { 
		VLOG(2) << "ForTask " << node->name;

		ConversionContext innerContext;
		convert(node->body, innerContext);
		VLOG(2) << innerContext;

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		convert(node->loopPorts, context);	
		convert(node->unionPorts, context);	
		
		VLOG(2) << "Counter";
		convert(node->counter, context);	

		VLOG(2) << "FromCounter";
		convert(node->fromCounter, context);	

		VLOG(2) << "ToCounter";
		assert(node->toCounter);	
		convert(node->toCounter, context);	

		VLOG(2) << "StepCounter";
		convert(node->stepCounter, context);	
		
		convert(node->links, context);

		//TODO turn into annotations
		convert(node->properties, context);
		//TODO turn into annotations
		convert(node->constraints, context);

		
		//decls
		//
		//() => {
		//	decls-body -- decls in innerContext
		//	for(var it = from ... to : step) {
		//		links-to-body
		//		body
		//		links-from-body
		//		loop-links[loopPorts]
		//		loop-links[unionPorts]
		//	}
		//}

		LinkCollector linkCollector(node->body,node->links); 

		VLOG(2) << "To body";
		for(auto l : linkCollector.getLinksToBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "From body";
		for(auto l : linkCollector.getLinksFromBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "In body";
		for(auto l : linkCollector.getLinksInBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "Outside body";
		for(auto l : linkCollector.getLinksOutsideBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "TaskOrder";
		for(auto t : linkCollector.getTaskOrder()) { 
			VLOG(2) << "\t" << *t;
		}
		
		VLOG(2) << "TaskLinkMap";
		for(auto tl : linkCollector.getTaskLinkMap()) { 
			VLOG(2) << "\tTask: " << *(tl.first);
			for(auto l : (tl.second)) {
				VLOG(2) << "\t " << *l;
			}
		}

		linkCollector.printToDotFile(node->name+".dot");
	}

	CONVERTER(ParallelForTask) { 
		VLOG(2) << "ParallelForTask " << node->name;

		ConversionContext innerContext;
		convert(node->body, innerContext);
		VLOG(2) << innerContext;

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		VLOG(2) << "Counter";
		convert(node->counter, context);	

		VLOG(2) << "FromCounter";
		convert(node->fromCounter, context);	

		VLOG(2) << "ToCounter";
		assert(node->toCounter);	
		convert(node->toCounter, context);	

		VLOG(2) << "StepCounter";
		convert(node->stepCounter, context);	

		convert(node->links, context);

		//TODO turn into annotations
		convert(node->properties, context);
		//TODO turn into annotations
		convert(node->constraints, context);

		//decls
		//
		//	() => {
		//		decls-body -- decls in innerContext
		//		pfor(TODO FILL IN) {
		//			for(var it = from ... to : step) {
		//				links-to-body
		//				body
		//				links-from-body
		//				loop-links[loopPorts]
		//				loop-links[unionPorts]
		//			}
		//		}
		//	}

		LinkCollector linkCollector(node->body,node->links); 

		VLOG(2) << "To body";
		for(auto l : linkCollector.getLinksToBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "From body";
		for(auto l : linkCollector.getLinksFromBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "In body";
		for(auto l : linkCollector.getLinksInBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "Outside body";
		for(auto l : linkCollector.getLinksOutsideBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "TaskOrder";
		for(auto t : linkCollector.getTaskOrder()) { 
			VLOG(2) << "\t" << *t;
		}
		
		VLOG(2) << "TaskLinkMap";
		for(auto tl : linkCollector.getTaskLinkMap()) { 
			VLOG(2) << "\tTask: " << *(tl.first);
			for(auto l : (tl.second)) {
				VLOG(2) << "\t " << *l;
			}
		}

		linkCollector.printToDotFile(node->name+".dot");
	}

	CONVERTER(ForEachTask) { 
		VLOG(2) << "ForEachTask" << node->name;
		
		ConversionContext innerContext;
		convert(node->body, innerContext);
		VLOG(2) << innerContext;

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		convert(node->loopPorts, context);	
		convert(node->unionPorts, context);	
		convert(node->loopElements, context);	
	
		convert(node->links, context);

		//TODO turn into annotations
		convert(node->properties, context);
		//TODO turn into annotations
		convert(node->constraints, context);

		//decls
		//
		//() => {
		//	decls-body -- decls in innerContext
		//	int lower = 0;
		//	int upper = loopElements.length;
		//	int step = 1;
		//	for(var it = lower ... upper : step) {
		//		loop-links[loopElements] : in = loopElements[in];
		//		links-to-body
		//		body
		//		links-from-body
		//		loop-links[loopPorts]
		//		loop-links[unionPorts]
		//	}
		//}

		LinkCollector linkCollector(node->body,node->links); 

		VLOG(2) << "To body";
		for(auto l : linkCollector.getLinksToBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "From body";
		for(auto l : linkCollector.getLinksFromBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "In body";
		for(auto l : linkCollector.getLinksInBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "Outside body";
		for(auto l : linkCollector.getLinksOutsideBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "TaskOrder";
		for(auto t : linkCollector.getTaskOrder()) { 
			VLOG(2) << "\t" << *t;
		}
		
		VLOG(2) << "TaskLinkMap";
		for(auto tl : linkCollector.getTaskLinkMap()) { 
			VLOG(2) << "\tTask: " << *(tl.first);
			for(auto l : (tl.second)) {
				VLOG(2) << "\t " << *l;
			}
		}

		linkCollector.printToDotFile(node->name+".dot");
	}

	CONVERTER(ParallelForEachTask) { 
		VLOG(2) << "ParallelForEachTask" << node->name;
		
		ConversionContext innerContext;
		convert(node->body, innerContext);
		VLOG(2) << innerContext;

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		convert(node->loopElements, context);	

		convert(node->links, context);
	
		//TODO turn into annotations
		convert(node->properties, context);
		//TODO turn into annotations
		convert(node->constraints, context);

		//decls
		//
		//() => {
		//	decls-body -- decls in innerContext
		//	int lower = 0;
		//	int upper = loopElements.length;
		//	int step = 1;
		//	pfor(TODO FILL IN) {
		//		for(var it = lower ... upper : step) {
		//			loop-links[loopElements] : in = loopElements[in];
		//			links-to-body
		//			body
		//			links-from-body
		//			loop-links[loopPorts]
		//			loop-links[unionPorts]
		//		}
		//	}
		//}

		LinkCollector linkCollector(node->body,node->links); 

		VLOG(2) << "To body";
		for(auto l : linkCollector.getLinksToBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "From body";
		for(auto l : linkCollector.getLinksFromBody()) { VLOG(2) << "\t" << *l; }
		VLOG(2) << "In body";
		for(auto l : linkCollector.getLinksInBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "Outside body";
		for(auto l : linkCollector.getLinksOutsideBody()) { VLOG(2) << "\t" << *l; }

		VLOG(2) << "TaskOrder";
		for(auto t : linkCollector.getTaskOrder()) { 
			VLOG(2) << "\t" << *t;
		}
		
		VLOG(2) << "TaskLinkMap";
		for(auto tl : linkCollector.getTaskLinkMap()) { 
			VLOG(2) << "\tTask: " << *(tl.first);
			for(auto l : (tl.second)) {
				VLOG(2) << "\t " << *l;
			}
		}

		linkCollector.printToDotFile(node->name+".dot");

	}

	CONVERTER(LoopCounter) { 
		VLOG(2) << "loopCounter " ;
		VLOG(2) << "hasvalue: " << node->hasValue;
		if(node->hasValue) {
			VLOG(2) << "value: " << node->value;
		} else {
			VLOG(2) << "port";
			convert(node->port, context);
		}
	}

	CONVERTER(TaskType) { 
		VLOG(2) << "TaskType : " << node->type;
	}

	CONVERTER(Type) { 
		VLOG(2) << "type";
		VLOG(2) << node->type;
	}

	CONVERTER(Condition) {
		VLOG(2) << "Condition : " << node->condition;
	}

	CONVERTER(Properties) {
		//TODO turn into annotations
		VLOG(2) << "Properties";
		for(Property* n : node->elements) {
			convert(n, context);
		}
	}
	CONVERTER(Constraints) {
		//TODO turn into annotations
		VLOG(2) << "Constraints";
		for(Constraint* n : node->elements) {
			convert(n, context);
		}
	}
	CONVERTER(Property) { 
		//TODO turn into annotations
		VLOG(2) << "Property: " << node->name << ":" << node->value;
	}
	CONVERTER(Constraint) { 
		//TODO turn into annotations
		VLOG(2) << "Constraint : " << node->name << ":" << node->value;
	}
	#undef CONVERTER
};

} // namespace iwir end
