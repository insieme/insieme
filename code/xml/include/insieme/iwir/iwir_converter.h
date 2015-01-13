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
#include "insieme/utils/logging.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/iwir/iwir_ast.h"
#include "insieme/iwir/iwir_builder.h"

#include "insieme/utils/graph_utils.h"
#include "insieme/utils/container_utils.h"
//Semantic checks
#include "insieme/core/checks/full_check.h"

#include "insieme/core/encoder/lists.h"
#include "insieme/iwir/iwir_extension.h"

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
using namespace insieme;
using namespace std;

class IWIRConverter {
	core::NodeManager& irMgr;
	const core::IRBuilder irBuilder;

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
		typedef map<pair<Task*, Port*>, core::VariablePtr> TaskVarMap;
		TaskVarMap taskVarMap;
		
		//TODO Decl var map				-- ( (Parent)Task* : IR_DeclStmt)
		//typedef map<Task*, list<IR_DeclStmt>> DeclVarMap;
		//TODO Decl var map				-- ( (Parent)Task* : list< (Task*, Port*))
		//typedef map<Task*, list<pair<Task*, Port*>>> DeclVarMap;
		
		//map of declared variable per task
		typedef map<Task*, list<core::StatementPtr>> DeclVarMap;
		DeclVarMap declMap;

		//TODO map of links and the linking ir stmt
		map<Link*, core::StatementPtr> linkStmtMap;


		std::ostream& printTo(std::ostream& out) const { 
			out << "ConversionContext( \n";
			out << "taskVars \t [ " << taskVarMap << "]\n"; 
			out << "decls \t [ " << declMap << "]\n";
			out << "linkStmts \t [ " << linkStmtMap << "]\n";
			out << ")\n";
			return out;
		}
	};

	typedef map<pair<Task*, Port*>, core::VariablePtr> VarMap;
	VarMap varMap;

	//mapping ir-expr to task
	typedef map<Task*, core::ExpressionPtr> TaskCache;
	TaskCache taskCache;


	//Graph 
	//	-- Vertex { Task* , Enum { begin, end, atomic} }
	//	-- Edge { Task*, Task*, Property <Port* from, Port* to> }

public:
	IWIRConverter(insieme::core::NodeManager& irMgr) : irMgr(irMgr), irBuilder(irMgr) {}

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
		Task* topLevel = ib.getTopLevel();
		convert(topLevel, context);
		VLOG(2) << context;

		//decls
		//task()

		core::StatementList bodyStmts;
		auto declMap = context.declMap[topLevel];
		bodyStmts.insert(bodyStmts.begin(), declMap.begin(), declMap.end());
		VLOG(2) << declMap;
		
		core::ExpressionPtr topLevelTask = taskCache[topLevel];
		assert(topLevelTask);
		bodyStmts.push_back(topLevelTask);

		core::ExpressionPtr topLevelTaskExpr = irBuilder.createCallExprFromBody(irBuilder.compoundStmt(bodyStmts), irBuilder.getLangBasic().getUnit(), /*lazy=*/false);

		VLOG(2) << "-- TopLevelTask --";
		dumpPretty(topLevelTaskExpr);
		VLOG(2) << "------------------";

		auto semantic = core::checks::check(topLevelTaskExpr);
		VLOG(2) << semantic;

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
			return taskOrder; 
		} 

		map<Task*, vector<Link*>> getTaskLinkMap() {
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
			auto fit = insertedTasks.find(cur);
			if(fit != insertedTasks.end()) {
				return fit->second;
			}
			
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

			if(linksInBody.empty()) {
				//we only have one task in body -> just add one vertex
				//in and out links do the rest
				//all links to body should have same target task
				//all links from body should have same origin task
				for(auto l : linksToBody) { addVertex(l->toTask, graph); }
				for(auto l : linksFromBody) { addVertex(l->fromTask, graph); }
			}
			else {
				//if tasks are "concurrent" and one task-"path" doesn't have links in body
				for(auto l : linksToBody) { addVertex(l->toTask, graph); }
				for(auto l : linksFromBody) { addVertex(l->fromTask, graph); }

				for(auto l : linksInBody) { addEdge(l, graph); }
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
	#define CONVERTER(name) void convert ## name(name* node, ConversionContext& context)

	#define CONDITION_CONVERTER(name) core::ExpressionPtr convert ## name(name* node, ConversionContext& context)
	#define CONVERT_CONDITION(condition, context) convertCondition(condition, context);


	
	#define LOOPCOUNTER_CONVERTER(name) core::ExpressionPtr convert ## name( name* node, ConversionContext& context)
	#define CONVERT_LOOPCOUNTER(loopcounter, context) convertLoopCounter(loopcounter, context);

	#define TYPE_CONVERTER(name) core::TypePtr convert ## name( name* node, ConversionContext& context)
	#define CONVERT_TYPE(type, context) convertType(type, context);

	CONVERTER(Links) {
		VLOG(2) << "links ";
		for(Link* n : node->elements) {
			convert(n, context);
		}
	}
	
	CONVERTER(Link) { 
		VLOG(2) << "link";
		VLOG(2) << node->fromTask->name << " " << node->from->name;
		VLOG(2) << node->toTask->name << " " << node->to->name;
				
		//lookup port-variable
		auto vFrom = varMap.find( {node->fromTask, node->from} );
		auto vTo = varMap.find( {node->toTask, node->to} );
		assert(vFrom != varMap.end());
		assert(vTo != varMap.end());

		convert(node->from, context);
		convert(node->to, context);

		bool isUnion = (node->to->kind == PK_UnionPort);
		bool isLoopElement = (node->from->kind == PK_LoopElement);

		//TODO generate linking statment in IR

		auto var1 = vFrom->second;
		auto var2 = vTo->second;

		//link(var1,var2) link(from:var1, to:var2);
		core::StatementPtr linkStmt;
		map<string, core::NodePtr> symbols;
		symbols["var1"] = var1;
		symbols["var2"] = var2;

		if(isUnion) {
			symbols["link"] = irBuilder.parse("lit(\"linkUnion\":('a,'b) -> unit)", symbols);
			linkStmt = irBuilder.parseStmt("link(var1,var2);", symbols);
		} else if(isLoopElement) {
			//TODO get iterator to acccess correct elemnt of LoopElements-collection
			//symbols["link"] = irBuilder.parse("lit(\"linkLoopElement\":('a,'b) -> unit)");
			symbols["link"] = irBuilder.parse("lit(\"linkLoopElement\":(ref<'a>,'b,'c) -> unit)", symbols);
			VLOG(2) << symbols;
			symbols["iterator"] = irBuilder.literal(irBuilder.getLangBasic().getInt4(), "leIterator");
			//symbols["linkLoopElement"] = irBuilder.parse("lit(\"linkLoopElement_\":(ref<'a>,'b,'c) -> unit)", symbols);
			//symbols["link"] = irBuilder.parse("(ref<'a> from,'b to, 'c iter) -> unit { linkLoopElement(from, to, iter); };", symbols);
			linkStmt = irBuilder.parseStmt("link(var1, var2, iterator);", symbols);
		} else {
			symbols["link"] = irBuilder.parse("lit(\"link\":('a,'b) -> unit)");
			linkStmt = irBuilder.parseStmt("link(var1,var2);", symbols);
			//auto linkStmt1 = irBuilder.parseStmt("var2 = var1;", symbols);
			//VLOG(2) << linkStmt1;
		}

		VLOG(2) << symbols;
		VLOG(2) << linkStmt;
		
		assert(linkStmt);
		context.linkStmtMap[node] = linkStmt;
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

		bool isUnion = false;

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
				isUnion = true;
				break;
			case PK_LoopCounter:
				VLOG(2) << "loopCounter";
				break;
			default: 
				assert(false && "Wrong PortKind");
		}

		VLOG(2) << "isInput: " << (node->isInput ? "true" : "false" );
		VLOG(2) << "isOutput: " << (node->isOutput ? "true" : "false" );

		core::TypePtr varType = CONVERT_TYPE(node->type, context);
		switch(node->kind) {
			case PK_Basic:
				varType = irBuilder.refType(varType);
				break;
			case PK_LoopPort:
				varType = irBuilder.refType(varType);
				break;
			case PK_LoopElement:
				varType = irBuilder.refType(varType);
				break;
			case PK_UnionPort:
				varType = irBuilder.refType(varType);
				break;
			case PK_LoopCounter:
				if(node->isInput) {
					varType = irBuilder.refType(varType);
				}				
				break;
			default: 
				assert(false && "Wrong PortKind");
		}
		assert(varType);
		core::VariablePtr var = irBuilder.variable(varType);

		//create IR variable for port
		if(varMap.find({node->parentTask, node}) == varMap.end()) {
			pair<Task*,Port*> key = { node->parentTask, node };
			string value = "port_" + node->parentTask->name + "\\" + node->name;

			assert(varType);
			
			context.taskVarMap.insert( {key, var});
			
			//IRVariable for Task/Port
			varMap.insert( {key, var} ) ;
			VLOG(2) << "port: " << node->parentTask->name << "\\" << node->name << " mapped to " << var;
		}
		
		//declare variables for every portkind EXCEPT OUTPUT LOOPCOUNTER --> is declared in
		//for-loop; INPUT of loopcounter (to, from, step) are "normal" declared variable

			
		core::DeclarationStmtPtr decl = nullptr;

		switch(node->kind) {
			case PK_Basic:
			case PK_LoopPort:
			case PK_LoopElement:
			case PK_UnionPort:
				decl = irBuilder.declarationStmt(var, irBuilder.getZero(var->getType()));
				break;
			case PK_LoopCounter:
				if(node->isInput) {
					decl = irBuilder.declarationStmt(var, irBuilder.getZero(var->getType()));
				}				
				//output -> to/from/step
				break;
			default: 
				assert(false && "Wrong PortKind");
		}
		if(decl) {
			VLOG(2) << decl;
			//DeclStmt for IRVariable per Port
			context.declMap[node->parentTask].push_back(decl);
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

		core::TypeList parameterTypes;
		vector<core::ExpressionPtr> args;
		for(auto port : node->inputPorts->elements) {
			auto p = varMap.find( {port->parentTask, port} );
			assert(p != varMap.end());
			if(p != varMap.end()) {
				auto var = p->second;
				parameterTypes.push_back(var->getType());
				args.push_back(var);
			}
		}
		for(auto port : node->outputPorts->elements) {
			auto p = varMap.find( {port->parentTask, port} );
			assert(p != varMap.end());
			if(p != varMap.end()) {
				auto var = p->second;
				parameterTypes.push_back(var->getType());
				args.push_back(var);
			}
		}

		core::TypePtr taskFuncTy = irBuilder.functionType(parameterTypes, irBuilder.getLangBasic().getUnit());
		map<string, core::NodePtr> symbols;
		symbols["taskFuncTy"] = taskFuncTy;
		core::ExpressionPtr taskExpr = irBuilder.parseExpr("lit(\""+node->name+"\": taskFuncTy )", symbols);
		dumpPretty(taskExpr);

		core::ExpressionPtr atomicTaskCall =  irBuilder.callExpr(taskExpr, args);
		taskCache[node] = atomicTaskCall;
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
		//	channel-links	-- between tasks
		//					--	link(channel from, list<channel> to) -> unit {
		//							auto t =  from.recv()
		//							for(auto c : to) {
		//								c.send(t)
		//							}
		//						}
		//
		//	body[parallelTasks] 
		//			{
		//		->	start:	read-links and write to taskPorts
		//			decls-task
		//			links-in-task -- read channel and write to input port
		//			taskBody
		//			finish: write-links from taskPorts
		//			links-out-task -- read output port and write to channel
		//			}
		//
		//	fillChannels -- (var, channel) -> unit { channel.send(var)}
		//	readChannels	(var, channel) -> unit { var = channel.recv()}
		//}
		//links-out

		//TODO	figure out how to map ports/vars with channels, and properly provide linkstmts...
		//		do it in port/link converter???
		//	NOTE: currently  done here, per input/output port of blockscope, and for input/ouput
		//	port of tasks

		map<string, core::NodePtr> symbols;
		core::StatementList declareChannels;
		core::StatementList releaseChannels;
		map<Port*, core::VariablePtr> portToChannel;
		map<Task*, core::StatementList> taskJob;
		for_each(node->body->begin(),node->body->end(), 
			[&](Task* task){
				core::StatementList jobBody;
				core::StatementList decls;
				core::ExpressionPtr callToTask = taskCache[task];

				core::StatementList chanLinkIn;
				core::StatementList chanLinkOut;

				for(auto d : innerContext.declMap[task]) {
					decls.push_back(d);
				}

				//declare channel for tasks inputports
				for_each(task->inputPorts->begin(), task->inputPorts->end(),
						[&](Port* ip) { 

						assert(ip);

						VLOG(2) << *ip;
						//declare and create channel -- before everything else in blockscope
						auto fit = varMap.find( {task, ip} );
						assert(fit != varMap.end());
						core::VariablePtr var = fit->second;
						auto varType = var->getType();

						//declare channel of size one and type "chanType"
						symbols["chanType"] = varType;
						core::DeclarationStmtPtr chanDecl = irBuilder.parseStmt("auto chan = channel.create(type(chanType), param(1));", symbols).as<core::DeclarationStmtPtr>();
						declareChannels.push_back(chanDecl);
						core::VariablePtr chan = chanDecl->getVariable();

						portToChannel[ip] = chan;
						symbols["chan"] = chan;
						symbols["var"] = var;

						//release channel -- when everythings done in BlockScope
						releaseChannels.push_back(irBuilder.parseStmt("channel.release(chan);", symbols));

						//fill channel links in with : var = channel.recv();
						chanLinkIn.push_back(irBuilder.parseStmt("var = *channel.recv(chan);", symbols));

						//fill channel links out with : channel.send(var );
						chanLinkOut.push_back(irBuilder.parseStmt("channel.send(chan, var);", symbols));
					}
				);
				
				//declare channel for tasks outputports
				for_each(task->outputPorts->begin(), task->outputPorts->end(),
						[&](Port* op) { 
						assert(op);

						//declare and create channel -- before everything else in blockscope
						auto fit = varMap.find( {task, op} );
						assert(fit != varMap.end());
						core::VariablePtr var = fit->second;
						auto varType = var->getType();

						//declare channel of size one and type "chanType"
						symbols["chanType"] = varType;
						core::DeclarationStmtPtr chanDecl = irBuilder.parseStmt("auto chan = channel.create(type(chanType), param(1));", symbols).as<core::DeclarationStmtPtr>();
						declareChannels.push_back(chanDecl);
						core::VariablePtr chan = chanDecl->getVariable();
						
						portToChannel[op] = chan;
						symbols["var"] = var;
						symbols["chan"] = chan;

						//release channel -- when everythings done in BlockScope
						releaseChannels.push_back(irBuilder.parseStmt("channel.release(chan);", symbols));

						//fill channel links in with : var = channel.recv();
						chanLinkIn.push_back(irBuilder.parseStmt("var = *channel.recv(chan);", symbols));

						//fill channel links out with : channel.send(var );
						chanLinkOut.push_back(irBuilder.parseStmt("channel.send(chan, var);", symbols));

					}				
				);

				//declarations for ip and op of task
				jobBody.insert(jobBody.begin(), decls.begin(), decls.end());
		
				//channel links in
				jobBody.insert(jobBody.end(), chanLinkIn.begin(), chanLinkIn.end());
				
				//task call with ip and op as argument
				jobBody.push_back(callToTask);
				
				//channel links out 
				jobBody.insert(jobBody.end(), chanLinkOut.begin(), chanLinkOut.end());

				VLOG(2) << jobBody;
				taskJob[task] = jobBody;
			}
		);

		core::StatementList readChannels;
		core::StatementList fillChannels;

		//TODO declare channels for BS_IP
		for_each(node->inputPorts->begin(), node->inputPorts->end(),
				[&](Port* ip) {
				assert(ip);

				//declare and create channel -- before everything else in blockscope
				auto fit = varMap.find( {node, ip} );
				assert(fit != varMap.end());
				core::VariablePtr var = fit->second;
				auto varType = var->getType();
				auto chanType = irBuilder.channelType(varType, irBuilder.concreteIntTypeParam(1));

				//declare channel of size one and type "chanType"
				symbols["chanType"] = varType;
				core::DeclarationStmtPtr chanDecl = irBuilder.parseStmt("auto chan = channel.create(type(chanType), param(1));", symbols).as<core::DeclarationStmtPtr>();
				declareChannels.push_back(chanDecl);
				core::VariablePtr chan = chanDecl->getVariable();
				
				portToChannel[ip] = chan;
				symbols["var"] = var;
				symbols["chan"] = chan;
				
				//release channel -- when everythings done in BlockScope
				releaseChannels.push_back(irBuilder.parseStmt("channel.release(chan);", symbols));

				//fill channel links out with : channel.send(var );
				fillChannels.push_back(irBuilder.parseStmt("channel.send(chan, var);", symbols));
			}
		);

		//TODO declare channels for BS_OP
		for_each(node->outputPorts->begin(), node->outputPorts->end(),
				[&](Port* op) {
				assert(op);

				//declare and create channel -- before everything else in blockscope
				auto fit = varMap.find( {node, op} );
				assert(fit != varMap.end());
				core::VariablePtr var = fit->second;
				auto varType = var->getType();
				
				//declare channel of size one and type "chanType"
				symbols["chanType"] = varType;
				core::DeclarationStmtPtr chanDecl = irBuilder.parseStmt("auto chan = channel.create(type(chanType), param(1));", symbols).as<core::DeclarationStmtPtr>();
				declareChannels.push_back(chanDecl);
				core::VariablePtr chan = chanDecl->getVariable();

				portToChannel[op] = chan;
				symbols["var"] = var;
				symbols["chan"] = chan;

				//release channel -- when everythings done in BlockScope
				releaseChannels.push_back(irBuilder.parseStmt("channel.release(chan);", symbols));

				//read channel links-out with : var = channel.recv();
				readChannels.push_back(irBuilder.parseStmt("var = *channel.recv(chan);", symbols));
			}
		);

		//TODO channel Links between tasks 
		core::StatementList channelLinks;

		//as input ports can be linked to several other ports, with channels we need a multiplexer
		map<Port*, list<Port*>> muxMap;
		//collect all links with same starting port
		for_each(node->links->begin(), node->links->end(),
			[&](Link* link) {
				muxMap[link->from].push_back(link->to);
			}
		);

		//creates mux function
		//NOTE: even link with only one target port is muxed currently
		//fun(chan from, chan to ...) {
		//	var t = from.recv
		//	to.send(t);
		//	to1.send(t);
		//	to2.send(t);
		//	...
		//}
		for(auto kv : muxMap) {
			core::StatementList muxBody;
			map<string, core::NodePtr> muxSymbols;

			Port* fromPort = kv.first;
			list<Port*> toPorts = kv.second;

			core::VariablePtr fromChan = portToChannel[fromPort];
			VLOG(2) << *fromPort;
			assert(fromChan);
			
			muxSymbols["from"] = fromChan;
			core::DeclarationStmtPtr tempDecl = irBuilder.parseStmt("auto temp = channel.recv(from);", muxSymbols).as<core::DeclarationStmtPtr>();
			muxBody.push_back(tempDecl);
			muxSymbols["temp"] = tempDecl->getVariable();

			for(Port* toPort : toPorts) {
				VLOG(2) << *toPort;
				core::VariablePtr toChan = portToChannel[toPort];
				assert(toChan);
				
				muxSymbols["to"] = toChan;
				core::StatementPtr sendStmt = irBuilder.parseStmt("channel.send(to, temp);", muxSymbols);
				muxBody.push_back(sendStmt);
			}
		
			core::ExpressionPtr muxer = irBuilder.createCallExprFromBody(irBuilder.compoundStmt(muxBody), irBuilder.getLangBasic().getUnit(), /*lazy=*/false);
			dumpPretty(muxer);
			channelLinks.push_back(muxer);
		}

 		//link channels in a paralle/task construct
 		core::StatementList channelLinkStmts;
		for(auto cl : channelLinks) {
			channelLinkStmts.push_back(irBuilder.parallel(cl, 1));
		}

		/*BUILD UP BLOCKSCOPE BODY*/
		core::StatementList bodyStmts;

		//declare channels (for input/output ports of blockscope, and tasks of inside of blockscope		
		bodyStmts.insert(bodyStmts.end(), declareChannels.begin(), declareChannels.end()); 
	
		//link channels between tasks
		bodyStmts.insert(bodyStmts.end(), channelLinkStmts.begin(), channelLinkStmts.end()); 

		//add all tasks as parallel
		for(auto tj : taskJob) {
			core::StatementList jobbody = tj.second;
			//put into parallel/task construct
			auto job =  irBuilder.parallel(irBuilder.compoundStmt(jobbody),1);
			bodyStmts.push_back(job); 
		}

		//fill channels
		bodyStmts.insert(bodyStmts.end(), fillChannels.begin(), fillChannels.end()); 
	
		//read channels
		bodyStmts.insert(bodyStmts.end(), readChannels.begin(), readChannels.end()); 
		
		//release channels -- no more needed
		bodyStmts.insert(bodyStmts.end(), releaseChannels.begin(), releaseChannels.end()); 
			
		//create a callExpr with the BlockScopes body
		core::StatementPtr body = irBuilder.compoundStmt(bodyStmts);
		core::ExpressionPtr bs = irBuilder.createCallExprFromBody(body, irBuilder.getLangBasic().getUnit(), /*lazy=*/false);
		dumpPretty(bs);
		taskCache[node] = bs;

		{
			VLOG(2) << "Declarations:";
			for_each(node->body->begin(),node->body->end(), 
				[&](Task* task){
					for(auto d : innerContext.declMap[task]) {
						VLOG(2) << "\t" << d;
					}
				}
			);
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
		core::StatementList decls;
		VLOG(2) << "Declarations:"; 
		if(node->hasElse) {
			for_each(node->thenBody->begin(),node->thenBody->end(), 
				[&](Task* task){
					for(auto d : thenContext.declMap[task]) {
						VLOG(2) << "\t" << d;
						decls.push_back(d);
					}
				}
			);
			 
			for_each(node->elseBody->begin(),node->elseBody->end(), 
				[&](Task* task){
					for(auto d : elseContext.declMap[task]) {
						VLOG(2) << "\t" << d;
						decls.push_back(d);
					}
				}
			);
		} else {
			for_each(node->thenBody->begin(),node->thenBody->end(), 
				[&](Task* task){
					for(auto d : thenContext.declMap[task]) {
						VLOG(2) << "\t" << d;
						decls.push_back(d);
					}
				}
			);		
		}

		//LINKS
		core::StatementList thenStmts;
		core::StatementList elseStmts;

		if(node->hasElse) {
			//Then Body
			{
				LinkCollector linkCollector(node->thenBody,node->links); 

				VLOG(2) << "Links to thenBody:";
				for(auto l : linkCollector.getLinksToBody()) { 
					core::StatementPtr linkStmt = context.linkStmtMap[l];
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << linkStmt; 
					thenStmts.push_back(linkStmt);
				}

				VLOG(2) << "Body (Links and Tasks):";
				auto taskLinkMap = linkCollector.getTaskLinkMap();
				for(auto t : linkCollector.getTaskOrder()) { 
					VLOG(2) << "\t" << *t << "(" << t << ")";

					//TODO get call to task
					auto taskStmt = taskCache[t];
					VLOG(2) << "\t" << taskStmt;
					thenStmts.push_back(taskStmt);
				
					//Links between tasks (if only one task in body -> empty taskLinkMap)
					for(auto link : taskLinkMap[t]) {
						VLOG(2) << "\t " << *link;
						auto linkStmt = context.linkStmtMap[link];
						thenStmts.push_back(linkStmt);
					}
				}
				
				VLOG(2) << "Links from thenBody:";
				for(auto l : linkCollector.getLinksFromBody()) {
					core::StatementPtr linkStmt = context.linkStmtMap[l];
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << linkStmt;
					thenStmts.push_back(linkStmt);
				}

				VLOG(2) << "Links outside thenBody:";
				for(auto l : linkCollector.getLinksOutsideBody()) {
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << context.linkStmtMap[l];
				}

				linkCollector.printToDotFile(node->name+"_then.dot");
			}
			//Else Body
			{
				LinkCollector linkCollector(node->elseBody,node->links); 

				VLOG(2) << "Links to elseBody:";
				for(auto l : linkCollector.getLinksToBody()) { 
					core::StatementPtr linkStmt = context.linkStmtMap[l];
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << linkStmt; 
					elseStmts.push_back(linkStmt);
				}

				VLOG(2) << "Links in elseBody:";
				for(auto l : linkCollector.getLinksInBody()) {
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << context.linkStmtMap[l];
				}

				VLOG(2) << "Body (Links and Tasks):";
				auto taskLinkMap = linkCollector.getTaskLinkMap();
				for(auto t : linkCollector.getTaskOrder()) { 
					VLOG(2) << "\t" << *t << "(" << t << ")";

					//TODO get call to task
					auto taskStmt = taskCache[t];
					VLOG(2) << "\t" << taskStmt;
					elseStmts.push_back(taskStmt);
				
					//Links between tasks (if only one task in body -> empty taskLinkMap)
					for(auto link : taskLinkMap[t]) {
						VLOG(2) << "\t " << *link;
						auto linkStmt = context.linkStmtMap[link];
						elseStmts.push_back(linkStmt);
					}
				}
				
				VLOG(2) << "Links from elseBody:";
				for(auto l : linkCollector.getLinksFromBody()) {
					core::StatementPtr linkStmt = context.linkStmtMap[l];
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << linkStmt;
					elseStmts.push_back(linkStmt);
				}

				VLOG(2) << "Links outside elseBody:";
				for(auto l : linkCollector.getLinksOutsideBody()) {
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << context.linkStmtMap[l];
				}

				linkCollector.printToDotFile(node->name+"_else.dot");
			}
		}
		else 
		{
			//then body
			{
				LinkCollector linkCollector(node->thenBody,node->links); 

				VLOG(2) << "Links to thenBody:";
				for(auto l : linkCollector.getLinksToBody()) { 
					core::StatementPtr linkStmt = context.linkStmtMap[l];
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << linkStmt; 
					thenStmts.push_back(linkStmt);
				}

				VLOG(2) << "Body (Links and Tasks):";
				auto taskLinkMap = linkCollector.getTaskLinkMap();
				for(auto t : linkCollector.getTaskOrder()) { 
					VLOG(2) << "\t" << *t << "(" << t << ")";

					//TODO get call to task
					auto taskStmt = taskCache[t];
					VLOG(2) << "\t" << taskStmt;
					thenStmts.push_back(taskStmt);
				
					//Links between tasks (if only one task in body -> empty taskLinkMap)
					for(auto link : taskLinkMap[t]) {
						auto linkStmt = context.linkStmtMap[link];
						VLOG(2) << "\t" << *link; 
						thenStmts.push_back(linkStmt);
					}
				}
				
				VLOG(2) << "Links from thenBody:";
				for(auto l : linkCollector.getLinksFromBody()) {
					core::StatementPtr linkStmt = context.linkStmtMap[l];
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << linkStmt;
					thenStmts.push_back(linkStmt);
				}

				VLOG(2) << "Links outside thenBody:";
				for(auto l : linkCollector.getLinksOutsideBody()) {
					VLOG(2) << "\t" << *l; 
					VLOG(2) << "\t" << context.linkStmtMap[l];
				}

				linkCollector.printToDotFile(node->name+"_then.dot");
			}
			{
				//some links are outside of thenBody, are needed because there is no elseBody
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
					core::StatementPtr linkStmt = context.linkStmtMap[l];
					VLOG(2) << "\t" << *l;
					VLOG(2) << "\t" << linkStmt;
					elseStmts.push_back(linkStmt);
				}
			}
		}
		
		//condition expression
		core::ExpressionPtr condition;
		condition = CONVERT_CONDITION(node->condition, context);

		core::CompoundStmtPtr thenBody = irBuilder.compoundStmt(thenStmts);
		core::CompoundStmtPtr elseBody = irBuilder.compoundStmt(elseStmts);
		VLOG(2) << thenBody;
		VLOG(2) << elseBody;
		VLOG(2) << condition;

		core::IfStmtPtr ifStmt = irBuilder.ifStmt(condition, thenBody, elseBody );
		VLOG(2) << ifStmt;

		core::ExpressionPtr ifTaskExpr =  irBuilder.createCallExprFromBody(ifStmt, irBuilder.getLangBasic().getUnit(), /*lazy=*/false);
		dumpPretty(ifTaskExpr);
		taskCache[node] = ifTaskExpr;
	}

	CONVERTER(WhileTask) {
		VLOG(2) << "WhileTask " << node->name;
		
		ConversionContext innerContext;
		convert(node->body, innerContext);
		VLOG(2) << innerContext;

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		//TODO loopports are outpuports - add them to outputPorts?
		convert(node->loopPorts, context);	

		//TODO unionports are outpuports - add them to outputPorts?
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

		VLOG(2) << "Declarations:";
		core::StatementList decls;
		for_each(node->body->begin(),node->body->end(), 
			[&](Task* task){
				for(auto d : innerContext.declMap[task]) {
					VLOG(2) << "\t" << d;
					decls.push_back(d);
				}
			}
		);

		core::StatementList whileBody;
		core::StatementList unionLinks;
		core::StatementList loopLinks;
		{
			LinkCollector linkCollector(node->body,node->links); 

			VLOG(2) << "Links to body:";
			for(auto l : linkCollector.getLinksToBody()) { 
				core::StatementPtr linkStmt = context.linkStmtMap[l];
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << linkStmt; 
				whileBody.push_back(linkStmt);
			}
			
			VLOG(2) << "Body (Links and Tasks):";
			auto taskLinkMap = linkCollector.getTaskLinkMap();
			for(auto t : linkCollector.getTaskOrder()) { 
				VLOG(2) << "\t" << *t;

				//TODO get call to task
				auto taskStmt = taskCache[t];
				VLOG(2) << "\t" << taskStmt;
				whileBody.push_back(taskStmt);
			
				//Links between tasks (if body contains only one task -> empty taskLinkMap)
				for(auto link : taskLinkMap[t]) {
					auto linkStmt = context.linkStmtMap[link];
					VLOG(2) << "\t " << *link;
					whileBody.push_back(linkStmt);
				}
			}

			VLOG(2) << "Links from body:";
			for(auto l : linkCollector.getLinksFromBody()) {
				core::StatementPtr linkStmt = context.linkStmtMap[l];
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << linkStmt;
				whileBody.push_back(linkStmt);
			}

			VLOG(2) << "Links outside body:";
			for(auto l : linkCollector.getLinksOutsideBody()) {
				core::StatementPtr linkStmt = context.linkStmtMap[l];
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << linkStmt;
				if(l->to->kind == PK_UnionPort) {
					//from == * && to == union
					unionLinks.push_back(linkStmt);
				} else if(l->from->kind == PK_LoopPort) { 
					//from == loop && to == * 
					loopLinks.push_back(linkStmt);
				} else {
					assert(false);
				}
			}

			//append union and loop links to whilebody
			VLOG(2) << "Links to UnionPorts:";
			for(auto l : unionLinks) { VLOG(2) << "\t" << *l;}
			VLOG(2) << "Links from LoopPorts:";
			for(auto l : loopLinks) { VLOG(2) << "\t" << *l;}

			//TODO unionLinks -- need to rewrite linkUnion(from, to) -- currently a generic "collection" type
			whileBody.insert(whileBody.end(), unionLinks.begin(), unionLinks.end());

			whileBody.insert(whileBody.end(), loopLinks.begin(), loopLinks.end());

			linkCollector.printToDotFile(node->name+".dot");
		}

		//TODO convert condition Expression
		core::ExpressionPtr condition;
		condition = CONVERT_CONDITION(node->condition, context);

		core::WhileStmtPtr whileStmt = irBuilder.whileStmt( condition, irBuilder.compoundStmt(whileBody));

		core::StatementList bodyStmts;
		bodyStmts.insert(bodyStmts.end(), decls.begin(), decls.end()); 
		bodyStmts.push_back(whileStmt);

		core::StatementPtr body = irBuilder.compoundStmt(bodyStmts);
		VLOG(2) << body;

		core::ExpressionPtr bind = irBuilder.createCallExprFromBody( body, irBuilder.getLangBasic().getUnit(), /*lazy=*/false);
		dumpPretty(bind);
		taskCache[node] = bind;
	}

	CONVERTER(ForTask) {
		VLOG(2) << "ForTask " << node->name;

		ConversionContext innerContext;
		convert(node->body, innerContext);
		VLOG(2) << innerContext;

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		//TODO loopports are outpuports - add them to outputPorts?
		convert(node->loopPorts, context);	
		//TODO unionports are outpuports - add them to outputPorts?
		convert(node->unionPorts, context);	
		
		//LoopCounter declarations are only needed locally
		VLOG(2) << "Counter";
		auto counter = CONVERT_LOOPCOUNTER(node->counter, context);	

		auto fromCounter = CONVERT_LOOPCOUNTER(node->fromCounter, context);	
		VLOG(2) << "FromCounter " << fromCounter;

		assert(node->toCounter);	
		auto toCounter = CONVERT_LOOPCOUNTER(node->toCounter, context);
		VLOG(2) << "ToCounter " << toCounter;

		auto stepCounter = CONVERT_LOOPCOUNTER(node->stepCounter, context);	
		VLOG(2) << "StepCounter " << stepCounter;
		
		convert(node->links, context);

		//TODO turn into annotations
		convert(node->properties, context);
		//TODO turn into annotations
		convert(node->constraints, context);

		core::StatementList decls;
		VLOG(2) << "Declarations:";
		for_each(node->body->begin(),node->body->end(), 
			[&](Task* task){
				for(auto d : innerContext.declMap[task]) {
					VLOG(2) << "\t" << d;
					decls.push_back(d);
				}
			}
		);
		
		core::StatementList forBodyStmts;
		core::StatementList unionLinks;
		core::StatementList loopLinks;
		core::StatementList loopCounterLinks;
		{
			LinkCollector linkCollector(node->body,node->links); 

			VLOG(2) << "Links to body:";
			for(auto l : linkCollector.getLinksToBody()) { 
				core::StatementPtr linkStmt = context.linkStmtMap[l];
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << linkStmt; 
				forBodyStmts.push_back(linkStmt);
			}
			
			VLOG(2) << "Body (Links and Tasks):";
			auto taskLinkMap = linkCollector.getTaskLinkMap();
			for(auto t : linkCollector.getTaskOrder()) { 
				VLOG(2) << "\t" << *t;

				//TODO get call to task
				auto taskStmt = taskCache[t];
				VLOG(2) << "\t" << taskStmt;
				forBodyStmts.push_back(taskStmt);
			
				//Links between tasks (if body contains only one task -> empty taskLinkMap)
				for(auto link : taskLinkMap[t]) {
					auto linkStmt = context.linkStmtMap[link];
					VLOG(2) << "\t " << *link;
					forBodyStmts.push_back(linkStmt);
				}
			}
		
			VLOG(2) << "Links from body:";
			for(auto l : linkCollector.getLinksFromBody()) {
				core::StatementPtr linkStmt = context.linkStmtMap[l];
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << linkStmt;
				forBodyStmts.push_back(linkStmt);
			}

			VLOG(2) << "Links outside body:";
			for(auto l : linkCollector.getLinksOutsideBody()) {
				core::StatementPtr linkStmt = context.linkStmtMap[l];
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << linkStmt;
				if(l->to->kind == PK_UnionPort) {
					//from == * && to == union
					unionLinks.push_back(linkStmt);
				} else if(l->from->kind == PK_LoopPort) { 
					//from == loop && to == * 
					loopLinks.push_back(linkStmt);
				} else if(l->to->kind == PK_LoopCounter) { 
					loopCounterLinks.push_back(linkStmt);
				} else {
					assert(false);
				}
			}

			//append union and loop links to forbody
			VLOG(2) << "Links to UnionPorts:";
			for(auto l : unionLinks) { VLOG(2) << "\t" << *l;}
			VLOG(2) << "Links from LoopPorts:";
			for(auto l : loopLinks) { VLOG(2) << "\t" << *l;}

			linkCollector.printToDotFile(node->name+".dot");
		}

		core::ExpressionPtr startVal = irBuilder.tryDeref(fromCounter);
		core::ExpressionPtr endVal = irBuilder.tryDeref(toCounter); 
		core::ExpressionPtr step = irBuilder.tryDeref(stepCounter);

		core::VariablePtr iterVar = counter.as<core::VariablePtr>(); //irBuilder.variable(irBuilder.getLangBasic().getInt4());

		VLOG(2) << iterVar << " " << startVal << " " << endVal << " " << step;

		forBodyStmts.insert(forBodyStmts.end(), unionLinks.begin(), unionLinks.end());
		forBodyStmts.insert(forBodyStmts.end(), loopLinks.begin(), loopLinks.end());
	
		core::CompoundStmtPtr forBody = irBuilder.compoundStmt(forBodyStmts);
		assert(forBody);

		core::ForStmtPtr forStmt = irBuilder.forStmt( iterVar, startVal, endVal, step, forBody);
		VLOG(2) << forStmt;
	
		//decls
		//
		//() => {
		//	decls-body -- decls in innerContext
		//	loop-links[loopCounters]
		//	for(var it = from ... to : step) {
		//		links-to-body
		//		body
		//		links-from-body
		//		loop-links[loopPorts]
		//		loop-links[unionPorts]
		//	}
		//}

		core::StatementList forTaskStmts;
		forTaskStmts.insert(forTaskStmts.end(), decls.begin(), decls.end());
		forTaskStmts.insert(forTaskStmts.end(), loopCounterLinks.begin(), loopCounterLinks.end());
		forTaskStmts.push_back(forStmt);

		core::CompoundStmtPtr forTaskBody = irBuilder.compoundStmt(forTaskStmts);
		VLOG(2) << forTaskBody;

		core::ExpressionPtr bind = irBuilder.createCallExprFromBody(forTaskBody,irBuilder.getLangBasic().getUnit(), /*lazy=*/false);
		dumpPretty(bind);
		taskCache[node] = bind;
	}

	CONVERTER(ParallelForTask) {
		VLOG(2) << "ParallelForTask " << node->name;

		ConversionContext innerContext;
		convert(node->body, innerContext);
		VLOG(2) << innerContext;

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		//LoopCounter declarations are only needed locally
		VLOG(2) << "Counter";
		auto counter = CONVERT_LOOPCOUNTER(node->counter, context);	

		auto fromCounter = CONVERT_LOOPCOUNTER(node->fromCounter, context);	
		VLOG(2) << "FromCounter " << fromCounter;

		assert(node->toCounter);	
		auto toCounter = CONVERT_LOOPCOUNTER(node->toCounter, context);
		VLOG(2) << "ToCounter " << toCounter;

		auto stepCounter = CONVERT_LOOPCOUNTER(node->stepCounter, context);	
		VLOG(2) << "StepCounter " << stepCounter;
		///

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

		core::StatementList decls;
		VLOG(2) << "Declarations:";
		for_each(node->body->begin(),node->body->end(), 
			[&](Task* task){
				for(auto d : innerContext.declMap[task]) {
					VLOG(2) << "\t" << d;
					decls.push_back(d);
				}
			}
		);	

		core::StatementList stmts;
		{
			LinkCollector linkCollector(node->body,node->links); 

			VLOG(2) << "To body";
			for(auto l : linkCollector.getLinksToBody()) { 
				core::StatementPtr linkStmt = context.linkStmtMap[l];
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << linkStmt; 
				stmts.push_back(linkStmt);
			}
			
			VLOG(2) << "In body";
			for(auto l : linkCollector.getLinksInBody()) {
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << context.linkStmtMap[l];
			}

			VLOG(2) << "TaskOrder";
			auto taskLinkMap = linkCollector.getTaskLinkMap();
			for(auto t : linkCollector.getTaskOrder()) { 
				VLOG(2) << "\t" << *t << "(" << t << ")";

				//TODO get call to task
				auto taskStmt = taskCache[t];
				VLOG(2) << taskStmt;
				stmts.push_back(taskStmt);
			
				//Links between tasks (if only one task in body -> empty taskLinkMap)
				for(auto link : taskLinkMap[t]) {
					auto linkStmt = context.linkStmtMap[link];
					stmts.push_back(linkStmt);
				}
			}
			
			VLOG(2) << "TaskLinkMap";
			for(auto tl : linkCollector.getTaskLinkMap()) { 
				VLOG(2) << "\tTask: " << *(tl.first);
				for(auto l : (tl.second)) {
					VLOG(2) << "\t " << *l;
				}
			}

			VLOG(2) << "From body";
			for(auto l : linkCollector.getLinksFromBody()) {
				core::StatementPtr linkStmt = context.linkStmtMap[l];
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << linkStmt;
				stmts.push_back(linkStmt);
			}

			VLOG(2) << "Outside body";
			for(auto l : linkCollector.getLinksOutsideBody()) {
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << context.linkStmtMap[l];
			}

			linkCollector.printToDotFile(node->name+".dot");
		}

		core::ExpressionPtr startVal = irBuilder.tryDeref(fromCounter);
		core::ExpressionPtr endVal = irBuilder.tryDeref(toCounter); 
		core::ExpressionPtr step = irBuilder.tryDeref(stepCounter);

		core::VariablePtr iterVar = counter.as<core::VariablePtr>(); //irBuilder.variable(irBuilder.getLangBasic().getInt4());
		VLOG(2) << iterVar << " " << startVal << " " << endVal << " " << step;

		core::CompoundStmtPtr forBody = irBuilder.compoundStmt(stmts);
		assert(forBody);

		core::ForStmtPtr forStmt = irBuilder.forStmt( iterVar, startVal, endVal, step, irBuilder.compoundStmt(forBody));
		VLOG(2) << "forStmt";
		dumpPretty(forStmt);
		
		core::ExpressionPtr pFor = irBuilder.pfor(forStmt);	
		VLOG(2) << "pFor";
		dumpPretty(pFor);
	
		core::StatementList forTaskStmts;
		forTaskStmts.insert(forTaskStmts.end(), decls.begin(), decls.end());
		forTaskStmts.push_back(pFor);

		core::CompoundStmtPtr forTaskBody = irBuilder.compoundStmt(forTaskStmts);
		VLOG(2) << forTaskBody;

		//TODO FILL WITH CORRECT STMTS
		core::ExpressionPtr parallelForTask = irBuilder.createCallExprFromBody(forTaskBody, irBuilder.getLangBasic().getUnit(), /*lazy=*/false);
		dumpPretty(parallelForTask);
		taskCache[node] = parallelForTask;
	}

	CONVERTER(ForEachTask) {
		VLOG(2) << "ForEachTask" << node->name;
		
		ConversionContext innerContext;
		convert(node->body, innerContext);
		VLOG(2) << innerContext;

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		//TODO loopports are outpuports - add them to outputPorts? 
		convert(node->loopPorts, context);	
		//TODO unionports are outpuports - add them to outputPorts?
		convert(node->unionPorts, context);	
		//TODO loopports are outpuports - add them to outputPorts?
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
		//		loop-links[loopElements] : in = loopElements[it];
		//		links-to-body
		//		body
		//		links-from-body
		//		loop-links[loopPorts]
		//		loop-links[unionPorts]
		//	}
		//}

		core::StatementList decls;
		VLOG(2) << "Declarations:";
		for_each(node->body->begin(),node->body->end(), 
			[&](Task* task){
				for(auto d : innerContext.declMap[task]) {
					VLOG(2) << "\t" << d;
					decls.push_back(d);
				}
			}
		);

		{
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

		//TODO FILL WITH CORRECT STMTS
		core::StatementList forBodyStmts;
		core::StatementList unionLinks;
		core::StatementList loopLinks;
		{
			LinkCollector linkCollector(node->body,node->links); 

			VLOG(2) << "Links to body:";
			for(auto l : linkCollector.getLinksToBody()) { 
				core::StatementPtr linkStmt = context.linkStmtMap[l];
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << linkStmt; 
				forBodyStmts.push_back(linkStmt);
			}
			
			VLOG(2) << "Body (Links and Tasks):";
			auto taskLinkMap = linkCollector.getTaskLinkMap();
			for(auto t : linkCollector.getTaskOrder()) { 
				VLOG(2) << "\t" << *t;

				//TODO get call to task
				auto taskStmt = taskCache[t];
				VLOG(2) << "\t" << taskStmt;
				forBodyStmts.push_back(taskStmt);
			
				//Links between tasks (if body contains only one task -> empty taskLinkMap)
				for(auto link : taskLinkMap[t]) {
					auto linkStmt = context.linkStmtMap[link];
					VLOG(2) << "\t " << *link;
					forBodyStmts.push_back(linkStmt);
				}
			}
		
			VLOG(2) << "Links from body:";
			for(auto l : linkCollector.getLinksFromBody()) {
				core::StatementPtr linkStmt = context.linkStmtMap[l];
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << linkStmt;
				forBodyStmts.push_back(linkStmt);
			}

			VLOG(2) << "Links outside body:";
			for(auto l : linkCollector.getLinksOutsideBody()) {
				core::StatementPtr linkStmt = context.linkStmtMap[l];
				VLOG(2) << "\t" << *l; 
				VLOG(2) << "\t" << linkStmt;
				if(l->to->kind == PK_UnionPort) {
					//from == * && to == union
					unionLinks.push_back(linkStmt);
				} else if(l->from->kind == PK_LoopPort) { 
					//from == loop && to == * 
					loopLinks.push_back(linkStmt);
				} else {
					assert(false);
				}
			}

			//append union and loop links to forbody
			VLOG(2) << "Links to UnionPorts:";
			for(auto l : unionLinks) { VLOG(2) << "\t" << *l;}
			VLOG(2) << "Links from LoopPorts:";
			for(auto l : loopLinks) { VLOG(2) << "\t" << *l;}

			linkCollector.printToDotFile(node->name+".dot");
		}

		core::ExpressionPtr startVal = irBuilder.intLit(0);
		core::ExpressionPtr endVal;
		if(node->loopElements->elements.size() == 1) {
			//we only need to iterate over one collection
			//TODO loopElements.length instead of 1
			Port* loopElementsNode = node->loopElements->elements[0];
			auto le = varMap.find( {node, loopElementsNode} );
			assert(le!=varMap.end());
			core::VariablePtr leVar = le->second;
			
			//IR: endVal = leVar.size;
			map<string, core::NodePtr> symbols;
			symbols["leVar"] = leVar;
			VLOG(2) << symbols;
			endVal = irBuilder.parseExpr("*(leVar.size)", symbols);
		} else if(node->loopElements->elements.size() > 1) {
			//we need to iterate over multiple collections
			//TODO handle multiple loopElements -> use length of "shortest" 
			//IR: findBiggestLoopElement(loopElements le1, ...) -> int<4> {
			//	...
			//	return biggestLoopElement->size;
			//}
			assert(false);
		} else {
			assert(false);
		}

		core::ExpressionPtr step = irBuilder.intLit(1); 

		core::VariablePtr iterVar = irBuilder.variable(irBuilder.getLangBasic().getInt4());
		VLOG(2) << iterVar << " " << startVal << " " << endVal << " " << step;

		forBodyStmts.insert(forBodyStmts.end(), unionLinks.begin(), unionLinks.end());
		forBodyStmts.insert(forBodyStmts.end(), loopLinks.begin(), loopLinks.end());

		core::CompoundStmtPtr forBody = irBuilder.compoundStmt(forBodyStmts);
		assert(forBody);

		//TODO replace "leIterator" literal in forBody with iterator variable
		core::LiteralPtr leIterator = irBuilder.literal(irBuilder.getLangBasic().getInt4(), "leIterator");
		forBody = core::transform::replaceAllGen(irMgr, forBody, leIterator, iterVar, /*limitScope=*/true);
		VLOG(2) << forBody;

		core::ForStmtPtr forStmt = irBuilder.forStmt( iterVar, startVal, endVal, step, forBody);
		VLOG(2) << forStmt;

		core::StatementList forTaskStmts;
		forTaskStmts.insert(forTaskStmts.end(), decls.begin(), decls.end());
		forTaskStmts.push_back(forStmt);

		core::CompoundStmtPtr forTaskBody = irBuilder.compoundStmt(forTaskStmts);
		VLOG(2) << forTaskBody;


		core::ExpressionPtr forEachTask = irBuilder.createCallExprFromBody(forTaskBody, irBuilder.getLangBasic().getUnit(), /*lazy=*/false);
		dumpPretty(forEachTask);
		taskCache[node] = forEachTask;
	}

	CONVERTER(ParallelForEachTask) {
		VLOG(2) << "ParallelForEachTask" << node->name;
		
		ConversionContext innerContext;
		convert(node->body, innerContext);
		VLOG(2) << innerContext;

		convert(node->inputPorts, context);
		convert(node->outputPorts, context);

		//TODO loopports are outpuports - add them to outputPorts?
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

		VLOG(2) << "Declarations:";
		for_each(node->body->begin(),node->body->end(), 
			[&](Task* task){
				for(auto d : innerContext.declMap[task]) {
					VLOG(2) << "\t" << d;
				}
			}
		);

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
		for(auto t : linkCollector.getTaskOrder()) { VLOG(2) << "\t" << *t; }
		
		VLOG(2) << "TaskLinkMap";
		for(auto tl : linkCollector.getTaskLinkMap()) { 
			VLOG(2) << "\tTask: " << *(tl.first);
			for(auto l : (tl.second)) {
				VLOG(2) << "\t " << *l;
			}
		}

		linkCollector.printToDotFile(node->name+".dot");

		//TODO FILL WITH CORRECT STMTS
		core::ExpressionPtr parallelForEachTask = irBuilder.createCallExprFromBody(irBuilder.compoundStmt(), irBuilder.getLangBasic().getUnit(), /*lazy=*/false);
		dumpPretty(parallelForEachTask);
		taskCache[node] = parallelForEachTask;
	}

	LOOPCOUNTER_CONVERTER(LoopCounter) { 
		core::ExpressionPtr ret;
		VLOG(2) << "loopCounter " ;
		VLOG(2) << "hasvalue: " << node->hasValue;
		if(node->hasValue) {
			VLOG(2) << "value: " << node->value;
			ret = irBuilder.intLit(node->value); 
		} else {
			convert(node->port, context);

			auto c = varMap.find( {node->parentTask, node->port} );
			assert(c != varMap.end());
			if(c != varMap.end()) {
				ret = c->second;
			}
			VLOG(2) << "port " << *(node->port) << " -- " << ret;
		}
		return ret;
	}

	CONVERTER(TaskType) { 
		VLOG(2) << "TaskType : " << node->type;
		//TODO generic type? annotation?
	}

	TYPE_CONVERTER(Type) { 
		VLOG(2) << "type";
		VLOG(2) << node->type;
		core::TypePtr retType;
	
		//TODO move to iwirBuilder?
		enum IWIRType { Integer, String, File, Bool };
		static map<string, IWIRType> typeMap = { {"integer", Integer}, {"string", String}, {"file", File}, {"boolean", Bool} };

		auto convertToIRType = [&](string type) {
			core::TypePtr ret;
			auto fit = typeMap.find(type);
			assert(fit != typeMap.end());

			switch(fit->second) {
				case IWIRType::Integer: 
					ret = irBuilder.getLangBasic().getInt4();
					break;
				case IWIRType::String:
					//TODO proper implementation
					ret = irBuilder.genericType("stringDummy");
					break;
				case IWIRType::File:
					//TODO proper implementation
					//TODO put into IRExtensions
					ret = irBuilder.genericType("fileDummy");
					break;
				case IWIRType::Bool:
					ret = irBuilder.getLangBasic().getBool();
					break;
				default:
					assert(false && "how did you get here?");
					ret = core::TypePtr();
			}
			return ret;
		};

		vector<string> typeSubs;
		boost::split(typeSubs, node->type, boost::is_any_of("/"));

		if(typeSubs.size() == 1) {
			//no collection
			retType = convertToIRType(typeSubs[0]);
		} else {
			//collection
			core::TypePtr elemTy;
			//iterate reverse
			auto it = typeSubs.rbegin();
			//last element in vector is the elementType
			string elemTypeString = *(it++);
			elemTy = convertToIRType(elemTypeString);
			assert(elemTy);

			//rest of vector have to be of collection type
			core::TypePtr collectionTy;
			for(auto end = typeSubs.rend(); it!=end;it++) {
				//TODO proper implementation
				//TODO use array or list?
				//arrayTy = irBuilder.arrayType( elemTy );
				//elemTy = arrayTy;

				map<string, core::NodePtr> symbols;
				symbols["elemTy"] =  elemTy;
				//TODO put into IRExtensions
				collectionTy = irBuilder.parseType("struct { ref<array<elemTy,1>> collection ; int<4> size; }", symbols);
				elemTy = collectionTy;
			}

			retType = collectionTy;
		}

		assert(retType);
		VLOG(2) << retType;
		return retType;
	}


	CONDITION_CONVERTER(Condition) {
		struct condition_ast_to_inspire : boost::static_visitor<core::ExpressionPtr>
		{
			Task* parentTask;
			const VarMap& varMap;
			const core::IRBuilder& irBuilder;
			condition_ast_to_inspire (Task* parentTask, const VarMap& varMap, const core::IRBuilder& irBuilder): parentTask(parentTask), varMap(varMap), irBuilder(irBuilder) {}

			core::VariablePtr lookup(Port* port) const {
				core::VariablePtr var = nullptr;
				auto p = varMap.find({parentTask, port});
				if(p != varMap.end()) {
					var = p->second;
				} 
				//VLOG(2) << var << "(" << var->getType() << ")" << " " << parentTask << " " << port;
				return var;
			};

			std::tuple<core::ExpressionPtr, core::ExpressionPtr> implicit_cast(core::ExpressionPtr lhs, core::ExpressionPtr rhs) const {
				const core::lang::BasicGenerator& gen = irBuilder.getLangBasic();
				core::TypePtr lTy = lhs->getType();
				core::TypePtr rTy = rhs->getType();
				if(*lTy == *rTy) {
					return std::make_tuple(lhs, rhs);
				}

				//string > integer > double > boolean
				//1234
				int cast = 0;

				if(gen.isString(lTy)) {cast = 1;}
				if(gen.isInt(lTy)) {cast = 2;}
				if(gen.isDouble(lTy)) {cast = 3;}
				if(gen.isBool(lTy)) {cast = 4;}

				if(gen.isString(rTy)) {cast += 10;}
				if(gen.isInt(rTy)) {cast += 20;}
				if(gen.isDouble(rTy)) {cast += 30;}
				if(gen.isBool(rTy)) {cast += 40;}

				switch(cast) {
					case 0: assert(false); break;
						//lhs = int -- rhs = string
					case 12: {
								 //rhs = stringToInt(rhs);
							auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToInt();
							rhs = irBuilder.callExpr(op,irBuilder.getIntParamLiteral(4),rhs);
							break;
							 }
						//lhs = double -- rhs = string
					case 13: {//rhs = stringToDouble(rhs);
							auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToDouble();
							rhs = irBuilder.callExpr(op,irBuilder.getIntParamLiteral(8),rhs);
							break;
							 }
						//lhs = bool -- rhs = string
					case 14: {//rhs = stringToBool(rhs);
							auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToBool();
							rhs = irBuilder.callExpr(op,rhs);
							break;
							 }

						//lhs = string-- rhs = int 
					case 21: //lhs = stringToInt(lhs);
							 break;
						//lhs = double -- rhs = int 
					case 23: //rhs = intToDouble(rhs); 
							 rhs = irBuilder.callExpr(gen.getSignedToReal(),irBuilder.getIntParamLiteral(8),rhs);
							 break;
						//lhs = bool -- rhs = int 
					case 24: // rhs = intToBool(rhs); 
							 rhs = irBuilder.callExpr(gen.getSignedToBool(),irBuilder.getIntParamLiteral(8),rhs);
							 break;

						//lhs = string -- rhs = double 
					case 31: {//lhs = stringToDouble(lhs);
							 auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToDouble();
							 lhs = irBuilder.callExpr(op,irBuilder.getIntParamLiteral(8),lhs);
							 break;
							 }
						//lhs = int -- rhs = double 
					case 32: //lhs = intToDouble(lhs); 
							 lhs = irBuilder.callExpr(gen.getSignedToReal(),irBuilder.getIntParamLiteral(8),lhs);
							 break;
						//lhs = bool -- rhs = double 
					case 34: //rhs = doubleToBool(rhs);
							 rhs = irBuilder.callExpr(gen.getRealToBool(),irBuilder.getIntParamLiteral(8),rhs);
							 break;
						
						//lhs = string -- rhs = bool 
					case 41: {//lhs = stringToBool(lhs);
							 auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToBool();
							 lhs = irBuilder.callExpr(op,lhs);
							 break;
							 }
						//lhs = int -- rhs = bool 
					case 42:  //lhs = intToBool(lhs);
							 lhs = irBuilder.callExpr(gen.getSignedToBool(), lhs);
							 break;
						//lhs = double -- rhs = bool 
					case 43: //lhs = doubleToBool(lhs);
							 lhs = irBuilder.callExpr(gen.getRealToBool(), lhs);
							 break;
				}

				return std::make_tuple(lhs, rhs);
			}

			core::ExpressionPtr cast_to_bool(core::ExpressionPtr oper) const {
				const core::lang::BasicGenerator& gen = irBuilder.getLangBasic();
				core::ExpressionPtr op = nullptr;
				core::TypePtr operTy = oper.getType();

				if(gen.isBool(operTy)) { 
					return oper;
				}

				if(gen.isString(operTy)) { op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringToBool(); }
				if(gen.isInt(operTy)) { op = gen.getSignedToBool(); }
				if(gen.isDouble(operTy)) { op = gen.getRealToBool(); }
				assert(op);
				return irBuilder.callExpr(op, oper);
			}


			core::ExpressionPtr operator()(int& v) const { return irBuilder.intLit(v); }
			core::ExpressionPtr operator()(double& v) const { return irBuilder.doubleLit(v); }
			core::ExpressionPtr operator()(bool& v) const { return irBuilder.boolLit(v); }
			core::ExpressionPtr operator()(std::string& v) const {  return irBuilder.stringLit("\"" + v + "\""); }
			core::ExpressionPtr operator()(condition_ast::port& v) const { 
				core::VariablePtr portVar = lookup(v.p);
				return irBuilder.tryDeref(portVar);
			}

			core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_and>& b) const { 
				core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
				core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
				assert(lhs);
				assert(rhs);

				lhs = cast_to_bool(lhs);
				rhs = cast_to_bool(rhs);

				return irBuilder.logicAnd(lhs, rhs);
			}

			core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_or >& b) const { 
				
				core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
				core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
				assert(lhs);
				assert(rhs);

				lhs = cast_to_bool(lhs);
				rhs = cast_to_bool(rhs);

				return irBuilder.logicOr(lhs, rhs);
			}

			core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_eq >& b) const { 
				core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
				core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
				assert(lhs);
				assert(rhs);

				if(*lhs->getType() != *rhs->getType()) {
					std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
				}
				
				if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
					auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringEq(); 
					return irBuilder.callExpr(op, lhs, rhs);
				} else {
					return irBuilder.eq(lhs, rhs);
				}
			}

			core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_neq>& b) const { 
				core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
				core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
				assert(lhs);
				assert(rhs);

				if(*lhs->getType() != *rhs->getType()) {
					std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
				}

				if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
					auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringNe(); 
					return irBuilder.callExpr(op, lhs, rhs);
				} else {
					return irBuilder.ne(lhs, rhs);
				}
			}
			
			core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_gt >& b) const { 
				core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
				core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
				assert(lhs);
				assert(rhs);

				if(*lhs->getType() != *rhs->getType()) {
					std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
				}

				if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
					auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringGt(); 
					return irBuilder.callExpr(op, lhs, rhs);
				} else { 
					return irBuilder.gt(lhs, rhs);
				}
			}
			core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_gte>& b) const { 
				core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
				core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
				assert(lhs);
				assert(rhs);

				if(*lhs->getType() != *rhs->getType()) {
					std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
				}

				if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
					auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringGe(); 
					return irBuilder.callExpr(op, lhs, rhs);
				} else { 
					return irBuilder.ge(lhs, rhs);
				}
			}
			core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_lt >& b) const { 
				core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
				core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
				assert(lhs);
				assert(rhs);

				if(*lhs->getType() != *rhs->getType()) {
					std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
				}
				
				if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
					auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringLt(); 
					return irBuilder.callExpr(op, lhs, rhs);
				} else { 
					return irBuilder.lt(lhs, rhs);
				}
			}
			core::ExpressionPtr operator()(condition_ast::binop<condition_ast::op_lte>& b) const { 
				core::ExpressionPtr lhs = boost::apply_visitor(*this, b.oper1);
				core::ExpressionPtr rhs = boost::apply_visitor(*this, b.oper2);
				assert(lhs);
				assert(rhs);

				if(*lhs->getType() != *rhs->getType()) {
					std::tie(lhs, rhs) = implicit_cast(lhs,rhs);
				}
				
				if(irBuilder.getLangBasic().isString(lhs->getType()) && irBuilder.getLangBasic().isString(rhs->getType())) { 
					auto op = irBuilder.getNodeManager().getLangExtension<core::lang::IWIRExtension>().getStringLe(); 
					return irBuilder.callExpr(op, lhs, rhs);
				} else { 
					return irBuilder.le(lhs, rhs);
				}
			}

			core::ExpressionPtr operator()(condition_ast::unop<condition_ast::op_not>& u) const {
				core::ExpressionPtr condExpr = nullptr;
				core::ExpressionPtr oper = boost::apply_visitor(*this, u.oper1); 
				assert(oper);

				oper = cast_to_bool(oper);
				condExpr = irBuilder.logicNeg(oper);
				
				return condExpr;
			}
		};

		VLOG(2) << "Condition : " << node->condition;
		core::ExpressionPtr condExpr = 
			boost::apply_visitor(condition_ast_to_inspire(node->parentTask,varMap,irBuilder), node->condition);
		dumpPretty(condExpr);
		return condExpr;
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
