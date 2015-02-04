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

#include "insieme/utils/logging.h"
#include "insieme/iwir/iwir_ast.h"
#include "insieme/utils/graph_utils.h"
#include <set>
#include <map>
#include <string>

namespace insieme {
namespace iwir {
namespace utils {
	
class TaskGraph {

	enum VertexKind {begin=0, end, atomic};
	struct Vertex { string taskName; VertexKind kind; }; 
	struct Edge { string fromPort; string toPort; };

	//Graph 
	//	-- Vertex { Task* , Enum { begin, end, atomic} }
	//	-- Edge { Task*, Task*, Property <Port* from, Port* to> }
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

	//parentTask needs begin/end entry
	set<Link*> edgeVisited;
	set<Task*> vertexVisited;
	map<Task*, VertexID> vertexBegin;
	map<Task*, VertexID> vertexEnd;
	map<Task*, VertexID> vertexAtomic;

	Graph g;
	
	public:
	void fillGraph(Task* task) {
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
					
					if(l->isDataLink) {
						if(ok) {
							g[edge].fromPort = l->from->name;
							g[edge].toPort = l->to->name;
						}
						VLOG(2) << task->name << " : link from " << f->name << " to "<< t->name;
					} else {
						if(ok) {
							g[edge].fromPort = "controlFlow";
							g[edge].toPort = "controlFlow";
						}
						VLOG(2) << task->name << " : link from " << f->name << " to "<< t->name;
					}
				}
			}
		}
	}

	void writeDotFile(const string& path) {
		VertexWriter<Graph> vw(g);
		EdgeWriter<Graph> ew(g);
		// write the dot file
		std::ofstream dotfile (path+"out.dot");
		boost::write_graphviz(dotfile, g, vw,  ew);
	}
};

} // utils end
} // iwir end
} // insieme end
