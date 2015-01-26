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

#include "insieme/utils/graph_utils.h"
#include "insieme/iwir/iwir_ast.h"
#include <vector>
#include <map>
#include <utility>

namespace iwir {

using namespace iwir::ast;
using namespace insieme;
using namespace std;

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
			if(name[v]->isDataLink) {
				out << "[label=" << "\"" <<  name[v]->from->name<< "\\n to \\n" << name[v]->to->name<<  "\"" << "]";
			} else {
				out << "[label=" << "\"" <<  name[v]->fromTask->name<< "\\n to \\n" << name[v]->toTask->name<<  "\"" << "]";
			}
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

} // iwir end
