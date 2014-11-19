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
#include <xercesc/dom/DOM.hpp>

#include "insieme/xml/xml_utils.h"
#include "insieme/xml/xsd_config.h"
#include "insieme/utils/logging.h"

#include "insieme/iwir/iwir_ast.h"

#include <boost/algorithm/string.hpp>

#include <memory>
#include <string>
#include <map>

namespace iwir {

using namespace iwir::ast;
using namespace insieme::utils::log;
using namespace insieme::xml;
using namespace std;

XERCES_CPP_NAMESPACE_USE

class IWIRBuilder {
	public:
	//map from taskName to Task node
	typedef map<string, Task*> TaskMap;
	//map from pair(taskName:portName) to Port node
	typedef map<pair<string, string>, Port*> PortMap;

	private: 
	SharedNodeManager mgr;
	Task* topLevel;
	string wfName;

	//map from taskName to Task node
	TaskMap taskCache;
	//map from pair(taskName:portName) to Port node
	PortMap portCache;

	class Context {
		Task* parentTask;
		public: 
		Context(Task* parentTask=nullptr) : parentTask(parentTask) {}
		Task* getParentTask() const { return parentTask; }
	};

	public:
	IWIRBuilder() : mgr(std::make_shared<NodeManager>()), topLevel(nullptr) {}

	Task* getTopLevel() const { return topLevel; }
	string getWFName() const { return wfName; }
	const TaskMap& getTaskCache() const { return taskCache; }
	const PortMap& getPortCache() const { return portCache; }

	#define DISPATCH(NODETYPE) if(nodeName == #NODETYPE) { result = handle_ ## NODETYPE(ctx, elem); }
	#define DISPATCH2HANDLER(NODETYPE, HANDLER) if(nodeName == #NODETYPE) { result = handle_ ## HANDLER(ctx, elem); }

	Task* handle_task(Context& ctx, const XmlElement& e) {
		string nodeName = e.getName();
		VLOG(2) << nodeName;
		Task* result = nullptr;
		const XmlElement& elem = e;

		//dispatch2handler(xml-taskSpecifier, taskHandler)
		DISPATCH2HANDLER(task,atomic);
		DISPATCH2HANDLER(if,if);
		DISPATCH2HANDLER(blockScope,blockScope);
		DISPATCH2HANDLER(while,while);
		DISPATCH2HANDLER(for,for);
		DISPATCH2HANDLER(forEach,forEach);
		DISPATCH2HANDLER(parallelFor,parallelFor);
		DISPATCH2HANDLER(parallelForEach,parallelForEach);

		VLOG(2) << result->name << " " << nodeName;
		assert(result && "no task was created");

			return result;
	}

	Tasks* handle_tasks(Context& ctx, const XmlElement& e) {
		Tasks* tasks = mgr->create<Tasks>();

		for(auto c : e.getChildren()) {
			tasks->elements.push_back(handle_task(ctx, c));
		}
		return tasks;
	}

	AtomicTask* handle_atomic(Context& ctx, const XmlElement& e) {
		AtomicTask* atomic = nullptr;

		string taskNameStr = e.getAttr("name");
		string taskTypeStr = e.getAttr("tasktype");
		TaskType* taskType = mgr->create<TaskType>(taskTypeStr);

		VLOG(2) << "atomicTask : " << taskNameStr << ":" << taskTypeStr;
		atomic = mgr->create<AtomicTask>(taskNameStr, taskType, ctx.getParentTask());
		//TODO taskMap (taskName : Task*)
		taskCache.insert( {atomic->name, atomic} );


		Context context(atomic);
		
		//inputPorts
		auto inputPorts = handle_inputPorts(context, *e.getFirstChildByName("inputPorts"));
		atomic->inputPorts = inputPorts;
		
		//outputPorts;
		auto outputPorts = handle_outputPorts(context, *e.getFirstChildByName("outputPorts"));
		atomic->outputPorts = outputPorts;
	
		//properties?
		auto propChild = e.getFirstChildByName("properties");
		if(propChild != nullptr) {
			atomic->properties = handle_properties(ctx, *propChild);
		}

		//constraints?
		auto constraintChild = e.getFirstChildByName("constraints");
		if(constraintChild != nullptr) {
			atomic->constraints = handle_constraints(ctx, *constraintChild);
		}
	
		return atomic;
	}
	
	IfTask* handle_if(Context& ctx, const XmlElement& e) {
		auto taskNameStr = e.getAttr("name");
		IfTask* ifTask = mgr->create<IfTask>(taskNameStr, ctx.getParentTask());
		//TODO taskMap (taskName : Task*)
		taskCache.insert( {ifTask->name, ifTask} );


		VLOG(2) << "IfTask:" << taskNameStr;

		Context context(ifTask);

		//condition
		auto condChild = e.getFirstChildByName("condition");	
		VLOG(2) << condChild->getText();
		Condition* cond = handle_condition(context, *condChild);
		ifTask->condition = cond;

		//Then
		auto thenChild = e.getFirstChildByName("then");
		auto thenBody = handle_tasks(context, *thenChild);
		ifTask->thenBody = thenBody;
		
		//Else
		auto elseChild = e.getFirstChildByName("else");
		if(elseChild != nullptr) {
			auto elseBody = handle_tasks(context, *elseChild);
			ifTask->elseBody = elseBody;
			ifTask->hasElse = true;
		}
	
		//inputPorts
		auto inputPorts = handle_inputPorts(context, *e.getFirstChildByName("inputPorts"));
		ifTask->inputPorts = inputPorts;

		//outputPorts;
		auto outputPorts = handle_outputPorts(context, *e.getFirstChildByName("outputPorts"));
		ifTask->outputPorts = outputPorts;

		//properties?
		auto propChild = e.getFirstChildByName("properties");
		if(propChild != nullptr) {
			ifTask->properties = handle_properties(context, *propChild);
		}
		
		//constraints?
		auto constraintChild = e.getFirstChildByName("constraints");
		if(constraintChild != nullptr) {
			ifTask->constraints= handle_constraints(context, *constraintChild);
		}

		//Links
		auto linksChild = e.getFirstChildByName("links");
		ifTask->links = handle_links(context, *linksChild);
		
		return ifTask;
	}

	BlockScope* handle_blockScope(Context& ctx, const XmlElement& e) {
		auto taskNameStr = e.getAttr("name");
		BlockScope* bs = mgr->create<BlockScope>(taskNameStr, ctx.getParentTask());
		//TODO taskMap (taskName : Task*)
		taskCache.insert( {bs->name, bs} );


		VLOG(2) << "BlockScope:" << taskNameStr;

		Context context(bs);

		//Body
		auto bodyChild= e.getFirstChildByName("body");
		bs->body = handle_tasks(context, *bodyChild);
		
		//inputPorts
		auto inputPorts = handle_inputPorts(context, *e.getFirstChildByName("inputPorts"));
		bs->inputPorts = inputPorts;

		//outputPorts;
		auto outputPorts = handle_outputPorts(context, *e.getFirstChildByName("outputPorts"));
		bs->outputPorts = outputPorts;

		//properties?
		auto propChild = e.getFirstChildByName("properties");
		if(propChild != nullptr) {
			bs->properties = handle_properties(context, *propChild);
		}
		
		//constraints?
		auto constraintChild = e.getFirstChildByName("constraints");
		if(constraintChild != nullptr) {
			bs->constraints= handle_constraints(context, *constraintChild);
		}

		//Links
		auto linksChild = e.getFirstChildByName("links");
		bs->links = handle_links(context, *linksChild);
		return bs;
	}

	WhileTask* handle_while(Context& ctx, const XmlElement& e) {
		auto taskNameStr = e.getAttr("name");
		WhileTask* whileTask = mgr->create<WhileTask>(taskNameStr, ctx.getParentTask());
		//TODO taskMap (taskName : Task*)
		taskCache.insert( {whileTask->name, whileTask} );


		VLOG(2) << "WhileTask:" << taskNameStr;

		Context innerCtx(whileTask);

		//Condition
		auto condChild = e.getFirstChildByName("condition");	
		VLOG(2) << condChild->getText();
		Condition* cond = handle_condition(innerCtx, *condChild);
		whileTask->condition = cond;

		//Body
		auto bodyChild= e.getFirstChildByName("body");
		whileTask->body = handle_tasks(innerCtx, *bodyChild);

		//InputPorts
		auto inputPortsChild = e.getFirstChildByName("inputPorts");
		auto inputPorts = handle_inputPorts(innerCtx, *inputPortsChild);
		whileTask->inputPorts = inputPorts;

		//TODO LoopPorts are wihtin <inputports>
		auto loopPortsChild = inputPortsChild->getFirstChildByName("loopPorts");
		auto loopPorts = handle_loopPorts(innerCtx, *loopPortsChild);
		whileTask->loopPorts = loopPorts;
		//TODO loopPorts are inputports - add them to the inputports?

		//OutputPorts
		auto outputPortsChild = e.getFirstChildByName("outputPorts");
		auto outputPorts = handle_outputPorts(innerCtx, *outputPortsChild);
		whileTask->outputPorts = outputPorts;

		//TODO UnionPorts are within <outputports>
		auto unionPortsChild = outputPortsChild->getFirstChildByName("unionPorts");
		auto unionPorts = handle_unionPorts(innerCtx, *unionPortsChild);
		whileTask->unionPorts = unionPorts;
		//TODO unionports are outpuports - add them to outputPorts?

		//Links
		auto linksChild = e.getFirstChildByName("links");
		whileTask->links = handle_links(innerCtx, *linksChild);

		//Properties
		auto constraintChild = e.getFirstChildByName("constraints");
		if(constraintChild != nullptr) {
			whileTask->constraints= handle_constraints(innerCtx, *constraintChild);
		}

		//Constraints
		auto propChild = e.getFirstChildByName("properties");
		if(propChild != nullptr) {
			whileTask->properties = handle_properties(innerCtx, *propChild);
		}

		return whileTask;
	}

	ForTask* handle_for(Context& ctx, const XmlElement& e) {
		auto taskNameStr = e.getAttr("name");
		ForTask* forTask = mgr->create<ForTask>(taskNameStr, ctx.getParentTask());
		taskCache.insert( {forTask->name, forTask} );
		VLOG(2) << "ForTask:" << taskNameStr;

		Context innerCtx(forTask);

		//Body
		auto bodyChild= e.getFirstChildByName("body");
		forTask->body = handle_tasks(innerCtx, *bodyChild);

		//InputPorts
		auto inputPortsChild = e.getFirstChildByName("inputPorts");
		auto inputPorts = handle_inputPorts(innerCtx, *inputPortsChild);
		forTask->inputPorts = inputPorts;

		//TODO LoopPorts are within <inputports>
		auto loopPortsChild = inputPortsChild->getFirstChildByName("loopPorts");
		auto loopPorts = handle_loopPorts(innerCtx, *loopPortsChild);
		forTask->loopPorts = loopPorts;
		//TODO loopPorts are inputports - add them to the inputports?

		//TODO LOOPCOUNTERS are within <inputports>
		auto loopCounterChild = inputPortsChild->getFirstChildByName("loopCounter");
		std::tie(forTask->counter, forTask->fromCounter, forTask->toCounter, forTask->stepCounter) = handle_loopCounter(innerCtx, *loopCounterChild);
		//NOTE: some loopcounters are inputports - add them to the inputports
		if(forTask->counter->port){ inputPorts->elements.push_back(forTask->counter->port); }
		if(forTask->fromCounter->port) { inputPorts->elements.push_back(forTask->fromCounter->port); }
		if(forTask->toCounter->port) {inputPorts->elements.push_back(forTask->toCounter->port); }
		if(forTask->stepCounter->port) {inputPorts->elements.push_back(forTask->stepCounter->port); }

		//OutputPorts
		auto outputPortsChild = e.getFirstChildByName("outputPorts");
		auto outputPorts = handle_outputPorts(innerCtx, *outputPortsChild );
		forTask->outputPorts = outputPorts;

		//TODO UnionPorts
		auto unionPortsChild = outputPortsChild->getFirstChildByName("unionPorts");
		auto unionPorts = handle_unionPorts(innerCtx, *unionPortsChild);
		forTask->unionPorts = unionPorts;
		//TODO unionports are outpuports - add them to outputPorts?

		//Links
		auto linksChild = e.getFirstChildByName("links");
		forTask->links = handle_links(innerCtx, *linksChild);


		//Constraints
		auto constraintChild = e.getFirstChildByName("constraints");
		if(constraintChild != nullptr) {
			forTask->constraints= handle_constraints(innerCtx, *constraintChild);
		}

		//Properties
		auto propChild = e.getFirstChildByName("properties");
		if(propChild != nullptr) {
			forTask->properties = handle_properties(innerCtx, *propChild);
		}

		return forTask;
	}

	ParallelForTask* handle_parallelFor(Context& ctx, const XmlElement& e) { 
		auto taskNameStr = e.getAttr("name");
		ParallelForTask* parFor = mgr->create<ParallelForTask>(taskNameStr, ctx.getParentTask());
		taskCache.insert( {parFor->name, parFor} );
		VLOG(2) << "ParallelForTask:" << taskNameStr;
		
		Context innerCtx(parFor);

		//Body
		auto bodyChild= e.getFirstChildByName("body");
		parFor->body = handle_tasks(innerCtx, *bodyChild);

		//InputPorts
		auto inputPortsChild = e.getFirstChildByName("inputPorts");
		auto inputPorts = handle_inputPorts(innerCtx, *inputPortsChild);
		parFor->inputPorts = inputPorts;

		//TODO LOOPCOUNTERS are within <inputports>
		auto loopCounterChild = inputPortsChild->getFirstChildByName("loopCounter");
		std::tie(parFor->counter, parFor->fromCounter, parFor->toCounter, parFor->stepCounter) = handle_loopCounter(innerCtx, *loopCounterChild);
		//NOTE: some loopcounters are inputports - add them to the inputports
		if(parFor->counter->port){ inputPorts->elements.push_back(parFor->counter->port); }
		if(parFor->fromCounter->port) { inputPorts->elements.push_back(parFor->fromCounter->port); }
		if(parFor->toCounter->port) {inputPorts->elements.push_back(parFor->toCounter->port); }
		if(parFor->stepCounter->port) {inputPorts->elements.push_back(parFor->stepCounter->port); }

		//OutputPorts
		auto outputPortsChild = e.getFirstChildByName("outputPorts");
		auto outputPorts = handle_outputPorts(innerCtx, *outputPortsChild );
		parFor->outputPorts = outputPorts;

		//Links
		auto linksChild = e.getFirstChildByName("links");
		parFor->links = handle_links(innerCtx, *linksChild);

		//Constraints
		auto constraintChild = e.getFirstChildByName("constraints");
		if(constraintChild != nullptr) {
			parFor->constraints= handle_constraints(innerCtx, *constraintChild);
		}

		//Properties
		auto propChild = e.getFirstChildByName("properties");
		if(propChild != nullptr) {
			parFor->properties = handle_properties(innerCtx, *propChild);
		}

		return parFor;
	}

	ForEachTask* handle_forEach(Context& ctx, const XmlElement& e) { 
		auto taskNameStr = e.getAttr("name");
		ForEachTask* forEach = nullptr;
		forEach = mgr->create<ForEachTask>(taskNameStr,ctx.getParentTask());
		taskCache.insert( {forEach->name, forEach} );
		VLOG(2) << "ForEachTask:" << taskNameStr;

		Context innerCtx(forEach);

		//Body
		auto bodyChild= e.getFirstChildByName("body");
		forEach->body = handle_tasks(innerCtx, *bodyChild);

		//InputPorts
		auto inputPortsChild = e.getFirstChildByName("inputPorts");
		auto inputPorts = handle_inputPorts(innerCtx, *inputPortsChild);
		forEach->inputPorts = inputPorts;

		//TODO LoopPorts are within <inputports>
		auto loopPortsChild = inputPortsChild->getFirstChildByName("loopPorts");
		auto loopPorts = handle_loopPorts(innerCtx, *loopPortsChild);
		forEach->loopPorts = loopPorts;
		//TODO loopPorts are inputports - add them to the inputports?

		//TODO LoopElements are within <inputports>
		auto loopElementsChild = inputPortsChild->getFirstChildByName("loopElements");
		auto loopElements = handle_loopElements(innerCtx, *loopElementsChild);
		forEach->loopElements = loopElements;
		//TODO loopElements are inputports - add them to inputPorts?
		
		//OutputPorts
		auto outputPortsChild = e.getFirstChildByName("outputPorts");
		auto outputPorts = handle_outputPorts(innerCtx, *outputPortsChild );
		forEach->outputPorts = outputPorts;

		//TODO UnionPorts
		auto unionPortsChild = outputPortsChild->getFirstChildByName("unionPorts");
		auto unionPorts = handle_unionPorts(innerCtx, *unionPortsChild);
		forEach->unionPorts = unionPorts;
		//TODO unionports are outpuports - add them to outputPorts?

		//Links
		auto linksChild = e.getFirstChildByName("links");
		forEach->links = handle_links(innerCtx, *linksChild);


		//Constraints
		auto constraintChild = e.getFirstChildByName("constraints");
		if(constraintChild != nullptr) {
			forEach->constraints= handle_constraints(innerCtx, *constraintChild);
		}

		//Properties
		auto propChild = e.getFirstChildByName("properties");
		if(propChild != nullptr) {
			forEach->properties = handle_properties(innerCtx, *propChild);
		}
		return forEach;
	}

	ParallelForEachTask* handle_parallelForEach(Context& ctx, const XmlElement& e) {
		auto taskNameStr = e.getAttr("name");
		ParallelForEachTask* parForEach = nullptr;
		parForEach = mgr->create<ParallelForEachTask>(taskNameStr,ctx.getParentTask());
		taskCache.insert( {parForEach->name, parForEach} );
		VLOG(2) << "ParallelForEach:" << taskNameStr;

		Context innerCtx(parForEach);

		//Body
		auto bodyChild= e.getFirstChildByName("body");
		parForEach->body = handle_tasks(innerCtx, *bodyChild);

		//InputPorts
		auto inputPortsChild = e.getFirstChildByName("inputPorts");
		auto inputPorts = handle_inputPorts(innerCtx, *inputPortsChild);
		parForEach->inputPorts = inputPorts;

		//TODO LoopElements are within <inputports>
		auto loopElementsChild = inputPortsChild->getFirstChildByName("loopElements");
		auto loopElements = handle_loopElements(innerCtx, *loopElementsChild);
		parForEach->loopElements = loopElements;
		//TODO loopElements are inputports - add them to inputPorts?
		//inputPorts->elements.insert(inputPorts->elements.end(),
		//parForEach->inputPorts->elements.insert(parForEach->inputPorts->elements.end(), loopElements.begin(), loopElements.begin());
		
		//OutputPorts
		auto outputPortsChild = e.getFirstChildByName("outputPorts");
		auto outputPorts = handle_outputPorts(innerCtx, *outputPortsChild );
		parForEach->outputPorts = outputPorts;

		//Links
		auto linksChild = e.getFirstChildByName("links");
		parForEach->links = handle_links(innerCtx, *linksChild);

		//Constraints
		auto constraintChild = e.getFirstChildByName("constraints");
		if(constraintChild != nullptr) {
			parForEach->constraints= handle_constraints(innerCtx, *constraintChild);
		}

		//Properties
		auto propChild = e.getFirstChildByName("properties");
		if(propChild != nullptr) {
			parForEach->properties = handle_properties(innerCtx, *propChild);
		}
		return parForEach;
	}

	Links* handle_links(Context& ctx, const XmlElement& e) { 
		Links* links = mgr->create<Links>();
		
		VLOG(2) << "links";

		auto children= e.getChildren();	
		for(auto l : children) {
			links->elements.push_back(handle_link(ctx, l));
		}
		return links;
	}

	Link* handle_link(Context& ctx, const XmlElement& e) {
		auto fromStr = e.getAttr("from");
		auto toStr = e.getAttr("to");

		VLOG(2) << "link : " << fromStr << "->" << toStr;

		vector<string> fromSubs;
		vector<string> toSubs;
		boost::split(fromSubs, fromStr, boost::is_any_of("/"));
		boost::split(toSubs, toStr, boost::is_any_of("/"));

		bool isDataLink = false;
		if(fromSubs.size() == 3 || toSubs.size() == 3) {
			//data link needs task and port
			VLOG(2) << "link with loopCounter";
			VLOG(2) << fromSubs;
			VLOG(2) << toSubs;
			//link from/to loopCounter?

			isDataLink = true;
		} else if(fromSubs.size() == 2 && toSubs.size() == 2) {
			//data link needs task and port
			isDataLink = true;
		} else if(fromSubs.size() == 1 && toSubs.size() == 1) {
			isDataLink = false;
			assert(false && "ControlFlowLink not implemented yet");
		} else {
			assert(false && "link is not of form task/[port]/[loopCounter]");
		}

		VLOG(2) << "from: " << fromSubs;
		VLOG(2) << "to: " << toSubs;

		Task* fromTask=nullptr;
		Port* fromPort=nullptr;
		Task* toTask=nullptr;
		Port* toPort=nullptr;

		//TODO split up from name -- fromTask, fromName
		string fromTaskStr = fromSubs[0];
		string toTaskStr = toSubs[0];

		VLOG(2) << "look for " << fromTaskStr << " in taskCache | " << taskCache;
		auto fT = taskCache.find(fromTaskStr);
		if(fT != taskCache.end()) {
			fromTask = fT->second;
		} 

		VLOG(2) << "look for " << toTaskStr << " in taskCache | " << taskCache; 
		auto tT = taskCache.find(toTaskStr);
		if(tT != taskCache.end()) {
			toTask = tT->second;
		}

		assert(fromTask);
		assert(toTask);

		Link* link = nullptr;
		if(isDataLink) {
			string toPortStr = (toSubs.size() == 3) ? toSubs[1]+"_"+toSubs[2] : toSubs[1];
			string fromPortStr = (fromSubs.size() == 3) ? fromSubs[1]+"_"+fromSubs[2] : fromSubs[1];;

			VLOG(2) << "look for " << toPortStr << " in portCache | " << portCache;
			auto tP = portCache.find({toTaskStr, toPortStr});
			if(tP != portCache.end()) {
				toPort = tP->second;
			}
			assert(toPort);

			VLOG(2) << "look for " << fromPortStr << " in portCache | " << portCache;
			auto fP = portCache.find({fromTaskStr, fromPortStr});
			if(fP != portCache.end()) {
				fromPort = fP->second;
			}
			assert(fromPort);
			link = mgr->create<Link>(fromTask, toTask, fromPort, toPort, ctx.getParentTask());
		} else {
			link = mgr->create<Link>(fromTask, toTask, ctx.getParentTask());
		}
		
		return link;
	}
	
	Ports* handle_inputPorts(Context& ctx, const XmlElement& e) {
		Ports* inputPorts = mgr->create<Ports>(PortsKind::PK_InputPorts);
		auto children= e.getChildren();	
		for(XmlElement c : children) {
			//only inputPorts!!!
			if(c.getName() == "inputPort") {
				auto i = handle_inputPort(ctx, c);
				inputPorts->elements.push_back(i);
			}
		}

		return inputPorts;
	}

	Port* handle_inputPort(Context& ctx, const XmlElement& e) {
		auto res = handle_port(ctx, e);
		res->isInput = true;
		res->isOutput = false;
		return res;
	}

	Ports* handle_outputPorts(Context& ctx, const XmlElement& e) {
		Ports* outputPorts = mgr->create<Ports>(PortsKind::PK_OutputPorts);
		auto children = e.getChildren();	
		for(XmlElement c : children) {
			if(c.getName() == "outputPort") {
				auto o = handle_outputPort(ctx, c);
				outputPorts->elements.push_back(o);
			}
		}
		return outputPorts;
	}

	Port* handle_outputPort(Context& ctx, const XmlElement& e) {
		auto res = handle_port(ctx, e);
		res->isInput = false;
		res->isOutput = true;
		return res; 
	}
	
	Ports* handle_loopElements(Context& ctx, const XmlElement& e) {
		Ports* loopElements= mgr->create<Ports>(PortsKind::PK_LoopElements);
		auto children = e.getChildren();	
		for(XmlElement c : children) {
			if(c.getName() == "loopElement") {
				auto o = handle_loopElement(ctx, c);
				loopElements->elements.push_back(o);
			}
		}
		return loopElements;
	}

	Port* handle_loopElement(Context& ctx, const XmlElement& e) {
		Port* le = handle_port(ctx, e); 
		le->kind = PortKind::PK_LoopElement;
		le->isInput = true;
		le->isOutput = false;
		return le;
	}
	
	Ports* handle_unionPorts(Context& ctx, const XmlElement& e) {
		auto res = mgr->create<Ports>(PortsKind::PK_UnionPorts);
		auto children = e.getChildren();	
		for(XmlElement c : children) {
			res->elements.push_back(dynamic_cast<Port*>(handle_unionPort(ctx, c)));
		}
		return res;
	}

	Port* handle_unionPort(Context& ctx, const XmlElement& e) {
		//TODO input/output
		auto up = handle_port(ctx, e);
		up->kind = PortKind::PK_UnionPort;
		up->isInput = false;
		up->isOutput = true;
		return up; 
	}

	Ports* handle_loopPorts(Context& ctx, const XmlElement& e) {
		auto res = mgr->create<Ports>(PortsKind::PK_LoopPorts);
		auto children= e.getChildren();	
		for(XmlElement c : children) {
			if(c.getName() == "loopPort") {
				Port* p = dynamic_cast<Port*>(handle_loopPort(ctx, c));
				res->elements.push_back(p);
			}
		}
		return res;
	}

	Port* handle_loopPort(Context& ctx, const XmlElement& e) {
		//TODO input/output
		auto lp = handle_port(ctx, e);
		lp->isInput = true;
		lp->isOutput = false;
		lp->kind = PortKind::PK_LoopPort;
		return lp;
	}

	Port* handle_port(Context& ctx, const XmlElement& e) {
		string portName(e.getAttr("name"));
		string portTypeStr = e.getAttr("type");
		auto portType = mgr->create<Type>(portTypeStr);

		VLOG(2) << "port : " << portName << ":" << portTypeStr;
		
		auto res = mgr->create<Port>(portName, portType, ctx.getParentTask());
		portCache.insert( { {res->parentTask->name, res->name}, res} );

		//properties?
		auto propChild = e.getFirstChildByName("properties");
		if(propChild != nullptr) {
			res->properties = handle_properties(ctx, *propChild);
		}
		
		//constraints?
		auto constraintChild = e.getFirstChildByName("constraints");
		if(constraintChild != nullptr) {
			res->constraints= handle_constraints(ctx, *constraintChild);
		}
		
		return res;
	}

	std::tuple<LoopCounter*, LoopCounter*, LoopCounter*, LoopCounter*> handle_loopCounter(Context& ctx, const XmlElement& e) {
		auto nameStr = e.getAttr("name");
		VLOG(2) << nameStr;

		//<loopCounter name="lc1" from="" to="10" step="2"/>
		LoopCounter* counter = nullptr;
		LoopCounter* from = nullptr; 
		LoopCounter* to = nullptr; 
		LoopCounter* step = nullptr;	

		//special port... loopcounter is always integer
		//the loopcounter itself is output port (for other tasks)
		//loopcoutner_from, loopcoutner_step, loopcoutner_to are input ports 
		Port* counterPort = nullptr;
		Type* type = mgr->create<Type>("integer");
		counterPort = mgr->create<Port>(nameStr, type, ctx.getParentTask(), PortKind::PK_LoopCounter);
		counterPort->isInput = false;
		counterPort->isOutput = true;
		portCache.insert({ { counterPort->parentTask->name, counterPort->name}, counterPort} );
		counter = mgr->create<LoopCounter>(ctx.getParentTask(), counterPort);

		//emptyString --> needs a port
		auto fromStr = e.getAttr("from");
		if(fromStr.empty()) {
			Port* fromPort = nullptr;
			//fromPort = = namestr+from:integer;
			string name = nameStr + "_from";
			Type* type = mgr->create<Type>("integer");
			fromPort = mgr->create<Port>(name, type, ctx.getParentTask(), PortKind::PK_LoopCounter);
			fromPort->isInput = true;
			fromPort->isOutput = false;
			portCache.insert({ { fromPort->parentTask->name, fromPort->name}, fromPort} );

			from = mgr->create<LoopCounter>(ctx.getParentTask(), fromPort);
		} else {
			int fromVal = std::stoi(fromStr,nullptr); //fromStr.toInteger
			VLOG(2) << "fromvalue: " <<  fromVal;
			from = mgr->create<LoopCounter>(ctx.getParentTask(), fromVal);
		}

		//emptyString --> needs a port
		auto toStr = e.getAttr("to");
		if(toStr.empty()) {
			Port* toPort = nullptr;
			//toPort = = namestr+to:integer;
			string name = nameStr + "_to";
			Type* type = mgr->create<Type>("integer");
			toPort = mgr->create<Port>(name, type, ctx.getParentTask(), PortKind::PK_LoopCounter);
			toPort->isInput = true;
			toPort->isOutput = false;
			portCache.insert({ { toPort->parentTask->name, toPort->name}, toPort} );
			to = mgr->create<LoopCounter>(ctx.getParentTask(), toPort);
		} else {
			int toVal = std::stoi(toStr,nullptr); //toStr.toInteger
			VLOG(2) << "tovalue: " << toVal;
			to = mgr->create<LoopCounter>(ctx.getParentTask(), toVal);
		}	

		int stepVal;
		bool hasStep = e.hasAttr("step");

		//step is optional --> no stepAttr -> step==1
		if(hasStep) {
			auto stepStr = e.getAttr("step");
			if(stepStr.empty()) {
				//step = "" --> results in port
				Port* stepPort = nullptr;
				//stepPort = namestr+step:integer;
				string name = nameStr + "_step";
				Type* type = mgr->create<Type>("integer");
				stepPort = mgr->create<Port>(name, type, ctx.getParentTask(), PortKind::PK_LoopCounter);
				stepPort->isInput = true;
				stepPort->isOutput = false;
				portCache.insert({ { stepPort->parentTask->name, stepPort->name}, stepPort} );
				step = mgr->create<LoopCounter>(ctx.getParentTask(), stepPort); 
			} else {
				//step = "val" --> stoi(val)
				stepVal = std::stoi(stepStr,nullptr); //stepStr.toInteger;
				VLOG(2) << "stepvalue: " << stepVal;
				step = mgr->create<LoopCounter>(ctx.getParentTask(), stepVal);
			}
		} else {
			//no step attribute -> steVal = 1
			stepVal = 1;
			VLOG(2) << "stepvalue: " << stepVal;
			step = mgr->create<LoopCounter>(ctx.getParentTask(), stepVal);
		}
		
		assert(counter);
		assert(from);
		assert(to);
		assert(step);

		return std::make_tuple(counter, from, to, step); 
	}

	Properties* handle_properties(Context& ctx, const XmlElement& e) { 
		Properties* res = mgr->create<Properties>();
		auto children= e.getChildren();	
		for(XmlElement c : children) {
			Property* p = dynamic_cast<Property*>(handle_property(ctx, c));
			res->elements.push_back(p);
		}

		return res; 
	}

	Property* handle_property(Context& ctx, const XmlElement& e) { 
		string name(e.getAttr("name"));
		string value(e.getAttr("value"));

		Property* res = mgr->create<Property>(name, value);
		return res; 
	}

	Constraints* handle_constraints(Context& ctx, const XmlElement& e) { 
		Constraints* res = mgr->create<Constraints>();
		auto children= e.getChildren();	
		for(XmlElement c : children) {
			Constraint* p = dynamic_cast<Constraint*>(handle_constraint(ctx, c));
			res->elements.push_back(p);
		}

		return res; 
	}

	Constraint* handle_constraint(Context& ctx, const XmlElement& e) { 
		string name(e.getAttr("name"));
		string value(e.getAttr("value"));

		Constraint* res = mgr->create<Constraint>(name, value);
		return res;  
	}

	Condition* handle_condition(Context& ctx, const XmlElement& e) { return mgr->create<Condition>(e.getText(), ctx.getParentTask()); }

	void buildIWIR(const XmlElement& iwir) {
		//TODO check for toplevel node ? <IWIR ... wfname = ...>
		this->wfName = iwir.getAttr("wfname");
		std::cout << "wfName:" << this->wfName << std::endl;

		Context ctx;
		auto children= iwir.getChildren();	

		//only one child -> the toplevel task
		for(XmlElement c : children) {
			VLOG(2) << "childName : " << c.getName();
			this->topLevel = handle_task(ctx, c);
		}
	}
	#undef DISPATCH
	#undef DISPATCH2HANDLER
};

}
