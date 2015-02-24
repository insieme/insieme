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

#include "insieme/iwir/iwir_converter.h"
#include "insieme/iwir/iwir_linkcollector.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/iwir/annotations/property_annotation.h"
#include "insieme/iwir/annotations/constraint_annotation.h"

namespace insieme {
namespace iwir {

using namespace iwir::ast;
using namespace insieme;
using namespace std;

#define CONVERTER(name) void IWIRConverter::convert ## name(name* node, ConversionContext& context)

CONVERTER(Tasks) {
	VLOG(2) << "Tasks";
	for(Task* n : node->elements) {
		convert(n, context);
	}
}

CONVERTER(AtomicTask) {
	VLOG(2) << "AtomicTask : " << node->name << ":" << node->type->type;

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

	//converting TaskType
	auto taskType = convertTaskType(node->type, context);

	//tasktype as parameter
	auto taskTypeLit = irBuilder.getTypeLiteral(taskType);
	parameterTypes.push_back(taskTypeLit->getType());
	//taskType literal as argument
	args.push_back(taskTypeLit);

	convert(node->inputPorts, context);
	convert(node->outputPorts, context);

	//prepare input parameters/arguments 
	for(auto port : node->inputPorts->elements) {
		auto p = varMap.find( {port->parentTask, port} );
		assert(p != varMap.end());
		if(p != varMap.end()) {
			auto var = p->second;
			parameterTypes.push_back(var->getType());
			args.push_back(var);
		}
	}
	//prepare output parameters/arguments 
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

	//handle constraints and properties - attached as annotation
	auto properties = CONVERT_PROPERTIES(node->properties, context);
	auto constraints = CONVERT_CONSTRAINTS(node->constraints, context);
	if(!constraints.empty()) {
		// attach constraints as annotation to the generated taskExpr
		annotations::iwir::attachConstraintMap(taskExpr, constraints);
	}
	if(!properties.empty()) {
		// attach property as annotation to the generated taskExpr
		annotations::iwir::attachPropertyMap(taskExpr, properties);
	}

	VLOG(2) << dumpPretty(taskExpr);

	core::ExpressionPtr atomicTaskCall =  irBuilder.callExpr(taskExpr, args);
	taskCache[node] = atomicTaskCall;
}

CONVERTER(BlockScope) {
	VLOG(2) << "BlockScope : " << node->name;

	// linkcollector not necessary as we model the parallelism with 
	// "parallel(taskJob)" and channels
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

		if(VLOG_IS_ON(2)) {
			linkCollector.printToDotFile(node->name+".dot");
		}
	}

	ConversionContext innerContext;
	convert(node->body, innerContext);
	VLOG(2) << innerContext;

	convert(node->inputPorts, context);
	convert(node->outputPorts, context);

	convert(node->links, context);

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

	// visit each task in the blockscope's body
	for_each(node->body->begin(),node->body->end(), 
		[&](Task* task) {
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

					//declare and create channel -- before everything else in blockscope
					auto fit = varMap.find( {task, ip} );
					assert(fit != varMap.end());
					core::VariablePtr var = fit->second;
					auto varType = var->getType();
					
					VLOG(2) << *ip << " -- " << var << "(" << varType << ")";

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

					VLOG(2) << *op << " -- " << var << "(" << varType << ")";

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
			taskJob[task] = jobBody;
		}
	);

	core::StatementList readChannels;
	core::StatementList fillChannels;

	//declare channels for BS_IP and provide linking stmts
	for_each(node->inputPorts->begin(), node->inputPorts->end(),
		[&](Port* ip) {
			assert(ip);

			//declare and create channel -- before everything else in blockscope
			auto fit = varMap.find( {node, ip} );
			assert(fit != varMap.end());
			core::VariablePtr var = fit->second;
			auto varType = var->getType();

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

	//declare channels for BS_OP and provide linking stmts
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

	// channel Links between tasks 
	core::StatementList channelLinks;

	map<Task*, list<Task*>> controlFlowMap;

	//declare control-flow-link-channels and provide linking stmts
	for_each(node->links->begin(), node->links->end(),
		[&](Link* link) {		
			if(!link->isDataLink) {
				controlFlowMap[link->fromTask].push_back(link->toTask);
			}
		}
	);

	// control links are modeled as channels
	map<string, core::NodePtr> cfSymbols;
	for(pair<Task*, list<Task*>> cf : controlFlowMap) {
		Task* from = cf.first;
		//decl cfChan -- in BlockScope Task
		//declare channel - queue size == #targets
		auto queueSize = cf.second.size();
		cfSymbols["queueSize"] = irBuilder.getIntParamLiteral(queueSize);
		core::DeclarationStmtPtr cfChanDecl = irBuilder.parseStmt("auto chan = channel.create(type(unit), queueSize);", cfSymbols).as<core::DeclarationStmtPtr>();
		declareChannels.push_back(cfChanDecl);

		cfSymbols["cfChan"] = cfChanDecl->getVariable();
		//release channel -- when everythings done in BlockScope
		releaseChannels.push_back(irBuilder.parseStmt("channel.release(cfChan);", cfSymbols));
	
		//get "from" task body
		auto fromBody = taskJob[from];
	
		for(Task* to : cf.second) {
			// for every dependent task send a token
			fromBody.insert(fromBody.end(), irBuilder.parseStmt("channel.send(cfChan,unit);", cfSymbols));
	
			// dependent tasks need to read channel
			auto toBody = taskJob[to];
			toBody.insert(toBody.begin(), irBuilder.parseStmt("channel.recv(cfChan);", cfSymbols));

			//update the "to" task body
			taskJob[to] = toBody;
		}

		//update the "from" task body
		taskJob[from] = fromBody;
	}
	
	//as input ports can be linked to several other ports, with channels we need a multiplexer
	map<Port*, list<Port*>> muxMap;

	//collect all links with same starting port
	for_each(node->links->begin(), node->links->end(),
		[&](Link* link) {
			if(link->isDataLink) {
				muxMap[link->from].push_back(link->to);
			} 
		}
	);

	//creates mux function
	//NOTE: also links with only ONE target port are muxed currently
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
		VLOG(2) << "Mux - from: " << *fromPort;
		assert(fromChan);
		
		muxSymbols["from"] = fromChan;
		core::DeclarationStmtPtr tempDecl = irBuilder.parseStmt("auto temp = channel.recv(from);", muxSymbols).as<core::DeclarationStmtPtr>();
		muxBody.push_back(tempDecl);
		muxSymbols["temp"] = tempDecl->getVariable();

		for(Port* toPort : toPorts) {
			VLOG(2)<< "Mux - To: " << *toPort;
			core::VariablePtr toChan = portToChannel[toPort];
			assert(toChan);
			
			muxSymbols["to"] = toChan;
			core::StatementPtr sendStmt = irBuilder.parseStmt("channel.send(to, temp);", muxSymbols);
			muxBody.push_back(sendStmt);
		}
	
		core::ExpressionPtr muxer = irBuilder.createCallExprFromBody(irBuilder.compoundStmt(muxBody), irBuilder.getLangBasic().getUnit(), /*lazy=*/false);
		VLOG(2) << dumpPretty(muxer);
		channelLinks.push_back(muxer);
	}

	//link channels in a parallel/task construct
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
	
	//release channels
	bodyStmts.insert(bodyStmts.end(), releaseChannels.begin(), releaseChannels.end()); 
		
	//create a callExpr with the BlockScopes body
	core::StatementPtr body = irBuilder.compoundStmt(bodyStmts);
	core::ExpressionPtr bs = irBuilder.createCallExprFromBody(body, irBuilder.getLangBasic().getUnit(), /*lazy=*/false);
	
	//handle constraints and properties - attached as annotation
	auto properties = CONVERT_PROPERTIES(node->properties, context);
	auto constraints = CONVERT_CONSTRAINTS(node->constraints, context);

	//TODO attach to correct IR node -> currently attached to CallExpr
	if(!constraints.empty()) {
		// attach constraints as annotation to the generated taskExpr
		annotations::iwir::attachConstraintMap(bs, constraints);
	}
	if(!properties.empty()) {
		// attach property as annotation to the generated taskExpr
		annotations::iwir::attachPropertyMap(bs, properties);
	}

	VLOG(2) << dumpPretty(bs);
	taskCache[node] = bs;
}

CONVERTER(IfTask) { 
	VLOG(2) << "IfTask : " << node->name;

	ConversionContext thenContext;
	convert(node->thenBody, thenContext);

	ConversionContext elseContext;
	if(node->hasElse) {
		convert(node->elseBody, elseContext);
	}

	convert(node->inputPorts, context);
	convert(node->outputPorts, context);

	convert(node->links, context);

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

				//get call to task
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

			if(VLOG_IS_ON(2)) {
				linkCollector.printToDotFile(node->name+"_then.dot");
			}
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

			if(VLOG_IS_ON(2)) {
				linkCollector.printToDotFile(node->name+"_else.dot");
			}
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

				//get call to task
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

			if(VLOG_IS_ON(2)) {
				linkCollector.printToDotFile(node->name+"_then.dot");
			}
		}
		{
			//some links are outside of thenBody, these are needed because there is no elseBody
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

	core::IfStmtPtr ifStmt = irBuilder.ifStmt(condition, thenBody, elseBody );

	core::ExpressionPtr ifTaskExpr =  irBuilder.createCallExprFromBody(ifStmt, irBuilder.getLangBasic().getUnit(), /*lazy=*/false);

	//handle constraints and properties - attached as annotation
	auto properties = CONVERT_PROPERTIES(node->properties, context);
	auto constraints = CONVERT_CONSTRAINTS(node->constraints, context);

	//TODO attach to correct IR node -> currently attached to CallExpr
	if(!constraints.empty()) {
		// attach constraints as annotation to the generated taskExpr
		annotations::iwir::attachConstraintMap(ifTaskExpr, constraints);
	}
	if(!properties.empty()) {
		// attach property as annotation to the generated taskExpr
		annotations::iwir::attachPropertyMap(ifTaskExpr, properties);
	}

	VLOG(2) << dumpPretty(ifTaskExpr);
	taskCache[node] = ifTaskExpr;
}

CONVERTER(WhileTask) {
	VLOG(2) << "WhileTask " << node->name;
	
	ConversionContext innerContext;
	convert(node->body, innerContext);

	convert(node->inputPorts, context);
	convert(node->outputPorts, context);

	//TODO loopports are outpuports - add them to outputPorts?
	convert(node->loopPorts, context);	

	//TODO unionports are outpuports - add them to outputPorts?
	convert(node->unionPorts, context);	

	convert(node->links, context);

	
	//decls
	//
	//() => {
	//	decls-body -- decls in innerContext
	//	loop-links[init LoopPorts]
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
	core::StatementList initLoopLinks;
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

			//get call to task
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
			} else if(l->to->kind == PK_LoopPort) { 
				//from == * && to == loop 
				//link to init the loopPort
				initLoopLinks.push_back(linkStmt);
			} else {
				assert(false);
			}
		}

		//append union and loop links to whilebody
		VLOG(2) << "Links to UnionPorts:";
		for(auto l : unionLinks) { VLOG(2) << "\t" << *l;}
		VLOG(2) << "Links from LoopPorts:";
		for(auto l : loopLinks) { VLOG(2) << "\t" << *l;}

		whileBody.insert(whileBody.end(), unionLinks.begin(), unionLinks.end());

		whileBody.insert(whileBody.end(), loopLinks.begin(), loopLinks.end());

		if(VLOG_IS_ON(2)) {
			linkCollector.printToDotFile(node->name+".dot");
		}
	}

	//convert condition Expression
	core::ExpressionPtr condition;
	condition = CONVERT_CONDITION(node->condition, context);

	core::WhileStmtPtr whileStmt = irBuilder.whileStmt(condition, irBuilder.compoundStmt(whileBody));

	core::StatementList bodyStmts;
	bodyStmts.insert(bodyStmts.end(), decls.begin(), decls.end()); 

	//init loopPorts before the while-loop if there is a "init"link
	bodyStmts.insert(bodyStmts.end(), initLoopLinks.begin(), initLoopLinks.end()); 

	bodyStmts.push_back(whileStmt);

	core::StatementPtr body = irBuilder.compoundStmt(bodyStmts);

	core::ExpressionPtr whileTask = irBuilder.createCallExprFromBody( body, irBuilder.getLangBasic().getUnit(), /*lazy=*/false);

	//handle constraints and properties - attached as annotation
	auto properties = CONVERT_PROPERTIES(node->properties, context);
	auto constraints = CONVERT_CONSTRAINTS(node->constraints, context);

	//TODO attach to correct IR node -> currently attached to CallExpr
	if(!constraints.empty()) {
		// attach constraints as annotation to the generated taskExpr
		annotations::iwir::attachConstraintMap(whileTask, constraints);
	}
	if(!properties.empty()) {
		// attach property as annotation to the generated taskExpr
		annotations::iwir::attachPropertyMap(whileTask, properties);
	}

	VLOG(2) << dumpPretty(whileTask);
	taskCache[node] = whileTask;
}

CONVERTER(ForTask) {
	VLOG(2) << "ForTask " << node->name;

	ConversionContext innerContext;
	convert(node->body, innerContext);

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
	core::StatementList initLoopLinks;
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

			//get call to task
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
			} else if(l->to->kind == PK_LoopPort) { 
				//from == * && to == loop 
				//link to init the loopPort
				initLoopLinks.push_back(linkStmt);
			} else if(l->to->kind == PK_LoopCounter) { 
				//from == * && to == LoopCounter 
				//links from some inputport of the forTask to one of loopCounter/[to,from,step]
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

		if(VLOG_IS_ON(2)) {
			linkCollector.printToDotFile(node->name+".dot");
		}
	}

	core::ExpressionPtr startVal = irBuilder.tryDeref(fromCounter);
	core::ExpressionPtr endVal = irBuilder.tryDeref(toCounter); 
	core::ExpressionPtr step = irBuilder.tryDeref(stepCounter);

	core::VariablePtr iterVar = counter.as<core::VariablePtr>(); 

	forBodyStmts.insert(forBodyStmts.end(), unionLinks.begin(), unionLinks.end());
	forBodyStmts.insert(forBodyStmts.end(), loopLinks.begin(), loopLinks.end());

	core::CompoundStmtPtr forBody = irBuilder.compoundStmt(forBodyStmts);
	assert(forBody);

	core::ForStmtPtr forStmt = irBuilder.forStmt( iterVar, startVal, endVal, step, forBody);

	//decls
	//
	//() => {
	//	decls-body -- decls in innerContext
	//	loop-links[loopCounters]
	//	loop-links[init LoopPorts]
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
	forTaskStmts.insert(forTaskStmts.end(), initLoopLinks.begin(), initLoopLinks.end()); 
	forTaskStmts.push_back(forStmt);

	core::CompoundStmtPtr forTaskBody = irBuilder.compoundStmt(forTaskStmts);

	core::ExpressionPtr forTask = irBuilder.createCallExprFromBody(forTaskBody,irBuilder.getLangBasic().getUnit(), /*lazy=*/false);

	//handle constraints and properties - attached as annotation
	auto properties = CONVERT_PROPERTIES(node->properties, context);
	auto constraints = CONVERT_CONSTRAINTS(node->constraints, context);

	//TODO attach to correct IR node -> currently attached to CallExpr
	if(!constraints.empty()) {
		// attach constraints as annotation to the generated taskExpr
		annotations::iwir::attachConstraintMap(forTask, constraints);
	}
	if(!properties.empty()) {
		// attach property as annotation to the generated taskExpr
		annotations::iwir::attachPropertyMap(forTask, properties);
	}

	VLOG(2) << dumpPretty(forTask);
	taskCache[node] = forTask;
}

CONVERTER(ParallelForTask) {
	VLOG(2) << "ParallelForTask " << node->name;

	ConversionContext innerContext;
	convert(node->body, innerContext);

	convert(node->inputPorts, context);
	//NOTE: outputPorts of ParallelFor need to be of collection type - handled in PortConverter
	//NOTE: ParallelFor has no unionPort
	//NOTE: ParallelFor has no loopPort
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

	convert(node->links, context);

	//decls
	//
	//	() => {
	//		decls-body -- decls in innerContext
	//		loop-links[loopCounters]
	//		pfor(TODO FILL IN) {
	//			for(var it = from ... to : step) {
	//				links-to-body
	//				body
	//				links-from-body
	//				loop-links[outputPorts] //output ports need to be collection
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
	core::StatementList loopCounterLinks;
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

			//get call to task
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
			core::StatementPtr linkStmt = context.linkStmtMap[l];
			VLOG(2) << "\t" << *l; 
			VLOG(2) << "\t" << linkStmt;
			//loopCounter links
			if(l->to->kind == PK_LoopCounter) { 
				//from == * && to == LoopCounter 
				//links from some inputport of the forTask to one of loopCounter/[to,from,step]
				loopCounterLinks.push_back(linkStmt);
			} else {
				assert(false);
			}
		}

		if(VLOG_IS_ON(2)) {
			linkCollector.printToDotFile(node->name+".dot");
		}
	}

	core::ExpressionPtr startVal = irBuilder.tryDeref(fromCounter);
	core::ExpressionPtr endVal = irBuilder.tryDeref(toCounter); 
	core::ExpressionPtr step = irBuilder.tryDeref(stepCounter);

	core::VariablePtr iterVar = counter.as<core::VariablePtr>(); 

	core::CompoundStmtPtr forBody = irBuilder.compoundStmt(stmts);
	assert(forBody);

	//replace "leIterator" literal in forBody with iterator variable
	//NOTE when we see a link for to a outputPort we create a "leIterator" literal in Link-converter
	core::LiteralPtr leIterator = irBuilder.literal(irBuilder.getLangBasic().getInt4(), "leIterator");
	forBody = core::transform::replaceAllGen(irMgr, forBody, leIterator, iterVar, /*limitScope=*/true);

	core::ForStmtPtr forStmt = irBuilder.forStmt( iterVar, startVal, endVal, step, irBuilder.compoundStmt(forBody));
	
	core::ExpressionPtr pFor = irBuilder.pfor(forStmt);	

	core::StatementList forTaskStmts;
	forTaskStmts.insert(forTaskStmts.end(), decls.begin(), decls.end());
	forTaskStmts.insert(forTaskStmts.end(), loopCounterLinks.begin(), loopCounterLinks.end());
	forTaskStmts.push_back(pFor);

	core::CompoundStmtPtr forTaskBody = irBuilder.compoundStmt(forTaskStmts);

	core::ExpressionPtr parallelForTask = irBuilder.createCallExprFromBody(forTaskBody, irBuilder.getLangBasic().getUnit(), /*lazy=*/false);

	//handle constraints and properties - attached as annotation
	auto properties = CONVERT_PROPERTIES(node->properties, context);
	auto constraints = CONVERT_CONSTRAINTS(node->constraints, context);

	//TODO attach to correct IR node -> currently attached to CallExpr
	if(!constraints.empty()) {
		// attach constraints as annotation to the generated taskExpr
		annotations::iwir::attachConstraintMap(parallelForTask, constraints);
	}
	if(!properties.empty()) {
		// attach property as annotation to the generated taskExpr
		annotations::iwir::attachPropertyMap(parallelForTask, properties);
	}

	VLOG(2) << dumpPretty(parallelForTask);
	taskCache[node] = parallelForTask;
}

CONVERTER(ForEachTask) {
	VLOG(2) << "ForEachTask" << node->name;
	
	ConversionContext innerContext;
	convert(node->body, innerContext);

	convert(node->inputPorts, context);
	convert(node->outputPorts, context);

	//TODO LoopPorts are outputports - add them to outputPorts? 
	convert(node->loopPorts, context);	
	//TODO UnionPorts are outputports - add them to outputPorts?
	convert(node->unionPorts, context);	
	//TODO LoopElementPorts are outputports - add them to outputPorts?
	convert(node->loopElements, context);	

	convert(node->links, context);

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

	core::StatementList forBodyStmts;
	core::StatementList unionLinks;
	core::StatementList loopLinks;
	core::StatementList initLoopLinks;
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

			//get call to task
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
			} else if(l->to->kind == PK_LoopPort) { 
				//from == * && to == loop 
				//link to init the loopPort
				initLoopLinks.push_back(linkStmt);
			} else {
				assert(false);
			}
		}

		//append union and loop links to forbody
		VLOG(2) << "Links to UnionPorts:";
		for(auto l : unionLinks) { VLOG(2) << "\t" << *l;}
		VLOG(2) << "Links from LoopPorts:";
		for(auto l : loopLinks) { VLOG(2) << "\t" << *l;}

		if(VLOG_IS_ON(2)) {
			linkCollector.printToDotFile(node->name+".dot");
		}
	}

	core::ExpressionPtr startVal = irBuilder.intLit(0);
	core::ExpressionPtr endVal;
	if(node->loopElements->elements.size() == 1) {
		//we only need to iterate over one collection
		Port* loopElementsNode = node->loopElements->elements[0];
		auto le = varMap.find( {node, loopElementsNode} );
		assert(le!=varMap.end());
		core::VariablePtr leVar = le->second;
		
		map<string, core::NodePtr> symbols;
		symbols["leVar"] = leVar;
		symbols["size"] = irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getRefCollectionSize();
		//endVal is set to loopElements.length -- IR: endVal = leVar.size;
		endVal = irBuilder.parseExpr("size(leVar)", symbols);
	} else if(node->loopElements->elements.size() > 1) {
		//we need to iterate over multiple collections
		//endvalue is length of "shortest" 
		//IR: findShortestLoopElement(loopElements le1, ...) -> int<4> {
		//	...
		//	return biggestLoopElement->size;
		//}
		typedef vector<insieme::core::ExpressionPtr> ExpressionList;
		ExpressionList elements;
		for_each(node->loopElements->begin(),node->loopElements->end(), 
			[&](Port* le) {
				auto fit = varMap.find( {node, le} );
				assert(fit!=varMap.end());
				core::VariablePtr leVar = fit->second;
				elements.push_back(leVar);
			}
		);

		auto shortestCollection= irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getShortestCollection();
		
		//endVal is set to "shortest" loopElements.length 
		endVal = irBuilder.callExpr(
			shortestCollection,
			core::encoder::toIR<ExpressionList,core::encoder::DirectExprListConverter>(irMgr, elements)
		);
	} else {
		assert(false && "not implemented");
	}

	core::ExpressionPtr step = irBuilder.intLit(1); 

	core::VariablePtr iterVar = irBuilder.variable(irBuilder.getLangBasic().getInt4());

	forBodyStmts.insert(forBodyStmts.end(), unionLinks.begin(), unionLinks.end());
	forBodyStmts.insert(forBodyStmts.end(), loopLinks.begin(), loopLinks.end());

	core::CompoundStmtPtr forBody = irBuilder.compoundStmt(forBodyStmts);
	assert(forBody);

	//replace "leIterator" literal in forBody with iterator variable
	//NOTE when we see a link for a loopElement we create a "leIterator" literal in Link-converter
	core::LiteralPtr leIterator = irBuilder.literal(irBuilder.getLangBasic().getInt4(), "leIterator");
	forBody = core::transform::replaceAllGen(irMgr, forBody, leIterator, iterVar, /*limitScope=*/true);

	core::ForStmtPtr forStmt = irBuilder.forStmt( iterVar, startVal, endVal, step, forBody);

	core::StatementList forTaskStmts;
	forTaskStmts.insert(forTaskStmts.end(), decls.begin(), decls.end());
	forTaskStmts.insert(forTaskStmts.end(), initLoopLinks.begin(), initLoopLinks.end());
	forTaskStmts.push_back(forStmt);

	core::CompoundStmtPtr forEachTaskBody = irBuilder.compoundStmt(forTaskStmts);

	core::ExpressionPtr forEachTask = irBuilder.createCallExprFromBody(forEachTaskBody, irBuilder.getLangBasic().getUnit(), /*lazy=*/false);

	//handle constraints and properties - attached as annotation
	auto properties = CONVERT_PROPERTIES(node->properties, context);
	auto constraints = CONVERT_CONSTRAINTS(node->constraints, context);

	//TODO attach to correct IR node -> currently attached to CallExpr
	if(!constraints.empty()) {
		// attach constraints as annotation to the generated taskExpr
		annotations::iwir::attachConstraintMap(forEachTask, constraints);
	}
	if(!properties.empty()) {
		// attach property as annotation to the generated taskExpr
		annotations::iwir::attachPropertyMap(forEachTask, properties);
	}

	VLOG(2) << dumpPretty(forEachTask);
	taskCache[node] = forEachTask;
}

CONVERTER(ParallelForEachTask) {
	VLOG(2) << "ParallelForEachTask" << node->name;
	
	ConversionContext innerContext;
	convert(node->body, innerContext);

	convert(node->inputPorts, context);
	//NOTE: ParallelForEach has no unionPort
	//NOTE: ParallelForEach has no loopPort
	
	//NOTE: outputPorts of ParallelFor need to be of collection type - handled in PortConverter
	convert(node->outputPorts, context);

	convert(node->loopElements, context);	

	convert(node->links, context);

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
	//			loop-links[outputPorts] //output ports need to be collection
	//		}
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

			//get call to task
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

		if(VLOG_IS_ON(2)) {
			linkCollector.printToDotFile(node->name+".dot");
		}
	}

	core::ExpressionPtr startVal = irBuilder.intLit(0);
	core::ExpressionPtr endVal;
	if(node->loopElements->elements.size() == 1) {
		//we only need to iterate over one collection
		Port* loopElementsNode = node->loopElements->elements[0];
		auto le = varMap.find( {node, loopElementsNode} );
		assert(le!=varMap.end());
		core::VariablePtr leVar = le->second;
		
		map<string, core::NodePtr> symbols;
		symbols["leVar"] = leVar;
		symbols["size"] = irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getRefCollectionSize();
		//endVal is set to loopElements.length -- IR: endVal = leVar.size;
		endVal = irBuilder.parseExpr("size(leVar)", symbols);
	} else if(node->loopElements->elements.size() > 1) {
		//we need to iterate over multiple collections
		//endvalue is length of "shortest" 
		//IR: findShortestLoopElement(loopElements le1, ...) -> int<4> {
		//	...
		//	return biggestLoopElement->size;
		//}
		typedef vector<insieme::core::ExpressionPtr> ExpressionList;
		ExpressionList elements;
		for_each(node->loopElements->begin(),node->loopElements->end(), 
			[&](Port* le) {
				auto fit = varMap.find( {node, le} );
				assert(fit!=varMap.end());
				core::VariablePtr leVar = fit->second;
				elements.push_back(leVar);
			}
		);

		auto shortestCollection= irMgr.getLangExtension<iwir::extension::CollectionTypeExtension>().getShortestCollection();
		
		//endVal is set to "shortest" loopElements.length 
		endVal = irBuilder.callExpr(
			shortestCollection,
			core::encoder::toIR<ExpressionList,core::encoder::DirectExprListConverter>(irMgr, elements)
		);
	} else {
		assert(false && "not implemented");
	}

	core::ExpressionPtr step = irBuilder.intLit(1); 

	core::VariablePtr iterVar = irBuilder.variable(irBuilder.getLangBasic().getInt4());

	core::CompoundStmtPtr forBody = irBuilder.compoundStmt(stmts);
	assert(forBody);

	//replace "leIterator" literal in forBody with iterator variable
	//NOTE when we see a link for a loopElement we create a "leIterator" literal in Link-converter
	core::LiteralPtr leIterator = irBuilder.literal(irBuilder.getLangBasic().getInt4(), "leIterator");
	forBody = core::transform::replaceAllGen(irMgr, forBody, leIterator, iterVar, /*limitScope=*/true);

	core::ForStmtPtr forStmt = irBuilder.forStmt( iterVar, startVal, endVal, step, forBody);

	core::ExpressionPtr pFor = irBuilder.pfor(forStmt);	

	core::StatementList forTaskStmts;
	forTaskStmts.insert(forTaskStmts.end(), decls.begin(), decls.end());
	forTaskStmts.push_back(pFor);

	core::CompoundStmtPtr forTaskBody = irBuilder.compoundStmt(forTaskStmts);

	core::ExpressionPtr parallelForEachTask = irBuilder.createCallExprFromBody(forTaskBody, irBuilder.getLangBasic().getUnit(), /*lazy=*/false);
	
	//handle constraints and properties - attached as annotation
	auto properties = CONVERT_PROPERTIES(node->properties, context);
	auto constraints = CONVERT_CONSTRAINTS(node->constraints, context);

	//TODO attach to correct IR node -> currently attached to CallExpr
	if(!constraints.empty()) {
		// attach constraints as annotation to the generated taskExpr
		annotations::iwir::attachConstraintMap(parallelForEachTask, constraints);
	}
	if(!properties.empty()) {
		// attach property as annotation to the generated taskExpr
		annotations::iwir::attachPropertyMap(parallelForEachTask, properties);
	}

	VLOG(2) << dumpPretty(parallelForEachTask);
	taskCache[node] = parallelForEachTask;
}

} // namespace iwir end
} // namespace insieme end
