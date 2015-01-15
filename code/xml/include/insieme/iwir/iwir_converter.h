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

#include "insieme/utils/container_utils.h"
//Semantic checks
#include "insieme/core/checks/full_check.h"

#include "insieme/iwir/iwir_builder.h"
#include "insieme/iwir/iwir_ast.h"
#include "insieme/iwir/iwir_extension.h"
#include "insieme/iwir/iwir_taskgraph.h"

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

public:
	typedef map<pair<Task*, Port*>, core::VariablePtr> VarMap;	// ((Parent)Task*,Port*) :  IR_Variable
	typedef map<Task*, list<core::StatementPtr>> DeclVarMap;	// (Parent)Task* : IR_DeclStmts)
	typedef map<Link*, core::StatementPtr> LinkStmtMap;			// Link* : IR_LinkStmt
	typedef map<Task*, core::ExpressionPtr> TaskCache;			// Task* : IR_LambdaExpr

private :
	core::NodeManager& irMgr;
	const core::IRBuilder irBuilder;

	//For every task with links
	struct ConversionContext : public boost::noncopyable , insieme::utils::Printable {
		//map of channels for ports used in blockscope
		typedef map<pair<BlockScope*, Port*>, string> BlockScopeChannelMap;	// (BlockScope*, Port*) : IR_Channel
		BlockScopeChannelMap bsChannelMap;
		
		//map of used variable per task/port
		VarMap taskVarMap;
		
		//map of declared variable per task
		DeclVarMap declMap;

		//TODO map of links and the linking ir stmt
		LinkStmtMap linkStmtMap;

		std::ostream& printTo(std::ostream& out) const { 
			out << "ConversionContext( \n";
			out << "taskVars \t [ " << taskVarMap << "]\n"; 
			out << "decls \t [ " << declMap << "]\n";
			out << "linkStmts \t [ " << linkStmtMap << "]\n";
			out << ")\n";
			return out;
		}
	};

	//map port to used variable for the workflow
	VarMap varMap;

	//mapping ir-expr to task
	TaskCache taskCache;

public:
	IWIRConverter(insieme::core::NodeManager& irMgr) : irMgr(irMgr), irBuilder(irMgr) {}

	//generates a dotfile out of the given workflow
	void writeTaskGraphToDot(const IWIRBuilder& ib) {
		iwir::utils::TaskGraph taskGraph;
		taskGraph.fillGraph(ib.getTopLevel());
		taskGraph.writeDotFile(ib.getWFName());
	}

	//main entry
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

		writeTaskGraphToDot(ib);
		return insieme::core::NodePtr();
	}
	
private:

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

			DISPATCH(SimpleType)
			DISPATCH(CollectionType)

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

	#define DECLARE_CONVERTER(name) void convert ## name(name* node, ConversionContext& context);
	#define DECLARE_TYPE_CONVERTER(name) core::TypePtr convert ## name( name* node, ConversionContext& context);
	#define DECLARE_CONDITION_CONVERTER(name) core::ExpressionPtr convert ## name(name* node, ConversionContext& context);
	#define DECLARE_LOOPCOUNTER_CONVERTER(name) core::ExpressionPtr convert ## name( name* node, ConversionContext& context);

	DECLARE_CONVERTER(Links)
	DECLARE_CONVERTER(Link)

	DECLARE_CONVERTER(Ports)
	DECLARE_CONVERTER(Port)

	DECLARE_CONVERTER(Properties)
	DECLARE_CONVERTER(Property)

	DECLARE_CONVERTER(Constraints)
	DECLARE_CONVERTER(Constraint)

	DECLARE_TYPE_CONVERTER(TaskType)
	DECLARE_TYPE_CONVERTER(Type)
	DECLARE_TYPE_CONVERTER(SimpleType)
	DECLARE_TYPE_CONVERTER(CollectionType)

	DECLARE_CONVERTER(Tasks)

	DECLARE_CONVERTER(AtomicTask)
	DECLARE_CONVERTER(BlockScope)
	DECLARE_CONVERTER(IfTask)
	DECLARE_CONVERTER(ForTask)
	DECLARE_CONVERTER(ForEachTask)
	DECLARE_CONVERTER(ParallelForTask)
	DECLARE_CONVERTER(ParallelForEachTask)
	DECLARE_CONVERTER(WhileTask)

	DECLARE_CONDITION_CONVERTER(Condition)
	DECLARE_LOOPCOUNTER_CONVERTER(LoopCounter)

	#undef DECLARE_CONVERTER
	#undef DECLARE_TYPE_CONVERTER
	#undef DECLARE_CONDITION_CONVERTER
	#undef DECLARE_LOOPCOUNTER_CONVERTER
};

} // namespace iwir end
