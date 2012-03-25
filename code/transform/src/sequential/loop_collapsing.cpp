/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/transform/sequential/loop_collapsing.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_mapper.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/analysis/attributes.h"
#include "insieme/core/parser/type_parse.h"
#include "insieme/core/ir_builder.h"

#include "insieme/analysis/dep_graph.h"
#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/annotation.h"
#include "insieme/utils/timer.h"

namespace insieme {

using namespace std;
using namespace core;
using namespace utils::log;

namespace cl = lang;
namespace us = utils::set;
namespace um = utils::map;
namespace ad = insieme::analysis::dep;
namespace scop = insieme::analysis::polyhedral::scop;
namespace manip = insieme::core::transform;

namespace {
int canCollapse(const ForStmtPtr& outer) {
	ForStmtPtr inner = dynamic_pointer_cast<const ForStmt>(outer->getBody()->getStatement(0));
	if(!inner) { return 0; }
	//LOG(INFO) << "+ Nested for in pfor.";
	if(outer->getBody()->getStatements().size() != 1) { return 0; }
	//LOG(INFO) << "++ Perfectly nested for in pfor.";
	//LOG(INFO) << "+++ Pfor is scop region.";
	auto scopOpt = scop::ScopRegion::toScop(inner);
	if(!scopOpt) { return 0; }
	//LOG(INFO) << "++++ Scop region is valid and resolved.";
	ad::DependenceGraph dg = ad::extractDependenceGraph(inner, ad::WRITE);
	ad::DependenceList dl = dg.getDependencies();
	if(dl.empty()) {
		//LOG(INFO) << "<<<<< Perfectly nested for in for, no dependencies:\n" << printer::PrettyPrinter(outer);
		return 1 + canCollapse(inner);
	} else {
		//LOG(INFO) << "pfor nested for deps:\n=====================================\n";
//		dg.printTo(std::cout);
		//LOG(INFO) << "=====================================\npfor nested for deps end.";
		for(auto it = dl.begin(); it != dl.end(); ++it) {
			ad::DependenceInstance di = *it;
			ad::FormulaList distances = get<0>(get<3>(di));
			core::arithmetic::Formula d0 = distances[0];
			if(!d0.isZero()) {
				return 0;
			}
		}
		//LOG(INFO) << "<<<<< Perfectly nested for in for, no outer loop dependencies:\n" << printer::PrettyPrinter(outer);
		return 1 + canCollapse(inner);
	}
}

ForStmtPtr collapseFor(const ForStmtPtr& outer, int collapseLevels) {
	if(collapseLevels == 0) return outer;
	NodeManager& mgr = outer->getNodeManager();
	IRBuilder build(mgr);
	// determine existing loop characteristics
	ForStmtPtr inner = static_pointer_cast<const ForStmt>(outer->getBody()->getStatement(0));
	TypePtr iteratorType = outer->getIterator()->getType();
	ExpressionPtr os = outer->getStart(), oe = outer->getEnd(), ost = outer->getStep();
	ExpressionPtr is = inner->getStart(), ie = inner->getEnd(), ist = inner->getStep();
	ExpressionPtr oext = build.div(build.sub(oe,os), ost); // outer loop extent
	ExpressionPtr iext = build.div(build.sub(ie,is), ist); // inner loop extent
	// calculate new loop bounds and generate body
	ExpressionPtr newExt = build.mul(oext, iext); // generated loop extent
	VariablePtr newI = build.variable(iteratorType);
	ExpressionPtr oIterReplacement = build.add(build.mul(build.div(newI, iext), ost), os);
	ExpressionPtr iIterReplacement = build.add(build.mul(build.mod(newI, iext), ist), is);
	NodeMap replacements;
	replacements.insert(make_pair(outer.getIterator(), oIterReplacement));
	replacements.insert(make_pair(inner.getIterator(), iIterReplacement));
	StatementPtr newBody = manip::replaceAllGen(mgr, inner.getBody(), replacements);
	return collapseFor(build.forStmt(newI, build.intLit(0), newExt, build.intLit(1), newBody), collapseLevels-1);
}

ForStmtPtr collapseForNest(const ForStmtPtr& outer) {
	ForStmtPtr ret = outer;
	int collapseLevels = canCollapse(ret); 
	if(collapseLevels>0) ret = collapseFor(ret, collapseLevels);
	if(*ret != *outer) {
		LOG(INFO) << "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		<< "\n%%%%%%%%%%% Replaced existing for loop:\n" << printer::PrettyPrinter(outer) 
		<< "\n%%%%%%%%%%% with collapsed loop:\n" << printer::PrettyPrinter(ret)
		<< "\n%%%%%%%%%%% collapased " << collapseLevels << " levels";
	}
	return ret;
}
} // anonymous namespace

namespace transform {
namespace sequential {

	core::NodePtr LoopCollapsing::apply(const core::NodePtr& target) const {
		ForStmtPtr forPtr = dynamic_pointer_cast<ForStmtPtr>(target);
		if(!forPtr) throw InvalidTargetException("Loop collapsing can only be applied to a for loop. Surprise!");
		unsigned maxLevels = canCollapse(forPtr), reqLevels = parameter::getValue<unsigned>(getParameters());
		if(maxLevels < reqLevels) 
			throw InvalidTargetException(format("Loop collapsing only possible up to %u levels, %u levels requested", maxLevels, reqLevels));
		return collapseFor(forPtr, reqLevels);
	}


} // end namespace sequential
} // end namespace transform
} // end namespace insieme
