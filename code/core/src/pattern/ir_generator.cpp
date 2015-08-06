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

#include "insieme/core/pattern/ir_generator.h"

#include "insieme/core/transform/simplify.h"

namespace insieme {
namespace core {
namespace pattern {
namespace generator {
namespace irg {

MatchExpressionPtr range(int start, int end) {
	return std::make_shared<impl::expression::Constructor<ptr_target>>([=](const Match<ptr_target>& match)->MatchValue<ptr_target> {
		core::NodeManager& manager = match.getRoot()->getNodeManager();
		core::IRBuilder builder(manager);
		
		vector<MatchValue<ptr_target>> res;
		for(int i=start; i<end; i++) {
			core::NodePtr expr = builder.stringValue(toString(i));
			res.push_back(MatchValue<ptr_target>(expr));
		}
		
		return res;
	}, format("[%d,..,%d)", start, end));
}


TreeGenerator add(const TreeGenerator& a, const TreeGenerator& b) {

	impl::NodeMatchExpressionPtr matchExpression = std::make_shared<impl::expression::Combine<ptr_target>>(
	toVector(a,b), [](const vector<core::NodePtr>& args)->MatchValue<ptr_target> {
	
		assert_eq(args.size(), 2u);
		core::ExpressionPtr a = dynamic_pointer_cast<core::ExpressionPtr>(args[0]);
		core::ExpressionPtr b = dynamic_pointer_cast<core::ExpressionPtr>(args[1]);
		assert(a && b);
		
		core::IRBuilder builder(a->getNodeManager());
		core::NodePtr res = builder.add(a, b);
		return MatchValue<ptr_target>(res);
		
	}, "add");
	
	return TreeGenerator(std::make_shared<impl::tree::Expression>(matchExpression));
}

TreeGenerator sub(const TreeGenerator& a, const TreeGenerator& b) {

	impl::NodeMatchExpressionPtr matchExpression = std::make_shared<impl::expression::Combine<ptr_target>>(
	toVector(a,b), [](const vector<core::NodePtr>& args)->MatchValue<ptr_target> {
	
		assert_eq(args.size(), 2u);
		core::ExpressionPtr a = dynamic_pointer_cast<core::ExpressionPtr>(args[0]);
		core::ExpressionPtr b = dynamic_pointer_cast<core::ExpressionPtr>(args[1]);
		assert(a && b);
		
		core::IRBuilder builder(a->getNodeManager());
		core::NodePtr res = builder.sub(a,b);
		return MatchValue<ptr_target>(res);
		
	}, "sub");
	
	return TreeGenerator(std::make_shared<impl::tree::Expression>(matchExpression));
}

TreeGenerator mul(const TreeGenerator& a, const TreeGenerator& b) {

	impl::NodeMatchExpressionPtr matchExpression = std::make_shared<impl::expression::Combine<ptr_target>>(
	toVector(a,b), [](const vector<core::NodePtr>& args)->MatchValue<ptr_target> {
	
		assert_eq(args.size(), 2u);
		core::ExpressionPtr a = dynamic_pointer_cast<core::ExpressionPtr>(args[0]);
		core::ExpressionPtr b = dynamic_pointer_cast<core::ExpressionPtr>(args[1]);
		assert(a && b);
		
		core::IRBuilder builder(a->getNodeManager());
		core::NodePtr res = builder.mul(a, b);
		return MatchValue<ptr_target>(res);
		
	}, "mul");
	
	return TreeGenerator(std::make_shared<impl::tree::Expression>(matchExpression));
}

TreeGenerator div(const TreeGenerator& a, const TreeGenerator& b) {

	impl::NodeMatchExpressionPtr matchExpression = std::make_shared<impl::expression::Combine<ptr_target>>(
	toVector(a,b), [](const vector<core::NodePtr>& args)->MatchValue<ptr_target> {
	
		assert_eq(args.size(), 2u);
		core::ExpressionPtr a = dynamic_pointer_cast<core::ExpressionPtr>(args[0]);
		core::ExpressionPtr b = dynamic_pointer_cast<core::ExpressionPtr>(args[1]);
		assert(a && b);
		
		core::IRBuilder builder(a->getNodeManager());
		core::NodePtr res = builder.div(a,b);
		return MatchValue<ptr_target>(res);
		
	}, "div");
	
	return TreeGenerator(std::make_shared<impl::tree::Expression>(matchExpression));
}

TreeGenerator mod(const TreeGenerator& a, const TreeGenerator& b) {

	impl::NodeMatchExpressionPtr matchExpression = std::make_shared<impl::expression::Combine<ptr_target>>(
	toVector(a,b), [](const vector<core::NodePtr>& args)->MatchValue<ptr_target> {
	
		assert_eq(args.size(), 2u);
		core::ExpressionPtr a = dynamic_pointer_cast<core::ExpressionPtr>(args[0]);
		core::ExpressionPtr b = dynamic_pointer_cast<core::ExpressionPtr>(args[1]);
		assert(a && b);
		
		core::IRBuilder builder(a->getNodeManager());
		core::NodePtr res = builder.mod(a,b);
		return MatchValue<ptr_target>(res);
		
	}, "mod");
	
	return TreeGenerator(std::make_shared<impl::tree::Expression>(matchExpression));
}

TreeGenerator min(const TreeGenerator& a, const TreeGenerator& b) {

	impl::NodeMatchExpressionPtr matchExpression = std::make_shared<impl::expression::Combine<ptr_target>>(
	toVector(a,b), [](const vector<core::NodePtr>& args)->MatchValue<ptr_target> {
	
		assert_eq(args.size(), 2u);
		core::ExpressionPtr a = dynamic_pointer_cast<core::ExpressionPtr>(args[0]);
		core::ExpressionPtr b = dynamic_pointer_cast<core::ExpressionPtr>(args[1]);
		assert(a && b);
		
		core::IRBuilder builder(a->getNodeManager());
		core::NodePtr res = builder.min(a,b);
		return MatchValue<ptr_target>(res);
		
	}, "min");
	
	return TreeGenerator(std::make_shared<impl::tree::Expression>(matchExpression));
}

TreeGenerator max(const TreeGenerator& a, const TreeGenerator& b) {

	impl::NodeMatchExpressionPtr matchExpression = std::make_shared<impl::expression::Combine<ptr_target>>(
	toVector(a,b), [](const vector<core::NodePtr>& args)->MatchValue<ptr_target> {
	
		assert_eq(args.size(), 2u);
		core::ExpressionPtr a = dynamic_pointer_cast<core::ExpressionPtr>(args[0]);
		core::ExpressionPtr b = dynamic_pointer_cast<core::ExpressionPtr>(args[1]);
		assert(a && b);
		
		core::IRBuilder builder(a->getNodeManager());
		core::NodePtr res = builder.max(a,b);
		return MatchValue<ptr_target>(res);
		
	}, "max");
	
	return TreeGenerator(std::make_shared<impl::tree::Expression>(matchExpression));
}



TreeGenerator simplify(const TreeGenerator& a) {

	impl::NodeMatchExpressionPtr matchExpression = std::make_shared<impl::expression::Combine<ptr_target>>(
	toVector(a), [](const vector<core::NodePtr>& args)->MatchValue<ptr_target> {
	
		assert_eq(args.size(), 1u);
		core::ExpressionPtr a = dynamic_pointer_cast<core::ExpressionPtr>(args[0]);
		assert_true(a);
		
		// simplify
		return transform::simplify(a->getNodeManager(), a.as<NodePtr>());
		
	}, "simplify");
	
	return TreeGenerator(std::make_shared<impl::tree::Expression>(matchExpression));
}


} // end namespace irg
} // end namespace generator
} // end namespace pattern
} // end namespace core
} // end namespace insieme
