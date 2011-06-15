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

#include "insieme/analysis/scop.h"
#include "insieme/analysis/polyhedral.h"

#include "insieme/core/ast_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/basic.h"

namespace insieme {
namespace analysis {
namespace scop {

const string SCoP::NAME = "SCoPAnnotation";
const utils::StringKey<SCoP> SCoP::KEY("SCoPAnnotationKey");

using namespace core;
using namespace poly;

class ScopVisitor : public core::ASTVisitor<IterationVector> {

	ScopList& scopList;
	core::NodePtr scopBeign;

public:
	ScopVisitor(ScopList& scopList) : ASTVisitor<IterationVector>(false), scopList(scopList) { }

	IterationVector visitIfStmt(const IfStmtPtr& ifStmt) {
		IterationVector ret;
		bool isSCoP = true;
		std::shared_ptr<NotAffineExpr> except; 

		try {
			// check the then body
			ret = merge(ret, visit(ifStmt->getThenBody()));
		} catch (const NotAffineExpr& e) { 
			isSCoP = false; 
			// save the exception to rethrow it
			except = std::make_shared<NotAffineExpr>(e); 
		}

		try {
		// check the else body
			ret = merge(ret, visit(ifStmt->getElseBody()));
		} catch (const NotAffineExpr& e) { 
			isSCoP = false; 
			// save the exception to rethrow it
			except = std::make_shared<NotAffineExpr>(e); 
		}
	
		// if either one of the branches is not a SCoP it means the if stmt is
		// not a SCoP, therefore we can rethrow the exeption and invalidate
		// this region
		if (!isSCoP) { assert(except); throw *except; }

		// check the condition expression
		ret = merge(ret, visitExpression(ifStmt->getCondition()));
		// FIXME: condition expression creates new Constrain in the domain,
		// therefore is needs special handling!!!
		
		// if no exception has been thrown we are sure the sub else and then
		// tree are SCoPs, therefore this node can be marked as SCoP as well.
		ifStmt->addAnnotation( std::make_shared<SCoP>( ret ) );
		return ret;
	}

	IterationVector visitForStmt(const ForStmtPtr& forStmt) {
		IterationVector&& bodyIV = visit(forStmt->getBody());
		
		return IterationVector();
	}

	IterationVector visitCompoundStmt(const CompoundStmtPtr& compStmt) {
		IterationVector ret;
		for_each(compStmt->getStatements().cbegin(), compStmt->getStatements().cend(), 
			[&](const StatementPtr& cur) { ret = merge(this->visit(cur), ret);	} );
		// Marks this compund statement with the iteration vector created from
		// its body
		compStmt->addAnnotation( std::make_shared<SCoP>(ret) );
		return ret;
	}

	IterationVector visitLambda(const LambdaPtr& lambda) {	return visit(lambda->getBody()); }

	IterationVector visitCallExpr(const CallExprPtr& callExpr) { 
		const NodeManager& mgr = callExpr->getNodeManager();

		if (core::analysis::isCallOf(callExpr, mgr.basic.getRefAssign())) {
			// we have to check whether assignments to iterators or parameters
			// exist in this region
		}

		if (core::analysis::isCallOf(callExpr, mgr.basic.getArraySubscript1D()) ||
			core::analysis::isCallOf(callExpr, mgr.basic.getArrayRefElem1D()) ) 
		{
			// This is a subscript expression and therefore the function
			// used to access the array has to analyzed to check if it is an
			// affine linear function. If not, then this branch is not a SCoP
			// and all the code regions embodying this statement has to be
			// marked as non-SCoP. 
			assert(callExpr->getArguments().size() == 2 && "Subscript expression with more than 2 arguments.");
			IterationVector&& subIV = visit(callExpr->getArgument(0));

			IterationVector it;
			AffineFunction af(it, callExpr->getArgument(1));
			it = merge(subIV, it);

			callExpr->getArgument(1)->addAnnotation( std::make_shared<SCoP>(it) );
			return it;
		}

		IterationVector ret;
		const std::vector<ExpressionPtr>& args = callExpr->getArguments();
		std::for_each(args.begin(), args.end(), 
				[&](const ExpressionPtr& cur) { ret = merge(this->visit(cur), ret); }	);
		return ret;
	}
	
	IterationVector visitStatement(const StatementPtr& stmt) {
		if (ExpressionPtr&& expr = dynamic_pointer_cast<const Expression>(stmt) ) {
			return visitExpression(expr);
		}
		return IterationVector();
	}

	IterationVector visitExpression(const ExpressionPtr& expr) { return IterationVector(); }

};


ScopList mark(const core::NodePtr& root, bool interproc) {
	ScopList ret;
	ScopVisitor sv(ret);
	try {
		sv.visit(root);
	} catch (const NotAffineExpr& e) { }
	return ret;	
}


} // end namespace scop
} // end namespace analysis
} // end namespace insieme

