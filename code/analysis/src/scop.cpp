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

#include "insieme/core/ast_builder.h"

namespace {
using namespace insieme::core;
using namespace insieme::core::lang;
using namespace insieme::analysis::poly;

std::set<Constraint> extractFromCondition(IterationVector& iv, const ExpressionPtr& cond) {

	NodeManager& mgr = cond->getNodeManager();
    if (cond->getNodeType() == NT_CallExpr) {
    	const CallExprPtr& callExpr = static_pointer_cast<const CallExpr>(cond);
        if ( mgr.basic.isIntCompOp(callExpr->getFunctionExpr()) || 
	  		 mgr.basic.isUIntCompOp(callExpr->getFunctionExpr()) ) 
		{
			assert(callExpr->getArguments().size() == 2 && "Malformed expression");

			std::set<Constraint> constraints;

			// First of all we check whether this condition is a composed by
			// multiple conditions connected through || or && statements 
			BasicGenerator::Operator&& op = 
				mgr.basic.getOperator( static_pointer_cast<const Literal>(callExpr->getFunctionExpr()) ); 

			switch (op) {
			case BasicGenerator::LOr:
				assert(false && "Logical OR not supported yet.");
			case BasicGenerator::LAnd:
				{
					std::set<Constraint>&& lhs = extractFromCondition(iv, callExpr->getArgument(0));
					if (op == BasicGenerator::LOr) {
						// because we can only represent conjuctions of
						// constraints, we apply demorgan rule to transform a
						// logical or to be expressed using logical and
						
					}
					std::copy(lhs.begin(), lhs.end(), std::inserter(constraints, constraints.begin()));
					
					std::set<Constraint>&& rhs = extractFromCondition(iv, callExpr->getArgument(1));
					std::copy(rhs.begin(), rhs.end(), std::inserter(constraints, constraints.begin()));
					return constraints;
				}
			default:
				break;
			}
			// A constraints is normalized having a 0 on the right side,
			// therefore we build a temporary expression by subtracting the rhs
			// to the lhs, Example: 
			//
			// if (a<b) { }    ->    if( a-b<0 ) { }
			ASTBuilder builder(mgr);
			ExpressionPtr&& expr = builder.callExpr( 
					mgr.basic.getSignedIntSub(), callExpr->getArgument(0), callExpr->getArgument(1) 
				);
			std::cout << *expr << std::endl;
			AffineFunction af(iv, expr);
			Constraint::Type type;
			switch (op) {
				case BasicGenerator::Eq: type = Constraint::EQ; break;
				case BasicGenerator::Ne: type = Constraint::NE; break;
				case BasicGenerator::Lt: type = Constraint::LT; break;
				case BasicGenerator::Le: type = Constraint::LE; break;
				case BasicGenerator::Gt: type = Constraint::GT; break;
				case BasicGenerator::Ge: type = Constraint::GE; break;
				default:
					assert(false && "Operation not supported!");
			}
			constraints.insert( Constraint(af,type) );
			return constraints;
		}
	}
	assert(false);
}

} // end namespace anonymous 

namespace insieme {
namespace analysis {
namespace scop {

const string SCoP::NAME = "SCoPAnnotation";
const utils::StringKey<SCoP> SCoP::KEY("SCoPAnnotationKey");

using namespace core;
using namespace poly;

const std::string SCoP::toString() const {
	std::ostringstream ss;
	ss << "SCoP {" << std::endl;
	ss << "\tIterationVector: " << iterVec;
	return ss.str();
}

class ScopVisitor : public core::ASTVisitor<IterationVector> {

	ScopList& scopList;
	core::NodePtr scopBeign;
    bool analyzeCond;

public:
	ScopVisitor(ScopList& scopList) : ASTVisitor<IterationVector>(false), scopList(scopList), analyzeCond(false) { }

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
		std::set<Constraint>&& c = extractFromCondition(ret, ifStmt->getCondition());

		// if no exception has been thrown we are sure the sub else and then
		// tree are SCoPs, therefore this node can be marked as SCoP as well.
		ifStmt->addAnnotation( std::make_shared<SCoP>( ret ) );
		return ret;
	}

	IterationVector visitCastExpr(const CastExprPtr& castExpr) {
		return visit(castExpr->getSubExpression());
	}

	IterationVector visitForStmt(const ForStmtPtr& forStmt) {
		IterationVector&& bodyIV = visit(forStmt->getBody());
		
		IterationVector ret;
		const DeclarationStmtPtr& decl = forStmt->getDeclaration();
		ret.add( Iterator(decl->getVariable()) ); 
		
		NodeManager& mgr = forStmt->getNodeManager();
		ASTBuilder builder(mgr);
		
		std::set<Constraint> cons; 
		// We assume the IR loop semantics to be the following: 
		// i: lb...ub:s 
		// which spawns a domain: lw <= i < ub exists x in Z : lb + x*s = i
		ExpressionPtr&& expr = 
			builder.callExpr(mgr.basic.getSignedIntSub(), decl->getVariable(), decl->getInitialization());
		std::cout << *expr << std::endl;
		AffineFunction lb(ret, expr	);
		cons.insert( Constraint(lb, Constraint::GE) );

		AffineFunction ub(ret, 
			builder.callExpr(mgr.basic.getSignedIntSub(), decl->getVariable(), forStmt->getEnd())
		);
		cons.insert( Constraint(ub, Constraint::LT) );
		ret = merge(ret,bodyIV);

		// Add constraint for the step 
		forStmt->addAnnotation( std::make_shared<SCoP>(ret) ); 
		return ret;
	}

	IterationVector visitWhileStmt(const WhileStmtPtr& whileStmt) {
		throw NotAffineExpr( ExpressionPtr() );
	}

	IterationVector visitCompoundStmt(const CompoundStmtPtr& compStmt) {
		IterationVector ret;
		for_each(compStmt->getStatements().cbegin(), compStmt->getStatements().cend(), 
			[&](const StatementPtr& cur) { ret = merge(ret, this->visit(cur));	} );
		// we don't need to mark compound statements as they do not cause
		// changes in terms of iteration domain or constraints 
		compStmt->addAnnotation( std::make_shared<SCoP>(ret) ); 
		return ret;
	}

	IterationVector visitLambda(const LambdaPtr& lambda) {	
		return visit(lambda->getBody()); 
	}

	IterationVector visitCallExpr(const CallExprPtr& callExpr) { 
		const NodeManager& mgr = callExpr->getNodeManager();

		if (core::analysis::isCallOf(callExpr, mgr.basic.getRefAssign())) {
			// we have to check whether assignments to iterators or parameters
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
			std::cout << af << std::endl;
			it = merge(subIV, it);

			callExpr->getArgument(1)->addAnnotation( std::make_shared<SCoP>(it) );
			return it;
		}
		
        IterationVector ret;
		const std::vector<ExpressionPtr>& args = callExpr->getArguments();
		std::for_each(args.begin(), args.end(), 
			[&](const ExpressionPtr& cur) { ret = merge(ret, this->visit(cur)); } );
		return ret;
	}

	IterationVector visitProgram(const ProgramPtr& prog) { 
		for_each(prog->getEntryPoints().cbegin(), prog->getEntryPoints().cend(), 
			[&](const ExpressionPtr& cur) { this->visit(cur); } );
		return IterationVector();
	}

	IterationVector visitLambdaExpr(const LambdaExprPtr& lambdaExpr) {
		return visit(lambdaExpr->getLambda());
	}
	
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

