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

// Expection which is thrown when a particular tree is defined to be not 
// a static control part. This exception has to be forwarded until the 
// root containing this node which has to be defined as a non ScopRegion
//
// Because this exception is only used within the implementation of the ScopRegion
// visitor, it is defined in the anonymous namespace and therefore not visible
// outside this translation unit.
//
class NotASCoP : public std::exception {
	const NodePtr& root;
public:
	NotASCoP( const NodePtr& root ) : std::exception(), root(root) { }

	virtual const char* what() const throw() { 
		std::ostringstream ss;
		ss << "Node '" << *root << "' is not a Static Control Part";
		return ss.str().c_str();
	}

	virtual ~NotASCoP() throw() { }
};

ConstraintCombinerPtr extractFromCondition(IterationVector& iv, const ExpressionPtr& cond) {

	NodeManager& mgr = cond->getNodeManager();
    if (cond->getNodeType() == NT_CallExpr) {
    	const CallExprPtr& callExpr = static_pointer_cast<const CallExpr>(cond);
        if ( mgr.basic.isIntCompOp(callExpr->getFunctionExpr()) || 
	  		 mgr.basic.isUIntCompOp(callExpr->getFunctionExpr()) ) 
		{
			assert(callExpr->getArguments().size() == 2 && "Malformed expression");

			// First of all we check whether this condition is a composed by
			// multiple conditions connected through || or && statements 
			BasicGenerator::Operator&& op = 
				mgr.basic.getOperator( static_pointer_cast<const Literal>(callExpr->getFunctionExpr()) ); 

			switch (op) {
			case BasicGenerator::LOr:
			case BasicGenerator::LAnd:
				{
					ConstraintCombinerPtr&& lhs = extractFromCondition(iv, callExpr->getArgument(0));
					ConstraintCombinerPtr&& rhs = extractFromCondition(iv, callExpr->getArgument(1));

					if (op == BasicGenerator::LAnd)		return makeConjunction(lhs, rhs);
					else 								return makeDisjunction(lhs, rhs);
				}
			case BasicGenerator::LNot:
				 return std::make_shared<NegatedConstraintCombiner>( 
						 extractFromCondition(iv, callExpr->getArgument(0))
					);
			default:
				break;
			}
			// A constraints is normalized having a 0 on the right side,
			// therefore we build a temporary expression by subtracting the rhs
			// to the lhs, Example: 
			//
			// if (a<b) { }    ->    if( a-b<0 ) { }
			try {
				ASTBuilder builder(mgr);
				ExpressionPtr&& expr = builder.callExpr( 
						mgr.basic.getSignedIntSub(), callExpr->getArgument(0), callExpr->getArgument(1) 
					);
				AffineFunction af(iv, expr);
				// Determine the type of this constraint
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
				return std::make_shared<RawConstraintCombiner>( Constraint(af, type) );

			} catch (const NotAffineExpr& e) { throw NotASCoP(cond); }
		}
	}
	assert(false);
}

/**
 * This visitor detects if an expression contains the use of an array. 
 * This is accomplished looking for subscript expressions. 
 *
 * Because a generic expression can use several arrays, the analysis must be
 * able to detect all the array references and for each of them return
 * information about the dimensionality and a pointer to expression utilized to
 * access the independent dimensions. 
 */
struct DetectArrayUse : public ASTVisitor<> {
	
// TODO: do me	

};

} // end namespace anonymous 

namespace insieme {
namespace analysis {
namespace scop {

using namespace core;
using namespace poly;

//===== ScopRegion ===============================================================
const string ScopRegion::NAME = "SCoPAnnotation";
const utils::StringKey<ScopRegion> ScopRegion::KEY("SCoPAnnotationKey");

const std::string ScopRegion::toString() const {
	std::ostringstream ss;
	ss << "IterationVector: " << iterVec << "\\n";
	if (constraints) {
		ss << "Constraints: " << *constraints;
	}
	if (!subScops.empty()) {
		ss << " SubScops: " << subScops.size();
	}
	return ss.str();
}

//===== AccessFunction ============================================================
const string AccessFunction::NAME = "AccessFuncAnn";
const utils::StringKey<AccessFunction> AccessFunction::KEY("AccessFuncAnnKey");

const std::string AccessFunction::toString() const {
	std::ostringstream ss;
	ss << "IV: " << iterVec << ", CONS: " << eqCons;
	return ss.str();
}


//===== ScopVisitor ===============================================================

class ScopVisitor : public core::ASTVisitor<IterationVector> {
	ScopList& scopList;

	ScopRegion::StmtList subScops;

public:
	ScopVisitor(ScopList& scopList) : ASTVisitor<IterationVector>(false), scopList(scopList) { }

	IterationVector visitIfStmt(const IfStmtPtr& ifStmt) {
		IterationVector ret, saveThen, saveElse;
		ScopRegion::StmtList thenScops, elseScops;
		bool isThenSCOP = true, isElseSCOP = true;

		try {
			subScops.clear();
			// check the then body
			ret = merge(ret, visit(ifStmt->getThenBody()));
			// save the sub scops of the then body
			thenScops = subScops;
			saveThen = ret;
		} catch (const NotASCoP& e) { isThenSCOP = false; }

		try {
			subScops.clear();
			// check the else body
			ret = merge(ret, visit(ifStmt->getElseBody()));
			// save the sub scops of the else body
			elseScops = subScops; 
			saveElse = ret;
		} catch (const NotASCoP& e) { isElseSCOP = false; }
	
		if ( isThenSCOP && !isElseSCOP ) {
			// then is a root of a ScopRegion, we add it to the list of scops
			scopList.push_back( std::make_pair(ifStmt->getThenBody(), saveThen) );
		}

		if ( !isThenSCOP && isElseSCOP ) {
			// else is a root of a ScopRegion, we add it to the list of scops
			scopList.push_back( std::make_pair(ifStmt->getElseBody(), saveElse) );
		}

		subScops.clear();
		// if either one of the branches is not a ScopRegion it means the if stmt is
		// not a ScopRegion, therefore we can rethrow the exeption and invalidate
		// this region
		if (!(isThenSCOP && isElseSCOP)) { throw NotASCoP(ifStmt); }

		// check the condition expression
		ConstraintCombinerPtr&& comb = extractFromCondition(ret, ifStmt->getCondition());
		
		// if no exception has been thrown we are sure the sub else and then
		// tree are ScopRegions, therefore this node can be marked as SCoP as well.
		ifStmt->getThenBody()->addAnnotation( std::make_shared<ScopRegion>(ret, comb, thenScops) ); 
		subScops.push_back( ifStmt->getThenBody() );
		
		// the else body is annotated with the negated domain
		ifStmt->getElseBody()->addAnnotation( std::make_shared<ScopRegion>(ret, negate(comb), elseScops) );
		subScops.push_back( ifStmt->getElseBody() );
		return ret;
	}

	IterationVector visitForStmt(const ForStmtPtr& forStmt) {
		subScops.clear();
		IterationVector&& bodyIV = visit(forStmt->getBody());
		
		IterationVector ret;

		const DeclarationStmtPtr& decl = forStmt->getDeclaration();
		ret.add( Iterator(decl->getVariable()) ); 
		
		NodeManager& mgr = forStmt->getNodeManager();
		ASTBuilder builder(mgr);
		
		ConstraintCombinerPtr cons; 
		try {
			// We assume the IR loop semantics to be the following: 
			// i: lb...ub:s 
			// which spawns a domain: lw <= i < ub exists x in Z : lb + x*s = i
			// Check the lower bound of the loop
			AffineFunction lb(ret, 
					builder.callExpr(mgr.basic.getSignedIntSub(), decl->getVariable(), decl->getInitialization())	
				);

			// check the upper bound of the loop
			AffineFunction ub(ret, 
					builder.callExpr(mgr.basic.getSignedIntSub(), decl->getVariable(), forStmt->getEnd())
				);
			// set the constraint: iter >= lb && iter < ub
			cons = makeConjunction( 
					makeCombiner(Constraint(lb, Constraint::GE)), makeCombiner(Constraint(ub, Constraint::LT)) 
				);
			ret = merge(ret,bodyIV);

			forStmt->addAnnotation( std::make_shared<ScopRegion>(ret, cons, subScops) ); 

			subScops.clear();
			// add this statement as a subscope
			subScops.push_back(forStmt);

		} catch (const NotAffineExpr& e) { 
			// one of the expressions are not affine constraints, therefore we
			// set this loop to be a non ScopRegion
			throw NotASCoP(forStmt);
		}
		return ret;
	}

	// While stmts cannot be represented in the polyhedral form (at least in
	// the general case). In the future we may develop a more advanced analysis
	// capable of representing while loops in the polyhedral model 
	IterationVector visitWhileStmt(const WhileStmtPtr& whileStmt) { throw NotASCoP( whileStmt ); }

	IterationVector visitCompoundStmt(const CompoundStmtPtr& compStmt) {
		IterationVector ret;
		bool isSCOP = true;
		
		ScopRegion::StmtList scops;

		for_each(compStmt->getStatements().cbegin(), compStmt->getStatements().cend(), 
			[&](const StatementPtr& cur) { 
				try {
					// clear Sub scops
					subScops.clear();
					ret = merge(ret, this->visit(cur));	
					// copy the sub spawned scops 
					std::copy(subScops.begin(), subScops.end(), std::back_inserter(scops));
				} catch(const NotASCoP& e) { isSCOP = false; }
			} 
		);

		// make the SCoPs available for the parent node 	
		subScops = scops;

		if (!isSCOP) { 
			// one of the statements in this compound statement broke a ScopRegion
			// therefore we add to the scop list the roots for valid ScopRegions
			// inside this compound statement 
			std::for_each(compStmt->getStatements().cbegin(), compStmt->getStatements().cend(), 
				[&](const StatementPtr& cur) { 
					if (cur->hasAnnotation(ScopRegion::KEY)) { 
						scopList.push_back( 
							std::make_pair(cur, cur->getAnnotation(ScopRegion::KEY)->getIterationVector()) 
						); 
					}
				} 
			);

			throw NotASCoP(compStmt); 
		}
		return ret;
	}

	IterationVector visitLambda(const LambdaPtr& lambda) {	
		IterationVector&& bodyIV = visit(lambda->getBody());
		lambda->addAnnotation( std::make_shared<ScopRegion>(bodyIV) );
		return bodyIV;
	}

	IterationVector visitCallExpr(const CallExprPtr& callExpr) { 
		const NodeManager& mgr = callExpr->getNodeManager();
		if (core::analysis::isCallOf(callExpr, mgr.basic.getRefAssign())) {
			// we have to check whether assignments to iterators or parameters
			// occurrs
				
		}

		if (core::analysis::isCallOf(callExpr, mgr.basic.getArraySubscript1D()) ||
			core::analysis::isCallOf(callExpr, mgr.basic.getArrayRefElem1D()) || 
			core::analysis::isCallOf(callExpr, mgr.basic.getVectorRefElem()) ) 
		{
			// This is a subscript expression and therefore the function
			// used to access the array has to analyzed to check if it is an
			// affine linear function. If not, then this branch is not a ScopRegion
			// and all the code regions embodying this statement has to be
			// marked as non-ScopRegion. 
			assert(callExpr->getArguments().size() == 2 && 
					"Subscript expression with more than 2 arguments.");
			IterationVector&& subIV = visit(callExpr->getArgument(0));
		
			IterationVector it;
			try {
				AffineFunction af(it, callExpr->getArgument(1));
					callExpr->getArgument(1)->addAnnotation( 
						std::make_shared<AccessFunction>( it, EqualityConstraint(af) ) 
					);
			} catch(const NotAffineExpr& e) { throw NotASCoP(callExpr); }

			it = merge(subIV, it);	
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

	// Generic method which recursively visit IR nodes and merges the resulting 
	// iteration vectors 
	IterationVector visitNode(const NodePtr& node) {
		const Node::ChildList& cl = node->getChildList();
		IterationVector ret;
		std::for_each( cl.begin(), cl.end(), [&](const NodePtr& cur) { 
			ret = merge(ret, this->visit(cur)); 
		} );
		return ret;
	}
	
};

ScopList mark(const core::NodePtr& root) {
	ScopList ret;
	ScopVisitor sv(ret);
	try {
		IterationVector iterVec = sv.visit(root);
		// if no exception was thrown it means the entire region is a ScopRegion	
		// therefore we add this node to the Scop list
		ret.push_back( std::make_pair(root, iterVec) );	
	} catch (const NotASCoP& e) { }

	return ret;     
}

} // end namespace scop
} // end namespace analysis
} // end namespace insieme

