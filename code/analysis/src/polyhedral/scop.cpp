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

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/backend.h"

// INCLUDE THE BACKENDS 
// this is needed for compiling this class as templates are used and the template specializations
// for Sets and Maps are needed in order to compile this translation unit 
#include "insieme/analysis/polyhedral/backends/isl_backend.h"
#include "isl/flow.h"

#define POLYHEDRAL_BACKEND ISL

#include "insieme/core/ast_visitor.h"
#include "insieme/core/ast_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/numeric_cast.h"
#include <stack>

#define AS_STMT_ADDR(addr) static_address_cast<const Statement>(addr)
#define AS_EXPR_ADDR(addr) static_address_cast<const Expression>(addr)

#define CHECK_STACK_SIZE \
	auto checkPostCond = [&](size_t stackInitialSize) -> void { 	 \
		assert(regionStmts.size() == stackInitialSize);				 \
	};																 \
	FinalActions __check_stack_size( std::bind(checkPostCond, regionStmts.size()) );

namespace {

using namespace insieme::core;
using namespace insieme::core::lang;
using namespace insieme::analysis;
using namespace insieme::analysis::poly;
using namespace insieme::analysis::scop;

/**************************************************************************************************
 * Expection which is thrown when a particular tree is defined to be not a static control part. This
 * exception has to be forwarded until the root containing this node which has to be defined as a
 * non ScopRegion
 * 
 * Because this exception is only used within the implementation of the ScopRegion visitor, it is
 * defined in the anonymous namespace and therefore not visible outside this translation unit.
 *************************************************************************************************/
class NotASCoP : public std::exception {
	NodePtr root;
	std::string msg;
public:
	NotASCoP( const NodePtr& root ) : root(root) { 
		std::ostringstream ss;
		ss << "Node '" << *root << "' is not a Static Control Part";
		msg = ss.str();
	}

	virtual const char* what() const throw() { return msg.c_str(); }

	virtual ~NotASCoP() throw() { }
};

/**************************************************************************************************
 * Extract constraints from a conditional expression. This is used for determining constraints for 
 * if and for statements. 
 *************************************************************************************************/
ConstraintCombinerPtr extractFromCondition(IterationVector& iv, const ExpressionPtr& cond) {

	NodeManager& mgr = cond->getNodeManager();
	assert (cond->getNodeType() == NT_CallExpr);

	const CallExprPtr& callExpr = static_pointer_cast<const CallExpr>(cond);
	if ( mgr.basic.isIntCompOp(callExpr->getFunctionExpr()) || 
		 mgr.basic.isUIntCompOp(callExpr->getFunctionExpr()) ) 
	{
		assert(callExpr->getArguments().size() == 2 && "Malformed expression");

		// First of all we check whether this condition is a composed by multiple conditions
		// connected through || or && statements 
		BasicGenerator::Operator&& op = 
			mgr.basic.getOperator( static_pointer_cast<const Literal>(callExpr->getFunctionExpr()) ); 

		switch (op) {
		case BasicGenerator::LOr:
		case BasicGenerator::LAnd:
			{
				ConstraintCombinerPtr&& lhs = extractFromCondition(iv, callExpr->getArgument(0));
				ConstraintCombinerPtr&& rhs = extractFromCondition(iv, callExpr->getArgument(1));

				if (op == BasicGenerator::LAnd)	{	return lhs and rhs; }
				else 							{	return lhs or rhs; }
			}
		case BasicGenerator::LNot:
			 return not_( extractFromCondition(iv, callExpr->getArgument(0)) );
		default:
			break;
		}
		// A constraints is normalized having a 0 on the right side, therefore we build a
		// temporary expression by subtracting the rhs to the lhs, Example: 
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
			return makeCombiner( Constraint(af, type) );
		} catch (arithmetic::NotAFormulaException&& e) { 
			throw NotASCoP(e.getExpr()); 
		} catch (NotAffineExpr&& e) { 
			throw NotASCoP(cond);
		}
	}
	// LOG(ERROR) << "Condition Expression not supported: " << *cond;
	throw NotASCoP(cond);
}

IterationVector markAccessExpression(const ExpressionPtr& expr) {
	// If the expression is already annotated because shared, then we simply return the iteration
	// vector precendly computed (because by definition it has to be the same)
	if (expr->hasAnnotation(scop::AccessFunction::KEY)) {
		return expr->getAnnotation(scop::AccessFunction::KEY)->getIterationVector();
	}

	// otherwise we have to build an affine function from the IR access expression
	try {
		IterationVector it;
		expr->addAnnotation( std::make_shared<AccessFunction>( it, AffineFunction(it, expr)) );
		return it;
	} catch(NotAffineExpr&& e) { 
		throw NotASCoP(expr);
	} catch(arithmetic::NotAFormulaException&& e) {
		throw NotASCoP(e.getExpr()); 
	}	 
}

//===== ScopVisitor ================================================================================

struct ScopVisitor : public ASTVisitor<IterationVector, Address> {

	ScopList& scopList;
	AddressList subScops;

	// Stack utilized to keep track of statements which are inside a SCoP.
	// because not all the compound statements of the IR are annotated by a SCoPRegion annotation,
	// we need to have a way to collect statements which can be inside nested scopes.
	typedef std::stack<ScopStmtList> RegionStmtStack;
	RegionStmtStack regionStmts;

	ScopVisitor(ScopList& scopList) : 
		ASTVisitor<IterationVector, Address>(false), scopList(scopList) 
	{
		regionStmts.push( RegionStmtStack::value_type() );
	}

	// Visit the body of a SCoP. This requires to collect the iteration vector for the body and
	// eventual ref access statements existing in the body. For each array access we extract the
	// equality constraint used for accessing each dimension of the array and store it in the
	// AccessFunction annotation.
	RefList collectRefs(IterationVector& iterVec, const StatementAddress& body) { 
		
		RefList&& refs = collectDefUse( body.getAddressedNode() );

		// For now we only consider array access. 
		//
		// FIXME: In the future also scalars should be properly handled using technique like scalar
		// arrays and so forth
		std::for_each(refs.arrays_begin(), refs.arrays_end(),
			[&](const ArrayRefPtr& cur) { 
				const ArrayRef::ExpressionList& idxExprs = cur->getIndexExpressions();
				std::for_each(idxExprs.begin(), idxExprs.end(), 
					[&](const ExpressionAddress& cur) { 
						iterVec = merge(iterVec, markAccessExpression(cur));
				});
			});
		return refs;
	}

	IterationVector visitStmt(NodeAddress addr) {
		CHECK_STACK_SIZE
		// Push a new recod 
		regionStmts.push( RegionStmtStack::value_type() );
		{
			// in the case an exception is thrown it makes sure the record allocated on the region stack
			// will be deallocated 
			FinalActions fa( [&] () -> void { regionStmts.pop(); } );
		
			// skip all the marker nodes 
			while ( addr->getNodeType() == NT_MarkerStmt || addr->getNodeType() == NT_MarkerExpr ) {
					addr = addr.getAddressOfChild(0);
				}
			assert(addr->getNodeType() != NT_MarkerStmt && addr->getNodeType() != NT_MarkerExpr);
			IterationVector&& ret = visit(addr);

			// we need to copy this value because we need to keep it alive after the pop operation
			ScopStmtList subStmts = regionStmts.top();
			fa.setEnabled(false);
			regionStmts.pop(); // discard empty node 

			if ( subStmts.empty() ) {
				// if there are no sub stmts it means this statement is a basic statement, 
				// therefore we can add it to the list of region stmts 
				assert(regionStmts.size() >= 1);

				RefList&& refs = collectRefs(ret, AS_STMT_ADDR(addr));
				// Add this statement to the scope for the parent node 
				regionStmts.top().push_back( ScopStmt(AS_STMT_ADDR(addr), refs) );
			} else {
				// the statement has generated a number of sub stmts, this can happen when a
				// compound statement is visited which is not relevant for polyhedral analysis,
				// we copy the subStmts on the current level 
				assert(regionStmts.size() >= 1);
				std::copy(subStmts.begin(), subStmts.end(), std::back_inserter(regionStmts.top()));
			}
			return ret;
		}
	}

	/**********************************************************************************************
	 * Visit of If Statements: 
	 * Visits the then and else body checking whether they are SCoPs. In the case both the branches
	 * are SCoPs the condition is evaluated and a constraint created out of it. Two annotations will
	 * be then created, one with the positive condition and the second one with the negated
	 * condition which will be attached respectively to the then and the else body of the if
	 * statement. In the case one of the two branches is not a SCoP, the SCoP branch is inserted in
	 * the list of root scops (scopList) and the NotAScop exception thrown to the parent node. 
	 *********************************************************************************************/
	IterationVector visitIfStmt(const IfStmtAddress& ifStmt) {

		CHECK_STACK_SIZE

		IterationVector ret, saveThen, saveElse;
		// RefList thenRefs, elseRefs;
		AddressList thenScops, elseScops;
		bool isThenSCOP = true, isElseSCOP = true;

		ExpressionAddress condAddr = AS_EXPR_ADDR(ifStmt.getAddressOfChild(0));
		StatementAddress  thenAddr = AS_STMT_ADDR(ifStmt.getAddressOfChild(1));
		StatementAddress  elseAddr = AS_STMT_ADDR(ifStmt.getAddressOfChild(2));

		regionStmts.push( RegionStmtStack::value_type() );
		FinalActions faThen( [&] () -> void { regionStmts.pop(); } );

		try {
			subScops.clear();
			// check the then body
			saveThen = visitStmt(thenAddr);
			// save the sub scops of the then body
			thenScops = subScops;
		} catch (NotASCoP&& e) { isThenSCOP = false; }

		regionStmts.push( RegionStmtStack::value_type() );
		FinalActions faElse( [&] () -> void { regionStmts.pop(); } );

		try {
			subScops.clear();
			// check the else body
			saveElse = visitStmt(elseAddr);
			// save the sub scops of the else body
			elseScops = subScops; 
		} catch (NotASCoP&& e) { isElseSCOP = false; }
	
		if ( isThenSCOP && !isElseSCOP ) {
			// then is a root of a ScopRegion, we add it to the list of scops
			scopList.push_back( std::make_pair(thenAddr, saveThen) ); // FIXME annotate this nodes?			
		}

		if ( !isThenSCOP && isElseSCOP ) {
			// else is a root of a ScopRegion, we add it to the list of scops
			scopList.push_back( std::make_pair(elseAddr, saveElse) ); // FIXME annotate this nodes?
		}

		subScops.clear();
		// if either one of the branches is not a ScopRegion it means the if statement is not a
		// ScopRegion, therefore we can re-throw the exception and invalidate this region
		if (!(isThenSCOP && isElseSCOP)) {
			throw NotASCoP( ifStmt.getAddressedNode() ); 
		}

		// reset the value of the iteration vector 
		ret = IterationVector();

		// check the condition expression
		ConstraintCombinerPtr&& comb = extractFromCondition(ret, condAddr.getAddressedNode());
	
		// At this point we are sure that both the then, else body are SCoPs and the condition of
		// this If statement is also an affine linear function. 
		ret = merge(ret, merge(saveThen, saveElse));

		// Process the ELSE body because it comes first in the stack of region statements 
		// the else body is annotated with the negated domain
		elseAddr->addAnnotation( 
			std::make_shared<ScopRegion>(saveElse, poly::ConstraintCombinerPtr(), regionStmts.top(), elseScops)
		);
		// we saved the else body statements, therefore we can pop the record we allocated for it
		regionStmts.pop();
		faElse.setEnabled(false);

		// if no exception has been thrown we are sure the sub else and then tree are ScopRegions,
		// therefore this node can be marked as SCoP as well.
		thenAddr->addAnnotation( 
			std::make_shared<ScopRegion>(saveThen, poly::ConstraintCombinerPtr(), regionStmts.top(), thenScops)
		);
		// we saved the then body statements, therefore we can pop the record we allocated for it		
		regionStmts.pop();
		faThen.setEnabled(false);

		ifStmt->addAnnotation( 
			std::make_shared<ScopRegion>(ret, comb, ScopStmtList(), AddressList({thenAddr, elseAddr}))
		);
		
		subScops.push_back( ifStmt );
		
		return ret;
	}

	IterationVector visitForStmt(const ForStmtAddress& forStmt) {
		CHECK_STACK_SIZE
	
		// if we already visited this forStmt, just return the precomputed iteration vector 
		if (forStmt->hasAnnotation(ScopRegion::KEY)) {
			// return the cached value
			return forStmt->getAnnotation(ScopRegion::KEY)->getIterationVector();
		}

		assert(subScops.empty());

		// Create a new scope for region stmts
		regionStmts.push( RegionStmtStack::value_type() );

		{
			// remove element from the stack of statements from all the exit paths 
			FinalActions fa( [&] () -> void { regionStmts.pop(); } );

			IterationVector&& bodyIV = visitStmt( forStmt.getAddressOfChild(3) ), ret;

			const DeclarationStmtPtr& decl = forStmt.getAddressedNode()->getDeclaration();
			ret.add( Iterator(decl->getVariable()) ); 
			
			NodeManager& mgr = forStmt->getNodeManager();
			ASTBuilder builder(mgr);

			ConstraintCombinerPtr cons; 

			try {
				ret = merge(ret, bodyIV);	

				// We assume the IR loop semantics to be the following: 
				// i: lb...ub:s 
				// which spawns a domain: lw <= i < ub exists x in Z : lb + x*s = i
				// Check the lower bound of the loop
				AffineFunction lb(ret, 
						builder.callExpr(mgr.basic.getSignedIntSub(), decl->getVariable(), 
							decl->getInitialization())	
					);

				// check the upper bound of the loop
				AffineFunction ub(ret, builder.callExpr(mgr.basic.getSignedIntSub(), decl->getVariable(), 
							forStmt.getAddressedNode()->getEnd())
						);
				// set the constraint: iter >= lb && iter < ub
				cons = Constraint(lb, Constraint::GE) and Constraint(ub, Constraint::LT);

				forStmt->addAnnotation( std::make_shared<ScopRegion>(ret, cons, regionStmts.top(), subScops) ); 

				// add this statement as a subscop
				subScops.push_back(forStmt);
			
			} catch (NotAffineExpr&& e) { 
				// one of the expressions are not affine constraints, therefore we set this loop to be a
				// non ScopRegion
				throw NotASCoP( forStmt.getAddressedNode() );

			}catch(arithmetic::NotAFormulaException&& e) {
				throw NotASCoP( e.getExpr() ); 
			}	 
			return ret;
		}
	}

	// While stmts cannot be represented in the polyhedral form (at least in the general case). In
	// the future we may develop a more advanced analysis capable of representing while loops in the
	// polyhedral model 
	IterationVector visitWhileStmt(const WhileStmtAddress& whileStmt) { 
		throw NotASCoP( whileStmt.getAddressedNode() );
	}

	IterationVector visitCompoundStmt(const CompoundStmtAddress& compStmt) {
		CHECK_STACK_SIZE;

		IterationVector ret;
		bool isSCOP = true;
		AddressList scops;
		
		if ( compStmt->hasAnnotation(ScopRegion::KEY) ) {
			// if the SCopRegion annotation is already attached, it means we already visited this
			// compoundstmt, therefore we can return the iteration vector already precomputed 
			return compStmt->getAnnotation(ScopRegion::KEY)->getIterationVector();
		}

		for(size_t i=0, end=compStmt->getStatements().size(); i!=end; ++i) {
			// make sure at every iteration the stack size is not growing within this compound stmt
			CHECK_STACK_SIZE
			try {
				// clear Sub scops
				subScops.clear();
				NodeAddress&& nodeAddr = compStmt.getAddressOfChild(i);
				ret = merge(ret, visitStmt( std::move(nodeAddr) ));
				// copy the sub spawned scops 
				std::copy(subScops.begin(), subScops.end(), std::back_inserter(scops));
			} catch(NotASCoP&& e) { isSCOP = false; }
		}

		// make the SCoPs available for the parent node 	
		subScops = scops;

		if (!isSCOP) { 
			// FIXME: Use the subScops 
			// one of the statements in this compound statement broke a ScopRegion therefore we add
			// to the scop list the roots for valid ScopRegions inside this compound statement 
			for(size_t i=0, end=compStmt->getStatements().size(); i!=end; ++i) {
				StatementAddress addr = AS_STMT_ADDR(compStmt.getAddressOfChild(i));
				if (addr->hasAnnotation(ScopRegion::KEY)) { 
					scopList.push_back( 
						std::make_pair(addr, addr->getAnnotation(ScopRegion::KEY)->getIterationVector()) 
					); 
				}
			}
			throw NotASCoP(compStmt.getAddressedNode()); 
		}

		return ret;
	}

	IterationVector visitLambda(const LambdaAddress& lambda) {	
		CHECK_STACK_SIZE

		if ( lambda->hasAnnotation(ScopRegion::KEY) ) {
			// if the SCopRegion annotation is already attached, it means we already visited this
			// function, therefore we can return the iteration vector already precomputed 
			return lambda->getAnnotation(ScopRegion::KEY)->getIterationVector();
		}
		
		IterationVector bodyIV;
		// otherwise we have to visit the body and attach the ScopRegion annotation 
		{
			regionStmts.push( RegionStmtStack::value_type() );
			// remove element from the stack of statements from all the exit paths 
			FinalActions fa( [&] () -> void { regionStmts.pop(); subScops.clear(); } );

			bodyIV = visitStmt( lambda.getAddressOfChild(lambda->getChildList().size()-1) /*getBody()*/ );

			//FIXME
			lambda->addAnnotation( 
					std::make_shared<ScopRegion>(bodyIV, ConstraintCombinerPtr(), regionStmts.top(), subScops) 
				);
		}
		scopList.push_back( std::make_pair(lambda, bodyIV) );
		subScops.push_back( lambda );

		return bodyIV;
	}

	//IterationVector visitCallExpr(const CallExprPtr& callExpr) {
		//// if we have a call to a lambda we have to take care of setting the constraints which
		//// enforce the fact that call expression arguments are assigned to the formal parameters fo
		//// the function. therefore for a call expression f(a,b) of a function int f(int d, int e) 
		//// the two constraints (d == a) and (e == b) needs to be generated 
		//const ExpressionPtr& func = callExpr->getFunctionExpr();

		//IterationVector&& ret = visit(func);
		//if (func->getNodeType() == NT_LambdaExpr) {
			
		//}
	//}

	IterationVector visitProgram(const ProgramAddress& prog) {
		for(size_t i=0, end=prog->getEntryPoints().size(); i!=end; ++i) {
			try { 
				visit( prog.getAddressOfChild(i) ); 
			} catch(NotASCoP&& e) { }
		} 
		return IterationVector();
	}

	// Generic method which recursively visit IR nodes and merges the resulting 
	// iteration vectors 
	IterationVector visitNode(const NodeAddress& node) {
 		IterationVector ret;
		for(size_t i=0, end=node->getChildList().size(); i!=end; ++i) {
			ret = merge(ret, visit(node.getAddressOfChild(i))); 
		} 
		return ret;
	}
	
};

} // end namespace anonymous 

namespace insieme {
namespace analysis {
namespace scop {

using namespace core;
using namespace poly;

//===== ScopRegion =================================================================================
const string ScopRegion::NAME = "SCoPAnnotation";
const utils::StringKey<ScopRegion> ScopRegion::KEY("SCoPAnnotationKey");

std::ostream& ScopRegion::printTo(std::ostream& out) const {
	out << "IterationVector: " << iterVec;
	if (constraints) {
		out << "\\nConstraints: " << *constraints;
	}
	out << "\\nNumber of direct stmts: " << stmts.size();
	if (!subScops.empty()) {
		out << "\\nSubScops: " << subScops.size();
	}
	//if (!accesses.empty()) {
		//out << "\\nAccesses: " << accesses.size() << "{" 
			//<< join(",", accesses, [&](std::ostream& jout, const RefPtr& cur){ jout << *cur; } ) 
			//<< "}";
	//}
	return out;
}


/**************************************************************************************************
 * Recursively process ScopRegions caching the information related to access functions and
 * scattering matrices for the statements contained in this Scop region
 *************************************************************************************************/
void ScopRegion::resolveScop(const poly::IterationVector& 	iterVec, 
							 poly::IterationDomain		 	parentDomain, 
			 	   		   	 const ScopRegion& 				region,
 							 const ScatteringFunction& 		curScat,
							 IteratorOrder&					iterators,
							 ScatteringMatrix& 				scat,
							 size_t&						sched_dim) 
{
	typedef std::set<Iterator> IteratorSet;
	
	// assert( parentDomain->getIterationVector() == iterVec );
	poly::ConstraintCombinerPtr currDomain = parentDomain and poly::cloneConstraint(iterVec, region.getDomainConstraints());

	size_t pos = 0;
	const ScopStmtList& scopStmts = region.stmts;
	// for every access in this region, convert the affine constraint to the new iteration vector 
	std::for_each(scopStmts.begin(), scopStmts.end(), [&] (const ScopStmt& cur) { 
			
		StatementPtr&& curPtr = cur.getAddr().getAddressedNode();
		assert(curPtr->getNodeType() != core::NT_MarkerExpr && curPtr->getNodeType() != core::NT_MarkerStmt);
		
		ScatteringFunction newScat(curScat);
		const IterationVector& iterVec = curScat.getIterationVector();
		AffineFunction af( iterVec );
		
		// If statements 
		if ( curPtr->getNodeType() == NT_IfStmt ) {
			assert( curPtr->hasAnnotation(ScopRegion::KEY) && "If stmt inside SCoP not correctly annotated");
			const ScopRegion& ifScop = *curPtr->getAnnotation(ScopRegion::KEY);
		
			ScatteringFunction thenScat(curScat);
			af.setCoeff(poly::Constant(), pos++);
			thenScat.appendRow( af );

			// if we are inside a SCoP we can safely access to the annotations of the then and
			// else body of this if statement because they must be SCoPs 
			StatementAddress thenBody = AS_STMT_ADDR( cur.getAddr().getAddressOfChild(1) );
			assert( thenBody->hasAnnotation(ScopRegion::KEY) && "If body inside SCoP not correctly annotated");

			// Actualize the iteration domain to the current iteration vector 
			IterationDomain&& currID = currDomain and poly::cloneConstraint(iterVec, ifScop.getDomainConstraints());

			resolveScop(iterVec, currID, *thenBody->getAnnotation(ScopRegion::KEY), thenScat, iterators, scat, sched_dim);

			ScatteringFunction elseScat(curScat);
			af.setCoeff(poly::Constant(), pos++);
			elseScat.appendRow( af );
			
			StatementAddress elseBody = AS_STMT_ADDR( cur.getAddr().getAddressOfChild(2) );
			assert( elseBody->hasAnnotation(ScopRegion::KEY) && "Else body inside SCoP not correctly annotated");
			
			currID = currDomain and not_(poly::cloneConstraint(iterVec, ifScop.getDomainConstraints()));
			resolveScop(iterVec, currID, *elseBody->getAnnotation(ScopRegion::KEY), elseScat, iterators, scat, sched_dim);
			
			// Once the branches of the IF has been analyzed, skip the if statement	
			return;
		}

		af.setCoeff(poly::Constant(), pos++);
		newScat.appendRow( af );

		// check wheather the statement is a SCoP
		// auto fit = std::find(region.subScops.begin(), region.subScops.end(), cur.getAddr());
		// if (fit != region.subScops.end()) {
			// this is a sub scop
		if (curPtr->hasAnnotation(ScopRegion::KEY)) {

			if ( curPtr->getNodeType() == NT_ForStmt ) {
				// if the statement is a loop, then we append a dimension with the corresponding
				// iterator variable and we go recursively to visit the body  
				const ForStmtPtr& forStmt = static_pointer_cast<const ForStmt>(curPtr);
				const VariablePtr& iter = forStmt->getDeclaration()->getVariable();

				AffineFunction newAf( iterVec );
				newAf.setCoeff( poly::Iterator(iter), 1 );
				newScat.appendRow(newAf); 
				
				iterators.push_back(poly::Iterator(iter));
			} 
			resolveScop(iterVec, currDomain, *cur->getAnnotation(ScopRegion::KEY), newScat, iterators, scat, sched_dim);
			// pop back the iterator in the case the statement was a for stmt
			if ( curPtr->getNodeType() == NT_ForStmt ) { iterators.pop_back(); }
			return;
		}
		// } 
		// Access expressions 
		const RefAccessList& refs = cur.getRefAccesses();
		AccessInfoList accInfo;
		std::for_each(refs.begin(), refs.end(), [&] (const RefPtr& curRef) {
			if ( curRef->getType() == Ref::ARRAY ) {
				poly::AffineSystemPtr idx = std::make_shared<poly::AffineSystem>(iterVec);
				const ArrayRef& array = static_cast<const ArrayRef&>(*curRef);
				std::for_each(array.getIndexExpressions().begin(), array.getIndexExpressions().end(), 
					[&](const ExpressionAddress& cur) { 
						assert(cur->hasAnnotation(scop::AccessFunction::KEY));
						scop::AccessFunction& ann = *cur->getAnnotation(scop::AccessFunction::KEY);
						idx->appendRow( ann.getAccessFunction().toBase(iterVec) );
					}
				);
				accInfo.push_back( 
					AccessInfo(
						AS_EXPR_ADDR( concat<Node>(cur.getAddr(), curRef->getBaseExpression()) ), 
						curRef->getUsage(), idx
					)
				);
			}
			// act for different kind of refs 	
		});

		IteratorSet nested_iters(iterators.begin(), iterators.end()), 
					domain_iters(iterVec.iter_begin(), iterVec.iter_end()), 
					notUsed;

		// Remove iterators which do not belong to this nested region
		std::set_difference(
				domain_iters.begin(), domain_iters.end(), nested_iters.begin(), nested_iters.end(), 
				std::inserter(notUsed, notUsed.begin())
			);
			
		// save the domain 
		IterationDomain saveDomain = currDomain;

		// set to zero all the not used iterators 
		std::for_each(notUsed.begin(), notUsed.end(), 
				[&] (const poly::Iterator& curIt) { 
					AffineFunction af(iterVec);
					af.setCoeff(curIt, 1);
					af.setCoeff(poly::Constant(), 0);
					currDomain = currDomain and Constraint(af, Constraint::EQ);
				}
			);

		scat.push_back( 
			std::make_tuple(cur.getAddr(), currDomain, 
				std::make_shared<poly::ScatteringFunction>(newScat), accInfo
			) 
		);
		
		if (newScat.size() > sched_dim) {
			sched_dim = newScat.size();
		}
		// Set back the domain
		currDomain = saveDomain;

	} ); 
}

const ScopRegion::ScatteringPair ScopRegion::getScatteringInfo() {
	if ( !scattering ) {
		// we compute the full scattering information for this domain and we cache the result for
		// later use. 
		ScopRegion::ScatteringPair ret;
		ScatteringFunction sf(iterVec);
		ScopRegion::IteratorOrder iterOrder;
		resolveScop(iterVec, constraints, *this, sf, iterOrder, ret.second, ret.first);
		scattering = ret; // cache the result to be used for successive call to this function
	}
	return *scattering;
}

//===== AccessFunction ============================================================
const string AccessFunction::NAME = "AccessFuncAnn";
const utils::StringKey<AccessFunction> AccessFunction::KEY("AccessFuncAnnKey");

std::ostream& AccessFunction::printTo(std::ostream& out) const {
	return out << "IV: " << iterVec << ", Access: " << access;
}

//===== mark ======================================================================
ScopList mark(const core::NodePtr& root) {
	ScopList ret;
	LOG(DEBUG) << std::setfill('=') << std::setw(80) << std::left << "# Starting SCoP analysis";
	ScopVisitor sv(ret);
	try {
		sv.visit( NodeAddress(root) );
	} catch (NotASCoP&& e) { LOG(WARNING) << e.what(); }

	return ret;
}

void computeDataDependence(const NodePtr& root) {

	if( !root->hasAnnotation( ScopRegion::KEY ) ) {
		LOG(WARNING) << "Not possible to compute dependence information from a non static control region.";
		return ;
	}
	
	// We are in a Scop 
	ScopRegion& ann = *root->getAnnotation( ScopRegion::KEY );
	const ScopRegion::ScatteringPair&& scat = ann.getScatteringInfo();
	const IterationVector& iterVec = ann.getIterationVector();
	auto&& ctx = BackendTraits<POLYHEDRAL_BACKEND>::ctx_type();

	std::shared_ptr<Set<IslContext>> domain;
	std::shared_ptr<Map<IslContext>> schedule;
	std::shared_ptr<Map<IslContext>> reads;
	std::shared_ptr<Map<IslContext>> writes;

	size_t stmtID = 0;
	std::for_each(scat.second.begin(), scat.second.end(), 
		[ & ] (const ScopRegion::StmtScattering& cur) { 
			std::string stmtid = "S" + utils::numeric_cast<std::string>(stmtID++);
			IterationDomain id = std::get<1>(cur);

			auto&& ids = makeSet<POLYHEDRAL_BACKEND>(ctx, iterVec, id, stmtid);
			domain = !domain ? ids : set_union(ctx, *domain, *ids);

			ScatteringFunctionPtr sf = std::get<2>(cur);
			// Because the scheduling of every statement has to have the same number of elements
			// (same dimensions) we append zeros until the size of the affine system is equal to 
			// the number of dimensions used inside this SCoP for the scheduling functions 
			for ( size_t s = sf->size(); s < scat.first; ++s ) {
				sf->appendRow( AffineFunction(iterVec) );
			}
			auto&& scattering = makeMap<POLYHEDRAL_BACKEND>(ctx, *static_pointer_cast<AffineSystem>(sf), stmtid);
			schedule = !schedule ? scattering : map_union(ctx, *schedule, *scattering);
				
			// Access Functions 
			const ScopRegion::AccessInfoList& ail = std::get<3>(cur);
			std::for_each(ail.begin(), ail.end(), [&](const ScopRegion::AccessInfo& cur){
				const ExpressionAddress& addr = std::get<0>(cur);
				AffineSystemPtr accessInfo = std::get<2>(cur);

				if (accessInfo) {
					auto&& access = makeMap<POLYHEDRAL_BACKEND>(ctx, *accessInfo, stmtid, addr->toString());

					switch ( std::get<1>(cur) ) {
					case Ref::USE: 
						reads = !reads ? access : map_union(ctx, *reads, *access);
						break;
					case Ref::DEF:
						writes = !writes ? access : map_union(ctx, *writes, *access);
						break;
					case Ref::UNKNOWN:
						reads = !reads ? access : map_union(ctx, *reads, *access);
						writes = !writes ? access : map_union(ctx, *writes, *access);
						break;
					default:
						assert(false);
					}
				}
			});
		}
	);


	std::cout << "D:=";
	domain->printTo(std::cout);
	std::cout << std::endl;
	std::cout << "S:=";	
	schedule->printTo(std::cout);
	std::cout << std::endl;
	if (reads) {
		std::cout << "R:=";
		reads->printTo(std::cout);
		std::cout << std::endl;
	}
	if (writes) {
		std::cout << "W:=";	
		writes->printTo(std::cout);
		std::cout << std::endl;
	}
	LOG(DEBUG) << "Computing RAW dependencies: ";
	buildDependencies(ctx, domain, schedule, reads, writes);
	
	std::cout << std::endl;

	LOG(DEBUG) << "Computing WAW dependencies: ";	
	buildDependencies(ctx, domain, schedule, writes, writes);
	std::cout << std::endl;

	LOG(DEBUG) << "Computing WAR dependencies: ";
	buildDependencies(ctx, domain, schedule, writes, reads);
	std::cout << std::endl;
}

//===== printSCoP ===================================================================
void printSCoP(std::ostream& out, const core::NodePtr& scop) {
	out << std::endl << std::setfill('=') << std::setw(80) << std::left << "@ SCoP PRINT";	
	out << *scop;
	// check whether the IR node has a SCoP annotation
	if( !scop->hasAnnotation( ScopRegion::KEY ) ) {
		out << "{ }\n";
		return ;
	}
	
	auto&& ctx = BackendTraits<POLYHEDRAL_BACKEND>::ctx_type();

	ScopRegion& ann = *scop->getAnnotation( ScopRegion::KEY );
	const ScopRegion::ScatteringMatrix&& scat = ann.getScatteringInfo().second;
	out << "\nNumber of sub-statements: " << scat.size() << std::endl;
		
	out << "IV: " << ann.getIterationVector() << std::endl;
	size_t stmtID = 0;
	std::for_each(scat.begin(), scat.end(), 
		[ &ann, &stmtID, &out, &ctx ] (const ScopRegion::StmtScattering& cur) { 
			out << std::setfill('~') << std::setw(80) << "" << std::endl;
			std::string stmtid = "S" + utils::numeric_cast<std::string>(stmtID++);
			out << "@ " << stmtid << ": " << std::endl 
				<< " -> " << printer::PrettyPrinter( std::get<0>(cur).getAddressedNode() ) << std::endl;
	
			IterationDomain id = std::get<1>(cur);
			out << " -> ID ";
			
			auto&& ids = makeSet<POLYHEDRAL_BACKEND>(ctx, ann.getIterationVector(), id, stmtid);

			if (!id) { out << "{}"; }
			else 	 { 
				out << *id << std::endl; 
				out << " => ISL: ";
				ids->printTo(out);
			}
			out << std::endl;
			ScatteringFunctionPtr sf = std::get<2>(cur);
			out << *sf;
			auto&& scattering = makeMap<POLYHEDRAL_BACKEND>(ctx, *static_pointer_cast<AffineSystem>(sf), stmtid);
			out << " => ISL: ";
			scattering->printTo(out);
			out << std::endl;
			const ScopRegion::AccessInfoList& ail = std::get<3>(cur);
			std::for_each(ail.begin(), ail.end(), [&](const ScopRegion::AccessInfo& cur){
				const ExpressionAddress& addr = std::get<0>(cur);
				out << " -> REF ACCESS: [" << Ref::useTypeToStr(std::get<1>(cur)) << "] "
					<< " -> VAR: " << printer::PrettyPrinter(addr.getAddressedNode()) ; 

				AffineSystemPtr accessInfo = std::get<2>(cur);
				out << " IDX: " << join("", accessInfo->begin(), accessInfo->end(), 
					[&](std::ostream& jout, const poly::AffineFunction& cur){ jout << "[" << cur << "]"; } );
				out << std::endl;
				if (accessInfo) {
					auto&& access = makeMap<POLYHEDRAL_BACKEND>(ctx, *accessInfo, stmtid, addr->toString());
					// map.intersect(ids);
					out << " => ISL: "; 
					access->printTo(out);
					out << std::endl;
				}
			});
		}
	);
	
	LOG(DEBUG) << std::endl << std::setfill('=') << std::setw(80) << "";
}

} // end namespace scop
} // end namespace analysis
} // end namespace insieme



