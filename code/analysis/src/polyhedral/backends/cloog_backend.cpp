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

#include <stack>

#include "insieme/analysis/polyhedral/polyhedral.h"
#include "insieme/analysis/polyhedral/backends/isl_backend.h"
#include "insieme/analysis/func_sema.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"

#include "insieme/core/printer/pretty_printer.h"

#define CLOOG_INT_GMP
#include "cloog/cloog.h"
#include "cloog/isl/cloog.h"

using namespace insieme;
using namespace insieme::analysis::polyhedral;

#ifdef CLOOG_INT_GMP 
#define PRINT_CLOOG_INT(out, val) \
	do { 										\
	char* str;										\
	cloog_int_print_gmp_free_t gmp_free;			\
	str = mpz_get_str(0, 10, val);					\
	out << str;										\
	mp_get_memory_functions(NULL, NULL, &gmp_free);	\
	(*gmp_free)(str, strlen(str)+1); }while(0)
#endif 

namespace {

template <class IterT>
core::ExpressionPtr buildGen(core::NodeManager& mgr, const IterT& begin, const IterT& end, const core::LiteralPtr& op) { 
	core::IRBuilder builder(mgr);
	
	size_t argSize = std::distance(begin, end);
	assert( argSize >= 2 && "Cannot create a binary expression with less than 2 arguments");

	// call recursively this function to build a min/max with more than 2 args
	if ( argSize > 2 ) {
		return builder.callExpr( mgr.getLangBasic().getSelect(), *begin, buildGen(mgr, begin+1, end, op), op );
	}

	assert( argSize == 2 && "2 arguments are required");
	return builder.callExpr( mgr.getLangBasic().getSelect(), *begin, *(begin+1), op );
}

enum Type {MIN, MAX};

template <Type T>
core::ExpressionPtr build(core::NodeManager& mgr, const core::ExpressionList& args) { 
	const core::lang::BasicGenerator& basic = mgr.getLangBasic();
	switch ( T ) {
	case MIN: return buildGen(mgr, args.rbegin(), args.rend(), basic.getSignedIntLt() );
	case MAX: return buildGen(mgr, args.rbegin(), args.rend(), basic.getSignedIntGt() );
	}
}

template <class RetTy=void>
struct ClastVisitor {

	// Visit of Clast Stmts 
	virtual RetTy visitClastRoot(const clast_root* rootStmt) = 0;

	virtual RetTy visitClastAssignment(const clast_assignment* assignmentStmt) = 0;

	virtual RetTy visitClastBlock(const clast_block* blockStmt) = 0;

	virtual RetTy visitClastUser(const clast_user_stmt* userStmt) = 0;

	virtual RetTy visitClastFor(const clast_for* forStmt) = 0;

	virtual RetTy visitClastGuard(const clast_guard* guardStmt) = 0;

	virtual RetTy visitCloogStmt(const CloogStatement* cloogStmt) = 0;

	// Visit of Expr Stmts
	virtual RetTy visitClastName(const clast_name* nameExpr) = 0;

	virtual RetTy visitClastTerm(const clast_term* termExpr) = 0;

	virtual RetTy visitClastBinary(const clast_binary* binExpr) = 0;

	virtual RetTy visitClastReduction(const clast_reduction* redExpr) = 0;

	virtual RetTy visitClastEquation(const clast_equation* equation) = 0;

	virtual RetTy visit(const clast_expr* expr) {
		assert(expr && "Expression is not valid!");
		switch(expr->type) {
		case clast_expr_name:
			return visitClastName( reinterpret_cast<const clast_name*>(expr) );
		case clast_expr_term:
			return visitClastTerm( reinterpret_cast<const clast_term*>(expr) );
		case clast_expr_bin:
			return visitClastBinary( reinterpret_cast<const clast_binary*>(expr) );
		case clast_expr_red:
			return visitClastReduction( reinterpret_cast<const clast_reduction*>(expr) );
		default:
			assert(false && "Clast Expression not valid!");
		}

	}

	virtual RetTy visit(const clast_stmt* clast_node) {
		assert(clast_node && "Clast node is not valid");

		if( CLAST_STMT_IS_A(clast_node, stmt_root) )
			return visitClastRoot( reinterpret_cast<const clast_root*>(clast_node) );
		
		if( CLAST_STMT_IS_A(clast_node, stmt_ass) )
			return visitClastAssignment( reinterpret_cast<const clast_assignment*>(clast_node) );

		if( CLAST_STMT_IS_A(clast_node, stmt_user) )
			return visitClastUser( reinterpret_cast<const clast_user_stmt*>(clast_node) );

		if( CLAST_STMT_IS_A(clast_node, stmt_block) )
			return visitClastBlock( reinterpret_cast<const clast_block*>(clast_node) );

		if( CLAST_STMT_IS_A(clast_node, stmt_for) )
			return visitClastFor( reinterpret_cast<const clast_for*>(clast_node) );
	
		if( CLAST_STMT_IS_A(clast_node, stmt_guard) )
			return visitClastGuard( reinterpret_cast<const clast_guard*>(clast_node) );

		assert(false && "Clast node not supported");
	}

	virtual RetTy visit(const CloogStatement* cloogStmt) {
		return visitCloogStmt( cloogStmt );
	}
};

template <class RetTy=void>
struct RecClastVisitor: public ClastVisitor<RetTy> {

	// Statements 
	RetTy visitClastRoot(const clast_root* rootStmt) { 
		return RetTy();
	}

	virtual RetTy visitClastAssignment(const clast_assignment* assignmentStmt) {
		ClastVisitor<RetTy>::visit( assignmentStmt->RHS );

		return RetTy();
	}

	virtual RetTy visitClastBlock(const clast_block* blockStmt) {
		return ClastVisitor<RetTy>::visit( blockStmt->body );
	}

	virtual RetTy visitClastUser(const clast_user_stmt* userStmt) { 
		ClastVisitor<RetTy>::visit( userStmt->statement );
		if (userStmt->substitutions ) {
			ClastVisitor<RetTy>::visit( userStmt->substitutions );
		}
		return RetTy();
	}

	virtual RetTy visitClastFor(const clast_for* forStmt) {
		ClastVisitor<RetTy>::visit( forStmt->LB );
		ClastVisitor<RetTy>::visit( forStmt->UB );
		ClastVisitor<RetTy>::visit( forStmt->body );
		return RetTy();
	}

	virtual RetTy visitClastGuard(const clast_guard* guardStmt) {
		ClastVisitor<RetTy>::visit( guardStmt->then );
		for(size_t i=0, e=guardStmt->n; i!=e; ++i) {
			visitClastEquation(&guardStmt->eq[i]);
		}
		return RetTy();
	}

	virtual RetTy visitClastEquation(const clast_equation* equation) {
		ClastVisitor<RetTy>::visit(equation->LHS);
		ClastVisitor<RetTy>::visit(equation->RHS);
		return RetTy();
	}

	virtual RetTy visitCloogStmt(const CloogStatement* cloogStmt) {
		if (cloogStmt->next)
			ClastVisitor<RetTy>::visit(cloogStmt->next);
		return RetTy();
	}

	// Expressions 
	virtual RetTy visitClastName(const clast_name* nameExpr) {
		return RetTy();
	}

	virtual RetTy visitClastTerm(const clast_term* termExpr) {
		if (termExpr->var)
			return ClastVisitor<RetTy>::visit(termExpr->var);

		return RetTy();
	}

	virtual RetTy visitClastBinary(const clast_binary* binExpr) {
		ClastVisitor<RetTy>::visit(binExpr->LHS);
		return RetTy();
	}

	virtual RetTy visitClastReduction(const clast_reduction* redExpr) {
		for(size_t i=0, e=redExpr->n; i!=e; ++i) {
			ClastVisitor<RetTy>::visit( redExpr->elts[i] );
		}
		return RetTy();
	}

	virtual RetTy visit(const clast_expr* expr) {
		return ClastVisitor<RetTy>::visit(expr);
	}

	virtual RetTy visit(const clast_stmt* clast_node) {
		clast_stmt* ptr = clast_node->next;

		if (!ptr)
			return ClastVisitor<RetTy>::visit(clast_node);
		
		ClastVisitor<RetTy>::visit(clast_node);
		visit(ptr);

		return RetTy();
	}

	virtual RetTy visit(const CloogStatement* cloogStmt) {
		const CloogStatement* ptr = cloogStmt->next;

		if( !ptr )
			return ClastVisitor<RetTy>::visit( cloogStmt );

		ClastVisitor<RetTy>::visit( cloogStmt );
		visit( ptr );

		return RetTy();
	}

};

/**************************************************************************************************
 * ClastDump: dump the cloog ast into output stream, this is useful to understand the structure of 
 * the cloog ast and test the visitor 
 *************************************************************************************************/
class ClastDump: public RecClastVisitor<void> {

	std::ostream& out;

	// Utility object used to manage indentation of the pretty printer for the cloog ast (clast)
	class Indent : public utils::Printable {
		size_t spaces;
		const char indent_symbol;

	public:
		Indent(size_t spaces = 0, const char indent_symbol = '\t') : 
			spaces(spaces), indent_symbol(indent_symbol) { }

		Indent& operator++() { ++spaces; return *this; }
		Indent& operator--() { --spaces; return *this; }

		std::ostream& printTo(std::ostream& out) const {
			return out << std::string(spaces, indent_symbol);
		}
	};

	Indent indent;

public:
	ClastDump(std::ostream& out) : out(out) { }

	void visitClastAssignment(const clast_assignment* assignmentStmt) {
		if (assignmentStmt->LHS) { out << assignmentStmt->LHS << "="; }
		visit( assignmentStmt->RHS );
		if (assignmentStmt->stmt.next )
			out << ", ";
	}
	
	void visitClastName(const clast_name* nameExpr) {
		out << nameExpr->name;
	}

	void visitClastTerm(const clast_term* termExpr) {
		PRINT_CLOOG_INT(out, termExpr->val);

		if (termExpr->var == NULL) 	{ return; }	
		out << "*";
		visit(termExpr->var);
	}

	void visitClastFor(const clast_for* forStmt) {
		out << indent << "for(" << forStmt->iterator << "=";
		visit(forStmt->LB);
		out << "; " << forStmt->iterator << "<=";
		visit(forStmt->UB);
		out << "; " << forStmt->iterator << "+=";
		PRINT_CLOOG_INT(out, forStmt->stride);
		out << ") {" << std::endl;
		++indent;
		visit(forStmt->body); 
		--indent;
		out << indent << "}" << std::endl;
	}

	void visitClastUser(const clast_user_stmt* userStmt) { 
		out << indent;
		visit( userStmt->statement );
		out << "(";
		visit( userStmt->substitutions );
		out << ")" <<std::endl; 
	}

	void visitClastGuard(const clast_guard* guardStmt) {
		out << indent << "if (";
		assert(guardStmt->n>0);
		visitClastEquation(guardStmt->eq);
		for(size_t i=1, e=guardStmt->n; i!=e; ++i) {
			out << " && ";
			visitClastEquation(&guardStmt->eq[i]);
		}
		out << ") {" << std::endl;
		++indent;
		visit( guardStmt->then );
		--indent;
		out << indent << "}" << std::endl;
	}

	void visitClastBlock(const clast_block* blockStmt) {
		out << indent << "{" << std::endl;
		++indent;
		visit( blockStmt->body );
		--indent;
		out << "}" << std::endl;
	}

	void visitClastBinary(const clast_binary* binExpr) {
		bool isFunc = false;
		switch (binExpr->type) {
		case clast_bin_fdiv:
			out << "floord("; break;
		case clast_bin_cdiv:
			out << "ceild(";  break;
		default:
			isFunc = true;
		}
		visit(binExpr->LHS);
		if (isFunc) { out << ","; }
		else {
			switch(binExpr->type) {
			case clast_bin_div: out << '/'; break;
			case clast_bin_mod:	out << '%'; break;
			default: 
				assert(false);
			}
		}
		PRINT_CLOOG_INT(out, binExpr->RHS);
		if (isFunc) { out << ")"; }
	}

	void visitClastReduction(const clast_reduction* redExpr) {
		if (redExpr->n == 1)
			return visit( redExpr->elts[0] );
		
		switch(redExpr->type) {
		case clast_red_sum: out << "sum"; 	break;
		case clast_red_min: out << "min";	break;
		case clast_red_max: out << "max";	break;
		default:
			assert(false && "Reduction operation not valid");
		}

		out << "(";
		assert(redExpr->n >= 1);

		visit( redExpr->elts[0] );
		for(size_t i=1, e=redExpr->n; i!=e; ++i) {
			out << ", ";
			visit( redExpr->elts[i] );
		}
		out << ")";
	}

	void visitClastEquation(const clast_equation* equation) {
		visit(equation->LHS);
		switch( equation->sign ) {
			case -1: out << "<="; 	 break;
			case 0:  out << "==";	 break;
			case 1:  out << ">=";	 break;
		}
		visit(equation->RHS);
	}

	void visitCloogStmt(const CloogStatement* cloogStmt) {
		out << cloogStmt->name;
	}
};

core::CallExprPtr buildBinCallExpr(core::NodeManager& 		mgr, 
								   const core::TypePtr&		opTy,
					 			   const core::LiteralPtr& 	op, 
								   core::ExpressionList::const_iterator 	arg_begin,
								   core::ExpressionList::const_iterator 	arg_end) {
	core::IRBuilder builder(mgr);

	if (std::distance(arg_begin, arg_end) == 2) {
		return builder.callExpr(opTy, op, builder.castExpr(opTy, *arg_begin), 
										  builder.castExpr(opTy, *(arg_begin+1))
								);
	}
	return builder.callExpr(opTy, op, builder.castExpr(opTy, *arg_begin), 
			buildBinCallExpr(mgr, opTy, op, arg_begin+1, arg_end)
		);

}


/** 
 * Exception used to capture the special case call expression for which the semantics 
 * of their behaviour is specified. This conditions will be erroneus interpreted by 
 * cloog as it will create a number of nested for loops for each range. We need to 
 * flatten the hierachy and subsequently proceed with the correct var replacement.
 */
struct RangedFunction : public std::exception {

	typedef std::vector<core::ExpressionPtr> VarVect;

	RangedFunction(const VarVect& ranges) : ranges(ranges) { }

	size_t getNumRanges() const { return ranges.size(); }

	const VarVect& getRangedVariables() const { return ranges; }

	VarVect::const_iterator begin() const { return ranges.begin(); }
	VarVect::const_iterator end() const { return ranges.end(); }
		
	virtual const char* what() const throw() { return (std::string("Found range stmt: ") + toString(ranges)).c_str(); }
	virtual ~RangedFunction() throw() { }

private:
	VarVect ranges;
};

#define STACK_SIZE_GUARD \
	auto checkPostCond = [&](size_t stackInitialSize) -> void { 	 \
		assert(stmtStack.size() == stackInitialSize);				 \
	};																 \
	 FinalActions __check_stack_size( std::bind(checkPostCond, stmtStack.size()) );

/**************************************************************************************************
      for (c4=0;c4<=2*floord(-v4,2)+2*v4-1;c4++) {
 * ClastToIr: converts a clast into an IR which will be used to replace the SCoP region
 *************************************************************************************************/
class ClastToIR : public RecClastVisitor< core::ExpressionPtr > {
	
public:
	typedef std::vector<core::StatementPtr> StatementList;
	typedef std::stack<StatementList> StatementStack;

	typedef std::map<std::string, core::ExpressionPtr> IRVariableMap;

	ClastToIR(const IslCtx& ctx, core::NodeManager& mgr, const IterationVector& iterVec) : 
		ctx(ctx), mgr(mgr), iterVec(iterVec)
	{
		// Builds a map which associates variables in the cloog AST to IR node. This kind of
		// handling has to be done to be able to remap parameters to correct IR nodes 
		std::for_each(iterVec.begin(), iterVec.end(), 
			[&] (const Element& cur) { 
				if ( cur.getType() == Element::ITER || cur.getType() == Element::PARAM ) {
					std::ostringstream ss;
					ss << cur;
					varMap.insert( std::make_pair(ss.str(), static_cast<const Expr&>(cur).getExpr()) );	
				}
			}
		);
		LOG(DEBUG) << iterVec;
		LOG(DEBUG) << varMap.size();
		assert ( varMap.size() == iterVec.size()-1 );

		stmtStack.push( StatementList() );
	}

	core::ExpressionPtr visitClastTerm(const clast_term* termExpr) {
		STACK_SIZE_GUARD;

		core::IRBuilder builder(mgr);
		
		std::ostringstream ss;
		PRINT_CLOOG_INT(ss, termExpr->val);
	
		core::LiteralPtr&& lit = builder.literal( mgr.getLangBasic().getInt4(), ss.str() );
		if (termExpr->var == NULL) 	{ return lit; }
		
		core::ExpressionPtr&& var = visit(termExpr->var);
		// If the coefficient is 1 then omit it 
		if (*lit == *builder.intLit(1) ) { return var; }

		return builder.callExpr( mgr.getLangBasic().getSignedIntMul(), 
				builder.castExpr( mgr.getLangBasic().getInt4(), lit), 
				builder.castExpr( mgr.getLangBasic().getInt4(), var)
			); 
	}

	core::ExpressionPtr visitClastName(const clast_name* nameExpr) {
		STACK_SIZE_GUARD;

		core::IRBuilder builder(mgr);

		auto&& fit = varMap.find(nameExpr->name);
		assert(fit != varMap.end() && "Variable not defined!");
		
		core::ExpressionPtr ret = fit->second;
		if(fit->second->getType()->getNodeType() == core::NT_RefType) {
			ret = builder.deref(ret);
		}
		return ret;
	}

	core::ExpressionPtr visitClastReduction(const clast_reduction* redExpr) {
		STACK_SIZE_GUARD;

		core::IRBuilder builder(mgr);
		if (redExpr->n == 1) { return visit( redExpr->elts[0] ); }

		std::vector<core::ExpressionPtr> args;
		for (size_t i=0, e=redExpr->n; i!=e; ++i) {
			args.push_back( visit( redExpr->elts[i] ) );
			assert( args.back() );
		}

		core::LiteralPtr op;
		switch(redExpr->type) {
		case clast_red_sum: op = mgr.getLangBasic().getSignedIntAdd();
							break;

		case clast_red_min: return build<MIN>(mgr, args);
		case clast_red_max: return build<MAX>(mgr, args);

		default:
			assert(false && "Reduction operation not valid");
		}

		assert(redExpr->n >= 1);
		return buildBinCallExpr(mgr, mgr.getLangBasic().getInt4(), op, args.begin(), args.end());
	}

	core::ExpressionPtr visitClastFor(const clast_for* forStmt) {
		STACK_SIZE_GUARD;

		core::IRBuilder builder(mgr);
		
		// We need to make sure the body is not any call to a literal expression. 
		// In those case we need to perform special handling as the semantic information 
		// could have introduced new domain which the cloog library wrongly interpret as for loops 
		
		auto&& fit = varMap.find(forStmt->iterator);
		assert(fit == varMap.end() && "Induction variable being utilizied!");
		core::VariablePtr&& inductionVar = builder.variable( mgr.getLangBasic().getInt4() );

		auto&& indPtr = varMap.insert( std::make_pair(std::string(forStmt->iterator), inductionVar) );
		
		VLOG(2) << "Induction variable for loop: " << *inductionVar;

		core::ExpressionPtr&& lowerBound = visit(forStmt->LB);
		assert( lowerBound && "Failed conversion of lower bound expression for loop!");

		core::ExpressionPtr&& upperBound = visit(forStmt->UB);
		// because cloog assumes the upperbound to be <=, we have to add a 1 to make it consistent
		// with the semantics of the IR 
		upperBound = builder.callExpr( mgr.getLangBasic().getSignedIntAdd(), upperBound, builder.intLit(1) );
		assert( upperBound && "Failed conversion of upper bound expression for loop!");

		std::ostringstream ss;
		PRINT_CLOOG_INT(ss, forStmt->stride);
		core::LiteralPtr&& strideExpr = builder.literal( mgr.getLangBasic().getInt4(), ss.str() );
		
		core::StatementPtr irStmt;
		stmtStack.push( StatementList() );

		// Visit he body of this forstmt
		try {
			visit(forStmt->body);
			core::CompoundStmtPtr body = builder.compoundStmt( stmtStack.top() );
	
			irStmt = builder.forStmt( 
							inductionVar, lowerBound,
							upperBound, 
							strideExpr,
							body );

		}catch(RangedFunction&& ex) {
			const RangedFunction::VarVect& ranges = ex.getRangedVariables();
			irStmt = stmtStack.top().front();

			assert((ranges.size() == 1 && *ranges.front() == *inductionVar));

			// Nasty handling for MPI functions (to be replaced)
			assert(irStmt->getNodeType() == core::NT_CallExpr);

			core::CallExprPtr callExpr = irStmt.as<core::CallExprPtr>();
			core::LiteralPtr  funcLit = callExpr->getFunctionExpr().as<core::LiteralPtr>();

			const std::string& name = funcLit->getStringValue();
			assert(name.compare(0,4,"MPI_") == 0 && "Not an MPI statement");
			if (name == "MPI_Send" || name == "MPI_Isend" || name == "MPI_Recv" || name == "MPI_Irecv") {
				// replace the starting address of the array with the lower bound of the array
				// and the size with the difference between starting address and ending address
				utils::map::PointerMap<core::NodePtr, core::NodePtr> replacements;
				//
				replacements.insert( 
						std::make_pair(callExpr->getArgument(0),
							builder.callExpr(
								builder.getLangBasic().getRefToAnyRef(),
								insieme::analysis::setDisplacement(callExpr->getArgument(0), 
									insieme::core::arithmetic::toFormula(lowerBound))
							)
						) 
					);
				
				replacements.insert( 
						std::make_pair(callExpr->getArgument(1), builder.sub(upperBound, lowerBound))
					);

				irStmt = core::static_pointer_cast<const core::Statement>( 
						core::transform::replaceAll(mgr, irStmt, replacements) 
					);	
			}
		}

		stmtStack.pop();

		// remove the induction variable from the map, this is requred because Cloog uses the same
		// loop iterator for different loops, in the IR this is not allowed, therefore we have to
		// renew the mapping of this induction variable 
		varMap.erase(indPtr.first);

		assert(irStmt);

		stmtStack.top().push_back( irStmt );

		return core::ExpressionPtr();
	}

	core::ExpressionPtr visitClastUser(const clast_user_stmt* userStmt) { 
		STACK_SIZE_GUARD;

		RangedFunction::VarVect ranges;
		stmtStack.push( StatementList() );

		try {
			visit( userStmt->statement );
		} catch(RangedFunction&& ex) {
			ranges = ex.getRangedVariables();
		}
		assert(stmtStack.top().size() == 1 && "Expected 1 statement!");
		utils::map::PointerMap<core::NodePtr, core::NodePtr> replacements;

		size_t pos=0;
		for(const clast_stmt* ptr = userStmt->substitutions; ptr; ptr=ptr->next,pos++) {
			STACK_SIZE_GUARD;

			assert(CLAST_STMT_IS_A(ptr, stmt_ass) && "Expected assignment statement");
			const clast_assignment* assignment = reinterpret_cast<const clast_assignment*>( ptr );
			assert(assignment->LHS == NULL);
			const core::ExpressionPtr& targetVar = visit(assignment->RHS);
			const core::VariablePtr& sourceVar = 
				core::static_pointer_cast<const core::Variable>(
						static_cast<const Expr&>(iterVec[pos]).getExpr() );

			auto&& fit = std::find(ranges.begin(), ranges.end(), sourceVar);
			if ( fit != ranges.end() ) {
				// We found the variable which should be replaced to this particular range
				*fit = targetVar;
				continue;
			}
			// Check inside the scop to see whether the loop contains 
			assert(sourceVar->getType()->getNodeType() != core::NT_RefType);
			replacements.insert( std::make_pair(sourceVar, targetVar) );
		}
		
		core::StatementPtr stmt = stmtStack.top().front();
		
		stmt = core::static_pointer_cast<const core::Statement>( 
					core::transform::replaceAll(mgr, stmt, replacements) 
				);	

		stmtStack.pop();
		stmtStack.top().push_back(stmt);
		
		if (!ranges.empty()) {
			throw RangedFunction(ranges);
		}
		return core::ExpressionPtr();
	}

	core::ExpressionPtr visitCloogStmt(const CloogStatement* cloogStmt) {
		STACK_SIZE_GUARD;
		
		using namespace insieme::analysis::polyhedral;
		assert(cloogStmt->name);

		// get the stmt object 
		StmtPtr stmt = ctx.getAs<StmtPtr>( cloogStmt->name );
		core::StatementPtr irStmt = stmt->getAddr().getAddressedNode();
		
		stmtStack.top().push_back(irStmt); 

		// If the statement is a callexpr to a literal for which we had range information 
		// then cloog has generated a number of for statements around which are as many as 
		// the number of range variable in this statement 
		if (core::CallExprPtr callExpr = core::dynamic_pointer_cast<const core::CallExpr>(irStmt) ) {

			if (callExpr->getFunctionExpr()->getNodeType() == core::NT_Literal) {

				RangedFunction::VarVect ranges;
				for_each(stmt->access_begin(), stmt->access_end(), [&](const AccessInfoPtr& cur) {
					if (cur->hasDomainInfo()) {	
						std::vector<core::VariablePtr> iters = getOrderedIteratorsFor(cur->getAccess());
						assert( !iters.empty() && iters.size() == 1 );
						ranges.push_back( iters.front() ); 
					}
				});
				
				if ( !ranges.empty() ) {
					// Range info were found within the loop, this means cloog is missinterpreting 
					// this statement inside a number of nested loops which should be flattened 
					throw RangedFunction(ranges);
				}
			}
		}

		return core::ExpressionPtr();
	}

	core::ExpressionPtr visitClastEquation(const clast_equation* equation) {
		STACK_SIZE_GUARD;

		core::IRBuilder builder(mgr);

		core::LiteralPtr op;
		switch( equation->sign ) {
			case -1: op = mgr.getLangBasic().getSignedIntLe();   break;
			case 0:  op = mgr.getLangBasic().getSignedIntEq();	 break;
			case 1: op = mgr.getLangBasic().getSignedIntGe();	 break;
		}

		return builder.callExpr( op, visit(equation->LHS), visit(equation->RHS) );
	}

	core::ExpressionPtr visitClastBinary(const clast_binary* binExpr) {
		STACK_SIZE_GUARD;
		core::IRBuilder builder(mgr);

		core::ExpressionPtr op;
		switch (binExpr->type) {
		case clast_bin_fdiv:
			op = mgr.getLangBasic().getCloogFloor();    break;
		case clast_bin_cdiv:
			op = mgr.getLangBasic().getCloogCeil();     break;
		case clast_bin_div: 
			op = mgr.getLangBasic().getSignedIntDiv();	break;
		case clast_bin_mod:
			op = mgr.getLangBasic().getCloogMod();		break;
		default: 
			assert(false && "Binary operator not defined");
		}

		core::ExpressionPtr&& lhs = visit(binExpr->LHS);
		std::ostringstream ss;
		PRINT_CLOOG_INT(ss, binExpr->RHS);
		core::LiteralPtr&& rhs = builder.literal( mgr.getLangBasic().getInt4(), ss.str() );
		// std::cout << *op << " " << *lhs << " " << *rhs << std::endl;

		if (!rhs) { return lhs; }

   /*     if (rhs && ( binExpr->type == clast_bin_fdiv || binExpr->type == clast_bin_cdiv ) ) {*/
			//return builder.callExpr( retTy, op, 
					//builder.callExpr(
						//mgr.getLangBasic().getReal8(),
						//mgr.getLangBasic().getRealDiv(),
						//builder.castExpr(mgr.getLangBasic().getReal8(), lhs), 
						//builder.castExpr(mgr.getLangBasic().getReal8(), rhs)
					//) 
				//);

		/*}*/
		// std::cout << *op << " " << *lhs << " " << *rhs << std::endl;
		return builder.callExpr(op, lhs, rhs);
	}

	core::ExpressionPtr visitClastGuard(const clast_guard* guardStmt) {
		STACK_SIZE_GUARD;

		assert(guardStmt->n>0);
		core::IRBuilder builder(mgr);

		core::ExpressionPtr&& condExpr = visitClastEquation(guardStmt->eq);
		for(size_t i=1, e=guardStmt->n; i!=e; ++i) {
			
			condExpr = builder.callExpr( mgr.getLangBasic().getBoolLAnd(), condExpr, 
					builder.createCallExprFromBody( 
						builder.returnStmt(visitClastEquation(&guardStmt->eq[i])), 
						mgr.getLangBasic().getBool(), 
						true 
					) 
				);
		}
		
		stmtStack.push( StatementList() );
		visit( guardStmt->then );

		core::StatementPtr&& irIfStmt = 
				builder.ifStmt( condExpr, builder.compoundStmt(stmtStack.top()) );
		stmtStack.pop();

		stmtStack.top().push_back( irIfStmt );

		return core::ExpressionPtr();
	}

	core::StatementPtr getIR() const {
		assert( stmtStack.size() == 1 );
		core::IRBuilder builder(mgr);

		if(stmtStack.top().size() == 1) 
			return stmtStack.top().front();
		
		return builder.compoundStmt( stmtStack.top() );
	}

private:
	const IslCtx&		ctx;
	core::NodeManager& 		mgr;
	const IterationVector& 	iterVec;
	IRVariableMap  			varMap;
	StatementStack 			stmtStack;
};


}// end anonymous namespace

namespace insieme { namespace analysis { namespace polyhedral {

void CloogOpts::set(CloogOptions& opts) const {
	opts.l = optCtrlUntil;
	opts.f = optCtrlFrom;
	opts.strides = strides;
	opts.sh = computeConvexHulls;
	opts.first_unroll = unrollFromLevel;
	opts.esp = spreadComplexEqualities;
	opts.fsp = spreadEqualitiesFrom;
	opts.otl = simplifyLoops;
	opts.quiet = quiet;
}

core::NodePtr toIR(core::NodeManager& mgr, 
					const IterationVector& iterVec, 
					IslCtx& ctx, 
					IslSet& domain, 
					IslMap& schedule,
					const CloogOpts& opts
				  ) 
{

	CloogState *state;
	CloogInput *input;
	CloogOptions *options;

	struct clast_stmt *root;
	state = cloog_state_malloc();
	options = cloog_options_malloc(state);

	VLOG(1) << "[CLOOG.SCHEDULE]: " << schedule;
	VLOG(1) << "[CLOOG.DOMAIN]:   " << domain;
	MapPtr<ISL>&& schedDom = schedule * domain;
	// LOG(DEBUG) << *schedDom;

	// Because the conversion of an IterationDomain and an AffineSystem will 
	// remove all the existential variables utilized within the iteration 
	// vector, the code generated by cloog will refer to the position of 
	// iterators and parameters which are in this new space where all 
	// existentially qualified variables have been stripped out 
	IterationVector vec = removeExistQualified(iterVec);

	isl_union_map* smap = schedDom->getIslObj();
	
	isl_space* space = isl_union_map_get_dim( smap );

	CloogUnionDomain* unionDomain = 
		cloog_union_domain_from_isl_union_map( smap );

	CloogDomain* context = cloog_domain_from_isl_set( isl_set_universe(space) );

	input = cloog_input_alloc(context, unionDomain);

	// Set cloog options
	opts.set(*options);

	root = cloog_clast_create_from_input(input, options);
	assert( root && "Generation of Cloog AST failed" );

	if(Logger::get().level() <= DEBUG) {
		clast_pprint(stderr, root, 0, options);
	}
	
	if ( VLOG_IS_ON(1) ) {
		//ClastDump dumper( LOG_STREAM(DEBUG) );
		//dumper.visit(root);
	}

	core::IRBuilder builder(mgr);
	typedef std::pair<unsigned, core::DeclarationStmtPtr> ElemTy;

	std::set<ElemTy, std::function<bool (const ElemTy& lhs, const ElemTy& rhs)>> stmts( 
			[](const ElemTy& lhs, const ElemTy& rhs ) -> bool { return lhs.first < rhs.first; } 
		);

	struct visit_tuple_info : public boost::static_visitor<bool> {
		bool operator()(const core::NodePtr& cur) const { return false; }
		bool operator()(const StmtPtr& cur) const { return true; }
	};

	core::StatementList decls;
	IslCtx::TupleMap& tm = ctx.getTupleMap();
	for_each(tm, [&] (IslCtx::TupleMap::value_type& cur) { 
		// if one of the statements inside the SCoP is a declaration statement we must be carefull
		// during the code generation. We move the declaration outside the SCoP and replace the
		// declaration statement with an assignment 
		
		if( boost::apply_visitor( visit_tuple_info(), cur.second ) ) {
			StmtPtr& stmt = boost::get<StmtPtr>(cur.second);

			if( core::DeclarationStmtPtr decl = 
				core::dynamic_pointer_cast<const core::DeclarationStmt>(stmt->getAddr().getAddressedNode()) ) 
			{
				unsigned id = utils::numeric_cast<unsigned>(cur.first.substr(1));
				stmts.insert( std::make_pair(id, decl) );

				// replace the declaration stmt with an assignment 
				cur.second = std::make_shared<Stmt>(
					id,
					core::StatementAddress(
						builder.callExpr( mgr.getLangBasic().getRefAssign(), 
							decl->getVariable(), 
							builder.deref( decl->getInitialization() )
						)
					),
					IterationDomain(iterVec, true),
					AffineSystem(iterVec)
				);
			}
		}

	});

	std::transform(stmts.begin(), stmts.end(), 
			std::back_inserter(decls), 
			std::bind(&ElemTy::second, std::placeholders::_1) 
		);

	ClastToIR converter(ctx, mgr, vec);
	converter.visit(root);
	
	core::StatementPtr&& retIR = converter.getIR();
	assert(retIR && "Conversion of Cloog AST to Insieme IR failed");

	// Append the generated code to the list of extracted declarations 
	decls.push_back(retIR);
	core::NodePtr ret = (decls.size() > 1) ? builder.compoundStmt( decls ) : decls.front();

	VLOG(1) << core::printer::PrettyPrinter(ret);
	// auto&& checks = [] (const core::NodePtr& ret) { 
	// 	return core::check( ret, core::checks::getFullCheck() );
	//};
	// Perform semantics check on the generated code 
	// assert(checks(ret).getAll().empty() && "Generated code from polyhedral model is not semantically correct"); 
		
	cloog_clast_free(root);
	cloog_options_free(options);
	cloog_state_free(state);

	return ret;
}

} } } // end insieme::analysis::polyhedral namespace 

