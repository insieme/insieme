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

#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/core/ast_builder.h"
#include "insieme/utils/logging.h"

#define CLOOG_INT_GMP
#include "cloog/cloog.h"
#include "cloog/isl/cloog.h"

using namespace insieme;
using namespace insieme::analysis::poly;

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
		visitCloogStmt( cloogStmt );
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

		if(!ptr)
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
			out << "floord(";
			break;
		case clast_bin_cdiv:
			out << "ceild(";
			break;
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
		case clast_red_sum: out << "sum";
							break;
		case clast_red_min: out << "min";
							break;
		case clast_red_max: out << "max";
							break;
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
			case -1: out << "<=";
					 break;
			case 0:  out << "==";
					 break;
			case 1:  out << ">=";
					 break;
		}
		visit(equation->RHS);
	}

	void visitCloogStmt(const CloogStatement* cloogStmt) {
		out << cloogStmt->name;
	}
};

/**************************************************************************************************
 * ClastToIr: converts a clast into an IR which will be used to replace the SCoP region
 *************************************************************************************************/
class ClastToIR : public RecClastVisitor< core::ExpressionPtr > {

public:
	typedef std::vector<core::StatementPtr> StatementList;
	typedef std::stack<StatementList> StatementStack;

	typedef std::map<std::string, core::ExpressionPtr> IRVariableMap;

	ClastToIR(core::NodeManager& mgr, const IterationVector& iterVec) : mgr(mgr) {

		std::for_each(iterVec.begin(), iterVec.end(), 
			[&] (const Element& cur) { 
				if ( cur.getType() == Element::ITER || cur.getType() == Element::PARAM ) {
					std::ostringstream ss;
					ss << cur;
					varMap.insert( std::make_pair(ss.str(), static_cast<const Expr&>(cur).getExpr()) );	
				}
			}
		);

		assert ( varMap.size() == iterVec.size()-1 );
	}

	core::ExpressionPtr visitClastTerm(const clast_term* termExpr) {
		core::ASTBuilder builder(mgr);
		
		std::ostringstream ss;
		PRINT_CLOOG_INT(ss, termExpr->val);
	
		core::LiteralPtr&& lit = builder.literal( mgr.basic.getInt4(), ss.str() );
		if (termExpr->var == NULL) 	{ return lit; }
		
		core::ExpressionPtr&& var = visit(termExpr->var);
		// If the coefficient is 1 then omit it 
		if (*lit == *builder.intLit(1) ) { return var; }

		return builder.callExpr( mgr.basic.getSignedIntMul(), lit, var ); 
	}

	core::ExpressionPtr visitClastName(const clast_name* nameExpr) {
		core::ASTBuilder builder(mgr);

		auto&& fit = varMap.find(nameExpr->name);
		assert(fit != varMap.end() && "Variable not defined!");
		
		core::ExpressionPtr ret = fit->second;
		if(fit->second->getType()->getNodeType() == core::NT_RefType) {
			ret = builder.deref(ret);
		}
		return ret;
	}

	core::ExpressionPtr visitClastReduction(const clast_reduction* redExpr) {
		core::ASTBuilder builder(mgr);
		if (redExpr->n == 1) { return visit( redExpr->elts[0] ); }
		
		core::LiteralPtr op;
		core::TypePtr&& intGen = mgr.basic.getIntGen();
		switch(redExpr->type) {
		case clast_red_sum: op = mgr.basic.getSignedIntAdd();
							break;
		case clast_red_min: op = builder.literal("min", builder.functionType( 
										core::TypeList( { intGen, intGen } ), intGen )
									);
							break;
		case clast_red_max: op = builder.literal("max", builder.functionType( 
										core::TypeList( { intGen, intGen } ), intGen )
									);
							break;
		default:
			assert(false && "Reduction operation not valid");
		}

		assert(redExpr->n >= 1);

		std::vector<core::ExpressionPtr> args;
		for (size_t i=0, e=redExpr->n; i!=e; ++i) {
			args.push_back( visit( redExpr->elts[i] ) );
			assert( args.back() );
		}

		return builder.callExpr( op, args );
	}

	core::ExpressionPtr visitClastFor(const clast_for* forStmt) {
		core::ASTBuilder builder(mgr);

		auto&& fit = varMap.find(forStmt->iterator);
		assert(fit == varMap.end() && "Induction variable being utilizied!");
		core::VariablePtr&& inductionVar = builder.variable( mgr.basic.getInt4() );

		auto&& indPtr = varMap.insert( std::make_pair(std::string(forStmt->iterator), inductionVar) );
		
		LOG(DEBUG) << "Induction variable for loop: " << *inductionVar;

		core::ExpressionPtr&& lowerBound = visit(forStmt->LB);
		assert( lowerBound && "Failed conversion of lower bound expression for loop!");

		core::ExpressionPtr&& upperBound = visit(forStmt->UB);
		assert( upperBound && "Failed conversion of upper bound expression for loop!");

		std::ostringstream ss;
		PRINT_CLOOG_INT(ss, forStmt->stride);
		core::LiteralPtr&& strideExpr = builder.literal( mgr.basic.getInt4(), ss.str() );
		
		stmtStack.push( StatementList() );

		visit(forStmt->body); 
	
		core::ForStmtPtr&& irForStmt = 
			builder.forStmt( 
					builder.declarationStmt(inductionVar, lowerBound), 
					builder.compoundStmt( stmtStack.top() ), 
					upperBound, 
					strideExpr 
				);
		LOG(DEBUG) << *irForStmt;

		stmtStack.pop();

		// remove the induction variable from the map, this is requred because Cloog uses the same
		// loop iterator for different loops, in the IR this is not allowed, therefore we have to
		// renew the mapping of this induction variable 
		varMap.erase(indPtr.first);

		stmtStack.top().push_back( irForStmt );

		return core::ExpressionPtr();
	}

	core::ExpressionPtr visitClastUser(const clast_user_stmt* userStmt) { 
		stmtStack.push( StatementList() );

		visit( userStmt->statement );
		//visit( userStmt->substitutions );

		stmtStack.pop();
		return core::ExpressionPtr();
	}

	core::ExpressionPtr visitCloogStmt(const CloogStatement* cloogStmt) {

		out << cloogStmt->name;
	}
	//core::ExpressionPtr visitClastGuard(const clast_guard* guardStmt) {
		//out << indent << "if (";
		//assert(guardStmt->n>0);
		//visitClastEquation(guardStmt->eq);
		//for(size_t i=1, e=guardStmt->n; i!=e; ++i) {
			//out << " && ";
			//visitClastEquation(&guardStmt->eq[i]);
		//}
		//out << ") {" << std::endl;
		//++indent;
		//visit( guardStmt->then );
		//--indent;
		//out << indent << "}" << std::endl;
	//	return core::ExpressionPtr();
	//}

private:
	core::NodeManager& 	mgr;
	IRVariableMap  		varMap;
	StatementStack 		stmtStack;
};

} // end anonymous namespace

namespace insieme {
namespace analysis {
namespace poly {

template <>
core::NodePtr toIR(core::NodeManager& mgr, 
		const IterationVector& iterVec, 
		IslContext& ctx, 
		const Set<IslContext>& domain, 
		const Map<IslContext>& schedule) 
{

	CloogState *state;
	CloogInput *input;
	CloogOptions *options;

	struct clast_stmt *root;
	state = cloog_state_malloc();
	options = cloog_options_malloc(state);

	schedule.printTo(std::cout);
	
	MapPtr<IslContext>&& schedDom = map_intersect_domain(ctx, schedule, domain);

	CloogUnionDomain* unionDomain = cloog_union_domain_from_isl_union_map( isl_union_map_copy( schedDom->getAsIslMap() ) );
	isl_dim* dim = isl_union_map_get_dim( isl_union_map_copy( schedDom->getAsIslMap() ) );
	CloogDomain* context = cloog_domain_from_isl_set( isl_set_universe(dim) );

	input = cloog_input_alloc(context, unionDomain);

	options->block = 1;
	root = cloog_clast_create_from_input(input, options);

	clast_pprint(stdout, root, 0, options);
	
	ClastDump dumper(std::cout);
	dumper.visit(root);

	ClastToIR converter(mgr, iterVec);
	converter.visit(root);


	cloog_clast_free(root);
	cloog_options_free(options);
	cloog_state_free(state);

}

} // end poly namespace 
} // end analysis namespace 
} // end insieme namespace 
