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

#include "insieme/analysis/dfa/analyses/const_prop.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/utils/logging.h"

using namespace insieme::core;

namespace insieme { namespace analysis { namespace dfa { 

typename container_type_traits< dfa::elem< AccessClassPtr >  >::type 
extract(const Entity< dfa::elem<AccessClassPtr> >& e, const CFG& cfg, analyses::ConstantPropagation& obj) {

	std::set<AccessClassPtr> entities;
	auto& aMgr = obj.getAccessManager();

	core::NodeManager& mgr = cfg.getNodeManager();

	auto collector = [&] (const cfg::BlockPtr& block) {
		size_t stmt_idx=0;

		auto storeAccess = [&](const ExpressionAddress& var) {
				entities.insert( 
					aMgr.getClassFor(
						getImmediateAccess(
							var->getNodeManager(), 
							cfg::Address(block, stmt_idx-1, var),
							cfg.getTmpVarMap()
						)
					) 
				);
			};

		for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& cur) {
			++stmt_idx;

			auto stmt = core::NodeAddress(cur.getAnalysisStatement());
			if (cur.getType() == cfg::Element::LOOP_INCREMENT) {  			}

			if (auto declStmt = core::dynamic_address_cast<const core::DeclarationStmt>(stmt)) {
				storeAccess(declStmt->getVariable());
				return;
			}

			if(auto expr = core::dynamic_address_cast<const core::Expression>(stmt)) {
				if (core::analysis::isCallOf(expr.getAddressedNode(), mgr.getLangBasic().getRefAssign())) {
					storeAccess(expr.as<core::CallExprAddress>()->getArgument(0));
					return;
				}
			}
		});
	};
	cfg.visitDFS(collector);

	return entities;
}

namespace analyses {


typedef ConstantPropagation::value_type value_type;

/**
 * ConstantPropagation
 *
 * The meet (or confluence) operator for the constant propagation merges the informations coming
 * from two or more edges of the control flow graph. If the same variable is propagated by two or
 * more predecessor blocks then we have to compute the new dataflow value. 
 */
value_type ConstantPropagation::meet(const value_type& lhs, const value_type& rhs) const  {

	typedef dfa::Value<LiteralPtr> ConstantType;

	LOG_STREAM(DEBUG) << "Meet (" << lhs << ", " << rhs << ") -> " << std::flush;
	
	/** 
	 * Given 2 dataflow values associated to a variable, returns the new dataflow value 
	 * after the meet operator is applied according to the following table:
	 *
	 * ---------------------
	 * TOP ^ x      = x 
	 * TOP ^ TOP    = TOP
	 * x   ^ y      = BOTTOM
	 * x   ^ BOTTOM = BOTTOM
	 * x   ^ x      = x 
	 *----------------------
	 */
	auto eval = [](const ConstantType& lhs, const ConstantType& rhs) -> ConstantType {

		if (lhs == dfa::top) { return rhs; }
		if (rhs == dfa::top) { return lhs; }

		if (lhs == dfa::bottom || rhs == dfa::bottom) { 
			return dfa::bottom; 
		}

		if (lhs == rhs ) { return lhs; }

		return dfa::bottom;
	};

	auto var = [](const value_type::value_type& cur) { return std::get<0>(cur); };
	auto val = [](const value_type::value_type& cur) { return std::get<1>(cur); };

	value_type ret;
	value_type::const_iterator lhs_it = lhs.begin(), rhs_it = rhs.begin(), it, end;

	while(lhs_it != lhs.end() && rhs_it != rhs.end()) {
		if(*var(*lhs_it) == *var(*rhs_it)) {
			ret.insert( std::make_tuple(var(*lhs_it), eval(val(*lhs_it), val(*rhs_it))) );
			++lhs_it; ++rhs_it;
			continue;
		}
		if (*lhs_it < *rhs_it) { ret.insert( *(lhs_it++) ); continue; }
		if (*lhs_it > *rhs_it) { ret.insert( *(rhs_it++) ); continue; }
	}

	// Take care of the remaining elements which have to be written back to the result 
	std::tie(it,end) = lhs_it == lhs.end() ? 
			std::make_tuple(rhs_it, rhs.end()) : 
			std::make_tuple(lhs_it, lhs.end());

	while( it != end) { ret.insert( *(it++) ); }

	LOG(DEBUG) << ret; 

	return std::move(ret);
}

/**
 * Lookup a variable name in the dataflow information available at the current node. It returns its
 * determined constant value which could be either a literal or the top/bottom element of the
 * lattice representing respectively "undefined" and "not constant". 
 */
dfa::Value<LiteralPtr> lookup( const AccessManager& aMgr, const AccessPtr& var, const value_type& in, const CFG& cfg ) {
	
	auto accessClass = aMgr.findClass(var);

	// If the class was not found, then return the top element 
	if (!accessClass) { return dfa::top; }

	auto fit = std::find_if(in.begin(), in.end(), [&](const value_type::value_type& cur) { 
			return *std::get<0>(cur) == *accessClass; 
		});

	if( fit != in.end() ) return std::get<1>(*fit);

	return dfa::top;
}	


dfa::Value<LiteralPtr> eval(const AccessManager&		aMgr,
							const ExpressionAddress& 	lit, 
							const cfg::BlockPtr& 		block, 
							const size_t& 				stmt_idx,
							const value_type& 			in, 
							const CFG& 					cfg) 
{
	using namespace arithmetic;

	const lang::BasicGenerator& basicGen = lit->getNodeManager().getLangBasic();

	try {

		Formula f = toFormula(lit.getAddressedNode());

		/**
		 * If the expression is a constant then our job is finishes, we can return the constant value
		 */
		if (f.isConstant()) { return toIR(lit->getNodeManager(), f).as<LiteralPtr>(); }

		/**
		 * Otherwise it could be that the value of lit depends on other variables. 
		 * In that case we look into the dataflow value propagated up to this node to check whether the
		 * variable used to define lit are constants. If everything is a constant up to this point then
		 * we replace the variables in the lit expression and compute the resulting constant value
		 */
		if (f.isPolynomial()) {
			// this is a polynomial, if the variables of the polynomial are all constants, then we can compute this value
			ValueReplacementMap replacements;

			for(const auto& value : f.extractValues()) {
				ExpressionPtr expr = value;
				/**
				 * This expression could be the deref of a variable. However we are interested in
				 * storing the variable in order to lookup for previous definitions. 
				 *
				 * We remove any deref operations present
				 */
				if (CallExprPtr call = dynamic_pointer_cast<const CallExpr>(expr)) {
					if (core::analysis::isCallOf(call, basicGen.getRefDeref())) { 
						expr = call->getArgument(0);
					}
				}

				auto exprAddr = core::Address<const core::Expression>::find(expr, lit.getAddressedNode());
				// Build an address starting from the analysis stmt 
				exprAddr = core::concat(lit, exprAddr);
				
				cfg::Address cfgAddr(block, stmt_idx, exprAddr);
				auto var = getImmediateAccess(exprAddr->getNodeManager(), cfgAddr, cfg.getTmpVarMap());

				dfa::Value<LiteralPtr> lit = lookup(aMgr, var, in, cfg);

				if (lit.isBottom()) { return dfa::bottom; }
				if (lit.isTop()) 	{ return dfa::top; 	  }

				replacements.insert( { value, toFormula(lit.value()) } );
			}

			// Replace values with the constant values 
			f = f.replace(replacements);

			assert(f.isConstant());

			return toIR(lit->getNodeManager(), f).as<LiteralPtr>();
		}

	} catch(NotAFormulaException&& e) { 
		// we cannot determine whether this is a constant value, we return the bottom symbol then 

		return lookup(aMgr, 
				      getImmediateAccess(lit->getNodeManager(), cfg::Address(block,stmt_idx,lit), cfg.getTmpVarMap()), 
					  in, cfg);
	}

	assert( false  && "Something odd happened" );
}


value_type ConstantPropagation::transfer_func(const value_type& in, const cfg::BlockPtr& block) const {

	value_type gen, kill;
	
	if (block->empty()) { return in; }

	LOG(DEBUG) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
	LOG(DEBUG) << "~ Block " << block->getBlockID();
	LOG(DEBUG) << "~ IN: " << in;

	core::NodeManager& mgr = getCFG().getNodeManager();

	size_t stmt_idx = 0;
	for_each(block->stmt_begin(), block->stmt_end(), [&] (const cfg::Element& cur) {

		++stmt_idx;

		StatementAddress stmt = core::StatementAddress(cur.getAnalysisStatement());

		const lang::BasicGenerator& basicGen = stmt->getNodeManager().getLangBasic();

		auto handle_def = [&](const VariableAddress& varAddr, const ExpressionAddress& init, bool isDecl) { 
				
			cfg::Address cfgAddr(block, stmt_idx-1, varAddr);
			auto defAccess = getImmediateAccess(stmt->getNodeManager(), cfgAddr, getCFG().getTmpVarMap());

			ExpressionAddress initVal = init;

			/**
			 * If the initial value is ref.var(...) or ref.new(...) we can simply remove those
			 * expressions because they have no effect on constant propagation semantics.
			 */
			if (CallExprAddress call = dynamic_address_cast<const CallExpr>(init)) {

				if (core::analysis::isCallOf(call.getAddressedNode(), basicGen.getRefVar()) ||
					core::analysis::isCallOf(call.getAddressedNode(), basicGen.getRefNew()) ) 
				{ 
					initVal = call->getArgument(0);
				}
			}

			dfa::Value<LiteralPtr> res = eval(aMgr, initVal, block, stmt_idx-1, in, getCFG());

			auto defClass = aMgr.findClass(defAccess);
			assert(defClass && "Invalid class for access. Something wrong in the extract() method");

			gen.insert( std::make_tuple(defClass, res) );

			AccessClassSet depClasses;
			depClasses.insert(defClass);
			// Add subclasses which are affected by this definition
			addSubClasses(defClass, depClasses);

			// Kill Entities 
			if (defAccess->isReference()) {
				for(auto it = in.begin(), end=in.end(); it != end; ++it) {
					if (std::find_if( depClasses.begin(), depClasses.end(), [&](const AccessClassPtr& cur) { 
								return *cur == *std::get<0>(*it); 
							}) != depClasses.end() ) { kill.insert( *it ); }
				}
			}
		};

		if (stmt->getNodeType() == NT_Literal) { return; }

		// assume scalar variables 
		if (DeclarationStmtAddress decl = dynamic_address_cast<const DeclarationStmt>(stmt)) {
			handle_def( decl->getVariable(), decl->getInitialization(), true );

		} else if (CallExprAddress call = dynamic_address_cast<const CallExpr>(stmt)) {

			if (core::analysis::isCallOf(call.getAddressedNode(), basicGen.getRefAssign()) ) { 
				handle_def( call->getArgument(0).as<VariableAddress>(), call->getArgument(1), false );
			}

			// do nothing otherwise
		} else if ( cur.getType() == cfg::Element::LOOP_INCREMENT ) {
			// make sure that the loop iterator is not a constant 
			//
//			Access acc = getImmediateAccess(
//					cur.getStatementAddress().as<ForStmtAddress>()->getDeclaration()->getVariable(),
//					{ nullptr, 0 },
//					getCFG().getTmpVarMap()
//				);
//			gen.insert( std::make_tuple(acc, dfa::bottom) );
//			
//			// kill all declarations reaching this block 
//			std::copy_if(in.begin(), in.end(), std::inserter(kill,kill.begin()), 
//					[&](const typename value_type::value_type& cur){
//						return isConflicting(std::get<0>(cur), acc, getCFG().getTmpVarMap());
//					} );
//

		} else {

			LOG(WARNING) << stmt;
			assert(false && "Stmt not handled");

		}
	});

	LOG(DEBUG) << "~ KILL: " << kill;
	LOG(DEBUG) << "~ GEN:  " << gen;

	value_type set_diff, ret;
	std::set_difference(in.begin(), in.end(), kill.begin(), kill.end(), std::inserter(set_diff, set_diff.begin()));
	std::set_union(set_diff.begin(), set_diff.end(), gen.begin(), gen.end(), std::inserter(ret, ret.begin()));

	//LOG(INFO) << "~ RET: " << ret;
	return ret;
}

} } } } // end insieme::analysis::dfa::analyses namespace 
