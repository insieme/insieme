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

#include "insieme/analysis/dfa/analyses/ir_var_entity.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/arithmetic/arithmetic_utils.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis::dfa::analyses;

namespace insieme { namespace analysis { namespace dfa { namespace analyses { 
	

std::set<VarEntity> extractFromStmt(const core::StatementPtr& stmt) {

	std::set<VarEntity> entities; 

	struct EntityVisitor : public IRVisitor<void, Address> {

		std::set<VarEntity>& entities;
		bool isDeref;

		EntityVisitor(std::set<VarEntity>& entities) : 
			IRVisitor<void, Address>(false),
			entities(entities),
			isDeref(false) { }

		void visitExpression(const ExpressionAddress& expr) {

			const lang::BasicGenerator& gen = expr->getNodeManager().getLangBasic();
	
			datapath::DataPathBuilder dpBuilder(expr->getNodeManager());

			if (expr->getNodeType() == NT_Variable) {
				entities.insert( 
					VarEntity(expr, 
						dpBuilder.getPath(), 
						VarType::VAR, 
						isDeref || !core::analysis::isRefType(expr->getType())
					) 
				);
				return;
			}
			
			if (CallExprAddress call = dynamic_address_cast<const CallExpr>(expr)) {

				ExpressionAddress funcExpr = call->getFunctionExpr();

				if (gen.isRefDeref(funcExpr)) {
					// Because of the construction of the CFG, it cannot happen that we have nested
					// deref operations in the same CFG block. 
					assert(!isDeref);
					isDeref = true;
				}

				// Handle member access functions 
				if ( gen.isMemberAccess(funcExpr) ) {

					// this follows from the construction of the CFG 
					assert( call->getArgument(0)->getNodeType() == NT_Variable );

					if ( gen.isUnsignedInt( call->getArgument(1)->getType() ) ) {
						// this is a tuple access
						entities.insert(  
							VarEntity(call->getArgument(0), 
								dpBuilder.component(call->getArgument(1).as<LiteralAddress>().getValue()).getPath(),
								VarType::TUPLE,
								isDeref || !core::analysis::isRefType(call->getType())
							)
						);
						return;
					}

					// This is a member access 
					if ( gen.isIdentifier( call->getArgument(1)->getType() ) ) {

						// this is a tuple access
						entities.insert(  
							VarEntity(call->getArgument(0),
								dpBuilder.member(call->getArgument(1).as<LiteralAddress>()->getValue().getValue()).getPath(),
								VarType::MEMBER, 
								isDeref || !core::analysis::isRefType(call->getType())
							)
						);
						return;
					}

					assert( false && "Type of member access not supported" );
				}

				// Handle Array/Vector subscript operator 
				if ( gen.isSubscriptOperator(funcExpr) ) {
					// this follows from the construction of the CFG 
					assert( call->getArgument(0)->getNodeType() == NT_Variable );

					try {
						arithmetic::Formula f = arithmetic::toFormula(call->getArgument(1));
						if (f.isConstant()) {
							entities.insert(  
								VarEntity(call->getArgument(0),
									dpBuilder.element(static_cast<int64_t>(f.getConstantValue())).getPath(),
								  	VarType::ARRAY, 
								  	isDeref || !core::analysis::isRefType(call->getType())
								)
							);
							return;
						}
					} catch (arithmetic::NotAFormulaException&& e) { }
					
					assert( call->getArgument(1)->getNodeType() == NT_Variable);
					// if the subscript is not a constant, then this could be potentially 
					// an access to any of the array elements
					entities.insert(  
						VarEntity(call->getArgument(0),
							dpBuilder.element(call->getArgument(1).getAddressedNode()).getPath(),
						  	VarType::ARRAY, 
						  	isDeref || !core::analysis::isRefType(call->getType())
						)
					);
					visit(call->getArgument(1));
					return ;
				}
			}

			visitNode(expr);
		}

		// Generic method which recursively visit IR nodes 
		void visitNode(const NodeAddress& node) {
			for(size_t it=0, end=node->getChildList().size(); it != end; ++it) {
				visit( node.getAddressOfChild(it) ); 
			}
		}
	};
	
	EntityVisitor(entities).visit(core::NodeAddress(stmt));
	return entities;
}

} } } } // end insieme::analysis::dfa::analyses namespace 

