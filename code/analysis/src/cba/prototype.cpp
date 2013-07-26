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

#include "insieme/analysis/cba/prototype.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	VariableAddress getDefinitionPoint(const VariableAddress& varAddress) {

		// extract the variable
		VariablePtr var = varAddress.getAddressedNode();

		// start walking up the address
		NodeAddress cur = varAddress;

		// check the parent
		while (!cur.isRoot()) {
			auto pos = cur.getIndex();
			cur = cur.getParentAddress();
			switch(cur->getNodeType()) {

			case NT_Parameters: {
				return varAddress;	// this variable is a parameter definition
			}

			case NT_Lambda: {

				// check parameters
				for(auto param : cur.as<LambdaAddress>()->getParameters()) {
					if (param.as<VariablePtr>() == var) {
						return param;		// found it
					}
				}

				// otherwise continue with parent
				break;
			}

			case NT_LambdaBinding: {
				// check the bound variable
				auto boundVar = cur.as<LambdaBindingAddress>()->getVariable();
				if (boundVar.as<VariablePtr>() == var) {
					return boundVar;
				}

				// keep on searching
				break;
			}

			case NT_BindExpr: {
				// check parameters
				for(auto param : cur.as<BindExprAddress>()->getParameters()) {
					if (param.as<VariablePtr>() == var) {
						return param;		// found it
					}
				}

				// not here
				break;
			}

			case NT_CompoundStmt: {

				// check whether there is an earlier declaration
				auto compound = cur.as<CompoundStmtAddress>();
				for(int i = pos; i >= 0; i--) {
					if (auto decl = compound[i].isa<DeclarationStmtAddress>()) {
						if (decl->getVariable().as<VariablePtr>() == var) {
							return decl->getVariable();
						}
					}
				}

				// otherwise continue with parent
				break;
			}

			default: break;
			}
		}

		// the variable is a free variable in this context
		return VariableAddress(var);
	}


	namespace {

		using namespace utils::set_constraint;

		class ConstraintCollector : public IRVisitor<void, Address> {

			typedef IRVisitor<void,Address> super;

			CBAContext& context;
			Constraints& constraints;

			utils::set::PointerSet<NodeAddress> processed;

			// the list of all terms in the targeted code
			std::vector<pair<Value, ExpressionAddress>> terms;

		public:

			ConstraintCollector(CBAContext& context, Constraints& contraints, const StatementAddress& root)
				: context(context), constraints(contraints) {

				// collect all terms in the code
				visitDepthFirst(root, [&](const ExpressionAddress& cur) {
					// TODO: also add all recursion variations
					terms.push_back(std::make_pair(context.getValue(cur), cur));
				});

			};

//			virtual void visit(const NodeAddress& node) {
//				// process all addresses only once
//				if (!processed.insert(node).second) return;
//				super::visit(node);
//			}


			void visitCompoundStmt(const CompoundStmtAddress& compound) {
				// just collect constraints from elements
				// TODO: add data flow constraints
				for(auto cur : compound) visit(cur);
			}

			void visitDeclarationStmt(const DeclarationStmtAddress& decl) {

				// add constraint r(var) \subset C(init)
				auto var = context.getVariable(decl->getVariable());
				auto l_init = context.getLabel(decl->getInitialization());

				// TODO: distinguish between control and data flow!
				auto r_var = context.getSet(r, var);
				auto C_init = context.getSet(C, l_init);
				constraints.insert(subset(C_init, r_var));		// TODO: add context (passed by argument)

				// finally, add constraints for init expression
				visit(decl->getInitialization());
			}


			void visitLiteral(const LiteralAddress& literal) {

				// TODO: distinguish between control and data flow

				// add constraint literal \in C(lit)
				auto value = context.getValue(literal);
				auto l_lit = context.getLabel(literal);

				auto C_lit = context.getSet(C, l_lit);
				constraints.insert(elem(value, C_lit));

			}

			void visitVariable(const VariableAddress& variable) {

				// TODO: distinguish between control and data flow

				// add constraint r(var) \subset C(var)
				auto var = context.getVariable(variable);
				auto l_var = context.getLabel(variable);

				auto r_var = context.getSet(r, var);
				auto C_var = context.getSet(C, l_var);

				constraints.insert(subset(r_var, C_var));
			}

			void visitLambdaExpr(const LambdaExprAddress& lambda) {

				// add constraint lambda \in C(lambda)
				auto label = context.getLabel(lambda);
				auto value = context.getValue(lambda);

				constraints.insert(elem(value, context.getSet(C, label)));

				// TODO: handle recursions

				// and add constraints for the body
				visit(lambda->getBody());
			}

			void visitCallExpr(const CallExprAddress& call) {

				// add constraints for function and argument expressions
				visit(call->getFunctionExpr());
				for(auto arg : call) visit(arg);

				// get values of function
				auto c_fun = context.getSet(C, context.getLabel(call->getFunctionExpr()));

				// fix pass-by-value semantic - by considering all potential terms
				for(auto cur : terms) {

					auto t = cur.first;
					auto expr = cur.second;

					// only searching for actual code
					if (!expr.isa<LambdaExprPtr>() && !expr.isa<BindExprPtr>()) continue;

					// check whether the term is a function with the right number of arguments
					// TODO: also check type?
					auto funType = expr.as<ExpressionPtr>()->getType().isa<FunctionTypePtr>();
					if(funType->getParameterTypes().size() != call.size()) continue;		// this is not a potential function

					assert(expr.isa<LambdaExprPtr>() && "Binds not implemented yet!");

					// add constraints for arguments
					auto lambda = expr.isa<LambdaExprAddress>();
					for(std::size_t i=0; i<call.size(); i++) {

						// add constraint: t \in C(fun) => C(arg) \subset r(param)
						auto l_arg = context.getLabel(call[i]);
						auto param = context.getVariable(lambda->getParameterList()[i]);

						auto c_arg = context.getSet(C, l_arg);
						auto r_param = context.getSet(r, param);
						constraints.insert(subsetIf(t, c_fun, c_arg, r_param));

						// TODO: add constraint for result type!
					}
				}
			}

			void visitNode(const NodeAddress& node) {
				std::cout << "Reached unsupported Node Type: " << node->getNodeType() << "\n";
				assert(false);
			}

		};

	}


	Constraints generateConstraints(CBAContext& context, const StatementPtr& stmt) {

		// create resulting list of constraints
		Constraints res;

		// let constraint collector do the job
		StatementAddress root(stmt);
		ConstraintCollector(context, res, root).visit(root);

		// done
		return res;


	}


	Solution solve(const Constraints& constraints) {
		// just use the utils solver
		return utils::set_constraint::solve(constraints);
	}

	core::ExpressionSet getValuesOf(CBAContext& context, const Solution& solution, const core::ExpressionAddress& expr) {
		core::ExpressionSet res;

		auto label = context.getLabel(expr);
		auto pos = solution.find(context.getSet(C, label));

		// check whether there is a result
		if (pos == solution.end()) return res;

		// convert result set into expressions
		for (auto i : pos->second) {
			res.insert(context.getExpr(i));
		}

		// done
		return res;
	}

	core::ExpressionSet getValuesOf(CBAContext& context, const Solution& solution, const core::VariableAddress& varAdr) {
		core::ExpressionSet res;

		auto var = context.getVariable(varAdr);
		auto pos = solution.find(context.getSet(r, var));

		// check whether there is a result
		if (pos == solution.end()) return res;

		// convert result set into expressions
		for (auto i : pos->second) {
			res.insert(context.getExpr(i));
		}

		// done
		return res;
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
