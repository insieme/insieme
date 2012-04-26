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

#include "insieme/transform/rulebased/transformations.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/transform/pattern/pattern.h"
#include "insieme/transform/pattern/ir_pattern.h"
#include "insieme/transform/pattern/generator.h"
#include "insieme/transform/pattern/ir_generator.h"

namespace insieme {
namespace transform {
namespace rulebased {

	using namespace pattern;
	using namespace pattern::generator;
	using namespace pattern::generator::irg;


	namespace p = pattern;
	namespace g = pattern::generator;
	namespace irp = pattern::irp;
	namespace irg = pattern::generator::irg;


	// --- Abstract base class of rule-based transformation ----

	RuleBasedTransformation::RuleBasedTransformation(const TransformationType& type, const parameter::Value& param, const vector<pattern::Rule>& rules)
		: Transformation(type, param), rules(rules) {}


	core::NodePtr RuleBasedTransformation::apply(const core::NodePtr& target) const {
		// the first matching rule will be applied
		for(auto it = rules.begin(); it != rules.end(); ++it) {
			core::NodePtr res = it->applyTo(target);
			if (res) {
				return res;
			}
		}
		throw InvalidTargetException(target);
	}



	CompoundElimination::CompoundElimination(const parameter::Value& value)
			: RuleBasedTransformation(
					CompoundEliminationType::getInstance(), value,

					pattern::Rule(  		// {{x}} => {x}
							irp::compoundStmt(irp::compoundStmt(p::listVar("stmts"))),
							irg::compoundStmt(g::listVar("stmts"))
					),
					pattern::Rule(			// {x...x {} y...y} => {x...x,y...y}
							irp::compoundStmt(p::listVar("before") << irp::compoundStmt() << p::listVar("after")),
							irg::compoundStmt(g::listVar("before") << g::listVar("after"))
					),
					pattern::Rule()   		// otherwise do nothing
			) {};


	LoopUnrolling::LoopUnrolling(const parameter::Value& params)
		: RuleBasedTransformation(
			LoopUnrollingType::getInstance(), params,

			pattern::Rule(

				// for[V.L.U.S.BODY]

				irp::forStmt(p::var("V", irp::variable(p::var("T"), p::any)),p::var("L"),p::var("U"),p::var("S"),p::var("BODY")),

				// parameter f ... unrolling factor
				// number of iterations of original loop: n  = ((U-L) div S) + 1
				// add(div(sub(U,L),S),1)
				// irg::add(irg::div(irg::sub(g::var("U"),g::var("L")),g::var("S")),irg::literal(irg::int4(),1))
				// number of iterations of unrolled loop: n' = n div f = (((U-L) div S) + 1) div f
				// div(n,f)
				// irg::div(irg::add(irg::div(irg::sub(g::var("U"),g::var("L")),g::var("S")),irg::literal(irg::int4(),1)),irg::literal(irg::int4(),parameter::getValue<unsigned>(params)))
				// remaining iterations of original loop: r  = n mod f = (((U-L) div S) + 1) mod f
				// mod(n,f)
				// irg::mod(irg::add(irg::div(irg::sub(g::var("U"),g::var("L")),g::var("S")),irg::literal(irg::int4(),1)),irg::literal(irg::int4(),parameter::getValue<unsigned>(params)))

				irg::compoundStmt(
					// unrolled loop
					// *lower bound* : L' = L
					//  g::var("L")
					// *upper bound* : U' = L+(n'-1)*S*f = L+(((((U-L) div S) + 1) div f)-1)*S*f
					//  add(L,mul(sub(n',1),mul(S,f)))
					//  irg:add(g::var("L"),irg::mul(irg::sub(irg::div(irg::add(irg::div(irg::sub(g::var("U"),g::var("L")),g::var("S")),irg::literal(irg::int4(),1)),irg::literal(irg::int4(),parameter::getValue<unsigned>(params))),irg::literal(irg::int4(),1)),irg::mul(g::var("S"),f)))
					// *step*        : S' = S*f
					//  mul(S,f)
					//  irg::mul(g::var("S"),irg::literal(irg::int4(),parameter::getValue<unsigned>(params)))
					irg::forStmt(g::var("V"),
						g::var("L"),
						// u - (u-l)%(f*s)
						//irg::sub(g::var("U"), irg::mod(irg::sub(g::var("U"), g::var("L")), irg::mul(irg::literal(g::var("T"),parameter::getValue<unsigned>(params)), g::var("S")))),

						// orig
						//irg::add(g::var("L"),irg::mul(irg::div(irg::add(irg::div(irg::sub(g::var("U"),g::var("L")),g::var("S")),irg::literal(g::var("T"),1)),irg::literal(g::var("T"),parameter::getValue<unsigned>(params))),irg::mul(g::var("S"),irg::literal(g::var("T"),parameter::getValue<unsigned>(params))))),
						// l+((u-l-1)/s + 1)/f * s * f;

						irg::simplify(irg::add(g::var("L"),irg::mul(irg::mul(irg::div(irg::add(irg::div(irg::sub(irg::sub(g::var("U"),g::var("L")),irg::literal(g::var("T"),1)),g::var("S")),irg::literal(g::var("T"),1)),irg::literal(g::var("T"),parameter::getValue<unsigned>(params))),g::var("S")),irg::literal(g::var("T"),parameter::getValue<unsigned>(params))))),
						irg::simplify(irg::mul(g::var("S"),irg::literal(g::var("T"),parameter::getValue<unsigned>(params)))),
						irg::forEach("_i",0,parameter::getValue<unsigned>(params),
							g::substitute(
								g::var("BODY"),
								irg::simplify(irg::add(g::var("V"), irg::mul(g::var("S"),irg::literal(g::var("T"),g::var("_i"))))),
								g::var("V")
							)
						)
					) <<
					// remaining loop
					// *lower bound* : L" = L+n'*S*f = L+((((U-L) div S) + 1) div f)*S*f
					//  add(L,mul(n',mul(S,f)))
					//  irg:add(g::var("L"),irg::mul(irg::div(irg::add(irg::div(irg::sub(g::var("U"),g::var("L")),g::var("S")),irg::literal(irg::int4(),1)),irg::literal(irg::int4(),parameter::getValue<unsigned>(params))),irg::mul(g::var("S"),f)))
					// *upper bound* : U" = L"+(r-1)*S = L"+(((((U-L) div S) + 1) mod f)-1)*S
					// *upper bound* : U" = U (simpler)
					// *step*        : S" = S
					irg::forStmt(g::var("V"),
						//irg::sub(g::var("U"), irg::mod(irg::sub(g::var("U"), g::var("L")), irg::mul(irg::literal(g::var("T"),parameter::getValue<unsigned>(params)), g::var("S")))),
						//irg::add(g::var("L"),irg::mul(irg::div(irg::add(irg::div(irg::sub(g::var("U"),g::var("L")),g::var("S")),irg::literal(g::var("T"),1)),irg::literal(g::var("T"),parameter::getValue<unsigned>(params))),irg::mul(g::var("S"),irg::literal(g::var("T"),parameter::getValue<unsigned>(params))))),
						irg::simplify(irg::add(g::var("L"),irg::mul(irg::mul(irg::div(irg::add(irg::div(irg::sub(irg::sub(g::var("U"),g::var("L")),irg::literal(g::var("T"),1)),g::var("S")),irg::literal(g::var("T"),1)),irg::literal(g::var("T"),parameter::getValue<unsigned>(params))),g::var("S")),irg::literal(g::var("T"),parameter::getValue<unsigned>(params))))),
						g::var("U"),
						g::var("S"),
						g::var("BODY")
					)
				)
				// ------------------------------------------------------------
			)
		) {

		if (parameter::getValue<unsigned>(params) < 2) {
			throw InvalidParametersException("Loop unrolling factor has to be >= 2!");
		}

	};


	TransformationPtr makeLoopUnrolling(size_t factor) {
		return std::make_shared<LoopUnrolling>(parameter::makeValue<unsigned>(factor));
	}


	namespace {

		/**
		 * A utility match-expression required by the total loop unrolling transformation. It creates
		 * a sequence of string-value nodes iterating over the interval [0 .. e-s : st) where e,
		 * s and st are obtained by interpreting the expressions bound to the variable names
		 * start, end and step.
		 */
		MatchExpressionPtr deltaRange(const string& var_start, const string& var_end, const string& var_step) {
			return std::make_shared<expression::Constructor<ptr_target>>([=](const Match<ptr_target>& match)->MatchValue<ptr_target> {
				core::NodeManager& manager = match.getRoot()->getNodeManager();
				core::IRBuilder builder(manager);

				// resolve start/end variables
				assert(match.isVarBound(var_start) && "Start variable not bound to value!");
				assert(match.isVarBound(var_end)   && "End variable not bound to value!");
				assert(match.isVarBound(var_step)  && "Step variable not bound to value!");

				const auto& start_value = match.getVarBinding(var_start).getValue();
				const auto& end_value   = match.getVarBinding(var_end).getValue();
				const auto& step_value  = match.getVarBinding(var_step).getValue();

				assert(start_value->getNodeCategory() == core::NC_Expression && "Start variable must be bound to an expression!");
				assert(end_value->getNodeCategory()   == core::NC_Expression && "End variable must be bound to an expression");
				assert(step_value->getNodeCategory()  == core::NC_Expression && "Step variable must be bound to an expression");


				try {

					auto start_formula = core::arithmetic::toPiecewise(start_value.as<core::ExpressionPtr>());
					auto end_formula   = core::arithmetic::toPiecewise(end_value.as<core::ExpressionPtr>());
					auto step_formula  = core::arithmetic::toPiecewise(step_value.as<core::ExpressionPtr>());

					auto diff_formula = end_formula - start_formula;

					// check whether values are constants
					if (!(diff_formula.isFormula() && diff_formula.toFormula().isInteger())) {
						throw InvalidTargetException("Number of iterations is not constant!");
					}
					if (!(step_formula.isFormula() && step_formula.toFormula().isInteger())) {
						throw InvalidTargetException("Step size is not constant!");
					}

					int diff = diff_formula.toFormula().getConstantValue();
					int step = step_formula.toFormula().getConstantValue();

					vector<MatchValue<ptr_target>> res;
					for(int i=0; i<diff; i+= step) {
						core::NodePtr expr = builder.stringValue(toString(i));
						res.push_back(MatchValue<ptr_target>(expr));
					}

					return res;

				} catch (const core::arithmetic::NotAFormulaException& nfe) {
					// fail transformation
					throw InvalidTargetException("Loop boundaries or step size is not a formula!");
				}
			}, format("[0,..,%s-%s : %s)", var_end.c_str(), var_start.c_str(), var_step.c_str()));
		}
	}


	TotalLoopUnrolling::TotalLoopUnrolling(const parameter::Value& params)
		: RuleBasedTransformation(
			TotalLoopUnrollingType::getInstance(), params,

			pattern::Rule(

				// match the for-loop
				irp::forStmt(p::var("V", irp::variable(p::var("T"), p::any)),p::var("L"),p::var("U"),p::var("S"),p::var("BODY")),

				// create an alternative unfolded list of statements
				irg::compoundStmt(
					g::forEach("_i",deltaRange("L","U","S"),
						g::substitute(
							g::var("BODY"),
							irg::simplify(irg::add(g::var("L"), irg::literal(g::var("T"),g::var("_i")))),
							g::var("V")
						)
					)
				)
				// ------------------------------------------------------------
			)
		) {};

	TransformationPtr makeTotalLoopUnrolling() {
		return std::make_shared<TotalLoopUnrolling>();
	}


	SimpleLoopTiling2D::SimpleLoopTiling2D(const parameter::Value& params)
		: RuleBasedTransformation(
			SimpleLoopTiling2DType::getInstance(), params,

			pattern::Rule(

				// match the 2 nested for-loops
				irp::forStmt(p::var("V1", irp::variable(p::var("T1"), p::any)),p::var("L1"),p::var("U1"),p::var("S1"),
					irp::forStmt(p::var("V2", irp::variable(p::var("T2"), p::any)),p::var("L2"),p::var("U2"),p::var("S2"),
						p::var("BODY")
					)
				),

				// create the tiled replacement
				g::let("ii", irg::variable(g::var("T1")), g::let("tsA", irg::literal(g::var("T1"),parameter::getValue<unsigned>(params, 0)),
				g::let("jj", irg::variable(g::var("T2")), g::let("tsB", irg::literal(g::var("T2"),parameter::getValue<unsigned>(params, 1)),

					irg::forStmt(g::var("ii"), g::var("L1"), g::var("U1"), irg::mul(g::var("tsA"),g::var("S1")),
						irg::forStmt(g::var("jj"), g::var("L2"), g::var("U2"), irg::mul(g::var("tsB"),g::var("S2")),
							irg::forStmt(g::var("V1"), g::var("ii"), irg::min(irg::add(g::var("ii"), g::var("tsA")), g::var("U1")), g::var("S1"),
								irg::forStmt(g::var("V2"), g::var("jj"), irg::min(irg::add(g::var("jj"), g::var("tsB")), g::var("U2")), g::var("S2"),
									g::var("BODY")
								)
							)
						)
					)

				))
				))
			)
		) {};

	TransformationPtr makeSimpleLoopTiling2D(unsigned tsA, unsigned tsB) {
		return std::make_shared<SimpleLoopTiling2D>(parameter::combineValues(tsA, tsB));
	}

	SimpleLoopTiling3D::SimpleLoopTiling3D(const parameter::Value& params)
		: RuleBasedTransformation(
			SimpleLoopTiling3DType::getInstance(), params,

			pattern::Rule(

				// match the 2 nested for-loops
				irp::forStmt(p::var("V1", irp::variable(p::var("T1"), p::any)),p::var("L1"),p::var("U1"),p::var("S1", irp::literal("1")),
					irp::forStmt(p::var("V2", irp::variable(p::var("T2"), p::any)),p::var("L2"),p::var("U2"),p::var("S2", irp::literal("1")),
						irp::forStmt(p::var("V3", irp::variable(p::var("T3"), p::any)),p::var("L3"),p::var("U3"),p::var("S3", irp::literal("1")),
								p::var("BODY")
						)
					)
				),

				// create the tiled replacement
				g::let("ii", irg::variable(g::var("T1")), g::let("tsA", irg::literal(g::var("T1"),parameter::getValue<unsigned>(params, 0)),
				g::let("jj", irg::variable(g::var("T2")), g::let("tsB", irg::literal(g::var("T2"),parameter::getValue<unsigned>(params, 1)),
				g::let("kk", irg::variable(g::var("T3")), g::let("tsC", irg::literal(g::var("T3"),parameter::getValue<unsigned>(params, 2)),

					irg::forStmt(g::var("ii"), g::var("L1"), g::var("U1"), g::var("tsA"),
						irg::forStmt(g::var("jj"), g::var("L2"), g::var("U2"), g::var("tsB"),
							irg::forStmt(g::var("kk"), g::var("L3"), g::var("U3"), g::var("tsC"),
								irg::forStmt(g::var("V1"), g::var("ii"), irg::min(irg::add(g::var("ii"), g::var("tsA")), g::var("U1")), g::var("S1"),
									irg::forStmt(g::var("V2"), g::var("jj"), irg::min(irg::add(g::var("jj"), g::var("tsB")), g::var("U2")), g::var("S2"),
										irg::forStmt(g::var("V3"), g::var("kk"), irg::min(irg::add(g::var("kk"), g::var("tsC")), g::var("U3")), g::var("S3"),
											g::var("BODY")
										)
									)
								)
							)
						)
					)

				))
				))
				))
			)
		) {};

	TransformationPtr makeSimpleLoopTiling3D(unsigned tsA, unsigned tsB, unsigned tsC) {
		return std::make_shared<SimpleLoopTiling3D>(parameter::combineValues(tsA, tsB, tsC));
	}

} // end namespace rulebased
} // end namespace transform
} // end namespace insieme
