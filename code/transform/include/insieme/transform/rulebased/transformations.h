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

#pragma once

#include <vector>
#include "insieme/transform/transformation.h"

#include "insieme/transform/pattern/rule.h"

namespace insieme {
namespace transform {
namespace rulebased {

	/**
	 * A class realizing a transformation based on a set of transformation rules.
	 * The list of rules is scanned until one of the rules is matching. The transformed
	 * code will then be returned. If no rule is matching, no transformation will
	 * be applied.
	 */
	class RuleBasedTransformation : public Transformation {

		/**
		 * The set of rules to be tested.
		 */
		vector<pattern::Rule> rules;

	public:

		/**
		 * A constructor allowing to specify an arbitrary number of rules.
		 *
		 * @param type the type of the derived transformation
		 * @param param the parameters specifying details of this transformation
		 * @param rules the set of rules to be used by the resulting transformation
		 */
		template<typename ... Rules>
		RuleBasedTransformation(const TransformationType& type, const parameter::Value& param, const Rules& ... rules)
			: Transformation(type, param), rules(toVector<pattern::Rule>(rules...)) {}

		/**
		 * A constructor accepting a list of rules.
		 *
		 * @param type the type of the derived transformation
		 * @param param the parameters specifying details of this transformation
		 * @param rules the set of rules to be used by the resulting transformation
		 */
		RuleBasedTransformation(const TransformationType& type, const parameter::Value& param, const vector<pattern::Rule>& rules);

		/**
		 * Implements the actual transformation by scanning through the internally
		 * stored list of rules.
		 *
		 * @param target the node to be transformed
		 */
		virtual core::NodePtr apply(const core::NodePtr& target) const;

		/**
		 * Obtains a reference to the internally used rules.
		 */
		const vector<pattern::Rule>& getRules() const {
			return rules;
		}
	};

	/**
	 * A simple example transformation based on rules. This transformation is eliminating
	 * superfluous brackets / compound statement nodes.
	 */
	struct CompoundElimination : public RuleBasedTransformation {

		CompoundElimination(const parameter::Value& value);

		/**
		 * Prints a readable representation of this transformation to the given output stream
		 * using the given indent.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
			return out << indent << "CompoundElimination";
		}

	};

	TRANSFORMATION_TYPE(
		CompoundElimination,
		"Eliminates superfluous compound statements.",
		parameter::no_parameters()
	);


	// -- Loop Unrolling --

	struct LoopUnrolling : public RuleBasedTransformation {

		LoopUnrolling(const parameter::Value& params);

		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
			return out << indent << "Loop Unrolling " << parameter::getValue<unsigned>(getParameters());
		}
	};

	/**
	 * Factory for the loop unrolling transformation, full version
	 */
	TRANSFORMATION_TYPE(
		LoopUnrolling,
		"Implementation of the loop unrolling transformation based on the pattern matcher.",
		parameter::atom<unsigned>("The unrolling factor to be used.")
	);


	/**
	 * Utility method to create a loop unrolling transformation which when applied to a loop
	 * is unrolling the body for the given number of times.
	 */
	TransformationPtr makeLoopUnrolling(size_t factor);


	// -- Total Loop Unrolling --

	/**
	 * Total loop unrolling tries to replace a loop with a fixed number of iterations with
	 * the corresponding unrolled program code.
	 */
	struct TotalLoopUnrolling : public RuleBasedTransformation {

		TotalLoopUnrolling(const parameter::Value& params = parameter::emptyValue);

		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
			return out << indent << "Total Loop Unrolling";
		}
	};

	/**
	 * Factory for the loop unrolling transformation, full version
	 */
	TRANSFORMATION_TYPE(
		TotalLoopUnrolling,
		"Implementation of the total loop unrolling transformation.",
		parameter::no_parameters()
	);

	/**
	 * Utility method to create a total loop unrolling transformation instance.
	 */
	TransformationPtr makeTotalLoopUnrolling();



	// -- Simple Loop Tiling --

	/**
	 * A simple non-checked implementation of loop tiling.
	 */
	struct SimpleLoopTiling2D : public RuleBasedTransformation {

		SimpleLoopTiling2D(const parameter::Value& params);

		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
			return out << indent << "Simple Loop Tiling 2D(" << getParameters() << ")";
		}
	};

	/**
	 * Factory for the simple loop tiling transformation.
	 */
	TRANSFORMATION_TYPE(
		SimpleLoopTiling2D,
		"Implementation of loop tiling using rules. Unlike the polyhedral implementation, this one is not checking for dependencies.",
		parameter::tuple(
				parameter::atom<unsigned>("Tilesize A"),
				parameter::atom<unsigned>("Tilesize B")
		)
	);

	/**
	 * Utility method to create a simple 2D loop tiling transformation.
	 */
	TransformationPtr makeSimpleLoopTiling2D(unsigned tsA, unsigned tsB);

	/**
	 * A simple non-checked implementation of loop tiling.
	 */
	struct SimpleLoopTiling3D : public RuleBasedTransformation {

		SimpleLoopTiling3D(const parameter::Value& params);

		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
			return out << indent << "Simple Loop Tiling 3D(" << getParameters() << ")";
		}
	};

	/**
	 * Factory for the simple loop tiling transformation.
	 */
	TRANSFORMATION_TYPE(
		SimpleLoopTiling3D,
		"Implementation of loop tiling using rules. Unlike the polyhedral implementation, this one is not checking for dependencies.",
		parameter::tuple(
				parameter::atom<unsigned>("Tilesize A"),
				parameter::atom<unsigned>("Tilesize B"),
				parameter::atom<unsigned>("Tilesize C")
		)
	);

	/**
	 * Utility method to create a simple 3D loop tiling transformation.
	 */
	TransformationPtr makeSimpleLoopTiling3D(unsigned tsA, unsigned tsB, unsigned tsC);



//	// TRAFO --------------------------------------------------------------------------
//	// loop interchange - two perfectly nested loops
//	// --------------------------------------------------------------------------------
//	struct BinaryLoopInterchange : public RuleBasedTransformation {
//			BinaryLoopInterchange() : RuleBasedTransformation(pattern::Rule(
//					// ------------------------------------------------------------
//					// for[V1.L1.U1.S1.for[V2.L2.U2.S2.BODY]]
//					irp::forStmt(p::var("V1"),p::var("L1"),p::var("U1"),p::var("S1"),
//						irp::forStmt(p::var("V2"),p::var("L2"),p::var("U2"),p::var("S2"),
//							p::var("BODY"))),
//					// =>
//					// for(V2,L2,U2,S2,for(V1,L1,U1,S1,BODY))
//					irg::forStmt(g::var("V2"),g::var("L2"),g::var("U2"),g::var("S2"),
//						irg::forStmt(g::var("V1"),g::var("L1"),g::var("U1"),g::var("S1"),
//							g::var("BODY")))
//					// ------------------------------------------------------------
//			)
//			) {};
//
//		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
//			return out << indent << "<loop interchange /2>";
//		}
//	};
//
//
//	// TRAFO --------------------------------------------------------------------------
//	// loop distribution - one level
//	// requires a loop body which is a compound-stmt
//	// assuming, that a compound statement always contains > 1 stmts
//	// --------------------------------------------------------------------------------
//	// INPUT:
//	//
//	// for(V = L to U step S) {
//	// 		s_1
//	// 		...
//	// 		s_n
//	// 		}
//	// --------------------------------------------------------------------------------
//	// OUTPUT:
//	//
//	// for(V = L to U step S) s_1
//	// ...
//	// for(V = L to U step S) s_n
//	// --------------------------------------------------------------------------------
//	struct LoopDistribution1: public RuleBasedTransformation {
//		   LoopDistribution1(): RuleBasedTransformation(pattern::Rule(
//			// ------------------------------------------------------------
//			// for[V.L.U.S.BODY:compound] // BODY: the compound-stmt
//			irp::forStmt(p::var("V"),p::var("L"),p::var("U"),p::var("S"),p::treeVar("BODY")),
//			// =>
//			// compound( { _s in BODY | for(V,L,U,S,_s)} )
//			irg::compoundStmt(g::forEach("_s",g::childrenOf(g::varExpr("BODY")), // ?? schreibweise (sw.) für "alle kinder von BODY" ?
//				irg::forStmt(g::var("V"),g::var("L"),g::var("U"),g::var("S"),g::var("_s"))))
//			// ------------------------------------------------------------
//			)
//		) {};
//
//		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
//			return out << indent << "<loop distribution /1>";
//		}
//	};
//
//
//	// TRAFO --------------------------------------------------------------------------
//	// loop distribution - one level, version b
//	// requires a loop body which is a compound-stmt
//	// --------------------------------------------------------------------------------
//	struct LoopDistribution1b: public RuleBasedTransformation {
//		   LoopDistribution1b(): RuleBasedTransformation(pattern::Rule(
//			// ------------------------------------------------------------
//			// for[V.L.U.S.BODY:compound] // BODY: the compound-stmt
//			irp::forStmt(p::var("V"),p::var("L"),p::var("U"),p::var("S"),irp::compoundStmt(*p::var("B"))),
//			// =>
//			// compound( { _s in BODY | for(V,L,U,S,_s)} )
//			irg::compoundStmt(g::forEach("_s",g::varExpr("B"),
//				irg::forStmt(g::var("V"),g::var("L"),g::var("U"),g::var("S"),g::var("_s"))))
//			// ------------------------------------------------------------
//			)
//		) {};
//
//		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
//			return out << indent << "<loop distribution /1 b>";
//		}
//	};
//
//
//	struct LoopDistribution22: public RuleBasedTransformation {
//		   LoopDistribution22(): RuleBasedTransformation(pattern::Rule(
//			// ------------------------------------------------------------
//			// for[V.L.U.S.compound[P<{!for}*>.{F<for.{!for}*>}+]]
//			// P[] 	 : sequence
//			// F()[] : vector of sequences,
//			// 		an element _f of this vector is a sequence starting with a loop _f[1]
//			irp::forStmt(p::var("V"),p::var("L"),p::var("U"),p::var("S"),
//					irp::compoundStmt(
//						p::listVar("B",*!irp::forStmt()) <<
//						+(single(p::treeVar("F", irp::forStmt())) << p::listVar("I",*!irp::forStmt()))
//					)
//		   ),
//			// =>
//			// for(V,L,U,S,compound(
//			//	{_p in P | _p}, // copy P
//			//	{_f in F |
//			//		// distribute inner loop _f[1], _f[1]%5 is its body
//			//		{_t in _f[1]%5 | for(_f[1]%1,_f[1]%2,_f[1]%3,_f[1]%4,_t)},
//			//		{_u in _f[2:] | _u} // _f[2:] is the remainder of _f
//			//		} ))
//			irg::forStmt(g::var("V"),g::var("L"),g::var("U"),g::var("S"),g::root)
//			// ------------------------------------------------------------
//			)
//		) {};
//		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
//			return out << indent << "<loop distribution /2/1>";
//		}
//	};


	//	struct LoopUnrollingComplete: public RuleBasedTransformation {
//		   LoopUnrollingComplete(): RuleBasedTransformation(pattern::Rule(
//			// ------------------------------------------------------------
//			// for[V.L:lit.U:lit.S:lit.BODY]
//			irp::forStmt(p::var("V"),p::var("L",irp::int4Literal),p::var("U",irp::int4Literal),p::var("S",irp::int4Literal), // ?? vorr: int4Literal in ir_pattern.h vordefiniert
//				var("BODY")),
//			// =>
//			// compound( { _i = int_val(L),int_val(U),int_val(S) | BODY{ int_lit(_i) / V } })
//			irg::compoundStmt(irg::forEach("_i",irg::evalInteger(g::varExpr("L")),irg::eval(g::varExpr("L")),irg::int4Value("S"), // ?? vorr: int4Value() in ir_generator.h vordefiniert
//				irg::substitute(g::var("BODY"),irg::literal(irg::int4(),g::var("_i")),g::var("V"))))
//			// ------------------------------------------------------------
//			)
//		) {};
//
//		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
//			return out << indent << "<loop unrolling /complete>";
//		}
//	};
//
//
//	struct LoopTiling1: public RuleBasedTransformation {
//		   LoopTiling1(const g::TreeGeneratorPtr& size): RuleBasedTransformation(pattern::Rule(
//			// ------------------------------------------------------------
//			// for[V.L.U.S.BODY]
//
//			irp::forStmt(p::var("V"),p::var("L"),p::var("U"),p::var("S"),p::var("BODY")),
//
//			// =>
//			// for(v=int_var("v_tmp"),L,U,call("mult",S,int_lit(size)),
//			// 		for(V,v,call("sub",call("add",v,
//			// 			call("mult",S,int_lit(size))),int_lit(-1)),S,BODY))
//
//			//irg::forStmt(TreePatternPtr v=irg::variable(irg::int4Type,"v_tmp"),
//			irg::forStmt(irg::bind(irg::freshVar(irg::int4Type),"v_tmp"),
//				g::var("L"),g::var("U"),irg::mul(g::var("S"),size),
//					irg::forStmt(g::var("V"),g::var("v_tmp"),irg::add(g::var("v_tmp"),irg::mul(g::var("S"),size)),g::var("S")),
//						g::var("BODY"))
//			// ------------------------------------------------------------
//			)
//		) {};
//
//		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const {
//			return out << indent << "<loop tiling /1>";
//		}
//	};

} // end namespace rulebased
} // end namespace transform
} // end namespace insieme
