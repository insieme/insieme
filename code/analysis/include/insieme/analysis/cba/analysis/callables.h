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

#include "insieme/analysis/cba/framework/set_type.h"
#include "insieme/analysis/cba/framework/basic_data_flow_constraint_resolver.h"

#include "insieme/core/forward_decls.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	// ----------------- inter-procedural control flow ------------------

	// the type used to represent functions / closures
	struct Callable : public utils::Printable {
		core::ExpressionAddress definition;
		Context context;

		Callable(const core::LiteralAddress& lit)
			: definition(lit.getAddressedNode()), context() {}
		Callable(const core::LambdaExprAddress& fun)
			: definition(fun), context() {}
		Callable(const core::BindExprAddress& bind, const Context& context)
			: definition(bind), context(context) {}
		Callable(const core::ExpressionAddress& expr, const Context& context = Context())
			: definition(expr), context(context) { assert(isBind() || isLambda()); }
		Callable(const Callable& other)
			: definition(other.definition), context(other.context) {}

		bool operator<(const Callable& other) const {
			if (definition != other.definition) return definition < other.definition;
			if (context != other.context) return context < other.context;
			return false;
		}
		bool operator==(const Callable& other) const {
			if (this == &other) return true;
			return definition == other.definition && context == other.context;
		}

		bool operator!=(const Callable& other) const { return !(*this == other); }

		bool isBind() const { return definition->getNodeType() == core::NT_BindExpr; }
		bool isLambda() const { return definition->getNodeType() == core::NT_LambdaExpr; };

		std::size_t getNumParams() const { return definition->getType().as<core::FunctionTypePtr>()->getParameterTypes().size(); }
		core::StatementAddress getBody() const {
			assert(isBind() || isLambda());
			return (isBind())
					? definition.as<core::BindExprAddress>()->getCall().as<core::StatementAddress>()
					: definition.as<core::LambdaExprAddress>()->getBody().as<core::StatementAddress>();
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			if (auto lit = definition.isa<core::LiteralPtr>()) return out << *lit;
			return out << "(" << definition->getNodeType() << "@" << definition << "," << context << ")";
		}
	};

	template<typename C> class ControlFlowConstraintResolver;

	const TypedSetType<Callable,ControlFlowConstraintResolver>& C() {
		static const TypedSetType<Callable,ControlFlowConstraintResolver> instance("C");
		return instance;
	}

	const TypedSetType<Callable,ControlFlowConstraintResolver>& c() {
		static const TypedSetType<Callable,ControlFlowConstraintResolver> instance("c");
		return instance;
	}


	template<typename Context>
	class ControlFlowConstraintResolver : public BasicDataFlowConstraintResolver<Callable,Context> {

		typedef BasicDataFlowConstraintResolver<Callable,Context> super;

	public:

		ControlFlowConstraintResolver(CBA& cba)
			: super(cba, C, c) { };

		void visitLiteral(const LiteralAddress& literal, const Context& ctxt, Constraints& constraints) {

			// and default handling
			super::visitLiteral(literal, ctxt, constraints);

			// only interested in functions ...
			if (!literal->getType().isa<FunctionTypePtr>()) return;

			// add constraint: literal \in C(lit)
			auto value = Callable(literal);
			auto l_lit = cba.getLabel(literal);

			auto C_lit = cba.getSet(C, l_lit, ctxt);
			constraints.add(elem(value, C_lit));

		}

		void visitLambdaExpr(const LambdaExprAddress& lambda, const Context& ctxt, Constraints& constraints) {

			// and default handling
			super::visitLambdaExpr(lambda, ctxt, constraints);

			// add constraint: lambda \in C(lambda)
			auto value = Callable(lambda);
			auto label = cba.getLabel(lambda);

			constraints.add(elem(value, cba.getSet(C, label, ctxt)));

			// TODO: handle recursions

		}

		void visitBindExpr(const BindExprAddress& bind, const Context& ctxt, Constraints& constraints) {

			// and default handling
			super::visitBindExpr(bind, ctxt, constraints);

			// add constraint: bind \in C(bind)
			auto value = Callable(bind, ctxt);
			auto label = cba.getLabel(bind);

			auto C_bind = cba.getSet(C, label, ctxt);
			constraints.add(elem(value, C_bind));

		}

	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
