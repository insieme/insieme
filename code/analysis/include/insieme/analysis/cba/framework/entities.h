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

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace cba {

	// a simple function
	typedef core::ExpressionAddress ContextFreeCallable;


	// the type used to represent functions / closures
	template<typename Context>
	struct Callable : public utils::Printable {
		core::ExpressionAddress definition;
		Context context;

		Callable()
			: definition(), context() {}
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
			if (!definition) return out << "-unknown-";
			if (auto lit = definition.isa<core::LiteralPtr>()) return out << *lit;
			return out << "(" << definition->getNodeType() << "@" << definition << "," << context << ")";
		}
	};


	// the type to represent memory locations
	template<typename Context>
	struct Location : public std::pair<core::ExpressionAddress, Context> {
		const core::ExpressionAddress& getAddress() const { return this->first; }
		const Context& getContext() const { return this->second; }
		Location(const core::ExpressionAddress& def = core::ExpressionAddress(), const Context& context = Context())
			: std::pair<core::ExpressionAddress, Context>(def, context) {}
	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
