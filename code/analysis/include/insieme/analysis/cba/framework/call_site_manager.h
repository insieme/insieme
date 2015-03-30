/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#include <map>
#include <set>

#include "insieme/analysis/cba/framework/context.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/assert.h"
#include "insieme/utils/typed_map.h"

namespace insieme {
namespace analysis {
namespace cba {

	using std::set;
	using std::map;
	using std::size_t;
	using std::vector;

	using namespace core;

	class Caller : public utils::Printable, public utils::Hashable {

		CallExprInstance call;

	public:

		Caller(const CallExprInstance& call)
			: call(call) { assert_true(call); }

		const CallExprInstance& getCall() const {
			return call;
		}

		// -- operators ------------------

		bool operator<(const Caller& other) const {
			return call < other.call;
		}

		bool operator==(const Caller& other) const {
			return this == &other || (call == other.call);
		}

		bool operator!=(const Caller& other) const {
			return !(*this == other);
		}

		operator const CallExprInstance& () const { return call; }

		std::size_t getNumArgs() const {
			assert_true(call) << "Must not be invoked on undefined call expression!";
			return call.size();
		}

		std::ostream& printTo(std::ostream& out) const {
			if (!call) return out << "-unknown-";
			return out << "(" << call << ")";
		}

		std::size_t hash() const {
			return (call) ? call->getNodeHashValue() : 0;
		}
	};

	class Callee : public utils::Printable, public utils::Hashable {

		NodeInstance definition;

	public:

		Callee(const core::LiteralInstance& lit)
			: definition(lit) { assert_true(definition); assert_true(lit->getType().isa<FunctionTypePtr>()); }
		Callee(const core::ExpressionInstance& expr)
			: definition(expr) { assert_true(definition); assert_true(expr->getType().isa<FunctionTypePtr>()); }
		Callee(const core::LambdaInstance& fun)
			: definition(fun) { assert_true(definition); }
		Callee(const core::LambdaExprInstance& fun)
			: definition(fun->getLambda()) { }
		Callee(const core::BindExprInstance& bind)
			: definition(bind) { assert_true(definition); }
		Callee(const Callee& other)
			: definition(other.definition) { assert_true(definition); }
		Callee(const NodeInstance& definition)
			: definition((definition.isa<LambdaExprInstance>() ? definition.as<LambdaExprInstance>()->getLambda() : definition)) {
			assert_true(definition);
			assert_true(isLiteral() || isBind() || isLambda());
		}

		const NodeInstance& getDefinition() const {
			return definition;
		}

		bool operator<(const Callee& other) const {
			return definition < other.definition;
		}
		bool operator==(const Callee& other) const {
			if (this == &other) return true;
			return definition == other.definition;
		}

		bool operator!=(const Callee& other) const {
			return !(*this == other);
		}

		bool isLiteral() const { return !isBind() && !isLambda();  }
		bool isBind()    const { return definition->getNodeType() == core::NT_BindExpr; }
		bool isLambda()  const { return definition->getNodeType() == core::NT_Lambda;   }

		size_t getNumParams() const {
			return getFunctionType()->getParameterTypes().size();
		}

		FunctionTypePtr getFunctionType() const {
			assert_true(definition);
			if (LambdaPtr lambda = definition.isa<LambdaPtr>()) {
				return lambda->getType();
			}
			return definition.as<ExpressionPtr>()->getType().as<FunctionTypePtr>();
		}

		StatementInstance getBody() const {
			if (isBind())   return definition.as<core::BindExprInstance>()->getCall();
			if (isLambda()) return definition.as<core::LambdaInstance>()->getBody();
			if (isLiteral()) return definition.as<core::ExpressionInstance>();
			assert_fail() << "No body within definition: " << definition << " (" << (definition?"null":toString(definition->getNodeType())) << ")";
			return StatementInstance();
		}

		ExpressionInstance getDefiningExpr() const {
			// for lambdas we have to go to the lambda expression
			if (isLambda()) return definition.getParentInstance(3).as<LambdaExprInstance>();

			// for the rest the expression is the definition
			return definition.as<ExpressionInstance>();
		}

		bool isRecursive() const {
			if (!isLambda()) return false;
			LambdaDefinitionPtr def = definition.getParentInstance(2).as<LambdaDefinitionPtr>();
			return def->isRecursive(getRecursiveVariable());
		}

		VariablePtr getRecursiveVariable() const {
			assert_true(isLambda());
			return definition.getParentNode().as<LambdaBindingPtr>()->getVariable();
		}

		std::ostream& printTo(std::ostream& out) const {
			if (!definition) return out << "-unknown-";
			if (auto lit = definition.isa<LiteralPtr>()) return out << *lit;
			return out << "(" << definition->getNodeType() << "@" << definition << ")";
		}

		std::size_t hash() const {
			return (definition) ? definition->getNodeHashValue() : 0;
		}
	};


	/**
	 * The task of this manager is simple - compute the (potential) links between
	 * callers and callees.
	 */
	class CallSiteManager {

		map<Caller, vector<Callee>> forward;
		map<Callee, vector<Caller>> backward;

		// a map of free callees/callers in the code fragment - indexed by the number of arguments
		map<unsigned, vector<Callee>> freeCallees;
		map<unsigned, vector<Caller>> freeCallers;

		// the set of calls for which entries in the call strings are added
		set<Caller> dynamicCalls;

	public:

		CallSiteManager(const StatementInstance& root);

		const vector<Caller>& getCaller(const Callee& callee);

		const vector<Callee>& getCallee(const Caller& caller);

		const set<Caller>& getDynamicCalls() const { return dynamicCalls; }

		const vector<Caller>& getFreeCallers(unsigned numArgs) const;

		const vector<Callee>& getFreeCallees(unsigned numParams) const;

		bool isFree(const Callee& callee) const;

	private:

		vector<Caller> computeCaller(const Callee& callee) const;

		vector<Callee> computeCallee(const Caller& caller) const;

	};


	inline bool causesContextShift(const CallExprInstance& call) {
		// a context shift always has to happen when it isn't a direct call
		auto trgType = call->getFunctionExpr()->getNodeType();
		return trgType != NT_Literal && trgType != NT_LambdaExpr && trgType != NT_BindExpr;
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
