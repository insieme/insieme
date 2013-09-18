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
#include <map>

#include "insieme/analysis/cba/framework/context.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/assert.h"
#include "insieme/utils/typed_map.h"

namespace insieme {
namespace analysis {
namespace cba {

	using std::map;
	using std::size_t;
	using std::vector;

	using namespace core;

	class Caller : public utils::Printable {

		CallExprAddress call;

	public:

		Caller(const CallExprAddress& call)
			: call(call) { assert_true(call); }

		const CallExprAddress& getCall() const {
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

		std::size_t getNumArgs() const {
			assert_true(call) << "Must not be invoked on undefined call expression!";
			return call.size();
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			if (!call) return out << "-unknown-";
			return out << "(" << call << ")";
		}
	};

	class Callee : public utils::Printable {

		NodeAddress definition;

	public:

		Callee(const core::LiteralAddress& lit)
			: definition(lit) { assert_true(definition); }
		Callee(const core::LambdaAddress& fun)
			: definition(fun) { assert_true(definition); }
		Callee(const core::BindExprAddress& bind)
			: definition(bind) { assert_true(definition); }
		Callee(const Callee& other)
			: definition(other.definition) { assert_true(definition); }

		const NodeAddress& getDefinition() const {
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

		bool isLiteral() const { return definition->getNodeType() == core::NT_Literal;  }
		bool isBind()    const { return definition->getNodeType() == core::NT_BindExpr; }
		bool isLambda()  const { return definition->getNodeType() == core::NT_Lambda;   }

		size_t getNumParams() const {
			return getFunctionType()->getParameterTypes().size();
		}

		FunctionTypePtr getFunctionType() const {
			assert(definition);
			if (LambdaPtr lambda = definition.isa<LambdaPtr>()) {
				return lambda->getType();
			}
			return definition.as<ExpressionPtr>()->getType().as<FunctionTypePtr>();
		}

		StatementAddress getBody() const {
			if (isBind())   return definition.as<core::BindExprAddress>()->getCall();
			if (isLambda()) return definition.as<core::LambdaExprAddress>()->getBody();
			assert_fail() << "No body within definition: " << definition << " (" << (definition?"null":toString(definition->getNodeType())) << ")";
			return StatementAddress();
		}

		ExpressionAddress getDefiningExpr() const {
			// for lambdas we have to go to the lambda expression
			if (isLambda()) return definition.getParentAddress(3).as<LambdaExprAddress>();

			// for the rest the expression is the definition
			return definition.as<ExpressionAddress>();
		}

		bool isRecursive() const {
			if (!isLambda()) return false;
			LambdaDefinitionPtr def = definition.getParentAddress(2).as<LambdaDefinitionPtr>();
			return def->isRecursive(getRecursiveVariable());
		}

		VariablePtr getRecursiveVariable() const {
			assert(isLambda());
			return definition.getParentNode().as<LambdaBindingPtr>()->getVariable();
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			if (!definition) return out << "-unknown-";
			if (auto lit = definition.isa<LiteralPtr>()) return out << *lit;
			return out << "(" << definition->getNodeType() << "@" << definition << ")";
		}
	};


	/**
	 * The task of this manager is simple - compute the (potential) links between
	 * callers and callees.
	 */
	class CallSiteManager {

		map<Caller, vector<Callee>> forward;
		map<Callee, vector<Caller>> backward;

	public:

		CallSiteManager(const StatementAddress& root);

		const vector<Caller>& getCaller(const Callee& callee);

		const vector<Callee>& getCallee(const Caller& caller);

	private:

		vector<Caller> computeCaller(const Callee& callee);

		vector<Callee> computeCallee(const Caller& caller);

	};


//	template<typename Context>
//	class Caller : public utils::Printable {
//
//		CallExprAddress call;
//		Context context;
//
//	public:
//
//		Caller(const CallExprAddress& call, const Context& context = Context())
//			: call(call), context(context) { assert_true(call); }
//
//		const CallExprAddress& getCall() const {
//			return call;
//		}
//
//		const Context& getContext() const {
//			return context;
//		}
//
//		// -- operators ------------------
//
//		bool operator<(const Caller& other) const {
//			if (call != other.call) return call < other.call;
//			return context < other.context;
//		}
//
//		bool operator==(const Caller& other) const {
//			return this == &other || (call == other.call && context == other.context);
//		}
//
//		bool operator!=(const Caller& other) const {
//			return !(*this == other);
//		}
//
////		operator bool() const {
////			return call;
////		}
//
//		std::size_t getNumArgs() const {
//			assert_true(call) << "Must not be invoked on undefined call expression!";
//			return call.size();
//		}
//
//	protected:
//
//		virtual std::ostream& printTo(std::ostream& out) const {
//			if (!call) return out << "-unknown-";
//			return out << "(" << call << "," << context << ")";
//		}
//	};
//
//	template<typename Context>
//	class Callee : public utils::Printable {
//
//		NodeAddress definition;
//		Context context;
//
//	public:
//
////		Callee() : definition(), context() {}
//		Callee(const core::LiteralAddress& lit)
//			: definition(lit), context() { assert_true(definition); }
//		Callee(const core::LambdaAddress& fun, Context context = Context())
//			: definition(fun), context(context) { assert_true(definition); }
//		Callee(const core::BindExprAddress& bind, Context context = Context())
//			: definition(bind), context()       { assert_true(definition); }
//		Callee(const Callee& other)
//			: definition(other.definition), context(other.context) { assert_true(definition); }
//
//		const NodeAddress& getDefinition() const {
//			return definition;
//		}
//
//		const Context& getContext() const {
//			return context;
//		}
//
//		bool operator<(const Callee& other) const {
//			if (definition != other.definition) return definition < other.definition;
//			return context < other.context;
//		}
//		bool operator==(const Callee& other) const {
//			if (this == &other) return true;
//			return definition == other.definition && context == other.context;
//		}
//
//		bool operator!=(const Callee& other) const {
//			return !(*this == other);
//		}
//
////		operator bool() const {
////			return definition;
////		}
//
//		bool isLiteral() const { return definition->getNodeType() == core::NT_Literal;  }
//		bool isBind()    const { return definition->getNodeType() == core::NT_BindExpr; }
//		bool isLambda()  const { return definition->getNodeType() == core::NT_Lambda;   }
//
//		size_t getNumParams() const {
//			return getFunctionType()->getParameterTypes().size();
//		}
//
//		const FunctionTypePtr& getFunctionType() const {
//			assert(definition);
//			if (LambdaPtr lambda = definition.isa<LambdaPtr>()) {
//				return lambda->getType();
//			}
//			return definition.as<ExpressionPtr>()->getType().as<FunctionTypePtr>();
//		}
//
//		StatementAddress getBody() const {
//			if (isBind())   return definition.as<core::BindExprAddress>()->getCall();
//			if (isLambda()) return definition.as<core::LambdaExprAddress>()->getBody();
//			assert_fail() << "No body within definition: " << definition << " (" << (definition?"null":toString(definition->getNodeType())) << ")";
//			return StatementAddress();
//		}
//
//		ExpressionAddress getDefiningExpr() const {
//			// for lambdas we have to go to the lambda expression
//			if (isLambda()) return definition.getParentAddress(3).as<LambdaExprAddress>();
//
//			// for the rest the expression is the definition
//			return definition.as<ExpressionAddress>();
//		}
//
//		bool isRecursive() const {
//			assert(isLambda());
//			LambdaDefinitionPtr def = definition.getParentAddress(2).as<LambdaDefinitionPtr>();
//			return def->isRecursive(getRecursiveVariable());
//		}
//
//		VariablePtr getRecursiveVariable() const {
//			assert(isLambda());
//			return definition.getParentNode().as<LambdaBindingPtr>()->getVariable();
//		}
//
//	protected:
//
//		virtual std::ostream& printTo(std::ostream& out) const {
//			if (!definition) return out << "-unknown-";
//			if (auto lit = definition.isa<LiteralPtr>()) return out << *lit;
//			return out << "(" << definition->getNodeType() << "@" << definition << "," << context << ")";
//		}
//	};
//
//
//	/**
//	 * The task of this manager is simple - compute the (potential) links between
//	 * callers and callees.
//	 */
//	class CallSiteManager {
//
//		struct CacheBase {
//			virtual ~CacheBase() {};
//		};
//
//		template<typename Context>
//		struct Cache : public CacheBase {
//
//			map<Caller<Context>, vector<Callee<Context>>> forward;
//			map<Callee<Context>, vector<Caller<Context>>> backward;
//
//			const vector<Callee<Context>>& getCallee(const Caller<Context>& caller) {
//
//				// check cache
//				auto pos = forward.find(caller);
//				if (pos != forward.end()) {
//					return pos->second;
//				}
//
//				const vector<Callee<Context>>& res = forward[caller] = computeCallee(caller);
//
//				// cross-check result - the assertion has to be split up due to a gcc limitation
//				assert_decl(bool ok = all(computeCallee(caller), [&](const Callee<Context>& cur)->bool {
//					return contains(this->getCaller(cur), caller);
//				}));
//				assert_true(ok);
//
//				return res;
//
////				// compute callee, cache result and return it
////				return forward[caller] = computeCallee(caller);
//			}
//
//			const vector<Caller<Context>>& getCaller(const Callee<Context>& callee) {
//
//				// check cache
//				auto pos = backward.find(callee);
//				if (pos != backward.end()) {
//					return pos->second;
//				}
//
//				const vector<Caller<Context>>& res = backward[callee] = computeCaller(callee);
//
//				// cross-check result - the assertion has to be split up due to a gcc limitation
//				assert_decl(bool ok = all(computeCaller(callee), [&](const Caller<Context>& cur)->bool {
//					return contains(this->getCallee(cur), callee);
//				}));
//				assert_true(ok);
//
//				return res;
//
////				// compute caller, cache result and return it
////				return backward[callee] = computeCaller(callee);
//			}
//
//		private:
//
//			vector<Callee<Context>> computeCallee(const Caller<Context>& caller) {
//
//				// investigate function expression
//				ExpressionAddress fun = caller.getCall()->getFunctionExpr();
//
//				// -- handle direct calls --
//				if (auto literal = fun.isa<LiteralAddress>()) {
//					// simply a direct call to a literal
//					return toVector(Callee<Context>(literal));
//				}
//				if (auto lambda = fun.isa<LambdaExprAddress>()) {
//					// this is a direct call to a lambda
//					return toVector(Callee<Context>(lambda->getLambda(), caller.getContext()));
//				}
//				if (auto bind = fun.isa<BindExprAddress>()) {
//					// this is a direct call to a local bind
//					return toVector(Callee<Context>(bind, caller.getContext()));
//				}
//
//				assert_fail() << "Unsupported function expression encountered: " << *fun << "\n";
//				return vector<Callee<Context>>();
//			}
//
//			vector<Caller<Context>> computeCaller(const Callee<Context>& callee) {
//				static const vector<Caller<Context>> empty;
//
//				// get defining expression
//				auto def = callee.getDefiningExpr();
//
//				// check whether there is a parent
//				if (def.isRoot()) return empty;
//				NodeAddress parent = def.getParentAddress();
//
//				// decide based on parent node type
//				switch(parent->getNodeType()) {
//
//					case NT_CallExpr: {
//						auto call = parent.as<CallExprAddress>();
//
//						// check whether the callee is the function expression
//						if (call->getFunctionExpr() == def) {
//							return toVector(Caller<Context>(call, callee.getContext()));
//						}
//
//						// TODO: if it is a argument, follow it!
//
//						break;
//					}
//
//					default: {
//						assert_fail() << "Unsupported parent type encountered: " << parent->getNodeType();
//						break;
//					}
//				}
//
//				assert_fail() << "Unimplemented\n";
//				return empty;
//			}
//
//		};
//
//		utils::TypedMap<Cache, CacheBase> caches;
//
//	public:
//
//		CallSiteManager(const StatementAddress& root);
//
//		template<typename Context>
//		const vector<Caller<Context>>& getCaller(const Callee<Context>& callee) {
//			return caches.get<Context>().getCaller(callee);
//		}
//
//		template<typename Context>
//		const vector<Callee<Context>>& getCallee(const Caller<Context>& caller) {
//			return caches.get<Context>().getCallee(caller);
//		}
//
//	};

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
