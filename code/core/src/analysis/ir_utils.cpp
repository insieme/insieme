/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/analysis/ir_utils.h"

#include <set>
#include <map>
#include <boost/regex.hpp>

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_cached_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/attributes.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/types/type_variable_deduction.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/static_vars.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/graph_utils.h"

namespace insieme {
namespace core {
namespace analysis {

	using std::map;

	NodeList getFreeNodes(const NodePtr& node, NodeType controlStmt, const vector<NodeType>& pruneNodes) {
		NodeList returnNodes;
		visitDepthFirstOncePrunable(node, [&](const NodePtr& cur) {
			auto curType = cur->getNodeType();
			if(cur->getNodeType() == controlStmt) { returnNodes.push_back(cur); }
			if(::contains(pruneNodes, curType)) {
				return true; // do not descent here
			}
			return false;
		});
		return returnNodes;
	}


	bool isSideEffectFree(const ExpressionPtr& expr) {
		// all variables and literals are side-effect free accessible
		NodeType type = expr->getNodeType();
		if(type == NT_Variable || type == NT_Literal) { return true; }

		// check for other operations
		if(type != NT_CallExpr) { return false; }

		// check whether function is side-effect free + all arguments are
		CallExprPtr call = expr.as<CallExprPtr>();
		auto& basic = expr->getNodeManager().getLangBasic();
		auto& refExt = expr->getNodeManager().getLangExtension<lang::ReferenceExtension>();

		// TODO: use different way of identifying pure functions!
		auto fun = call->getFunctionExpr();
		return (basic.isPure(fun) || refExt.isRefDeref(fun))
			   && all(call->getArgumentDeclarations(), [](const DeclarationPtr& decl) { return isSideEffectFree(decl); });
	}


	bool isSideEffectFree(const DeclarationPtr& decl) {
		auto initExpr = decl->getInitialization();
		auto var = decl->getVariable();

		// if initExpr is assignment to declared var, just test RHS
		if(lang::isAssignment(initExpr)) {
			if(getArgument(initExpr, 0) == var) {
				return isSideEffectFree(getArgument(initExpr, 2));
			}
		}
		// if initExpr is an InitExpr node, test all of its subexpressions
		if(auto initNode = initExpr.isa<InitExprPtr>()) {
			if(initNode->getMemoryExpr() == var) {
				return all(initNode->getInitExprs(), [](const ExpressionPtr& expr) { return isSideEffectFree(expr); });
			}
		}
		// if initExpr is a constructor call, return false for now
		if(isConstructorCall(initExpr)) {
			return false;
		}

		return false;
	}

	bool isCallOf(const CallExprPtr& candidate, const NodePtr& function) {
		// check for null
		if(!candidate) { return false; }

		// check invoked function
		return *(stripAttributes(candidate->getFunctionExpr())) == *function;
	}

	bool isCallOf(const NodePtr& candidate, const NodePtr& function) {
		// check for null
		if(!candidate) { return false; }

		// check invoked function
		return isCallOf(candidate.isa<CallExprPtr>(), function);
	}

	bool isMaterializingCall(const NodePtr& candidate) {
		auto call = candidate.isa<CallExprPtr>();
		if(!call) return false;
		auto callType = call->getType();
		auto function = call->getFunctionExpr();
		auto funType = function->getType().as<FunctionTypePtr>()->getReturnType();
		return !analysis::equalTypes(callType, funType) && !types::getTypeVariableInstantiation(call->getNodeManager(), funType, callType);
	}

	bool isNoOp(const StatementPtr& candidate) {
		// check for null
		if(!candidate) { return false; }

		// check type of statement => must be a compound statement
		if(candidate->getNodeType() != NT_CompoundStmt) { return false; }

		// must be an empty compound statement
		return candidate.as<CompoundStmtPtr>().empty();
	}

	// ------------------------------------ Begin :: isGeneric ----------------------------

	namespace {

		/**
		 * Defines a set of type variables required to be passed as a context
		 * through the IsGeneric visitor.
		 */
		typedef std::set<TypeVariablePtr> TypeVariableSet;

		/**
		 * A visitor searching for free type variables or variable int type parameters.
		 * The visitor is used by the isGeneric(..) function. If one of those is present,
		 * the type is generic.
		 */
		class IsGeneric : private IRVisitor<bool, Pointer> {
		  public:
			IsGeneric() : IRVisitor<bool, Pointer>(true) {}

			/**
			 * An entry point for this visitor testing whether the given type
			 * is a generic type or not.
			 */
			bool test(const TypePtr& type) {
				return this->visit(type);
			}

		  private:
			/**
			 * When encountering a variable, check whether it is bound.
			 */
			bool visitTypeVariable(const TypeVariablePtr& var) override {
				return true; // there is a free generic variable
			}

			bool visitFunctionType(const FunctionTypePtr& funType) override {
				return any(funType->getParameterTypes(), [&](const TypePtr& type) { return this->visit(type); })
				       || this->visit(funType->getReturnType());
			}

			bool visitTupleType(const TupleTypePtr& tuple) override {
				return any(tuple->getElements(), [&](const TypePtr& type) { return this->visit(type); });
			}

			bool visitGenericType(const GenericTypePtr& type) override {
				// generic type is generic if one of its parameters is
				return any(type->getTypeParameter()->getTypes(), [&](const TypePtr& type) { return this->visit(type); });
			}

			bool visitNumericType(const NumericTypePtr&) override {
				// numeric types are never generic
				return false;
			}

			bool visitTagType(const TagTypePtr& tagType) override {
				// check types within recursive bindings
				return any(tagType->getDefinition(), [&](const TagTypeBindingPtr& binding) { return this->visit(binding->getRecord()); });
			}

			bool visitTagTypeReference(const TagTypeReferencePtr& tagType) override {
				// nothing generic here
				return false;
			}

			bool visitRecord(const RecordPtr& record) override {
				return any(record->getFields(), [&](const FieldPtr& field) { return this->visit(field->getType()); });
			}

			bool visitType(const TypePtr& type) override {
				return any(type->getChildList(), [&](const NodePtr& node) { return this->visit(node); });
			}

			/**
			 * A terminal visit capturing all non-covered types. This one should
			 * never be reached since all cases need to be covered within specialized
			 * visit members.
			 */
			bool visitNode(const NodePtr& node) override {
				LOG(FATAL) << "Reaching " << *node << " of type " << node->getNodeType() << " within IsGeneric visitor!";
				assert_fail() << "Should not be reached!";
				return false;
			}
		};
	}


	bool isGeneric(const TypePtr& type) {
		// just use the IsGeneric recursive visitor based test
		return IsGeneric().test(type);
	}

	// ------------------------------------ End :: isGeneric ----------------------------


	StructPtr isStruct(const NodePtr& node) {
		if (auto expr = node.isa<ExpressionPtr>()) return isStruct(expr->getType());
		if (auto tagType = node.isa<TagTypePtr>()) return isStruct(tagType->getRecord());
		return node.isa<StructPtr>();
	}

	UnionPtr isUnion(const NodePtr& node) {
		if (auto expr = node.isa<ExpressionPtr>()) return isUnion(expr->getType());
		if (auto tagType = node.isa<TagTypePtr>()) return isUnion(tagType->getRecord());
		return node.isa<UnionPtr>();
	}

	bool isParallel(const NodePtr& node) {
		return visitDepthFirstOnceInterruptible(
		    node, [](const CallExprPtr& call) { return call->getNodeManager().getLangExtension<lang::ParallelExtension>().isParallel(call->getFunctionExpr()); });
	}

	TypeList getElementTypes(const TypePtr& type) {

		struct Extractor : public IRVisitor<TypeList> {
			Extractor() : IRVisitor(true) {}

			TypeList visitGenericType(const GenericTypePtr& type) override {
				return type->getTypeParameter()->getTypes();
			}

			TypeList visitGenericTypeVariable(const GenericTypeVariablePtr& type) override {
				return type->getTypeParameter()->getTypes();
			}

			TypeList visitFunctionType(const FunctionTypePtr& type) override {
				TypeList res;
				for(const auto& cur : type->getParameterTypes()) res.push_back(cur);
				res.push_back(type->getReturnType());
				return res;
			}

			TypeList visitTupleType(const TupleTypePtr& type) override {
				return type->getElementTypes();
			}

			TypeList visitTagType(const TagTypePtr& type) override {
				TypeList res;
				for(const auto& field : type->getFields()) {
					res.push_back(field->getType());
				}
				return res;
			}

			TypeList visitType(const TypePtr& type) override {
				assert_fail() << "Unsupported type encountered: " << *type << "\n";
				return TypeList();
			}
		};

		return Extractor()(type);
	}

	bool isRefOf(const NodePtr& candidate, const NodePtr& type) {
		// check for null
		if(!candidate) { return false; }

		NodePtr adjustedCandidate = candidate;
		// check if expression, if so use type
		if(ExpressionPtr expr = dynamic_pointer_cast<ExpressionPtr>(candidate)) { adjustedCandidate = expr->getType(); }

		// check type of node
		if(!isRefType(adjustedCandidate)) { return false; }

		// check element type
		return *getReferencedType(adjustedCandidate) == *type;
	}

	bool isRefOf(const NodePtr& candidate, const NodeType kind) {
		// check for null
		if(!candidate) { return false; }

		NodePtr adjustedCandidate = candidate;
		// check if expression, if so use type
		if(ExpressionPtr expr = dynamic_pointer_cast<ExpressionPtr>(candidate)) { adjustedCandidate = expr->getType(); }

		// check type of node
		if(!isRefType(adjustedCandidate)) { return false; }

		// check element type (kind)
		return getReferencedType(adjustedCandidate)->getNodeType() == kind;
	}

	bool isRefOf(const NodePtr& candidate, const std::function<bool(const NodePtr&)>& filter) {
		return core::lang::isReference(candidate) && filter(core::lang::ReferenceType(candidate).getElementType());
	}

	bool isTypeLiteralType(const GenericTypePtr& type) {
		// check family name as well as type and name of parameters
		return type->getName()->getValue() == "type" && type->getTypeParameter().size() == static_cast<std::size_t>(1);
	}

	bool isTypeLiteralType(const TypePtr& type) {
		// check node type
		if(type->getNodeType() != core::NT_GenericType) { return false; }

		// forward test
		return isTypeLiteralType(static_pointer_cast<const core::GenericType>(type));
	}

	bool isConstructorExpr(const NodePtr& node) {
		NodeType pnt = node->getNodeType();
		return pnt == NT_InitExpr || pnt == NT_TupleExpr || pnt == NT_JobExpr;
	}


	// ------ Free Variable Extraction ----------

	namespace {

		/**
		 * A functor template for the free-variable collector handling the critical part distinguishing
		 * the collection of pointers and nodes.
		 */
		template <template <class Target> class Ptr>
		struct rec_free_var_collector;

		/**
		 * The specialization of the rec_free_var_collector template for pointers.
		 */
		template <>
		struct rec_free_var_collector<Pointer> {
			vector<VariablePtr> operator()(const NodePtr& cur) const {
				return analysis::getFreeVariables(cur);
			}
			VariablePtr extend(const NodePtr&, const VariablePtr& res) const {
				return res;
			}
			rec_free_var_collector() {}
		};

		/**
		 * The specialization of the rec_free_var_collector template for addresses.
		 */
		template <>
		struct rec_free_var_collector<Address> {
			vector<VariableAddress> operator()(const NodeAddress& cur) const {
				return getFreeVariableAddresses(cur);
			}
			VariableAddress extend(const NodeAddress& head, const VariableAddress& tail) const {
				return concat(head, tail);
			}
			rec_free_var_collector() {}
		};

		/**
		 * Will certainly determine the declaration status of variables inside a block.
		 */
		template <template <class Target> class Ptr>
		struct FreeVariableCollector : private IRVisitor<void, Ptr, VariableSet&, std::set<Ptr<const Variable>>&> {
			typedef std::set<Ptr<const Variable>> ResultSet;
			typedef vector<Ptr<const Variable>> ResultList;

			FreeVariableCollector() : IRVisitor<void, Ptr, VariableSet&, std::set<Ptr<const Variable>>&>(true) {}

			ResultSet run(const Ptr<const Node>& root) {
				// run visitor
				VariableSet bound;
				ResultSet free;

				// run visit
				this->visit(root, bound, free);

				// return result set
				return free;
			}

		  private:
			void visitNode(const Ptr<const Node>& node, VariableSet& bound, ResultSet& free) {
				// visit all sub-nodes
				this->visitAll(node->getChildList(), bound, free);
			}

			void visitDeclaration(const Ptr<const Declaration>& decl, VariableSet& bound, ResultSet& free) {
				// first add variable to set of bound variables
				bound.insert(decl->getVariable());

				// then visit the defining expression
				this->visit(decl->getInitialization(), bound, free);
			}

			void visitStatement(const Ptr<const Statement>& stmt, VariableSet& bound, ResultSet& free) {
				// compound, return and for statements create new scopes
				auto nt = stmt->getNodeType();
				if(nt == NT_CompoundStmt || nt == NT_ReturnStmt || nt == NT_ForStmt) {
					VariableSet innerBound = bound;
					visitNode(stmt, innerBound, free);
				} else {
					visitNode(stmt, bound, free);
				}
			}

			void visitCatchClause(const Ptr<const CatchClause>& clause, VariableSet& bound, ResultSet& free) {
				// a catch clause creates a new scope
				VariableSet innerBound = bound;

				// the catch-variable is a bound one
				innerBound.insert(clause->getVariable());

				// continue visiting with the limited scope
				visitNode(clause, innerBound, free);
			}

			void visitVariable(const Ptr<const Variable>& var, VariableSet& bound, ResultSet& free) {
				if(bound.find(var) == bound.end()) { free.insert(var); }

				// continue visiting variable type
				visitNode(var->getType(), bound, free);
			}

			void visitLambda(const Ptr<const Lambda>& lambda, VariableSet& bound, ResultSet& free) {
				// a lambda creates a new scope
				VariableSet innerBound = bound;

				// all parameters are bound within this scope
				const auto& params = lambda->getParameters();
				innerBound.insert(params.begin(), params.end());

				// continue visiting with the limited scope
				visitNode(lambda, innerBound, free);
			}


			// due to the structure of the IR, nested lambdas can never reuse outer variables
			//  - also prevents variables in LamdaDefinition from being inadvertently captured
			void visitLambdaExpr(const Ptr<const LambdaExpr>& lambda, VariableSet& bound, ResultSet& free) {
				static const rec_free_var_collector<Ptr> collectRecursive;


				// The type used to annotate free variable lists.
				struct FreeVariableSet : public ResultSet, public value_annotation::cloneable {
					FreeVariableSet(const ResultSet& set) : ResultSet(set) {}
					FreeVariableSet(const ResultList& list) : ResultSet(list.begin(), list.end()) {}

					static VariablePtr cloneTo(NodeManager& manager, const VariablePtr& ptr) {
						return manager.get(ptr);
					}
					static VariableAddress cloneTo(NodeManager& manager, const VariableAddress& addr) {
						return addr.cloneTo(manager);
					}

					// clone free variable set to new node manager during clone operations
					void cloneTo(const NodePtr& target) const {
						ResultSet newSet;
						for(auto cur : *this) {
							newSet.insert(cloneTo(target->getNodeManager(), cur));
						}
						target->attachValue(FreeVariableSet(newSet));
					}
				};

				// check annotation
				auto definition = lambda->getDefinition();
				if(!definition->template hasAttachedValue<FreeVariableSet>()) {
					// evaluate recursively
					definition->attachValue(FreeVariableSet(collectRecursive(definition)));
				}

				// should be fixed now
				assert(definition->template hasAttachedValue<FreeVariableSet>());
				assert(all(definition->template getAttachedValue<FreeVariableSet>(),
				           [&](const VariablePtr& cur) -> bool { return definition->getNodeManager().contains(cur); }));

				// add free variables to result set
				for(const auto& cur : definition->template getAttachedValue<FreeVariableSet>()) {
					// if variable is not defined by the enclosed lambda definition block
					free.insert(collectRecursive.extend(definition, cur));
				}
			}

			void visitBindExpr(const Ptr<const BindExpr>& bindExpr, VariableSet& bound, ResultSet& free) {
				// first search for free variables within bound expressions
				auto expressions = bindExpr->getBoundExpressions();
				for_each(expressions, [&](const Ptr<const Expression>& e) { this->visit(e, bound, free); });

				// add free variables encountered within call target
				this->visit(bindExpr->getCall()->getFunctionExpr(), bound, free);
			}
		};
	}

	bool hasFreeVariables(const NodePtr& code) {
		return !getFreeVariables(code).empty();
	}

	VariableList getFreeVariables(const NodePtr& code) {
		// collect free variables
		auto set = FreeVariableCollector<Pointer>().run(code);

		// convert result into list
		VariableList res(set.begin(), set.end());
		std::sort(res.begin(), res.end(), compare_target<VariablePtr>());
		return res;
	}

	vector<VariableAddress> getFreeVariableAddresses(const NodePtr& code) {
		// collect free variables
		auto set = FreeVariableCollector<Address>().run(NodeAddress(code));

		// convert result into list
		auto res = vector<VariableAddress>(set.begin(), set.end());
		std::sort(res.begin(), res.end());
		return res;
	}

	bool hasFreeVariable(const NodePtr& code, const std::function<bool(VariablePtr)>& filter) {
		if(!code.isa<StatementPtr>()) { return false; }
		// TODO: improve this implementation by forwarding filter to visitor
		return any(getFreeVariables(code), filter);
	}

	vector<StatementAddress> getExitPoints(const StatementPtr& stmt) {
		// delegate request to address-based implementation
		return getExitPoints(StatementAddress(stmt));
	}

	vector<StatementAddress> getExitPoints(const StatementAddress& stmt) {
		// list of exit points
		vector<StatementAddress> res;

		// the statement must not contain a "free" return
		visitDepthFirstPrunable(stmt, [&](const NodeAddress& cur) {
			if(cur->getNodeType() == NT_LambdaExpr) {
				return true; // do not decent here
			}
			if(cur->getNodeType() == NT_ReturnStmt) {
				// free return identified
				res.push_back(cur.as<StatementAddress>());
				return true;
			}
			// skip non-full expression
			if(StatementAddress stmt = cur.isa<StatementAddress>()) {
				return !stmt.isRoot() && !stmt.isa<CompoundStmtAddress>() && !stmt.getParentAddress().isa<CompoundStmtAddress>();
			}

			return false; // check the rest
		});

		// search for "bound" break or continue statements
		visitDepthFirstPrunable(stmt, [&](const NodeAddress& cur) {
			if(cur->getNodeType() == NT_ForStmt || cur->getNodeType() == NT_WhileStmt) {
				return true; // do not decent here
			}
			if(cur->getNodeType() == NT_BreakStmt || cur->getNodeType() == NT_ContinueStmt) {
				// free break / continue found
				res.push_back(cur.as<StatementAddress>());
				return true;
			}

			// skip non-full expression
			if(StatementAddress stmt = cur.isa<StatementAddress>()) {
				return !stmt.isRoot() && !stmt.isa<CompoundStmtAddress>() && !stmt.getParentAddress().isa<CompoundStmtAddress>();
			}

			return false; // check the rest
		});

		// done
		return res;
	}


	namespace {

		struct VariableCollector : public IRVisitor<> {
			struct VariableSetAnnotation : public VariableSet {
				VariableSetAnnotation(const VariableSet& varSet) : VariableSet(varSet) {}
			};

			VariableSet varSet;

			void visitVariable(const VariablePtr& var) {
				varSet.insert(var); // collect value, that's all
			}

			void visitLambda(const LambdaPtr& lambda) {
				// add a cut-off at lambdas and cache results
				if(!lambda->hasAttachedValue<VariableSetAnnotation>()) {
					VariableSet local = getAllVariables(lambda->getBody());
					local.insert(lambda->getParameters().begin(), lambda->getParameters().end());
					lambda->attachValue<VariableSetAnnotation>(local);
				}
				assert(lambda->hasAttachedValue<VariableSetAnnotation>());
				const VariableSetAnnotation& set = lambda->getAttachedValue<VariableSetAnnotation>();
				varSet.insert(set.begin(), set.end());
			}

			void visitNode(const NodePtr& node) {
				// collect recursively
				visitAll(node->getChildList());
			}
		};
	}


	VariableSet getAllVariables(const NodePtr& code) {
		VariableCollector collector;
		collector.visit(code);
		return collector.varSet;
	}


	VariableSet getAllVariablesInScope(const NodePtr& code) {
		// Collect variables in current scope
		VariableSet res;
		visitDepthFirstOncePrunable(code, [&](const NodePtr& cur) -> bool {
			auto type = cur->getNodeType();
			if(type == NT_Variable) { res.insert(cur.as<VariablePtr>()); }

			// prune search
			return cur->getNodeCategory() == NC_Type || type == NT_Variable || type == NT_LambdaExpr;
		});

		// + add free variables (recursive function variables)
		for(const VariablePtr& var : getFreeVariables(code)) {
			res.insert(var);
		}

		// done
		return res;
	}

	CallExprAddress findLeftMostOutermostCallOf(const NodeAddress& root, const ExpressionPtr& fun) {
		CallExprAddress res;
		core::visitDepthFirstInterruptible(root, [&](const CallExprAddress& call) -> bool {
			if(isCallOf(call.getAddressedNode(), fun)) {
				res = call;
				return true;
			}
			return false;
		});
		return res;
	}


	bool contains(const NodePtr& code, const NodePtr& element) {
		assert_true(element) << "Element to be searched must not be empty!";
		return code && makeCachedLambdaVisitor([&](const NodePtr& cur, rec_call<bool>::type& rec) -> bool {
			       return *cur == *element || any(cur->getChildList(), rec);
			   }, true)(code);
	}

	unsigned countInstances(const NodePtr& code, const NodePtr& element, bool limitScope) {
		assert_true(element) << "Element to be searched must not be empty!";
		return code ? makeCachedLambdaVisitor([&](const NodePtr& cur, rec_call<unsigned>::type& rec) -> unsigned {
			if(limitScope && cur.isa<LambdaExprPtr>()) return 0;
			if(*cur == *element) return 1;
			auto children = cur->getChildList();
			unsigned ret = 0;
			for(const auto& c : children) {
				ret += rec(c);
			}
			return ret;
		}, true)(code) : 0;
	}


	namespace {


		/**
		 * A container storing results and assumptions for read-only parameters
		 * in a recursive context.
		 */
		class ReadOnlyCheck {
			bool VisitScopes;
			int indirectionLevels;

		public:

			ReadOnlyCheck(bool v = true) : VisitScopes(v), indirectionLevels(0) {}

		private:

			/**
			 * A map mapping lambda references and parameter positions to a flag
			 * indicating whether the corresponding parameter is a read-only parameter.
			 */
			map<pair<LambdaReferencePtr, int>, bool> cache;

			map<LambdaReferencePtr, LambdaPtr> bodyMap;

			bool isReadOnly(const LambdaReferencePtr& ref, int paramPos) {
				// check whether this is a known variable
				auto bodyPair = bodyMap.find(ref);
				if(bodyPair == bodyMap.end()) { return false; }

				// check cache
				auto pos = cache.find(std::make_pair(ref, paramPos));
				if(pos != cache.end()) { return pos->second; }

				// evaluation is necessary
				LambdaPtr lambda = bodyPair->second;
				VariablePtr param = lambda->getParameters()[paramPos];
				bool res = isReadOnly(lambda, param);

				// safe result in cache
				cache.insert({{ref, paramPos}, res});

				return res;
			}

			bool isReadOnly(const LambdaPtr& lambda, const VariablePtr& param) {
				indirectionLevels++;
				bool res = isReadOnly(lambda->getBody(), param);
				indirectionLevels--;
				return res;
			}

		public:

			bool isReadOnly(const LambdaExprPtr& lambda, const VariablePtr& param) {
				if(!VisitScopes) { return true; }

				// check the parameter
				assert_true(::contains(lambda->getParameterList(), param)) << "Asking for non-existing parameter.";

				// simple case for non-recursive lambdas
				if(!lambda->isRecursive()) {
					// just check body
					return isReadOnly(lambda->getLambda(), param);
				}

				// ---- recursive lambdas ----

				// remember bodies
				for(const LambdaBindingPtr& cur : lambda->getDefinition()) {
					// add body
					bodyMap[cur->getReference()] = cur->getLambda();
				}

				// assume parameter is save (co-induction)
				int pos = 0;
				ParametersPtr params = lambda->getParameterList();
				while(*params[pos] != *param) {
					pos++;
				}
				cache[std::make_pair(lambda->getReference(), pos)] = true;

				// compute result
				bool res = isReadOnly(lambda->getLambda(), param);

				// update cache
				cache[std::make_pair(lambda->getReference(), pos)] = res;

				// done
				return res;
			}

			bool isReadOnly(const StatementPtr& stmt, const VariablePtr& var) {
				// non-ref values are always read-only
				if(!isRefType(var->getType())) { return true; }

				// get deref token
				auto deref = var->getNodeManager().getLangExtension<lang::ReferenceExtension>().getRefDeref();

				bool readOnly = true;
				visitDepthFirstPrunable(NodeAddress(stmt), [&](const NodeAddress& node)->bool {
					NodeAddress cur = node;

					// already violated => abort
					if(!readOnly) { return true; }

					// prune inner scopes
					if(cur->getNodeType() == NT_LambdaExpr) { return true; }

					// only interested in the given variable
					if(*cur != *var) { return false; }

					// peeling of enclosing deref calls
					int peeledLevels = 0;
					for(int i=0; i<indirectionLevels; i++) {
						if (!cur.isRoot() && isCallOf(cur.getParentNode(), deref)) {
							cur = cur.getParentAddress();
							peeledLevels++;
						}
					}

					// check whether value is used
					if(cur.getParentNode()->getNodeType() == NT_CompoundStmt) { return true; }

					// if it is a call to a lambda, check the lambda
					if(VisitScopes) {
						indirectionLevels -= peeledLevels;
						if(CallExprPtr call = cur.getParentNode().isa<CallExprPtr>()) {
							// check calls to nested functions
							if(LambdaExprPtr fun = call->getFunctionExpr().isa<LambdaExprPtr>()) {
								if(!isReadOnly(fun, fun->getParameterList()[cur.getIndex() - 2])) { // -1 for type, -1 for function expr
									readOnly = false;                                               // it is no longer read-only
								}

								// we can stop the decent here
								return true;
							}

							// check calls to recursive functions
							if(auto ref = call->getFunctionExpr().isa<LambdaReferencePtr>()) {
								if(!isReadOnly(ref, cur.getIndex() - 2)) { // -1 for type, -1 for function expr
									readOnly = false;                      // it is no longer read-only
								}
								// we can stop the decent here
								return true;
							}
						}
						indirectionLevels += peeledLevels;
					}

					// check whether variable is dereferenced at this location
					if(peeledLevels == indirectionLevels && !isCallOf(cur.getParentNode(), deref)) {
						// => it is not, so it is used by reference
						readOnly = false; // it is no longer read-only
						return true;      // we can stop the decent here
					}

					// continue search
					return false;
				});

				// done
				return readOnly;
			}
		};

	}


	bool isReadOnly(const LambdaExprPtr& lambda, const VariablePtr& param) {
		return ReadOnlyCheck().isReadOnly(lambda, param);
	}

	bool isReadOnly(const StatementPtr& stmt, const VariablePtr& var) {
		return ReadOnlyCheck().isReadOnly(stmt, var);
	}

	bool isReadOnlyWithinScope(const StatementPtr& stmt, const VariablePtr& var) {
		return ReadOnlyCheck(false).isReadOnly(stmt, var);
	}

	bool isStaticVar(const ExpressionPtr& var) {
		if(!var) return false;

		auto type = var->getType();
		if(!isRefType(type)) { return false; }

		if(const LiteralPtr lit = var.as<LiteralPtr>()) {
			const lang::StaticVariableExtension& ext = var->getNodeManager().getLangExtension<lang::StaticVariableExtension>();
			return (ext.isStaticType(getReferencedType(type)));
		} else {
			return false;
		}
	}


	const std::set<TagTypeReferencePtr>& getFreeTagTypeReferences(const TagTypePtr& tagType) {

		// TODO: cache
		struct FreeTagTypeReferences {
			std::set<TagTypeReferencePtr> references;
			bool operator==(const FreeTagTypeReferences& other) const {
				return references == other.references;
			}
		};

		// check for annotated result
		auto def = tagType->getDefinition();
		if (def.hasAttachedValue<FreeTagTypeReferences>()) {
			return def.getAttachedValue<FreeTagTypeReferences>().references;
		}

		// create a tag-type reference collector
		std::set<TagTypeReferencePtr> allReferences;
		auto visitor = makeLambdaVisitor([&](const TypePtr& type)->bool {

			if (auto ref = type.isa<TagTypeReferencePtr>()) {
				allReferences.insert(ref);
			}

			if (auto tagType = type.isa<TagTypePtr>()) {
				for (const auto& cur : getFreeTagTypeReferences(tagType)) {
					allReferences.insert(cur);
				}
				return true;	// stop descent here
			}

			// keep descending
			return false;

		}, true);
		auto collector = makeDepthFirstOncePrunableVisitor(visitor);

		// collect all present tag-type references
		for (const auto& binding : tagType->getDefinition()) {
			collector(binding->getRecord());
		}

		// filter out locally defined tags
		std::set<TagTypeReferencePtr> res;
		for (const auto& cur : allReferences) {
			if (!tagType->getDefinition()->getDefinitionOf(cur)) {
				res.insert(cur);
			}
		}

		// attach result
		def.attachValue(FreeTagTypeReferences{ res });

		// done
		return def.getAttachedValue<FreeTagTypeReferences>().references;
	}


	bool hasFreeTagTypeReferences(const TypePtr& type) {

		// if it is a tag type reference => it is free
		if (type.isa<TagTypeReferencePtr>()) return true;

		// for tag types => check whether there are free tag types
		if (auto tagType = type.isa<TagTypePtr>()) {
			return !getFreeTagTypeReferences(tagType).empty();
		}

		// for all others: check child types
		bool foundFree = false;
		visitDepthFirstOncePrunable(type, [&](const NodePtr& node)->bool {

			// update found flag
			foundFree = foundFree ||
				node.isa<TagTypeReferencePtr>() ||
				(node.isa<TagTypePtr>() && !getFreeTagTypeReferences(node.as<TagTypePtr>()).empty());

			// prune nested expressions and tag types
			return foundFree || node.isa<TagTypePtr>() || node.isa<ExpressionPtr>();

		}, true);

		// return result
		return foundFree;
	}


	namespace {

		// TODO: move this to the normalizer
		// TODO: remove canonical type function and use normalizer instead

		TypePtr normalizeTypeVariables(const TypePtr& type) {

			// get type variable substitution
			NodeMap mapping;

			char letter = 'a';
			auto& mgr = type.getNodeManager();
			visitDepthFirstOncePrunable(type, [&](const NodePtr& node) {
				// record variables
				if (auto var = node.isa<TypeVariablePtr>()) {
					mapping[var] = TypeVariable::get(mgr, toString(letter++));
				}
				// prune nested expressions
				if (node.isa<ExpressionPtr>()) return true;
				// otherwise continue
				return false;
			}, true);

			// check that there have been enouth variable names
			assert_le(letter,'z') << "Sorry, needs more than " << (letter - 'a') << " variable names!";

			// apply mapping
			return transform::replaceAll(mgr, type, mapping, transform::localReplacement).as<TypePtr>();
		}

		namespace {

			// a type to manage equality classes
			class EqualityClasses {

				// the number of elements managed, to be fixed on construction
				unsigned numEntries;

				// a buffer to store whether two elements are equal
				std::vector<bool> different;

			public:

				EqualityClasses(unsigned numEntries)
					: numEntries(numEntries), different(numEntries*numEntries) {}

				bool areEqual(unsigned a, unsigned b) const {
					assert_lt(a, numEntries);
					assert_lt(b, numEntries);
					if (a > b) return areEqual(b, a);
					return !different[a*numEntries + b];
				}

				void markDifferent(unsigned a, unsigned b) {
					assert_lt(a, numEntries);
					assert_lt(b, numEntries);
					if (a > b) return markDifferent(b, a);
					different[a*numEntries + b] = true;
				}

				std::vector<std::vector<unsigned>> getClasses() const {
					std::vector<std::vector<unsigned>> res;

					std::vector<bool> covered(numEntries);

					for (unsigned i = 0; i < numEntries; i++) {
						if (!covered[i]) {
							// create a new class
							res.emplace_back();
							auto& clss = res.back();
							for (unsigned j = i; j < numEntries; j++) {
								if (areEqual(i, j)) {
									clss.push_back(j);
									covered[j] = true;
								}
							}
						}

					}

					return res;
				}

			};


			RecordAddress getDefinition(const TagTypeReferenceAddress& tag) {
				RecordAddress none;
				NodeAddress cur = tag;

				// skip free tag types
				if (cur.isRoot()) return none;

				// skip binding and selection location
				if (cur.getParentNode().isa<TagTypePtr>()) return none;
				if (cur.getParentNode().isa<TagTypeBindingPtr>()) return none;

				// walk up until definition is found
				while (!cur.isRoot()) {
					cur = cur.getParentAddress();
					if (auto bind = cur.isa<TagTypeBindingAddress>()) {
						if (*tag == *bind->getTag()) {
							return bind->getRecord();
						}
					}
					if (auto def = cur.isa<TagTypeDefinitionAddress>()) {
						auto res = def->getDefinitionOf(tag);
						if (res) return res;
					}
				}
				return none;
			}

			bool equalUnder(const NodeAddress& a, const NodeAddress& b, const EqualityClasses& classes, const std::map<NodeAddress, unsigned>& index, bool topLevel = true) {

				// if it is the same address, it is always the same
				if (a == b) return true;

				// if the two nodes are completely identical and have no free tag type references
				if (*a == *b && (!a.isa<TypePtr>() || !hasFreeTagTypeReferences(a.as<TypePtr>()))) {
					return true;
				}

				// check whether they are equivalent according to the equivalence classes
				if (!topLevel) {
					auto posA = index.find(a);
					if (posA != index.end()) {
						auto posB = index.find(b);
						if (posB != index.end()) {
							// std::cout << "    Classes are equivalent: " << classes.areEqual(posA->second, posB->second) << "\n";
							return classes.areEqual(posA->second, posB->second);
						}
					}
				}

				// resolve tag type references
				if (auto t = a.isa<TagTypeReferenceAddress>()) {
					auto def = getDefinition(t);
					// If needed: check whether other side is the same free reference
					assert_true(def) << "Free tag type references not yet supported!";
					return equalUnder(def, b, classes, index, topLevel);
				}

				if (auto t = b.isa<TagTypeReferenceAddress>()) {
					auto def = getDefinition(t);
					// If needed: check whether other side is the same free reference
					assert_true(def) << "Free tag type references not yet supported: " << t << " = " << *t << " - " << t.getParentNode()->getNodeType();
					return equalUnder(a, def, classes, index, topLevel);
				}

				// skip the tag type wrapper for a
				if (auto t = a.isa<TagTypeAddress>()) {
					return equalUnder(t->getRecord(), b, classes, index, topLevel);
				}

				// and for b
				if (auto t = b.isa<TagTypeAddress>()) {
					return equalUnder(a, t->getRecord(), classes, index, topLevel);
				}

				// check whether they are of the same type
				if (a->getNodeType() != b->getNodeType()) return false;

				// check value nodes
				if (a->getNodeCategory() == NC_Value) {
					return *a == *b;
				}

				// check child-list
				bool res = equals(a->getChildList(), b->getChildList(), [&](const NodeAddress& x, const NodeAddress& y) {
					return equalUnder(x, y, classes, index, false);
				});

				return res;
			}

			struct CannotReachTagTypeTag {};

			struct CannotReachTagTypeTagger : public CachedVisitor<bool> {
				CannotReachTagTypeTagger() : CachedVisitor<bool>(true) {}

				virtual bool resolve(const NodePtr& node) {
					bool ret = false;
					if(node->hasAttachedValue<CannotReachTagTypeTag>()) return false;
					if(node->getNodeType() == NT_TagType) return true;
					if(node->getNodeType() == NT_TagTypeReference) return true;
					ret = ::any(node.getChildList(), [&](const NodePtr& child) { return visit(child); });
					if(!ret) node->attachValue<CannotReachTagTypeTag>();
					return ret;
				}
			};

			bool isReachable(const NodeAddress& from, const std::vector<RecordAddress>& trg, std::set<NodeAddress>& visited) {
				// stop if we are at a builtin
				if(core::lang::isBuiltIn(from)) return false;

				// stop if we can't reach a tag type
				if(from->hasAttachedValue<CannotReachTagTypeTag>()) return false;

				// if we reached a target, we are done
				for(const auto& t : trg) {
					if(t == from) return true;
				}

				// stop here when already visited
				if(!visited.insert(from).second) return false;

				// skip the tag type constructs
				if(auto type = from.isa<TagTypeAddress>()) {
					return isReachable(type->getRecord(), trg, visited);
				}

				// if it is a tag type reference, continue with definition
				if(auto tag = from.isa<TagTypeReferenceAddress>()) {
					auto def = getDefinition(tag);
					return def && isReachable(def, trg, visited);
				}

				// check all child nodes
				return any(from->getChildList(), [&](const NodeAddress& cur) {
					return isReachable(cur, trg, visited);
				});
			}

			bool dependsOn(const std::vector<RecordAddress>& a, const std::vector<RecordAddress>& b) {

				// quick check: if any of the addresses in a is a prefix of any of the addresses in b
				for(const auto& x : a) {
					for(const auto& y : b) {
						if(x != y && isChildOf(y, x)) return true;
					}
				}

				// more expensive: if you can reach through one of the types one of the other types => dependency
				// works on entire vectors at once for performance reasons
				for(const auto& y : b) {
					auto copy = a;
					copy.erase(std::remove(copy.begin(), copy.end(), y), copy.end());
					std::set<NodeAddress> visited;
					if(isReachable(y, copy, visited)) return true;
				}

				// otherwise there is no dependency
				return false;
			}

			RecordAddress getRepresentForClass(const std::vector<RecordAddress>& clss) {
				assert_false(clss.empty());

				// pick the first named reference
				for (const auto& record : clss) {
					if (auto binding = record.getParentNode().isa<TagTypeBindingPtr>()) {
						auto tag = binding->getTag();
						if (tag->getName()->getValue() != "_") return record;
					}
				}

				// so there is no named one, take the first
				return clss.front();
			}

			TagTypeReferencePtr getTagForClass(const std::vector<RecordAddress>& clss) {
				return getRepresentForClass(clss).getParentNode().as<TagTypeBindingPtr>()->getTag();
			}

			TagTypeAddress getEnclosingTagType(const RecordAddress& record) {
				const static TagTypeAddress none;

				// move up to the tag type enclosing this record
				if (record.isRoot()) return none;

				auto binding = record.getParentAddress().isa<TagTypeBindingAddress>();
				if (!binding || binding.isRoot()) return none;

				auto def = binding.getParentAddress().isa<TagTypeDefinitionAddress>();
				if (!def || def.isRoot()) return none;

				auto tagType = def.getParentAddress().isa<TagTypeAddress>();
				if (!tagType || *tagType->getTag() != *binding->getTag()) return none;

				return tagType;
			}
		}

		TypePtr normalizeRecursiveTypes(const TypePtr& type) {
			// This function is converting recursive types into their most compact form.
			// Thus, (partially) unrolled or peeled fragments will be identified as such
			// and pruned to obtain a representation which can not be further reduced.

			// function specific debugging flag
			static const bool debug = false;

			// 1) get a list of all record types and tag type references in the given type (second most time consuming part)
			CannotReachTagTypeTagger tagger;
			tagger(type); // preprocessing pass to add annotations used to speed up steps 1) and 3)

			vector<RecordAddress> records;
			visitDepthFirstPrunable(TypeAddress(type), [&](const NodeAddress& node) {
				if(core::lang::isBuiltIn(node)) return true;
				if(node->hasAttachedValue<CannotReachTagTypeTag>()) return true;
				if(auto record = node.isa<RecordAddress>()) records.push_back(record);
				return false;
			}, true);

			// print some debugging
			if (debug) {
				std::cout << "Num Records: " << records.size() << "\n";
				int i = 0;
				for (const auto& cur : records) {
					std::cout << "\t" << i << ": " << cur << " = " << *cur << "\n";
					i++;
				}
			}

			// if there are no records or a single record => we are done
			if (records.size() <= 1) return type;

			// 2) compute equivalence classes of those records
			EqualityClasses classes(records.size());
			std::map<NodeAddress, unsigned> recordToId;
			for (unsigned i = 0; i < records.size(); ++i) {
				recordToId[records[i]] = i;
			}

			bool changed = true;
			while (changed) {
				changed = false;

				// compare each pair of records
				for (unsigned i = 0; i < records.size(); ++i) {
					for (unsigned j = i + 1; j < records.size(); ++j) {
						// if they are still considered equal ..
						if (classes.areEqual(i, j)) {
							// .. verify this
							if (!equalUnder(records[i], records[j], classes, recordToId)) {
								classes.markDifferent(i, j);
								changed = true;
							}
						}
					}
				}

			}

			// extract equivalence classes
			auto listOfClasses = classes.getClasses();

			// print equivalence classes as a debug message
			if (debug) {
				std::cout << "Equivalence classes:\n";
				for (const auto& cur : listOfClasses) {
					std::cout << "\t" << cur << "\n";
				}
			}

			// 3) compute dependencies between equivalence classes (MOST time consuming part!)
			using EquivalenceClass = std::vector<RecordAddress>;
			utils::graph::Graph<EquivalenceClass> depGraph;

			// add classes as vertices
			std::vector<EquivalenceClass> vertices;
			for (const auto& cls : listOfClasses) {
				EquivalenceClass cur;
				for (unsigned i : cls) cur.push_back(records[i]);
				depGraph.addVertex(cur);
				vertices.push_back(cur);
			}

			// add dependencies
			for (const auto& a : vertices) {
				for (const auto& b : vertices) {
					if (&a != &b && dependsOn(a, b)) {
						depGraph.addEdge(a, b);
					}
				}
			}

			// debug-print the dependency graph
			if (debug) {
				std::cout << "Dependency Graph:\n";
				depGraph.printGraphViz(std::cout);
			}

			// 4) compute strongly connected components
			auto componentGraph = utils::graph::computeSCCGraph(depGraph.asBoostGraph());

			// debug-print the component graph
			if (debug) {
				std::cout << "Component Graph:\n";
				componentGraph.printGraphViz(std::cout);
			}

			// 5) compute topological order on component graph
			auto order = utils::graph::getTopologicalOrder(componentGraph);

			// print the topological order
			if (debug) {
				std::cout << "Topological order:\n";
				for (const auto& cur : order) {
					std::cout << "\t" << cur << "\n";
				}
			}

			// 6) process each component bottom up to build minimal type
			IRBuilder builder(type.getNodeManager());
			std::map<NodeAddress, NodePtr> replacements;
			for (const auto& group : order) {

				// build up definitions for current tag type definition block
				TagTypeBindingMap bindings;

				// register tag type replacements for recursive dependencies
				for (const auto& cls : group) {

					// the first is the represent to be transformed
					auto represent = getRepresentForClass(cls);

					// get the tag type reference to be used for this class
					auto tag = getTagForClass(cls);

					// for all others add a temporary replacement
					for (const auto& record : cls) {

						// get enclosing tag type
						auto tagType = getEnclosingTagType(record);
						if (!tagType) continue;

						// substitute this tag type with the common tag type reference of this group
						assert_true(tagType.isValid());
						replacements[tagType] = tag;
					}

				}

				// print current replacements
				if (debug) std::cout << "Replacements: " << replacements << "\n";

				// compute canonical form for each class
				for (const auto& cls : group) {

					// obtain tag
					auto tag = getTagForClass(cls);

					// compute unified replacement
					auto represent = getRepresentForClass(cls);

					// filter replacement map to nested addresses
					std::map<NodeAddress, NodePtr> local_replacements;
					for (const auto& cur : replacements) {
						if (isChildOf(represent, cur.first)) {
							local_replacements[cur.first] = cur.second;
						}
					}

					// print filtered replacement
					if (debug) {
						std::cout << "Local Replacements:\n";
						std::cout << local_replacements << "\n";
					}

					// unify current record
					auto unified_record = represent.as<RecordPtr>();
					if (!local_replacements.empty()) {
						unified_record = represent.switchRoot(
							transform::replaceAll(type.getNodeManager(), local_replacements)
						).as<RecordPtr>();
					}

					// print the current unification step
					if (debug) {
						std::cout << "Represent:\n";
						std::cout << represent << "\n";
						std::cout << *represent << "\n";
						std::cout << "Unified struct:\n";
						std::cout << *unified_record << "\n";
						std::cout << "\n";
					}

					// register the unified record
					bindings[tag] = unified_record;
				}


				// create definition
				auto def = builder.tagTypeDefinition(bindings);

				// create the complete tag type representation for each of the classes in the current group
				for (const auto& cls : group) {

					auto tag = getTagForClass(cls);
					auto tagType = builder.tagType(tag, def);

					// register resulting tag types
					for (const auto& cur : cls) {
						auto type = getEnclosingTagType(cur);
						if (type) replacements[type] = tagType;
					}
				}
			}

			// shortcut if there have not been any replacements
			if (replacements.empty()) return type;

			// convert the input type
			auto result = transform::replaceAll(type.getNodeManager(), replacements).as<TypePtr>();

			// make sure the resulting type is OK
			if(debug) assert_true(checks::check(result).empty()) << checks::check(result);

			// done
			return result;
		}
	}


	TypePtr getCanonicalType(const TypePtr& a) {

		// An annotation associating the canonical type to types.
		struct CanonicalAnnotation {
			TypePtr canonical;
			bool operator==(const CanonicalAnnotation& other) const {
				return *canonical == *other.canonical;
			}
		};

		// check the annotation
		if (a.hasAttachedValue<CanonicalAnnotation>()) {
			return a.getAttachedValue<CanonicalAnnotation>().canonical;
		}


		TypePtr res = a;

		// step 1 - normalize type variables
		res = normalizeTypeVariables(res);

		// step 2 - normalize recursions
		res = normalizeRecursiveTypes(res);

		// attach result
		a.attachValue(CanonicalAnnotation{ res });

		// done
		return res;
	}


	bool equalTypes(const TypePtr& a, const TypePtr& b) {
		// simply compare the canonical types
		return *a == *b || getCanonicalType(a) == getCanonicalType(b);
	}

	bool isZero(const core::ExpressionPtr& value) {
		const auto& refExt = value->getNodeManager().getLangExtension<lang::ReferenceExtension>();
		core::IRBuilder builder(value->getNodeManager());

		// if initialization is zero ...
		if(value == builder.getZero(value->getType())) {
			// no initialization required
			return true;
		}

		// ... or a zero literal ..
		if(value->getNodeType() == core::NT_Literal) {
			boost::regex zeroRegex(R"(((-?0*)u?(l|ll)?)|(-?0\.0*[fF]*))", (boost::regex::flag_type)(boost::regex::optimize | boost::regex::ECMAScript));

			const string& strValue = static_pointer_cast<const core::Literal>(value)->getStringValue();

			if(boost::regex_match(strValue, zeroRegex)) { return true; }
		}

		// ... or the ref_null literal
		if(refExt.isRefNull(value)) { return true; }

		// otherwise, it is not zero
		return false;
	}

	bool hasFreeControlStatement(const StatementPtr& stmt, NodeType controlStmt, const vector<NodeType>& pruneStmts) {
		bool hasFree = false;
		visitDepthFirstOncePrunable(stmt, [&](const NodePtr& cur) {
			auto curType = cur->getNodeType();
			if(::contains(pruneStmts, curType)) {
				return true; // do not descent here
			}
			if(cur->getNodeType() == controlStmt) {
				hasFree = true; // "bound" stmt found
			}
			return hasFree;
		});
		return hasFree;
	}

	bool isOutlineAble(const StatementPtr& stmt, bool allowReturns) {
		if(!allowReturns) {
			// the statement must not contain a "free" return
			if(hasFreeReturnStatement(stmt)) { return false; }
		}
		// search for "bound" break or continue statements
		bool hasFreeBreakOrContinue = hasFreeContinueStatement(stmt) || hasFreeBreakStatement(stmt);
		return !hasFreeBreakOrContinue;
	}

	bool hasFreeBreakStatement(const StatementPtr& stmt) {
		return hasFreeControlStatement(stmt, NT_BreakStmt, { NT_ForStmt, NT_SwitchStmt, NT_WhileStmt, NT_LambdaExpr });
	}

	bool hasFreeContinueStatement(const StatementPtr& stmt) {
		return hasFreeControlStatement(stmt, NT_ContinueStmt, { NT_ForStmt, NT_WhileStmt, NT_LambdaExpr });
	}

	bool hasFreeReturnStatement(const StatementPtr& stmt) {
		return hasFreeControlStatement(stmt, NT_ReturnStmt, { NT_LambdaExpr });
	}

	namespace {

		template<typename T>
		struct free_reference_collector;

		template<>
		struct free_reference_collector<TagTypeReferencePtr> : public IRVisitor<void,Address,TagTypeReferenceAddressList&,const TagTypeReferenceSet&> {

			using reference_list_type = TagTypeReferenceAddressList;

			free_reference_collector(const TagTypeDefinitionPtr&) : IRVisitor(true) {}

			TagTypeReferenceAddressList operator()(const NodePtr& node) {
				TagTypeReferenceAddressList res;
				TagTypeReferenceSet bound;
				visit(NodeAddress(node),res,bound);
				return res;
			}

			void visitTagTypeReference(const TagTypeReferenceAddress& ref, TagTypeReferenceAddressList& res, const TagTypeReferenceSet& bound) override {
				if (!bound.contains(ref)) res.push_back(ref);
			}

			void visitTagTypeDefinition(const TagTypeDefinitionAddress& def, TagTypeReferenceAddressList& res, const TagTypeReferenceSet& bound) override {
				TagTypeReferenceSet nestedBound = bound;
				for(const auto& cur : def) nestedBound.insert(cur->getTag());
				for(const auto& cur : def) visit(cur->getRecord(), res, nestedBound);
			}

			void visitTagType(const TagTypeAddress& def, TagTypeReferenceAddressList& res, const TagTypeReferenceSet& bound) override {
				visit(def->getDefinition(), res, bound);
			}

			void visitNode(const NodeAddress& node, TagTypeReferenceAddressList& res, const TagTypeReferenceSet& bound) override {
				visitAll(node->getChildList(), res, bound);
			}

		};

		template<>
		struct free_reference_collector<LambdaReferencePtr> : public IRVisitor<void,Address,VariableAddressList&,const VariableSet&>  {

			using reference_list_type = LambdaReferenceAddressList;

			std::map<LambdaPtr, LambdaReferenceAddressList> index;

			free_reference_collector(const LambdaDefinitionPtr& def) : IRVisitor(true) {
				for (const auto& cur : def) {
					for (const auto& ref : def->getRecursiveCallsOf(cur->getReference())) {

						// lower root
						LambdaAddress lambda;
						visitPathTopDownInterruptible(ref, [&](const NodeAddress& cur)->bool {
							return lambda = cur.isa<LambdaAddress>();
						});

						// add to index
						if (!lambda) continue;
						index[lambda].push_back(cropRootNode(ref, lambda));
					}
				}
			}

			LambdaReferenceAddressList operator()(const NodePtr& node) {
				if (auto lambda = node.as<LambdaPtr>()) {
					return index[lambda];
				}
				return LambdaReferenceAddressList();
			}

		};

		template<
			typename Var,
			typename Construct,
			typename Value,
			typename Def
		>
		std::map<Var,Pointer<const Construct>> minimizeRecursiveGroupsGen(const Pointer<const Def>& def) {

			using Collector = free_reference_collector<Var>;
			using RefAddressList = typename Collector::reference_list_type;

			// instantiate reference collector
			Collector collector(def);

			// collect references
			std::map<NodePtr,RefAddressList> references;
			for(const auto& cur : def) {
				references[cur->getChild(0)] = collector(cur->getChild(1));
			}

			// extract dependency graph
			utils::graph::PointerGraph<NodePtr> depGraph;
			for(const auto& a : def) {
				auto var = a->getChild(0);
				depGraph.addEdge(var, var);
				for(const auto& c : references[var]) {
					if (def->getDefinitionOf(c)) {
						depGraph.addEdge(var, c);
					}
				}
			}

			// compute strongly connected components order
			auto compGraph = utils::graph::computeSCCGraph(depGraph.asBoostGraph());

			// compute topological order -- there is a utility for this too
			auto components = utils::graph::getTopologicalOrder(compGraph);

			// build up resulting map
			NodeManager& mgr = def.getNodeManager();
			std::map<Var, Pointer<const Construct>> res;

			// process in reverse order
			for(auto it = components.rbegin(); it != components.rend(); ++it) {
				auto& comp = *it;

				// build new bindings
				utils::map::PointerMap<Var, Value> bindings;
				for(const auto& var : comp) {
					auto v = var.as<Var>();

					// get old definition
					auto oldDef = def->getDefinitionOf(v);

					// if there is no old definition for this variable => skip
					if (!oldDef) continue;

					// create replacement map
					std::map<NodeAddress, NodePtr> replacements;
					for(const auto& ref : references[var]) {
						// if this recursive variable has already been resolved
						if (res[ref]) {
							// us the resolved version
							replacements[ref] = res[ref];
						}
					}

					// fix definition
					auto fixedDef =
							(replacements.empty()) ? oldDef :
							(transform::replaceAll(mgr, replacements).as<decltype(oldDef)>());

					assert_true(fixedDef) << "No new version for " << v;

					// move annotations
					transform::utils::migrateAnnotations(oldDef,fixedDef);

					// add binding
					bindings.insert({ v, fixedDef });
				}

				// build reduced definition group
				auto def = Def::get(mgr, bindings);

				// add definitions to result
				for(const auto& var : comp) {
					auto v = var.as<Var>();
					res[v] = Construct::get(mgr, v, def);
				}
			}

			// done
			return res;
		}

	}

	std::map<TagTypeReferencePtr, TagTypePtr> minimizeRecursiveGroup(const TagTypeDefinitionPtr& def) {
		// conduct minimization
		return minimizeRecursiveGroupsGen<TagTypeReferencePtr, TagType, RecordPtr>(def);
	}

	std::map<LambdaReferencePtr, LambdaExprPtr> minimizeRecursiveGroup(const LambdaDefinitionPtr& def) {
		// conduct minimization
		return minimizeRecursiveGroupsGen<LambdaReferencePtr, LambdaExpr, LambdaPtr>(def);
	}


} // end namespace utils
} // end namespace core
} // end namespace insieme
