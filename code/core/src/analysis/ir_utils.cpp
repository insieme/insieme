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
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation_utils.h"

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
		return (basic.isPure(fun) || refExt.isRefDeref(fun)) && all(call->getArguments(), &isSideEffectFree);
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
			bool visitTypeVariable(const TypeVariablePtr& var) {
				return true; // there is a free generic variable
			}

			bool visitFunctionType(const FunctionTypePtr& funType) {
				return any(funType->getParameterTypes(), [&](const TypePtr& type) { return this->visit(type); })
				       || this->visit(funType->getReturnType());
			}

			bool visitTupleType(const TupleTypePtr& tuple) {
				return any(tuple->getElements(), [&](const TypePtr& type) { return this->visit(type); });
			}

			bool visitGenericType(const GenericTypePtr& type) {
				// generic type is generic if one of its parameters is
				return any(type->getTypeParameter()->getTypes(), [&](const TypePtr& type) { return this->visit(type); });
			}

			bool visitNumericType(const NumericTypePtr&) {
				// numeric types are never generic
				return false;
			}

			bool visitTagType(const TagTypePtr& tagType) {
				// check types within recursive bindings
				return any(tagType->getDefinition(), [&](const TagTypeBindingPtr& binding) { return this->visit(binding->getRecord()); });
			}

			bool visitTagTypeReference(const TagTypeReferencePtr& tagType) {
				// nothing generic here
				return false;
			}

			bool visitRecord(const RecordPtr& record) {
				return any(record->getFields(), [&](const FieldPtr& field) { return this->visit(field->getType()); });
			}

			bool visitType(const TypePtr& type) {
				return any(type->getChildList(), [&](const NodePtr& node) { return this->visit(node); });
			}

			/**
			 * A terminal visit capturing all non-covered types. This one should
			 * never be reached since all cases need to be covered within specialized
			 * visit members.
			 */
			bool visitNode(const NodePtr& node, TypeVariableSet& boundVars) {
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
		return pnt == NT_StructExpr || pnt == NT_UnionExpr || pnt == NT_TupleExpr || pnt == NT_JobExpr;
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

			// do not visit types
			FreeVariableCollector() : IRVisitor<void, Ptr, VariableSet&, std::set<Ptr<const Variable>>&>(false) {}

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
				// ignore types
				if(node->getNodeCategory() == NC_Type) { return; }

				// visit all sub-nodes
				this->visitAll(node->getChildList(), bound, free);
			}

			void visitDeclarationStmt(const Ptr<const DeclarationStmt>& decl, VariableSet& bound, ResultSet& free) {
				// first add variable to set of bound variables
				bound.insert(decl->getVariable());

				// then visit the defining expression
				this->visit(decl->getInitialization(), bound, free);
			}

			void visitCompoundStmt(const Ptr<const CompoundStmt>& compound, VariableSet& bound, ResultSet& free) {
				// a compound statement creates a new scope
				VariableSet innerBound = bound;

				// continue visiting with the limited scope
				visitNode(compound, innerBound, free);
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

	namespace {
		class RenamingVarVisitor : public core::IRVisitor<void, Address> {
			core::VariableAddress varAddr;

			void visitCallExpr(const CallExprAddress& call) {
				if(LambdaExprAddress lambda = dynamic_address_cast<const LambdaExpr>(call->getFunctionExpr())) {
					for_each(make_paired_range(call->getArguments(), lambda->getLambda()->getParameters()),
					          [&](const std::pair<const core::ExpressionAddress, const core::VariableAddress>& pair) {
						          if(*varAddr == *pair.second) {
							          if(VariableAddress tmp = dynamic_address_cast<const Variable>(extractVariable(pair.first))) { varAddr = tmp; }
						          }
						      });
				}
			}

			ExpressionAddress extractVariable(ExpressionAddress exp) {
				if(VariableAddress var = dynamic_address_cast<const Variable>(exp)) { return var; }

				if(CastExprAddress cast = dynamic_address_cast<const CastExpr>(exp)) { return extractVariable(cast->getSubExpression()); }

				if(CallExprAddress call = dynamic_address_cast<const CallExpr>(exp)) {
					NodeManager& manager = exp->getNodeManager();
					if(manager.getLangExtension<lang::ReferenceExtension>().isRefDeref(call->getFunctionExpr())) {
						return extractVariable(call->getArgument(0));
					}
				}

				return exp;
			}

		  public:
			VariableAddress& getVariableAddr() {
				return varAddr;
			}

			RenamingVarVisitor(const core::VariableAddress& va) : IRVisitor<void, Address>(false), varAddr(va) {}
		};
	}

	namespace {
		class VariableNameVisitor : public core::IRVisitor<void, Address> {
			core::VariableAddress varAddr;
			std::vector<VariablePtr>& varVec;

			void visitCallExpr(const CallExprAddress& call) {
				if(LambdaExprAddress lambda = dynamic_address_cast<const LambdaExpr>(call->getFunctionExpr())) {
					for_each(make_paired_range(call->getArguments(), lambda->getLambda()->getParameters()),
					          [&](const std::pair<const core::ExpressionAddress, const core::VariableAddress>& pair) {
						          if(*varAddr == *pair.second) {
							          if(VariableAddress tmp = dynamic_address_cast<const Variable>(extractVariable(pair.first))) { varAddr = tmp; }
							          VariableAddress second = dynamic_address_cast<const Variable>(extractVariable(pair.second));

							          if(std::find(std::begin(varVec), std::end(varVec), second.getAddressedNode()) == std::end(varVec)) {
								          varVec.push_back(second.getAddressedNode());
							          }

							          if(std::find(std::begin(varVec), std::end(varVec), varAddr.getAddressedNode()) == std::end(varVec)) {
								          varVec.push_back(varAddr.getAddressedNode());
							          }
						          }
						      });
				}
			}

			ExpressionAddress extractVariable(ExpressionAddress exp) {
				if(VariableAddress var = dynamic_address_cast<const Variable>(exp)) { return var; }

				if(CastExprAddress cast = dynamic_address_cast<const CastExpr>(exp)) { return extractVariable(cast->getSubExpression()); }

				if(CallExprAddress call = dynamic_address_cast<const CallExpr>(exp)) {
					NodeManager& manager = exp->getNodeManager();
					if(manager.getLangExtension<lang::ReferenceExtension>().isRefDeref(call->getFunctionExpr())) {
						return extractVariable(call->getArgument(0));
					}
				}

				return exp;
			}

		  public:
			VariableNameVisitor(const core::VariableAddress& va, std::vector<VariablePtr>& vv) : IRVisitor<void, Address>(false), varAddr(va), varVec(vv) {}
		};
	}


	std::vector<VariablePtr> getVariableNames(const VariablePtr& var, const NodePtr& code) {
		VariableAddress varAddr = core::Address<const core::Variable>::find(var, code);

		std::vector<VariablePtr> varVec;
		VariableNameVisitor rvv(varAddr, varVec);
		visitPathBottomUp(varAddr, rvv);

		return varVec;
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
		bool checkTypes = element->getNodeCategory() == NC_Type || element->getNodeCategory() == NC_IntTypeParam;
		return code && makeCachedLambdaVisitor([&](const NodePtr& cur, rec_call<bool>::type& rec) -> bool {
			       return *cur == *element || any(cur->getChildList(), rec);
			   }, checkTypes)(code);
	}

	unsigned countInstances(const NodePtr& code, const NodePtr& element) {
		assert_true(element) << "Element to be searched must not be empty!";
		bool checkTypes = element->getNodeCategory() == NC_Type || element->getNodeCategory() == NC_IntTypeParam;
		return code ? makeCachedLambdaVisitor([&](const NodePtr& cur, rec_call<unsigned>::type& rec) -> unsigned {
			if(*cur == *element) return 1;
			auto children = cur->getChildList();
			unsigned ret = 0;
			for(const auto& c : children) {
				ret += rec(c);
			}
			return ret;
		}, checkTypes)(code) : 0;
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

		TagTypeSet getNestedTagTypes(const TypePtr& type) {
			TagTypeSet res;
			visitDepthFirstOnce(type, [&](const TagTypePtr& tagType) {
				// record types
				res.insert(tagType);
			}, true);
			return res;
		}

		TagTypePtr tryUnpeelingType(const TagTypePtr& type) {

			// for recursive types this is done
			if (type->isRecursive()) return type;

			// get set of nested tag types
			TagTypeSet tagTypes = getNestedTagTypes(type);

			// if there are non, done
			if (tagTypes.empty()) return type;

			// walk through candidate list
			auto& mgr = type.getNodeManager();
			for(TagTypePtr cur : tagTypes) {

				// check whether current candidate is recursive
				if (!cur->isRecursive()) continue;

				// if not contained => skip
				if (!tagTypes.contains(cur)) continue;

				// get list of all recursive types based on this definition
				TagTypeList recTypes;
				for(const auto& def : cur->getDefinition()) {
					recTypes.push_back(TagType::get(mgr, def->getTag(), cur->getDefinition()));
				}

				// start with unpeeled types
				int peelFactor = 0;
				TagTypeList peeled = recTypes;

				// while any of the types is still present
				while(any(peeled, [&](const TagTypePtr& cur) { return tagTypes.contains(cur); })) {
					// check whether we have a match
					for(const auto& cur : make_paired_range(peeled, recTypes)) {
						if (*cur.first == *type) return cur.second;
					}

					// peel one more level
					peelFactor++;
					peeled.clear();
					for(const auto& cur : recTypes) {
						peeled.push_back(cur->peel(mgr,peelFactor));
					}
				}
			}

			// no corresponding recursive type found
			return type;
		}

		TypePtr normalizeRecursiveTypes(const TypePtr& type) {

			// get a set of all nested types
			TagTypeSet types = getNestedTagTypes(type);

			// check for all tag types whether they are peeled recursive types
			NodeMap map;
			for(const auto& cur : types) {
				auto unpeeled = tryUnpeelingType(cur);
				if (*cur != *unpeeled) {
					map[cur] = unpeeled;
				}
			}

			// apply replacement
			return transform::replaceAll(type.getNodeManager(), type, map).as<TypePtr>();
		}

	}


	TypePtr getCanonicalType(const TypePtr& a) {
		TypePtr res = a;

		// step 1 - normalize type variables
		res = normalizeTypeVariables(res);

		// step 2 - normalize recursions
		res = normalizeRecursiveTypes(res);

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

		// TODO: remove this when frontend is fixed!!
		// => compensate for silly stuff like var(*getNull()) or NULL aka ref_deref(ref_null)
		if(core::analysis::isCallOf(value, refExt.getRefVarInit()) || core::analysis::isCallOf(value, refExt.getRefDeref())) {
			return isZero(core::analysis::getArgument(value, 0));
		}

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
