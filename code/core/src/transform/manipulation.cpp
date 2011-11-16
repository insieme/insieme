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

#include <algorithm>

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/type_utils.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace transform {

using namespace std;

namespace us = insieme::utils::set;
namespace um = insieme::utils::map;

/**
 * A utility function to apply an arbitrary manipulation to the statements within a compound statement.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement which should be manipulated
 * @param manipulator the manipulation to be applied as a functor
 * @return the root node of the modified AST tree (according to the root of the address)
 */
template<typename Manipulator>
NodePtr manipulate(NodeManager& manager, const CompoundStmtAddress& target, Manipulator manipulator) {

	// get and manipulate statement list
	vector<StatementPtr> list = target.getAddressedNode()->getStatements();

	// apply manipulation
	manipulator(list);

	// create and apply replacement
	CompoundStmtPtr replacement = CompoundStmt::get(manager, list);
	return replaceNode(manager, target, replacement);
}


NodePtr insert(NodeManager& manager, const CompoundStmtAddress& target, const StatementPtr& statement, unsigned index) {
	// use generic manipulation function
	return manipulate(manager, target, [index, &statement](vector<StatementPtr>& list){
		// limit index and insert element
		unsigned pos = min((unsigned)(list.size()), index);
		list.insert(list.begin() + pos, statement);
	});
}

NodePtr insert(NodeManager& manager, const CompoundStmtAddress& target, const StatementList& statements, unsigned index) {
	// use generic manipulation function
	return manipulate(manager, target, [index, &statements](vector<StatementPtr>& list){
		// limit index and insert element
		unsigned pos = min((unsigned)(list.size()), index);
		list.insert(list.begin() + pos, statements.cbegin(), statements.cend());
	});
}


NodePtr insertBefore(NodeManager& manager, const StatementAddress& target, const StatementPtr& statement) {

	// check whether target is within a compound statement
	if(auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress())) {
		return insert(manager, compoundParent, statement, target.getIndex());
	}

	IRBuilder build(manager);
	auto newCompound = build.compoundStmt(statement, target.getAddressedNode());
	return replaceNode(manager, target, newCompound);
}

NodePtr insertBefore(NodeManager& manager, const StatementAddress& target, const StatementList& statements) {

	// check whether target is within a compound statement
	if(auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress())) {
		return insert(manager, compoundParent, statements, target.getIndex());
	}

	IRBuilder build(manager);
	StatementList allStatements;
	allStatements.insert(allStatements.begin(), statements.cbegin(), statements.cend());
	allStatements.push_back(target.getAddressedNode());
	auto newCompound = build.compoundStmt(allStatements);
	return replaceNode(manager, target, newCompound);
}

NodePtr insertAfter(NodeManager& manager, const StatementAddress& target, const StatementPtr& statement) {
	auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress());
	if(compoundParent) {
		return insert(manager, compoundParent, statement, target.getIndex()+1);
	} else {
		IRBuilder build(manager);
		auto newCompound = build.compoundStmt(target.getAddressedNode(), statement);
		return replaceNode(manager, target, newCompound);
	}
}

NodePtr insertAfter(NodeManager& manager, const StatementAddress& target, const StatementList& statements) {
	auto compoundParent = dynamic_address_cast<const CompoundStmt>(target.getParentAddress());
	if(compoundParent) {
		return insert(manager, compoundParent, statements, target.getIndex()+1);
	} else {
		IRBuilder build(manager);
		StatementList allStatements;
		allStatements.push_back(target.getAddressedNode());
		allStatements.insert(allStatements.end(), statements.cbegin(), statements.cend());
		auto newCompound = build.compoundStmt(allStatements);
		return replaceNode(manager, target, newCompound);
	}
}


NodePtr remove(NodeManager& manager, const CompoundStmtAddress& target, unsigned index) {
	// use generic manipulation function
	return manipulate(manager, target, [index](vector<StatementPtr>& list){
		// remove element
		assert( index < list.size() && "Index out of range!");
		list.erase(list.begin() + index);
	});
}

NodePtr replace(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, const StatementPtr& replacement) {
	// use generic manipulation function
	return manipulate(manager, target, [index, replacement](vector<StatementPtr>& list){
		// remove element
		assert( index < list.size() && "Index out of range!");
		list[index] = replacement;
	});
}

NodePtr replace(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, const StatementList& replacements) {
	// use generic manipulation function
	return manipulate(manager, target, [index, &replacements](vector<StatementPtr>& list){
		// remove element
		assert( index < list.size() && "Index out of range!");
		list.erase(list.begin() + index);
		list.insert(list.begin() + index, replacements.cbegin(), replacements.cend());
	});
}

NodePtr move(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, int displacement) {

	// shortcut for no offset
	if (displacement == 0) {
		return target.getRootNode();
	}

	// use generic manipulation function
	return manipulate(manager, target, [index, displacement](vector<StatementPtr>& list){
		// check index
		assert( index < list.size() && "Index out of range!");

		// limit displacement
		int newPos = index + displacement;
		newPos = max(min((int)(list.size()-1), newPos), 0);

		StatementPtr element = list[index];
		list.erase(list.begin() + index);
		list.insert(list.begin() + newPos, element);
	});
}

namespace {

	class InlineSubstituter : public NodeMapping {

		bool successful;

		um::PointerMap<VariablePtr, ExpressionPtr>& replacements;
		us::PointerSet<VariablePtr> replacedOnce;

	public:

		InlineSubstituter(um::PointerMap<VariablePtr, ExpressionPtr>& replacements)
			: successful(true), replacements(replacements) { }

		const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
			if (!successful) {
				return ptr;
			}

			if (ptr->getNodeType() != NT_Variable) {
				if (ptr->getNodeType() == NT_LambdaExpr) {
					// entering new scope => ignore this
					return ptr;
				}
				return ptr->substitute(ptr->getNodeManager(), *this);
			}

			const VariablePtr& var = static_pointer_cast<const Variable>(ptr);

			// check whether variable has been already encountered
			if (replacedOnce.find(var) != replacedOnce.end()) {
				// variable may only be replaced once!
				successful = false;
				return ptr;
			}

			auto pos = replacements.find(var);
			if (pos != replacements.end()) {
				ExpressionPtr res = pos->second;

				// to ensure a single evaluation!
				NodeType type = res->getNodeType();
				if (type != NT_Variable && type != NT_Literal) {
					replacedOnce.insert(var);
				}
				return res;
			}

			// no modification required
			return ptr;
		}

		bool wasSuccessful() const {
			return successful;
		}
	};




	ExpressionPtr tryInlineBindToExpr(NodeManager& manager, const CallExprPtr& call) {

		// extract bind and call expression
		assert(call->getFunctionExpr()->getNodeType() == NT_BindExpr && "Illegal argument!");
		BindExprPtr bind = static_pointer_cast<const BindExpr>(call->getFunctionExpr());

		// process call recursively
		CallExprPtr innerCall = bind->getCall();
		ExpressionPtr inlined = tryInlineToExpr(manager, innerCall);

		// check for matching argument number
		auto parameter = bind->getParameters();
		auto arguments = call->getArguments();
		if (parameter.size() != arguments.size()) {
			return call;
		}

		// substituted call arguments with bind parameters
		um::PointerMap<VariablePtr, ExpressionPtr> replacements;

		replacements.insert(
				make_paired_iterator(parameter.begin(), arguments.begin()),
				make_paired_iterator(parameter.end(), arguments.end())
		);

		// substitute variables within body
		InlineSubstituter substituter(replacements);
		ExpressionPtr res = static_pointer_cast<const Expression>(substituter.mapElement(0, inlined));

		// check result
		if (!substituter.wasSuccessful()) {
			return call;
		}
		return res;
	}

	ExpressionPtr tryInlineToExprInternal(NodeManager& manager, const CallExprPtr& call) {

		// Step 1 - get capture init and lambda expression
		ExpressionPtr target = call->getFunctionExpr();
		LambdaExprPtr lambda;

		// check for bind expression ...
		if (target->getNodeType() == NT_BindExpr) {
			return tryInlineBindToExpr(manager, call);
		}

		// check for lambda ...
		if (target->getNodeType() == NT_LambdaExpr) {
			lambda = static_pointer_cast<const LambdaExpr>(target);
		} else {
			// no in-lining possible
			return call;
		}

		// Step 2 - check body => has to be a return statement
		StatementPtr bodyStmt = lambda->getLambda()->getBody();

		if (CompoundStmtPtr compound = dynamic_pointer_cast<const CompoundStmt>(bodyStmt)) {
			const vector<StatementPtr>& stmts = compound->getStatements();
			if (stmts.size() == 1) {
				bodyStmt = stmts[0];
			} else {
				// no in-lining possible (to many statements)
				return call;
			}
		}

		// check for expression
		ExpressionPtr body;
		if (ReturnStmtPtr returnStmt = dynamic_pointer_cast<const ReturnStmt>(bodyStmt)) {
			body = returnStmt->getReturnExpr();
		} else if (bodyStmt->getNodeCategory() == core::NC_Expression){
			// single expression => can also be inlined
			body = static_pointer_cast<const core::Expression>(bodyStmt);
		} else {
			// no in-lining possible (not a simple expression)
			return call;
		}

		// Step 3 - collect variables replacements
		const ParametersPtr& paramList = lambda->getParameterList();

		um::PointerMap<VariablePtr, ExpressionPtr> replacements;

		// add call parameters
		int index = 0;
		::for_each(call->getArguments(), [&](const ExpressionPtr& cur) {
			replacements.insert(std::make_pair(paramList[index++], cur));
		});

		// Step 4 - substitute variables within body
		InlineSubstituter substituter(replacements);
		ExpressionPtr res = static_pointer_cast<const Expression>(substituter.mapElement(0, body));

		// check result
		if (substituter.wasSuccessful()) {
			return res;
		}
		return call;
	}

}


ExpressionPtr tryInlineToExpr(NodeManager& manager, const CallExprPtr& call) {

	bool successful = true;
	ExpressionPtr res = call;
	while(successful && res->getNodeType() == NT_CallExpr) {
		ExpressionPtr tmp = tryInlineToExprInternal(manager, static_pointer_cast<const CallExpr>(res));
		successful = (*tmp != *res);
		res = tmp;
	}
	return res;
}

namespace {

	class ParameterFixer : public core::transform::CachedNodeMapping {

		NodeManager& manager;
		const VariablePtr var;
		const ExpressionPtr replacement;

	public:

		ParameterFixer(NodeManager& manager, const VariablePtr& var, const ExpressionPtr& replacement)
			: manager(manager), var(var), replacement(replacement) {}

		const NodePtr resolveElement(const NodePtr& ptr) {
			// check for replacement
			if (*ptr == *var) {
				return replacement;
			}

			// cut off types and stop if already unsuccessful
			if (ptr->getNodeCategory() == NC_Type) {
				return ptr;
			}

			// handle call expressions
			if (ptr->getNodeType() == NT_CallExpr) {
				CallExprPtr call = static_pointer_cast<const CallExpr>(ptr);
				if (::contains(call->getArguments(), var)) {

					const ExpressionList& args = call->getArguments();
					ExpressionList newArgs;
					std::size_t size = args.size();

					// Push value through to sub-call ...
					ExpressionPtr fun = call->getFunctionExpr();
					if (fun->getNodeType() != NT_LambdaExpr) {
						// cannot be pushed through ... just substitute within arguments and done
						::transform(args, std::back_inserter(newArgs), [&](const ExpressionPtr& cur)->const ExpressionPtr {
							if (*cur == *var) {
								return replacement;
							}
							return this->map(cur);
						});
					} else {
						LambdaExprPtr original = static_pointer_cast<const LambdaExpr>(fun);
						LambdaExprPtr lambda = original;

						// replace one parameter after another (back to front)
						for (int i = size-1; i>=0; i--) {
							// check if it is the variable

							// try to fix the parameter for called function
							if (*args[i] == *var) {
								LambdaExprPtr newLambda = tryFixParameter(manager, lambda, i, replacement);
								if (*newLambda != *lambda) {
									// => nice, it worked!
									lambda = newLambda;
									continue;
								}
							}

							// exchange argument
							ExpressionPtr newArg = this->map(args[i]);
							newArgs.insert(newArgs.begin(), newArg);
						}

						// restore annotations
						// TODO: let annotations decide which should be preserved
						lambda->setAnnotations(original->getAnnotations());
						lambda->getLambda()->setAnnotations(original->getLambda()->getAnnotations());

						// exchange function
						fun = lambda;
					}

					// create new call
					return CallExpr::get(manager, call->getType(), fun, newArgs);
				}
			}

			// handle lambda expressions ...
			if (ptr->getNodeType() == NT_LambdaExpr) {
				// .. end of replacement scope
				return ptr;
			}

			// else: process recursively
			return ptr->substitute(manager, *this);

		}

	};


}


LambdaExprPtr tryFixParameter(NodeManager& manager, const LambdaExprPtr& lambda, unsigned index, const ExpressionPtr& value) {

	if (lambda->isRecursive()) {
		// TODO: support recursive lambdas
		return lambda;
	}

	// check parameters
	const FunctionTypePtr& funType = static_pointer_cast<const FunctionType>(lambda->getType());
	TypeList paramTypes = funType->getParameterTypes()->getTypes();
	assert(index < paramTypes.size() && "Index out of bound - no such parameter!");

	assert(isSubTypeOf(value->getType(), paramTypes[index]) && "Cannot substitute non-compatible value for specified parameter.");

	// conduct replacement

	const VariablePtr& param = lambda->getParameterList()[index];
	ParameterFixer fixer(manager, param, value);
	CompoundStmtPtr body = fixer.map(lambda->getBody());

	// create new function type
	paramTypes.erase(paramTypes.begin() + index);
	FunctionTypePtr newFunType = FunctionType::get(manager, paramTypes, funType->getReturnType(), true);

	// create new parameter list
	vector<VariablePtr> params = lambda->getParameterList()->getParameters();
	params.erase(params.begin() + index);

	// build resulting lambda
	return LambdaExpr::get(manager, newFunType, params, body);
}

StatementPtr fixVariable(NodeManager& manager, const StatementPtr& statement, const VariablePtr& var, const ExpressionPtr& value) {
	ParameterFixer fixer(manager, var, value);
	return fixer.map(statement);
}


// ------------------------------ lambda extraction -------------------------------------------------------------------

namespace { 
	/**
	 * Will certainly determine the declaration status of variables inside a block.
	 */
	struct LambdaDeltaVisitor : public IRVisitor<bool, Address> {
		us::PointerSet<VariablePtr> declared;
		us::PointerSet<VariablePtr> undeclared;

		// do not visit types
		LambdaDeltaVisitor() : IRVisitor<bool, Address>(false) {}

		bool visitNode(const NodeAddress& node) { return false; } // default behaviour: continue visiting

		bool visitDeclarationStmt(const DeclarationStmtAddress &decl) {
			declared.insert(decl->getVariable());
			return false;
		}

		bool visitVariable(const VariableAddress& var) {
			auto vp = var.getAddressedNode();
			if(declared.find(vp) == declared.end()) undeclared.insert(vp);
			return false;
		}

		// due to the structure of the IR, nested lambdas can never reuse outer variables
		//  - also prevents variables in LamdaDefinition from being inadvertently captured
		bool visitLambdaExpr(const LambdaExprAddress&) {
			return true;
		}
	};

	NodePtr extractLambdaImpl(NodeManager& manager, const StatementPtr& root, IRBuilder::VarValueMapping& captures,
			um::PointerMap<NodePtr, NodePtr>& replacements, std::vector<VariablePtr>& passAsArguments) {
		LambdaDeltaVisitor ldv;
		visitDepthFirstPrunable(StatementAddress(root), ldv);

		// sort set to ensure code identity
		std::vector<VariablePtr> undeclared(ldv.undeclared.cbegin(), ldv.undeclared.cend());
		std::sort(undeclared.begin(), undeclared.end(), [](const VariablePtr& p1, const VariablePtr& p2) { return p1->getId() > p2->getId(); });

		IRBuilder build(manager);
		for_each(undeclared, [&](VariablePtr p) {
			auto var = build.variable(p->getType());
			if(std::find(passAsArguments.cbegin(), passAsArguments.cend(), p) == passAsArguments.end()) 
				captures[var] = p;
			replacements[p] = var;
		});

		// replace arguments with mapped variables
		for_each(passAsArguments, [&](VariablePtr& v) { 
			if(replacements.find(v) != replacements.end()) {
				v = static_pointer_cast<const Variable>(replacements[v]);
			}
		});

		return replaceAll(manager, root, replacements);
	}
}

BindExprPtr extractLambda(NodeManager& manager, const StatementPtr& root, std::vector<VariablePtr> passAsArguments) {
	IRBuilder build(manager);
	IRBuilder::VarValueMapping captures;
	um::PointerMap<NodePtr, NodePtr> replacements;
	StatementPtr newStmt = static_pointer_cast<const Statement>(extractLambdaImpl(manager, root, captures, replacements, passAsArguments));
	return build.lambdaExpr(newStmt, captures, passAsArguments);
}

BindExprPtr extractLambda(NodeManager& manager, const ExpressionPtr& root, std::vector<VariablePtr> passAsArguments) {
	IRBuilder build(manager);
	IRBuilder::VarValueMapping captures;
	um::PointerMap<NodePtr, NodePtr> replacements;
	ExpressionPtr newExpr = static_pointer_cast<const Expression>(extractLambdaImpl(manager, root, captures, replacements, passAsArguments));
	auto body = build.returnStmt(newExpr);
	return build.lambdaExpr(root->getType(), body, captures, passAsArguments);
}

LambdaExprPtr privatizeVariables(NodeManager& manager, const LambdaExprPtr& root, const std::vector<VariablePtr>& varsToPrivatize) {
	
	auto body = root->getBody();

	IRBuilder build(manager);
	um::PointerMap<NodePtr, NodePtr> replacements;

	for_each(varsToPrivatize, [&](VariablePtr p) {
		auto var = build.variable(p->getType());
		replacements[p] = var;
	});
	auto newBody = replaceAll(manager, body, replacements);

	return dynamic_pointer_cast<const LambdaExpr>(newBody);
}

LambdaExprPtr instantiate(NodeManager& manager, const LambdaExprPtr& lambda, const SubstitutionOpt& substitution) {

	// check for early exit
	if (!substitution || substitution->empty()) {
		return lambda;
	}

	assert(!lambda->isRecursive() && "I owe you the support for recursive functions!");

	// update type
	const FunctionTypePtr funType = static_pointer_cast<FunctionTypePtr>(substitution->applyTo(lambda->getType()));

	// update body
	const CompoundStmtPtr body = static_pointer_cast<CompoundStmtPtr>(transform::replaceTypeVars(manager, lambda->getBody(), substitution));

	// update parameters
	VariableList params;
	::transform(lambda->getParameterList()->getParameters(), std::back_inserter(params), [&](const VariablePtr& cur)->VariablePtr {
		TypePtr newType = substitution->applyTo(cur->getType());
		if (newType == cur->getType()) {
			return cur;
		}
		return Variable::get(manager, newType, cur->getId());
	});

	// construct result
	return LambdaExpr::get(manager, funType, params, body);
}

DeclarationStmtPtr createGlobalStruct(NodeManager& manager, ProgramPtr& prog, const NamedValueList& globals) {
	//if(!prog->isMain()) {
	//	LOG(WARNING) << "createGlobalStruct called on non-main program.";
	//}

	LambdaExprPtr lambda = dynamic_pointer_cast<const LambdaExpr>(prog->getEntryPoints().front());
	auto compound = lambda->getBody();
	auto addr = CompoundStmtAddress::find(compound, prog);
	IRBuilder build(manager);

	// generate type list from initialization expression list in "globals"
	NamedTypeList entries = ::transform(globals, [&](const NamedValuePtr& val) { return build.namedType(val->getName(), val->getValue()->getType()); });
	auto structType = build.structType(entries);
	auto declStmt = build.declarationStmt(structType, build.structExpr(globals));
	auto newProg = static_pointer_cast<const Program>(insert(manager, addr, declStmt, 0));
	utils::migrateAnnotations(prog, newProg);
	prog = newProg;

	// migrate annotations on body
	lambda = dynamic_pointer_cast<const LambdaExpr>(prog->getEntryPoints().front());
	compound = lambda->getBody();
	utils::migrateAnnotations(addr.getAddressedNode(), compound);

	return declStmt;
}

namespace {
class VariableSearchVisitor : public IRVisitor<bool, Address> {

	VariablePtr target;
	VariableAddress location;
	NodePtr stopIndicator;
public:
	VariableSearchVisitor(const VariablePtr& target, const NodePtr& stopIndicator) : 
		IRVisitor<bool, Address>(false), target(target), stopIndicator(stopIndicator) {}
	
	bool visitNode(const NodeAddress& node) {
		// interrupt if stop indicator reached
		return *node == *stopIndicator;
	}

	bool visitCompoundStmt(const CompoundStmtAddress& compound) {
		visitBreadthFirstInterruptible(compound, [&](const DeclarationStmtAddress& decl) -> bool {
			if(*decl->getVariable() == *target) {
				location = static_address_cast<const Variable>(decl.getAddressOfChild(0));
				return true;
			}
			return false;
		});
		return location;
	}

	bool visitLambda(const CallExprAddress& call) {

	}

	const VariableAddress& getLocation() { return location; }
};
} // end anonymous namespace

VariablePtr makeAvailable(NodeManager& manager, const VariablePtr& var, const NodeAddress& location, NodePtr& outNewRoot) {
	// find variable address
	VariableAddress va;
	NodeAddress lastAddress;
	auto varFinder = makeLambdaVisitor([&](const NodeAddress& node) -> bool {
		VariableSearchVisitor searcher(var, lastAddress.getAddressedNode());
		searcher.visit(node);
		va = searcher.getLocation();
		// if found, interrupt
		if(va) return true;
		lastAddress = node;
		return false; // continue search
	});
	visitPathBottomUpInterruptible(location, varFinder);
	if(!va) {
		LOG(WARNING) << "transform::makeAvailable variable address not found";
		return VariablePtr();
	}
	// forward variable through calls
	return makeAvailable(manager, va, location, outNewRoot);
}

VariablePtr makeAvailable(NodeManager& manager, const VariableAddress& var, const NodeAddress& location, NodePtr& outNewRoot) {

}

} // end namespace transform
} // end namespace core
} // end namespace insieme
