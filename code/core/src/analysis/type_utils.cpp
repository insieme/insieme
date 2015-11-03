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

#include "insieme/core/analysis/type_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/types/subtype_constraints.h"

#include "insieme/utils/assert.h"

namespace insieme {
namespace core {
namespace analysis {

	bool hasFreeTypeVariables(const TypePtr& type) {
		// if it is not a type, there are no type variables
		if(!type) { return false; }

		struct HasFreeTypeVariableVisitor : public IRVisitor<bool, Pointer, NodeSet&> {
			HasFreeTypeVariableVisitor() : IRVisitor<bool, Pointer, NodeSet&>(true) {}

			bool visitTypeVariable(const TypeVariablePtr& cur, NodeSet& knownVariables) {
				return !contains(knownVariables, cur);
			}

			bool visitFunctionType(const FunctionTypePtr& cur, NodeSet& knownVariables) {
				return false; // function types are binding their free type variables
			}

			bool visitNode(const NodePtr& cur, NodeSet& knownVariables) {
				return any(cur.getChildList(), [&](const NodePtr& cur) -> bool { return this->visit(cur, knownVariables); });
			}
		};

		NodeSet tmp;
		return HasFreeTypeVariableVisitor().visit(type, tmp);
	}

	TypeVariableSet getFreeTypeVariables(const TypePtr& type) {
		TypeVariableSet res;

		// collect all nested type variables
		visitDepthFirstOncePrunable(type, [&](const TypePtr& type)->bool {
			// prune function types
			if (type.isa<FunctionTypePtr>()) return true;
			if (auto var = type.isa<TypeVariablePtr>()) res.insert(var);
			// continue searching free variables
			return false;
		});

		return res;
	}

	TypeVariableSet getTypeVariablesBoundBy(const FunctionTypePtr& funType) {
		// collect all free type variables int he parameters
		TypeVariableSet res;

		// collect all type variables
		for(const auto& paramType : funType->getParameterTypes()) {
			visitDepthFirstOnce(paramType, [&](const TypeVariablePtr& var) {
				res.insert(var);
			});
		}

		// done
		return res;
	}

	insieme::core::TypePtr autoReturnType(NodeManager& nodeMan, const CompoundStmtPtr& body) {
		auto debug = false;
		if(debug) { std::cout << "{{{{{{ autoReturnType ----\n"; }

		// find all returns
		TypePtr newReturnType = nodeMan.getLangBasic().getUnit();
		auto returns = analysis::getFreeNodes(body, NT_ReturnStmt, toVector(NT_LambdaExpr, NT_JobExpr, NT_ReturnStmt));
		if(debug) { std::cout << "{{{{{{{{{{{{{ Returns: " << returns << "\n"; }

		// if no returns, unit is fine
		if(!returns.empty()) {
			auto typeList = ::transform(returns, [](const NodePtr& ret) { return ret.as<ReturnStmtPtr>()->getReturnExpr()->getType(); });
			if(debug) { std::cout << "{{{{{{{{{{{{{ typeList: " << typeList << "\n"; }

			newReturnType = types::getSmallestCommonSuperType(typeList);
			if(debug) { std::cout << "{{{{{{{{{{{{{ returnType: " << newReturnType << "\n"; }

			assert_true(newReturnType) << "Return type deduction, multiple return types have no common supertype.";
		}

		return newReturnType;
	}

	/**
	 * This function tests whether the given type is a 'trivial' class/struct type in the C++ interpretation.
	 *
	 * In C++, a trivial class or struct is defined as one that:
	 *
	 * - Has a trivial default constructor. This may use the default constructor syntax (SomeConstructor() = default;).
	 * - Has trivial copy and move constructors, which may use the default syntax.
	 * - Has trivial copy and move assignment operators, which may use the default syntax.
	 * - Has a trivial destructor, which must not be virtual.
	 *
	 * Constructors are trivial only if there are no virtual member functions of the class and no virtual base classes.
	 * Copy/move operations also require that all the non-static data members be trivial.
	 *
	 * The copy assignment operator for class T is trivial if all of the following is true:
	 *
	 * - It is not user-provided (meaning, it is implicitly-defined or defaulted), and if it is defaulted, its signature is the same as implicitly-defined
	 * - T has no virtual member functions
	 * - T has no virtual base classes
	 * - The copy assignment operator selected for every direct base of T is trivial
	 * - The copy assignment operator selected for every non-static class type (or array of class type) member of T is trivial
	 * - T has no non-static data members of volatile-qualified type (since C++14)
	 */
	/// TODO: add trivial marker for tagtypes
	bool isTrivial(const TypePtr& type) {
		auto ttype = type.isa<TagTypePtr>();

		// non-tag-types are always trivial
		if(!ttype) return true;

		auto record = ttype->getRecord();

		IRBuilder builder(type->getNodeManager());

		auto containsCtor = [&](const LambdaExprPtr& ctor)->bool {
			return any(record->getConstructors(), [&](const ExpressionPtr& cur) {
				return *cur == *ctor;
			});
		};

		ParentsPtr parents =
				(record.isa<StructPtr>()) ?
				record.as<StructPtr>()->getParents() :
				builder.parents();

		// check for trivial constructors
		bool trivialDefaultConstructor = containsCtor(builder.getDefaultConstructor(record->getName(), parents, record->getFields()));
		if (!trivialDefaultConstructor) return false;

		bool trivialCopyConstructor = containsCtor(builder.getDefaultCopyConstructor(record->getName(), parents, record->getFields()));
		if (!trivialCopyConstructor) return false;

		bool trivialMoveConstructor = containsCtor(builder.getDefaultMoveConstructor(record->getName(), parents, record->getFields()));
		if (!trivialMoveConstructor) return false;


		auto containsMemberFunction = [&](const MemberFunctionPtr& member)->bool {
			return any(record->getMemberFunctions(), [&](const MemberFunctionPtr& cur) {
				return *cur == *member;
			});
		};

		// check for trivial copy and move assignments
		bool trivialCopyAssignment = containsMemberFunction(builder.getDefaultCopyAssignOperator(record->getName(), parents, record->getFields()));
		if (!trivialCopyAssignment) return false;

		bool trivialMoveAssignment = containsMemberFunction(builder.getDefaultMoveAssignOperator(record->getName(), parents, record->getFields()));
		if (!trivialMoveAssignment) return false;

		// check for trivial, non-virtual destructor
		if(record->getDestructor().as<LambdaExprPtr>()->getBody().size() != 0 || record->getDestructorVirtual().getValue()) return false;

		// check for virtual member functions
		for(auto memFun : record->getMemberFunctions()) {
			if(memFun->getVirtualFlag().getValue()) return false;
		}

		// check for virtual base classes
		if(ttype->isStruct()) {
			auto stype = ttype->getStruct();
			for(auto par : stype->getParents()) {
				if(par->getVirtual().getValue()) return false;
			}
		}

		// check that all non-static members are trivial
		for(auto field : record->getFields()) {
			if(!isTrivial(field->getType())) return false;
		}
		
		return true;
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
