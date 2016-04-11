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

#include "insieme/core/analysis/type_utils.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/analysis/compare.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/types/subtype_constraints.h"

#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/name_mangling.h"

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

	VariadicTypeVariableSet getVariadicTypeVariablesBoundBy(const FunctionTypePtr& funType) {
		// collect all free type variables int he parameters
		VariadicTypeVariableSet res;

		// collect all type variables
		for (const auto& paramType : funType->getParameterTypes()) {
			visitDepthFirstOnce(paramType, [&](const VariadicTypeVariablePtr& var) {
				res.insert(var);
			});
		}

		// done
		return res;
	}

	TypesPtr normalizeGenericTypeVarialbeParameters(const TypesPtr& params) {
		// some preparation
		NodeManager& mgr = params->getNodeManager();
		auto blankName = StringValue::get(mgr, "_");
		auto blankType = TypeVariable::get(mgr, blankName);

		TypeList res = params->getTypes();
		for (TypePtr& cur : res) {

			if (cur.isa<VariadicTypeVariablePtr>()) {

				cur = VariadicTypeVariable::get(mgr, blankName);

			} else if (const GenericTypeVariablePtr& var = cur.isa<GenericTypeVariablePtr>()) {

				auto tmp = normalize(var);
				cur = GenericTypeVariable::get(mgr, blankName, tmp->getTypeParameter());

			} else if (const VariadicGenericTypeVariablePtr& var = cur.isa<VariadicGenericTypeVariablePtr>()) {

				auto tmp = normalize(var);
				cur = VariadicGenericTypeVariable::get(mgr, blankName, tmp->getTypeParameter());

			} else {
				cur = blankType;
			}
		}

		// build normalized type list
		return Types::get(mgr, res);
	}

	bool isReturnTypePotentiallyDeducible(const FunctionTypePtr& funType) {
		auto retTypeVars = getFreeTypeVariables(funType->getReturnType());
		if(retTypeVars.empty()) return true;
		for(const auto& param : funType.getParameterTypeList()) {
			auto paramTypeVars = getFreeTypeVariables(param);
			for(const auto& t : paramTypeVars) {
				retTypeVars.erase(t);
			}
		}
		return retTypeVars.empty();
	}

	GenericTypeVariablePtr normalize(const GenericTypeVariablePtr& var) {
		if (!var) return var;
		return GenericTypeVariable::get(var->getNodeManager(), var->getVarName(), normalizeGenericTypeVarialbeParameters(var->getTypeParameter()));
	}

	VariadicGenericTypeVariablePtr normalize(const VariadicGenericTypeVariablePtr& var) {
		if (!var) return var;
		return VariadicGenericTypeVariable::get(var->getNodeManager(), var->getVarName(), normalizeGenericTypeVarialbeParameters(var->getTypeParameter()));
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
		if(!ttype) {
			if (core::lang::isArray(type)) {
				// in case of an array, check the enclosed type for triviality
				return isTrivial(core::lang::ArrayType(type).getElementType());
			}
			// non-tag-type & non-array types are always trivial
			return true;
		}

		auto record = ttype->getRecord();

		IRBuilder builder(type->getNodeManager());

		auto containsCtor = [&](const LambdaExprPtr& ctor)->bool {
			return any(record->getConstructors(), [&](const ExpressionPtr& cur) {
				return analysis::equalNameless(builder.normalize(ctor), builder.normalize(cur));
			});
		};

		auto containsMemberFunction = [&](const MemberFunctionPtr& member)->bool {
			return any(record->getMemberFunctions(), [&](const MemberFunctionPtr& cur) {
				return analysis::equalNameless(builder.normalize(member), builder.normalize(cur));
			});
		};

		auto thisType = builder.refType(builder.tagTypeReference(record->getName()));
		ParentsPtr parents =
				(record.isa<StructPtr>()) ?
				record.as<StructPtr>()->getParents() :
				builder.parents();

		// check that there are the right number of constructors
		if (record->getConstructors().size() != 3) return false;

		// and there is a non-virtual destructor
		if (record->hasVirtualDestructor()) return false;

		// check for trivial constructors
		bool trivialDefaultConstructor = containsCtor(builder.getDefaultConstructor(thisType, parents, record->getFields()));
		if (!trivialDefaultConstructor) return false;

		bool trivialCopyConstructor = containsCtor(builder.getDefaultCopyConstructor(thisType, parents, record->getFields()));
		if (!trivialCopyConstructor) return false;

		bool trivialMoveConstructor = containsCtor(builder.getDefaultMoveConstructor(thisType, parents, record->getFields()));
		if (!trivialMoveConstructor) return false;

		// check for trivial, non-virtual destructor
		if (!hasDefaultDestructor(ttype)) return false;

		// check for trivial copy and move assignments
		bool trivialCopyAssignment = containsMemberFunction(builder.getDefaultCopyAssignOperator(thisType, parents, record->getFields()));
		if (!trivialCopyAssignment) return false;

		bool trivialMoveAssignment = containsMemberFunction(builder.getDefaultMoveAssignOperator(thisType, parents, record->getFields()));
		if (!trivialMoveAssignment) return false;

		// check for virtual member functions
		for(auto memFun : record->getMemberFunctions()) {
			if(memFun->getVirtualFlag().getValue()) return false;
		}

		if(!record->getPureVirtualMemberFunctions().empty()) return false;

		// check for virtual & non-trivial base classes
		if(ttype->isStruct()) {
			auto stype = ttype->getStruct();
			for(auto par : stype->getParents()) {
				if(par->getVirtual().getValue()) return false;
				// if our direct base class is non-trivial, we cannot be trivial per-se
				if(!isTrivial(par->getType())) return false;
			}
		}

		// check that all non-static members are trivial
		for(auto field : record->getFields()) {
			auto fieldType = field->getType();
			if(!isTrivial(fieldType)) return false;
			//check cpp_ref field types
			if(analysis::isRefType(fieldType) && lang::isCppReference(fieldType)) {
				//TODO this is an over approximation which has to be refined
				return false;
			}
		}

		return true;
	}

	boost::optional<ExpressionPtr> hasConstructorOfType(const TagTypePtr& type, const FunctionTypePtr& funType) {
		auto record = type->getRecord();
		for(const auto& cur : record->getConstructors()) {
			if(*cur->getType() == *funType) return cur;
		}
		return {};
	}

	boost::optional<ExpressionPtr> hasConstructorAccepting(const TypePtr& type, const TypePtr& paramType) {

		// check that the given type is a tag type
		auto tagType = type.isa<TagTypePtr>();
		if(!tagType) return {};

		// unpeel
		auto canonicalTagType = analysis::getCanonicalType(type).as<TagTypePtr>();
		auto adjustedParam = tagType->unpeel(paramType);

		// search for constructor
		IRBuilder builder(type->getNodeManager());
		auto thisType = builder.refType(tagType->getTag());
		auto ctorType = builder.functionType(TypeList{ thisType, adjustedParam }, thisType, FK_CONSTRUCTOR);
		return hasConstructorOfType(canonicalTagType, ctorType);
	}

	bool hasMemberOfType(const TagTypePtr& type, const std::string& name, const FunctionTypePtr& funType) {
		auto record = type->getRecord();
		for (const auto& cur : record->getMemberFunctions()) {
			if (*funType == *cur->getImplementation()->getType() && cur->getNameAsString() == name) return true;
		}
		for (const auto& cur : record->getPureVirtualMemberFunctions()) {
			if (*funType == *cur->getType() && cur->getNameAsString() == name) return true;
		}
		return false;
	}

	bool hasDefaultConstructor(const TagTypePtr& type) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto thisType = builder.refType(type->getTag());
		auto ctorType = builder.functionType(TypeList{ thisType }, thisType, FK_CONSTRUCTOR);
		return hasConstructorOfType(type, ctorType);
	}

	bool hasCopyConstructor(const TagTypePtr& type) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto otherType = builder.refType(type->getTag(), true, false, lang::ReferenceType::Kind::CppReference);
		return hasConstructorAccepting(type, otherType);
	}

	bool hasMoveConstructor(const TagTypePtr& type) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto otherType = builder.refType(type->getTag(), false, false, lang::ReferenceType::Kind::CppRValueReference);
		return hasConstructorAccepting(type, otherType);
	}

	bool hasCopyAssignment(const TagTypePtr& type) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto thisType = builder.refType(type->getTag());
		auto otherType = builder.refType(type->getTag(), true, false, lang::ReferenceType::Kind::CppReference);
		auto resType = builder.refType(type->getTag(), false, false, lang::ReferenceType::Kind::CppReference);
		auto funType = builder.functionType(TypeList{ thisType, otherType }, resType, FK_MEMBER_FUNCTION);
		return hasMemberOfType(type, utils::getMangledOperatorAssignName(), funType);
	}

	bool hasMoveAssignment(const TagTypePtr& type) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto thisType = builder.refType(type->getTag());
		auto otherType = builder.refType(type->getTag(), false, false, lang::ReferenceType::Kind::CppRValueReference);
		auto resType = builder.refType(type->getTag(), false, false, lang::ReferenceType::Kind::CppReference);
		auto funType = builder.functionType(TypeList{ thisType, otherType }, resType, FK_MEMBER_FUNCTION);
		return hasMemberOfType(type, utils::getMangledOperatorAssignName(), funType);
	}

	bool isaDefaultConstructor(const TagTypePtr& type, const ExpressionPtr& ctor) {
		auto record = type->getRecord();
		IRBuilder builder(record->getNodeManager());
		auto thisType = builder.refType(builder.tagTypeReference(record->getName()));

		auto checkCtor = [&](const ExpressionPtr& ctor, const ExpressionPtr& candidate)->bool {
			return analysis::equalNameless(ctor, builder.normalize(candidate)) ||
				analysis::equalNameless(ctor, builder.normalize(type->peel(candidate)));
		};

		ParentsPtr parents =
				(record.isa<StructPtr>()) ?
				record.as<StructPtr>()->getParents() :
				builder.parents();

		//compare with all three default generated constructors
		auto norm_ctor = builder.normalize(ctor);
		if (checkCtor(norm_ctor, builder.getDefaultConstructor(thisType, parents, record->getFields()))) return true;
		if (checkCtor(norm_ctor, builder.getDefaultCopyConstructor(thisType, parents, record->getFields()))) return true;
		if (checkCtor(norm_ctor, builder.getDefaultMoveConstructor(thisType, parents, record->getFields()))) return true;

		return false;
	}

	bool isDefaultDestructor(const TagTypePtr& type, const ExpressionPtr& dtor) {
		auto record = type->getRecord();
		IRBuilder builder(type->getNodeManager());
		auto thisType = builder.refType(builder.tagTypeReference(record->getName()));

		auto checkDtor = [&](const ExpressionPtr& dtor, const ExpressionPtr& candidate)->bool {
			return analysis::equalNameless(dtor, builder.normalize(candidate)) ||
				analysis::equalNameless(dtor, builder.normalize(type->peel(candidate)));
		};

		ParentsPtr parents =
			(record.isa<StructPtr>()) ?
			record.as<StructPtr>()->getParents() :
			builder.parents();

		//compare with all three default generated constructors
		auto norm_dtor = builder.normalize(dtor);
		return checkDtor(norm_dtor, builder.getDefaultDestructor(thisType));
	}

	bool hasDefaultDestructor(const TagTypePtr& type) {
		auto record = type->getRecord();
		IRBuilder builder(type->getNodeManager());
		auto thisType = builder.refType(builder.tagTypeReference(record->getName()));

		//check the virtual flag and compare with the default generated destructor
		if(!record->getDestructorVirtual().getValue()
				&& analysis::equalNameless(builder.normalize(record->getDestructor()), builder.normalize(builder.getDefaultDestructor(thisType)))) return true;

		return false;
	}

	bool isaDefaultMember(const TagTypePtr& type, const MemberFunctionPtr& memberFunction) {
		//only assignment operators can be default member functions
		if (memberFunction->getNameAsString() != utils::getMangledOperatorAssignName()) return false;

		auto record = type->getRecord();
		IRBuilder builder(type->getNodeManager());
		auto thisType = builder.refType(builder.tagTypeReference(record->getName()));

		auto checkMemberFunction = [&](const MemberFunctionPtr& member, const MemberFunctionPtr& candidate)->bool {
			return analysis::equalNameless(member, builder.normalize(candidate)) ||
					analysis::equalNameless(member, builder.normalize(type->peel(candidate)));
		};

		ParentsPtr parents =
				(record.isa<StructPtr>()) ?
				record.as<StructPtr>()->getParents() :
				builder.parents();

		//compare with both default generated assignment operators
		auto norm_member = builder.normalize(memberFunction);
		if (checkMemberFunction(norm_member, builder.getDefaultCopyAssignOperator(thisType, parents, record->getFields()))) return true;
		if (checkMemberFunction(norm_member, builder.getDefaultMoveAssignOperator(thisType, parents, record->getFields()))) return true;

		return false;
	}

	std::string getTypeName(const TypePtr& objTy) {
		if(auto gt = objTy.isa<GenericTypePtr>()) return toString(*gt);
		if(auto tt = objTy.isa<TagTypePtr>()) return tt->getName()->getValue();
		if(auto tt = objTy.isa<TagTypeReferencePtr>()) return tt->getName()->getValue();
		assert_fail() << "Could not retrieve object type name for:\n" << *objTy;
		return "NEVERMORE";
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
