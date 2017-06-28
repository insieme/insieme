/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/annotations/naming.h"
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
				if(cur->getNodeCategory() == NC_Statement || cur->getNodeCategory() == NC_Expression) return false;
				auto ret = any(cur.getChildList(), [&](const NodePtr& cur) -> bool { return this->visit(cur, knownVariables); });
				return ret;
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

	namespace {
		bool isTrivialOrTriviallyCopyable(const TypePtr& type, const bool checkTrivial) {
			auto ttype = type.isa<TagTypePtr>();
		if(!ttype) {
			if(core::lang::isArray(type)) {
				// in case of an array, check the enclosed type for the same properties
				return isTrivialOrTriviallyCopyable(core::lang::ArrayType(type).getElementType(), checkTrivial);
			}
			// non-tag-type & non-array types are always trivial
			return true;
		}

		auto record = ttype->getRecord();

		IRBuilder builder(type->getNodeManager());

		auto getCtorByType = [&](const FunctionTypePtr& ctorType) -> ExpressionPtr {
			for(const auto& ctor : record->getConstructors()) {
				if(ctor->getType() == ctorType) {
					return ctor;
				}
			}
			return {};
		};

		auto getAssignmentOperatorByType = [&](const FunctionTypePtr& mfunType) -> MemberFunctionPtr {
			for(const auto& mfun : record->getMemberFunctions()) {
				if(mfun->getNameAsString() == utils::getMangledOperatorAssignName() && mfun->getImplementation()->getType() == mfunType) {
					return mfun;
				}
			}
			return {};
		};

		auto thisType = builder.refType(builder.tagTypeReference(record->getName()));

		// a virtual destructor means this type is not trivially copyable
		if(record->hasVirtualDestructor()) return false;

		// if we should check for a trivial type, we also check for a trivial default constructor here
		if(checkTrivial) {
			// the defult constructor must not be deleted and be either implicit or defaulted)
			auto defaultConstructor = getCtorByType(builder.getDefaultConstructorType(thisType));
			if(!defaultConstructor || !isaDefaultMember(defaultConstructor)) return false;
		}

		// check for trivial constructors. They have to be either deleted or (implicitly) defaulted
		auto copyConstructor = getCtorByType(builder.getDefaultCopyConstructorType(thisType));
		if(copyConstructor && !isaDefaultMember(copyConstructor)) return false;

		auto moveConstructor = getCtorByType(builder.getDefaultMoveConstructorType(thisType));
		if(moveConstructor && !isaDefaultMember(moveConstructor)) return false;

		// check trivial destructor
		auto destructor = record->getDestructor();
		if(destructor && !isaDefaultMember(destructor)) return false;

		// check for trivial copy and move assignments
		auto copyAssignment = getAssignmentOperatorByType(builder.getDefaultCopyAssignOperatorType(thisType));
		if(copyAssignment && !isaDefaultMember(copyAssignment)) return false;

		auto moveAssignment = getAssignmentOperatorByType(builder.getDefaultMoveAssignOperatorType(thisType));
		if(moveAssignment && !isaDefaultMember(moveAssignment)) return false;

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
	}

	/**
	 * The only trivially copyable types are scalar types, trivially copyable classes, and arrays of such types/classes.
	 *
	 * Requirements for trivially copyable classes are:
	 * - Every copy constructor is trivial or deleted
	 * - Every move constructor is trivial or deleted
	 * - Every copy assignment operator is trivial or deleted
	 * - Every move assignment operator is trivial or deleted
	 * - Trivial non-deleted destructor
	 *
	 * This implies that the class has no virtual functions or virtual base classes.
	 *
	 * As an example what trivial means for these default constructs:
	 *
	 * The copy constructor for class T is trivial if all of the following are true:
	 *
	 * it is not user-provided (that is, it is implicitly-defined or defaulted), and if it is defaulted, its signature is the same as implicitly-defined (until C++14);
	 * T has no virtual member functions;
	 * T has no virtual base classes;
	 * the copy constructor selected for every direct base of T is trivial;
	 * the copy constructor selected for every non-static class type (or array of class type) member of T is trivial;
	 *
	 * A trivial copy constructor creates a bytewise copy of the object representation of the argument, and performs no other action.
	 * TriviallyCopyable objects can be copied by copying their object representations manually, e.g. with std::memmove. All data types compatible
	 * with the C language (POD types) are trivially copyable.
	 */
	/// TODO: add trivially copyable marker for tagtypes
	bool isTriviallyCopyable(const TypePtr& type) {
		return isTrivialOrTriviallyCopyable(type, false);
	}

	/// TODO: add trivial marker for tagtypes
	bool isTrivial(const TypePtr& type) {
		return isTrivialOrTriviallyCopyable(type, true);
	}

	boost::optional<ExpressionPtr> hasConstructorOfType(const TagTypePtr& type, const FunctionTypePtr& funType) {
		auto record = type->getRecord();
		for(const auto& cur : record->getConstructors()) {
			if(*cur->getType() == *funType) return cur;
		}
		return {};
	}

	boost::optional<ExpressionPtr> hasConstructorAccepting(const TypePtr& type, const TypePtr& paramType) {

		// check if the given type is an intercepted type
		// -> if so, assume everything is in order
		// (this is unsafe, but the only way to avoid it is to only partially intercept types, which is very challenging to implement for templates)
		auto genType = type.isa<GenericTypePtr>();
		if(genType && core::annotations::hasAttachedName(genType)) {
			return ExpressionPtr();
		}

		// check that the given type is a tag type
		auto tagType = type.isa<TagTypePtr>();
		if(!tagType) {
			return {};
		}

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

	bool isaDefaultConstructor(const ExpressionPtr& ctor) {
		if (auto lambda = ctor.isa<LambdaExprPtr>()) {
			return lambda->getType().as<FunctionTypePtr>().isConstructor() && isaDefaultMember(lambda);
		}
		return false;
	}

	bool isDefaultDestructor(const ExpressionPtr& dtor) {
		if (auto lambda = dtor.isa<LambdaExprPtr>()) {
			return lambda->getType().as<FunctionTypePtr>().isDestructor() && isaDefaultMember(lambda);
		}
		return false;
	}

	bool hasDefaultDestructor(const TagTypePtr& type) {
		auto record = type->getRecord();
		return hasDefaultDestructor(record);
	}

	bool hasDefaultDestructor(const RecordPtr& record) {
		return record->hasDestructor() && isDefaultDestructor(record->getDestructor());
	}

	bool isaDefaultMember(const MemberFunctionPtr& memberFunction) {
		//only assignment operators can be default member functions
		if (memberFunction->getNameAsString() != utils::getMangledOperatorAssignName()) return false;

		const auto& impl = memberFunction->getImplementation();
		if(auto lambda = impl.isa<LambdaExprPtr>()) {
			return lambda->getType().as<FunctionTypePtr>().isMemberFunction() && isaDefaultMember(lambda);
		}
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
