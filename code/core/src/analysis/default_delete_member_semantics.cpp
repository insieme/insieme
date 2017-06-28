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

#include "insieme/core/analysis/default_delete_member_semantics.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/default_delete.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/name_mangling.h"


namespace insieme {
namespace core {
namespace analysis {

	namespace {
		MemberProperties getMemberPropertiesForConstructor(IRBuilder& builder, const ExpressionPtr& constructor) {
			if(auto literal = constructor.isa<LiteralPtr>()) {
				return { literal, nullptr, nullptr };
			} else if(auto lambda = constructor.isa<LambdaExprPtr>()) {
				return { builder.getLiteralForConstructor(lambda->getType()), lambda, nullptr };
			}
			assert_fail() << "Don't know how to convert constructor expression " << dumpReadable(constructor) << " to MemberProperties";
			return {};
		}

		MemberProperties getMemberPropertiesForDestructor(IRBuilder& builder, const ExpressionPtr& destructor) {
			if(auto literal = destructor.isa<LiteralPtr>()) {
				return { literal, nullptr, nullptr };
			} else if(auto lambda = destructor.isa<LambdaExprPtr>()) {
				return { builder.getLiteralForDestructor(lambda->getType()), lambda, nullptr };
			}
			assert_fail() << "Don't know how to convert destructor expression " << dumpReadable(destructor) << " to MemberProperties";
			return {};
		}

		MemberProperties getMemberPropertiesForMemberFunction(IRBuilder& builder, const MemberFunctionPtr& memberFunction) {
			auto name = memberFunction->getNameAsString();
			const auto& implementation = memberFunction->getImplementation();
			if(auto literal = implementation.isa<LiteralPtr>()) {
				return { literal, nullptr, memberFunction };
			} else if(auto lambda = implementation.isa<LambdaExprPtr>()) {
				auto literal = builder.getLiteralForMemberFunction(lambda->getType(), name);
				return { literal, lambda, builder.memberFunction(memberFunction->isVirtual(), name, literal) };
			}
			assert_fail() << "Don't know how to convert memberFunction expression " << dumpReadable(implementation) << " to MemberProperties";
			return {};
		}
	}

	CppDefaultDeleteMembers::CppDefaultDeleteMembers(NodeManager& mgr, const ExpressionList& constructors, const ExpressionPtr& destructor,
	                                                 const MemberFunctionList& memberFunctions) : CppDefaultDeleteMembers() {
		IRBuilder builder(mgr);
		::for_each(constructors, [&](const auto& constructor) { this->constructors.push_back(getMemberPropertiesForConstructor(builder, constructor)); });
		if(destructor) { this->destructor = getMemberPropertiesForDestructor(builder, destructor); }
		::for_each(memberFunctions, [&](const auto& memberFunction) { this->memberFunctions.push_back(getMemberPropertiesForMemberFunction(builder, memberFunction)); });
	}

	ExpressionList CppDefaultDeleteMembers::getConstructorLiteralList() {
		return ::transform(constructors, [](const MemberProperties& ctor) -> core::ExpressionPtr {
			return ctor.literal;
		});
	}

	ExpressionPtr CppDefaultDeleteMembers::getDestructorLiteral() {
		if(destructor) {
			return destructor->literal;
		}
		return {};
	}

	MemberFunctionList CppDefaultDeleteMembers::getMemberFunctionList() {
		return ::transform(memberFunctions, [](const MemberProperties& mfun) {
			return mfun.memberFunction;
		});
	}

	CppDefaultDeleteMembers applyCppDefaultDeleteSemantics(const GenericTypePtr& thisType, const ParentsPtr& parents, const FieldsPtr& fields,
	                                                       const CppDefaultDeleteMembers& inputMembers) {

		assert_true(analysis::isRefType(thisType)) << "thisType has to be a ref type";
		auto builder = core::IRBuilder(thisType->getNodeManager());

		// lambda to check for the presence of a specific constructor type
		auto getCtorByType = [&](const TypePtr& ctorType) -> MemberProperties {
			for(const auto& cur : inputMembers.constructors) {
				if(cur.literal->getType() == ctorType) {
					return cur;
				}
			}
			return {};
		};
		// lambda to check for the presence of a specific member function (with the same name and type as the passed one)
		auto getAssignmentOperatorByType = [&](const TypePtr& operatorType) -> MemberProperties {
			for(const auto& cur : inputMembers.memberFunctions) {
				if(cur.memberFunction->getName()->getValue() == utils::getMangledOperatorAssignName() && cur.literal->getType() == operatorType) {
					return cur;
				}
			}
			return {};
		};

		// the types of the default constructs
		auto defaultConstructorType = builder.getDefaultConstructorType(thisType);
		auto copyConstructorType = builder.getDefaultCopyConstructorType(thisType);
		auto moveConstructorType = builder.getDefaultMoveConstructorType(thisType);
		auto copyAssignmentType = builder.getDefaultCopyAssignOperatorType(thisType);
		auto moveAssignmentType = builder.getDefaultMoveAssignOperatorType(thisType);

		// pointers to them if they are present
		auto providedDefaultConstructor = getCtorByType(defaultConstructorType);
		auto providedCopyConstructor = getCtorByType(copyConstructorType);
		auto providedMoveConstructor = getCtorByType(moveConstructorType);
		auto providedDestructor = inputMembers.destructor.get_value_or({});
		auto providedCopyAssignment = getAssignmentOperatorByType(copyAssignmentType);
		auto providedMoveAssignment = getAssignmentOperatorByType(moveAssignmentType);

		// boolean flags to check for them
		bool hasDefaultConstructor = providedDefaultConstructor.literal;
		bool hasCopyConstructor = providedCopyConstructor.literal;
		bool hasMoveConstructor = providedMoveConstructor.literal;
		bool hasDestructor = providedDestructor.literal;
		bool hasCopyAssignment = providedCopyAssignment.literal;
		bool hasMoveAssignment = providedMoveAssignment.literal;

		// all the default constructs
		auto defaultDefaultConstructor = getMemberPropertiesForConstructor(builder, builder.getDefaultConstructor(thisType, parents, fields));
		auto defaultCopyConstructor = getMemberPropertiesForConstructor(builder, builder.getDefaultCopyConstructor(thisType, parents, fields));
		auto defaultMoveConstructor = getMemberPropertiesForConstructor(builder, builder.getDefaultMoveConstructor(thisType, parents, fields));
		auto defaultDestructor = getMemberPropertiesForDestructor(builder, builder.getDefaultDestructor(thisType));
		auto defaultCopyAssignment = getMemberPropertiesForMemberFunction(builder, builder.getDefaultCopyAssignOperator(thisType, parents, fields));
		auto defaultMoveAssignment = getMemberPropertiesForMemberFunction(builder, builder.getDefaultMoveAssignOperator(thisType, parents, fields));

		// the resulting object with the final members
		CppDefaultDeleteMembers res(inputMembers);

		// first we get rid of all the methods which should be deleted
		res.constructors.erase(std::remove_if(res.constructors.begin(), res.constructors.end(), [](const MemberProperties& member) {
			return annotations::isMarkedDeletedPreTU(member.literal);
		}), res.constructors.end());
		if(hasDestructor && annotations::isMarkedDeletedPreTU(providedDestructor.literal)) {
			res.destructor.reset();
		}
		res.memberFunctions.erase(std::remove_if(res.memberFunctions.begin(), res.memberFunctions.end(), [](const MemberProperties& member) {
			return annotations::isMarkedDeletedPreTU(member.literal);
		}), res.memberFunctions.end());

		// now we replace all constructs which the user defaulted with the generated default versions
		if(hasDefaultConstructor && annotations::isMarkedDefaultedPreTU(providedDefaultConstructor.literal)) {
			std::replace(res.constructors.begin(), res.constructors.end(), providedDefaultConstructor, defaultDefaultConstructor);
		}
		if(hasCopyConstructor && annotations::isMarkedDefaultedPreTU(providedCopyConstructor.literal)) {
			std::replace(res.constructors.begin(), res.constructors.end(), providedCopyConstructor, defaultCopyConstructor);
		}
		if(hasMoveConstructor && annotations::isMarkedDefaultedPreTU(providedMoveConstructor.literal)) {
			std::replace(res.constructors.begin(), res.constructors.end(), providedMoveConstructor, defaultMoveConstructor);
		}
		if(hasDestructor && annotations::isMarkedDefaultedPreTU(providedDestructor.literal)) {
			res.destructor = defaultDestructor;
		}
		if(hasCopyAssignment && annotations::isMarkedDefaultedPreTU(providedCopyAssignment.literal)) {
			std::replace(res.memberFunctions.begin(), res.memberFunctions.end(), providedCopyAssignment, defaultCopyAssignment);
		}
		if(hasMoveAssignment && annotations::isMarkedDefaultedPreTU(providedMoveAssignment.literal)) {
			std::replace(res.memberFunctions.begin(), res.memberFunctions.end(), providedMoveAssignment, defaultMoveAssignment);
		}

		// now we finally add members to the record, according to what C++ semantics require based on what the user specified
		// we add them one by one according to the conditions which tell us to do so. A summary image of these conditions can be seen at
		// https://stackoverflow.com/questions/4943958/conditions-for-automatic-generation-of-default-copy-move-ctor-and-copy-move-assi

		bool hasNoMoveOperation = !hasMoveConstructor && !hasMoveAssignment;
		bool hasNoCopyOrMoveOperation = !hasCopyConstructor && !hasCopyAssignment && !hasMoveConstructor && !hasMoveAssignment;

		// only if the user didn't provide any constructor (not even defaulted or deleted ones), we create a default constructor
		if(inputMembers.constructors.size() == 0) {
			res.constructors.push_back(defaultDefaultConstructor);
		}

		// a destructor is added if the user didn't specify one
		if(!hasDestructor) {
			res.destructor = defaultDestructor;
		}

		// the copy constructor is added if we didn't have one, and the user didn't specify any move operations
		if(!hasCopyConstructor && hasNoMoveOperation) {
			res.constructors.push_back(defaultCopyConstructor);
		}

		// the copy assignment is added if we didn't have one, and the user didn't specify any move operations
		if(!hasCopyAssignment && hasNoMoveOperation) {
			res.memberFunctions.push_back(defaultCopyAssignment);
		}

		// the move constructor is added if we didn't have one, and the user didn't specify a destructor or any copy and move operations
		if(!hasMoveConstructor && !hasDestructor && hasNoCopyOrMoveOperation) {
			res.constructors.push_back(defaultMoveConstructor);
		}

		// the move assignment is added if we didn't have one, and the user didn't specify a destructor or any copy and move operations
		if(!hasMoveAssignment && !hasDestructor && hasNoCopyOrMoveOperation) {
			res.memberFunctions.push_back(defaultMoveAssignment);
		}

		return res;
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme
