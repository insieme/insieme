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

#include "insieme/core/ir_class_info.h"

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/dump/annotations.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/encoder/tuples.h"

#include "insieme/core/transform/node_replacer.h"

namespace insieme {
namespace core {

	using core::printer::PrettyPrinter;

	// --------- Member Function --------------------

	std::ostream& MemberFunction::printTo(std::ostream& out) const {
		// print information regarding this member function
		if (isVirtual()) out << "virtual ";
		if (isConst()) out << "const ";
		out << name << " = ";
		out << PrettyPrinter(lambda, PrettyPrinter::NO_LET_BINDINGS);
		return out;
	}


	// --------- Class Meta-Info --------------------

	void ClassMetaInfo::setConstructors(const vector<LambdaExprPtr>& constructors) {
		// check new constructors
		assert(all(constructors, [&](const LambdaExprPtr& cur) { return cur->getFunctionType()->isConstructor(); }));
		assert(all(constructors, [&](const LambdaExprPtr& cur) { return checkObjectType(cur); }));

		// exchange the list of constructors
		this->constructors = constructors;

		// invalidate child list
		childList.reset();
	}

	void ClassMetaInfo::addConstructor(const LambdaExprPtr& constructor) {
		// check constructor type
		assert(constructor->getFunctionType()->isConstructor());
		assert(checkObjectType(constructor));

		// add new constructor
		this->constructors.push_back(constructor);

		// invalidate child list
		childList.reset();
	}

	void ClassMetaInfo::setDestructor(const LambdaExprPtr& destructor) {
		// check destructor type
		assert(!destructor || destructor->getFunctionType()->isDestructor());
		assert(!destructor || checkObjectType(destructor));

		// update destructor
		this->destructor = destructor;

		// invalidate child list
		childList.reset();
	}

	void ClassMetaInfo::setMemberFunctions(const vector<MemberFunction>& functions) {
		// check new functions
		assert(all(functions, [&](const MemberFunction& cur) { return cur.getImplementation()->getFunctionType()->isMemberFunction(); }));
		assert(all(functions, [&](const MemberFunction& cur) { return checkObjectType(cur.getImplementation()); }));

		// exchange the list of member functions
		this->memberFunctions.clear();
		for(auto cur : functions) {
			addMemberFunction(cur);
		}

		// invalidate child list
		childList.reset();
	}

	void ClassMetaInfo::addMemberFunction(const MemberFunction& function) {
		// check member function type
		assert(function.getImplementation()->getFunctionType()->isMemberFunction());
		assert(checkObjectType(function.getImplementation()));

		// create the index key for this function
		auto key = std::make_tuple(function.getName(), function.getImplementation()->getFunctionType(), function.isConst());

		// check that there are not duplicates
		assert(memberFunctionIndex.find(key) == memberFunctionIndex.end() &&
				"Member functions may not exhibit the same name, type and const-flag state.");

		// add new member function
		this->memberFunctions.push_back(function);

		// add to index
		memberFunctionIndex[key] = &memberFunctions.back();

		// invalidate child list
		childList.reset();
	}

	TypePtr ClassMetaInfo::getClassType() const {

		// try destructor
		if (destructor) return destructor->getLambda()->getType()->getObjectType();

		// try constructors
		if (!constructors.empty()) return constructors.front()->getLambda()->getType()->getObjectType();

		// try member functions
		if (!memberFunctions.empty()) return memberFunctions.front().getImplementation()->getLambda()->getType()->getObjectType();

		// type is unknown
		return TypePtr();
	}

	bool ClassMetaInfo::checkObjectType(const LambdaExprPtr& lambda) const {

		FunctionTypePtr funType = lambda->getFunctionType();
		assert(funType->isConstructor() || funType->isDestructor() || funType->isMemberFunction());

		TypePtr classType = getClassType();
		if (!classType) return true; // everything is allowed if object type is not fixed yet

		// check object type
		return *classType == *funType->getObjectType();
	}

	std::ostream& ClassMetaInfo::printTo(std::ostream& out) const {

		out << "class-info <\n";

		// add ctor code
		if (!constructors.empty()) {
			out << " -- Constructors --\n" << join("\n", constructors, [](std::ostream& out, const LambdaExprPtr& cur) {
				out << PrettyPrinter(cur, PrettyPrinter::NO_LET_BINDINGS);
			}) << "\n\n";
		}

		// add dtor
		if (destructor) {
			out << " -- ";
			if (virtualDestructor) out << "virtual ";
			out << "Destructor --\n";
			out << PrettyPrinter(destructor, PrettyPrinter::NO_LET_BINDINGS);
			out << "\n\n";
		}

		// add member functions
		if (!memberFunctions.empty()) {
			out << " -- Member Functions --\n" << join("\n", memberFunctions) << "\n";
		}
		out << ">\n";

		return out;
	}

	bool ClassMetaInfo::migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const {
		assert(before != after);
		assert(before.isa<TypePtr>());

		// check whether new version is still an object type
		if (after->getNodeCategory() != NC_Type || !analysis::isObjectType(after.as<TypePtr>())) {
			return false;	// can not migrate this information to such a type
		}

		TypePtr oldClassType = before.as<TypePtr>();
		TypePtr newClassType = after.as<TypePtr>();

		NodeManager& mgr = after.getNodeManager();
		IRBuilder builder(mgr);
		VariablePtr newParam = builder.variable(builder.refType(newClassType));

		// create helper for updating functions
		auto alter = [&](const LambdaExprPtr& in)->LambdaExprPtr {
			assert(!in->getParameterList().empty());

			VariableMap replacement;
			replacement[in->getParameterList()->getElement(0)] = newParam;

			// update first parameter => switch to new variable
			return core::transform::replaceVarsRecursiveGen(mgr, in, replacement);
		};

		// create a copy of this class meta info and replace parameters
		ClassMetaInfo res;

		// move constructors
		for(auto cur : constructors) {
			res.addConstructor(alter(cur));
		}

		// move destructor
		if (destructor) {
			res.setDestructor(alter(destructor));
		}

		// update virtual destructor field
		res.setDestructorVirtual(isDestructorVirtual());

		// update members
		for(auto cur : memberFunctions) {
			MemberFunction newMember = cur;
			newMember.setImplementation(alter(newMember.getImplementation()));
			res.addMemberFunction(newMember);
		}

		// attach result to modified node
		setMetaInfo(newClassType, res);
		return true;
	}


	void ClassMetaInfo::cloneTo(const NodePtr& target) const {

		// create a copy of this class referencing instances managed by the new target node manager
		NodeManager& newMgr = target->getNodeManager();

		ClassMetaInfo newInfo = *this;

		// migrate constructors
		for(auto& cur : newInfo.constructors) { cur = newMgr.get(cur); }

		// migrate destructor
		if (newInfo.destructor) newInfo.destructor = newMgr.get(newInfo.destructor);

		// migrate member functions
		for (auto& cur : newInfo.memberFunctions) { cur.setImplementation(newMgr.get(cur.getImplementation())); }

		// attach info value
		target->attachValue(newInfo);

	}

	const NodeList& ClassMetaInfo::getChildNodes() const {

		// use lazy-evaluated value if possible
		if (childList) {
			return childList;
		}

		// update child list
		NodeList res;
		for(auto cur : constructors) res.push_back(cur);
		if (destructor) res.push_back(destructor);
		for(auto cur : memberFunctions) res.push_back(cur.getImplementation());

		// update lazy and return internal value
		return childList = res;
	}

	// --------- Class Meta-Info Dump-Support --------------------

	// create and register a IR-Dump converter for the meta-info class
	VALUE_ANNOTATION_CONVERTER(ClassMetaInfo)

		typedef core::value_node_annotation<ClassMetaInfo>::type annotation_type;

		typedef std::tuple<string, LambdaExprPtr, bool, bool> encoded_member_fun_type;
		typedef std::tuple<vector<LambdaExprPtr>, LambdaExprPtr, bool, vector<encoded_member_fun_type>> encoded_class_info_type;

		virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
			assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only Class-Info annotations are supported!");
			const ClassMetaInfo& info = static_pointer_cast<annotation_type>(annotation)->getValue();

			// convert member functions
			auto encodedMemberFuns = ::transform(info.getMemberFunctions(), [](const MemberFunction& cur)->encoded_member_fun_type {
				return encoded_member_fun_type(
						cur.getName(),
						cur.getImplementation(),
						cur.isVirtual(),
						cur.isConst()
				);
			});

			// encode class meta-info object
			return encoder::toIR(
				manager,
				encoded_class_info_type (
						info.getConstructors(),
						info.getDestructor(),
						info.isDestructorVirtual(),
						encodedMemberFuns
				)
			);
		}

		virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
			assert(encoder::isEncodingOf<encoded_class_info_type>(node.as<ExpressionPtr>()) && "Invalid encoding encountered!");

			// decode the class object
			auto tuple = encoder::toValue<encoded_class_info_type>(node);

			// restore info
			ClassMetaInfo res;

			res.setConstructors(std::get<0>(tuple));
			res.setDestructor(std::get<1>(tuple));
			res.setDestructorVirtual(std::get<2>(tuple));
			res.setMemberFunctions(::transform(std::get<3>(tuple), [](const encoded_member_fun_type& cur) {
				return MemberFunction(std::get<0>(cur), std::get<1>(cur), std::get<2>(cur), std::get<3>(cur));
			}));

			// convert
			return std::make_shared<annotation_type>(res);
		}
	};

	// --------- Class Meta-Info Utilities --------------------

	namespace {

		static ClassMetaInfo defaultInfo;

	}

	const bool hasMetaInfo(const TypePtr& type) {
		return type->hasAttachedValue<ClassMetaInfo>();
	}

	const ClassMetaInfo& getMetaInfo(const TypePtr& type) {
		assert(analysis::isObjectType(type) && "Meta-Information may only be attached to object types!");

		// check whether meta-information is present
		if (!type->hasAttachedValue<ClassMetaInfo>()) {
			// return the default configuration
			return defaultInfo;
		}

		// return a reference to the attached meta-information
		return type->getAttachedValue<ClassMetaInfo>();
	}

	void setMetaInfo(const TypePtr& type, const ClassMetaInfo& info) {
		assert(analysis::isObjectType(type) && "Meta-Information may only be attached to object types!");
		assert(!info.getClassType() || info.getClassType() == type);

		// if information is not different to the default => just drop it
		if (info == defaultInfo) {
			type->detachValue<ClassMetaInfo>();
		} else {
			type->attachValue(info);
		}
	}

} // end namespace core
} // end namespace insieme
