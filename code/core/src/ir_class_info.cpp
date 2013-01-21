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
	}

	void ClassMetaInfo::addConstructor(const LambdaExprPtr& constructor) {
		// check constructor type
		assert(constructor->getFunctionType()->isConstructor());
		assert(checkObjectType(constructor));

		// add new constructor
		this->constructors.push_back(constructor);
	}

	void ClassMetaInfo::setDestructor(const LambdaExprPtr& destructor) {
		// check destructor type
		assert(!destructor || destructor->getFunctionType()->isDestructor());
		assert(!destructor || checkObjectType(destructor));

		// update destructor
		this->destructor = destructor;
	}

	void ClassMetaInfo::setMemberFunctions(const vector<MemberFunction>& functions) {
		// check new functions
		assert(all(functions, [&](const MemberFunction& cur) { return cur.getLambdaExpr()->getFunctionType()->isMemberFunction(); }));
		assert(all(functions, [&](const MemberFunction& cur) { return checkObjectType(cur.getLambdaExpr()); }));

		// exchange the list of member functions
		this->memberFunctions = functions;
	}

	void ClassMetaInfo::addMemberFunction(const MemberFunction& function) {
		// check member function type
		assert(function.getLambdaExpr()->getFunctionType()->isMemberFunction());
		assert(checkObjectType(function.getLambdaExpr()));

		// add new member function
		this->memberFunctions.push_back(function);
	}

	TypePtr ClassMetaInfo::getClassType() const {

		// try destructor
		if (destructor) return destructor->getLambda()->getType()->getObjectType();

		// try constructors
		if (!constructors.empty()) return constructors.front()->getLambda()->getType()->getObjectType();

		// try member functions
		if (!memberFunctions.empty()) return memberFunctions.front().getLambdaExpr()->getLambda()->getType()->getObjectType();

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

	void ClassMetaInfo::migrateTo(const NodePtr& target) const {

		// create a copy of this class referencing instances managed by the new target node manager
		NodeManager& newMgr = target->getNodeManager();

		ClassMetaInfo newInfo = *this;

		// migrate constructors
		for(auto& cur : newInfo.constructors) { cur = newMgr.get(cur); }

		// migrate destructor
		if (newInfo.destructor) newInfo.destructor = newMgr.get(newInfo.destructor);

		// migrate member functions
		for (auto& cur : newInfo.memberFunctions) { cur.setLambdaExpr(newMgr.get(cur.getLambdaExpr())); }

		// attach info value
		target->attachValue(newInfo);

	}

	// --------- Class Meta-Info Utilities --------------------

	namespace {

		static ClassMetaInfo defaultInfo;

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
