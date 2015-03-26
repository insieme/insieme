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
#include "insieme/core/encoder/ir_class_info.h"

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
		out << PrettyPrinter(impl, PrettyPrinter::NO_LET_BINDINGS);
		if (impl.isa<LiteralPtr>()) out << " : " << *impl->getType();
		return out;
	}


	// --------- Class Meta-Info --------------------

	void ClassMetaInfo::setConstructors(const vector<ExpressionPtr>& constructors) {
		assert(all(constructors, [&](const ExpressionPtr& cur) { return cur->getNodeType() == NT_Literal || cur->getNodeType() == NT_LambdaExpr; }));
		// check new constructors
		assert(all(constructors, [&](const ExpressionPtr& cur) { return cur->getType().isa<FunctionTypePtr>() && cur->getType().as<FunctionTypePtr>()->isConstructor(); }));
		assert(all(constructors, [&](const ExpressionPtr& cur) { return checkObjectType(cur); }));

		// exchange the list of constructors
		this->constructors = constructors;

		// normalize constructors
		for(auto& cur : this->constructors) {
			cur = core::analysis::normalize(cur);
		}

		// invalidate child list
		childList.reset();
	}

	void ClassMetaInfo::addConstructor(const ExpressionPtr& constructor) {
		assert(constructor->getNodeType() == NT_Literal || constructor->getNodeType() == NT_LambdaExpr);

		// check constructor type
		assert(constructor->getType().isa<FunctionTypePtr>() && constructor->getType().as<FunctionTypePtr>()->isConstructor());
		assert_true(checkObjectType(constructor));

		// add new constructor
		this->constructors.push_back(core::analysis::normalize(constructor));

		// invalidate child list
		childList.reset();
	}

	void ClassMetaInfo::setDestructor(const ExpressionPtr& destructor) {
		assert_true(!destructor || destructor->getNodeType() == NT_Literal || destructor->getNodeType() == NT_LambdaExpr);

		// check destructor type
		assert_true(!destructor || (destructor->getType().isa<FunctionTypePtr>() && destructor->getType().as<FunctionTypePtr>()->isDestructor()));
		assert(!destructor || checkObjectType(destructor));

		// update destructor
		this->destructor = (destructor)?core::analysis::normalize(destructor):ExpressionPtr();

		// invalidate child list
		childList.reset();
	}

	void ClassMetaInfo::setMemberFunctions(const vector<MemberFunction>& functions) {
		// check new functions
		assert(all(functions, [&](const MemberFunction& cur) { return cur.getImplementation()->getType().as<FunctionTypePtr>()->isMemberFunction(); }));
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
		assert(function.getImplementation()->getType()->getNodeType() == NT_FunctionType);
		assert(function.getImplementation()->getType().as<FunctionTypePtr>()->isMemberFunction());
		assert_true(checkObjectType(function.getImplementation()));

		// if duplicates, retur, but check if we are incorporating a different implementation
		if(hasMemberFunction(function.getName(), function.getImplementation()->getType().as<FunctionTypePtr>(), function.isConst())) {
			assert_eq(*analysis::normalize(getMemberFunction(function.getName(), function.getImplementation()->getType().as<FunctionTypePtr>(), function.isConst())->getImplementation()), 
					  *analysis::normalize(function.getImplementation()))
					<< "Member functions may not exhibit the same name, type and const-flag state. \n" 
					<< "\t name: " << function.getName() << "\n\t"
					<< "\t impl: " << function.getImplementation() << "\n";
			return;
		}

		// add new member function
		this->memberFunctions.push_back(function);

		// invalidate child list
		childList.reset();
	}

	TypePtr ClassMetaInfo::getClassType() const {

		// try destructor
		if (destructor) return destructor->getType().as<FunctionTypePtr>()->getObjectType();

		// try constructors
		if (!constructors.empty()) return constructors.front()->getType().as<FunctionTypePtr>()->getObjectType();

		// try member functions
		if (!memberFunctions.empty()) return memberFunctions.front().getImplementation()->getType().as<FunctionTypePtr>()->getObjectType();

		// type is unknown
		return TypePtr();
	}

	bool ClassMetaInfo::checkObjectType(const ExpressionPtr& lambda) const {

		assert(lambda->getType()->getNodeType() == NT_FunctionType);

		FunctionTypePtr funType = lambda->getType().as<FunctionTypePtr>();
		assert_true(funType->isConstructor() || funType->isDestructor() || funType->isMemberFunction());

		TypePtr typeA = getClassType();
		if (!typeA) return true; // everything is allowed if object type is not fixed yet

		TypePtr typeB = funType->getObjectType();

		// check object type
		assert_eq(*typeA, *typeB);
		return *typeA == *typeB;
	}

	std::ostream& ClassMetaInfo::printTo(std::ostream& out) const {

		out << "class-info <\n";

		// add ctor code
		if (!constructors.empty()) {
			out << " -- Constructors --\n" << join("\n", constructors, [](std::ostream& out, const ExpressionPtr& cur) {
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
		assert(&getMetaInfo(before.as<TypePtr>()) == this);

		// check whether new version is still an object type
		if (after->getNodeCategory() != NC_Type || !analysis::isObjectType(after.as<TypePtr>())) {
			return false;	// can not migrate this information to such a type
		}

		TypePtr oldClassType = before.as<TypePtr>();
		TypePtr newClassType = after.as<TypePtr>();

		assert(*oldClassType != *newClassType);

		NodeManager& mgr = after.getNodeManager();
		IRBuilder builder(mgr);
		VariablePtr newParam = builder.variable(builder.refType(newClassType));

		// create helper for updating functions
		auto alter = [&](const ExpressionPtr& in)->ExpressionPtr {

			// update the object type of functions
			if (auto fun = in.isa<LambdaExprPtr>()) {
				assert_false(fun->getParameterList().empty());

				VariableMap replacement;
				replacement[fun->getParameterList()->getElement(0)] = newParam;

				// update first parameter => switch to new variable
				return core::transform::replaceVarsRecursiveGen(mgr, fun, replacement);
			}

			// update the type of literals
			if (auto lit = in.isa<LiteralPtr>()) {

				// get address to class type node
				auto classType = LiteralAddress(lit)->getType().as<FunctionTypeAddress>()->getObjectType();

				// replace it by new class type
				return core::transform::replaceNode(mgr, classType, newClassType).as<LiteralPtr>();
			}

			assert_fail() << "Unsupported Function node type: " << in->getNodeType() << "\n";
			return in;
		};

		// create a copy of this class meta info and replace parameters
		ClassMetaInfo newInfo;

		// move constructors
		for(auto cur : constructors) {
			
			if (auto lit = cur.isa<LiteralPtr>()) {

				// update function type
				newInfo.addConstructor(core::transform::replaceNode(
						mgr,
						LiteralAddress(lit)->getType().as<FunctionTypeAddress>()->getParameterType(0),
						builder.refType(newClassType)
				).as<LiteralPtr>());

			} else {
				// handle as all other implementations
				assert(cur.isa<LambdaExprPtr>());
				newInfo.addConstructor(alter(cur.as<LambdaExprPtr>()));
			}
		}

		// move destructor
		if (destructor) {
			if (auto lit = destructor.isa<LiteralPtr>()) {
				// update function type
				newInfo.setDestructor(core::transform::replaceNode(
						mgr,
						LiteralAddress(lit)->getType().as<FunctionTypeAddress>()->getParameterType(0),
						builder.refType(newClassType)
				).as<LiteralPtr>());
			} else {
				// handle as all other implementations
				assert(destructor.isa<LambdaExprPtr>());
				newInfo.setDestructor(alter(destructor.as<LambdaExprPtr>()));
			}
		}

		// update virtual destructor field
		newInfo.setDestructorVirtual(isDestructorVirtual());

		// update members
		for(auto cur : memberFunctions) {
			MemberFunction newMember = cur;

			// extract implementation
			auto impl = newMember.getImplementation();

			// update implementation
			if (analysis::isPureVirtual(impl)) {
				// update type
				auto oldFunType = impl->getType().as<FunctionTypePtr>();
				auto newFunType = core::transform::replaceNode(
						mgr,
						FunctionTypeAddress(oldFunType)->getParameterType(0),
						builder.refType(newClassType)
				).as<FunctionTypePtr>();

				// create new pure-virtual implementation
				newMember.setImplementation(builder.getPureVirtual(newFunType));

			} else if (auto lit = impl.isa<LiteralPtr>()) {

				// update function type
				newMember.setImplementation(core::transform::replaceNode(
						mgr,
						LiteralAddress(lit)->getType().as<FunctionTypeAddress>()->getParameterType(0),
						builder.refType(newClassType)
				).as<LiteralPtr>());

			} else {
				// handle as all other implementations
				assert(impl.isa<LambdaExprPtr>());
				newMember.setImplementation(alter(impl.as<LambdaExprPtr>()));
			}

			newInfo.addMemberFunction(newMember);
		}

		// attach result to modified node
		setMetaInfo(newClassType, merge(newInfo, getMetaInfo(newClassType)));
		return true;
	}


	void ClassMetaInfo::cloneTo(const NodePtr& target) const {

		assert(target.isa<TypePtr>());

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
		target->attachValue(merge(newInfo, getMetaInfo(target.as<TypePtr>())));

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

		virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
			assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only Class-Info annotations are supported!");
			return encoder::toIR(manager, static_pointer_cast<annotation_type>(annotation)->getValue());
		}

		virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
			// wrap result into annotation pointer
			return std::make_shared<annotation_type>(encoder::toValue<ClassMetaInfo>(node));
		}
	};

	// --------- Class Meta-Info Utilities --------------------

	namespace {

		static const ClassMetaInfo defaultInfo;

	}

	const bool hasMetaInfo(const TypePtr& type) {
		return type && type->hasAttachedValue<ClassMetaInfo>();
	}

	const ClassMetaInfo& getMetaInfo(const TypePtr& type) {
		assert_true(analysis::isObjectType(type)) << "Meta-Information may only be attached to object types!";

		// check whether meta-information is present
		if (!type->hasAttachedValue<ClassMetaInfo>()) {
			// return the default configuration
			return defaultInfo;
		}

		// return a reference to the attached meta-information
		return type->getAttachedValue<ClassMetaInfo>();
	}

	void setMetaInfo(const TypePtr& type, const ClassMetaInfo& info) {
		assert_true(analysis::isObjectType(type)) << "Meta-Information may only be attached to object types!";
		assert_true(!info.getClassType() || *info.getClassType() == *type)
			<< "Target Type: " << *type << "\n"
			<< "Class Type: " << (info.getClassType() ? toString(*info.getClassType()) : "-unknown-") << "\n";

		// if information is not different to the default => just drop it
		if (info == defaultInfo) {
			type->detachValue<ClassMetaInfo>();
		} else {
			type->attachValue(info);
		}
	}

	void removeMetaInfo(const TypePtr& type) {
		type->detachValue<ClassMetaInfo>();
	}

	ClassMetaInfo merge(const ClassMetaInfo& a, const ClassMetaInfo& b) {

		// see whether one of those is empty
		if (a == defaultInfo) return b;
		if (b == defaultInfo) return a;

		// can only merge meta infos for the same types
		assert_eq(*a.getClassType(), *b.getClassType())
			<< "Cannot merge\n" << a << "and\n" << b;

		// merge them
		ClassMetaInfo res = a;

		// copy constructors
		for(auto cur : b.getConstructors()) {
			//prevent multiple copies of constructors
			if (!res.containsConstructor(cur)) {
				res.addConstructor(cur);
			}
		}

		// a utility function to prevent multiple copies of the same member function
		auto containsMember = [&](const MemberFunction& cur) {
			return contains(res.getMemberFunctions(), cur, [](const MemberFunction& a, const MemberFunction& b)->bool {
				return a.isConst() == b.isConst() && a.getName() == b.getName() && *a.getImplementation()->getType() == *b.getImplementation()->getType();
			});
		};

		// copy member functions
		for(auto cur : b.getMemberFunctions()) {
			if (!containsMember(cur)) {
				res.addMemberFunction(cur);
			}
		}

		// update destructor
		if (res.hasDestructor() && b.hasDestructor()) {
			assert_eq(*analysis::normalize(res.getDestructor()), *analysis::normalize(b.getDestructor()))
					<< "Unable to merge distinct destructors!";
		} else if (!res.hasDestructor() && b.getDestructor()) {
			res.setDestructor(b.getDestructor());
		}

		// update virtual flag
		res.setDestructorVirtual(res.isDestructorVirtual() || b.isDestructorVirtual());

		// done
		return res;
	}

} // end namespace core
} // end namespace insieme
