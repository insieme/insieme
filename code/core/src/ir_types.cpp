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

#include "insieme/core/ir_types.h"

#include <set>

#include "insieme/core/analysis/type_utils.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/reference.h"

#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace core {

	bool detail::semanticNodeLessThan(const core::NodePtr& a, const core::NodePtr& b) {
		if (a->getNodeType() != b->getNodeType()) {
			return a->getNodeType() > b->getNodeType();
		}

		if(a->getNodeType() == NT_MemberFunction && b->getNodeType() == NT_MemberFunction) {
			auto aMem = a.as<MemberFunctionPtr>();
			auto bMem = b.as<MemberFunctionPtr>();
			if(aMem->getNameAsString() == bMem->getNameAsString()) {
				return semanticNodeLessThan(aMem->getImplementation(), bMem->getImplementation());
			}
			return aMem.getNameAsString() < bMem.getNameAsString();
		}

		TypePtr aT, bT;
		if(a->getNodeCategory() == NC_Expression && b->getNodeCategory() == NC_Expression) {
			aT = a.as<ExpressionPtr>()->getType();
			bT = b.as<ExpressionPtr>()->getType();
		}
		if(a->getNodeCategory() == NC_Type && b->getNodeCategory() == NC_Type) {
			aT = a.as<TypePtr>();
			bT = b.as<TypePtr>();
		}

		if(aT && bT && aT->getNodeType() == NT_FunctionType && bT->getNodeType() == NT_FunctionType) {
			auto aFT = aT.as<FunctionTypePtr>();
			auto bFT = bT.as<FunctionTypePtr>();
			auto aParams = aFT.getParameterTypeList();
			auto bParams = bFT.getParameterTypeList();
			if(aParams.size() != bParams.size()) return aParams.size() < bParams.size();
			for(const auto& pair : make_paired_range(aParams, bParams)) {
				if(*pair.first != *pair.second) return semanticNodeLessThan(pair.first, pair.second);
			}
		}

		if(aT && bT && aT->getNodeType() == NT_GenericType && bT->getNodeType() == NT_GenericType) {
			if(lang::isReference(aT) && lang::isReference(bT)) {
				auto aRefT = lang::ReferenceType(aT);
				auto bRefT = lang::ReferenceType(bT);
				if(aRefT.getElementType() != bRefT.getElementType()) return semanticNodeLessThan(aRefT.getElementType(), bRefT.getElementType());
				if(aRefT.getKind() != bRefT.getKind()) return aRefT.getKind() < bRefT.getKind();
			}
			auto aGT = aT.as<GenericTypePtr>();
			auto bGT = bT.as<GenericTypePtr>();
			if(aGT->getName() != bGT->getName()) return aGT.getName()->getValue() < bGT.getName()->getValue();
			auto aTypeParameters = aGT->getTypeParameterList();
			auto bTypeParameters = bGT->getTypeParameterList();
			if(aTypeParameters.size() != bTypeParameters.size()) return aTypeParameters.size() < bTypeParameters.size();
			for(const auto& pair : make_paired_range(aTypeParameters, bTypeParameters)) {
				if(*pair.first != *pair.second) return semanticNodeLessThan(pair.first, pair.second);
			}
		}

		// fallback
		return !(*a < *b);
	}


	std::ostream& GenericType::printTo(std::ostream& out) const {
		// create output buffer
		out << *getName();

		// check whether there are parent types
		if(!getParents().empty()) {
			// print parents list
			out << ":" << *getParents();
		}

		// check whether there are type parameters
		auto typeParam = getTypeParameter();
		if(!typeParam->empty()) {
			// print parameters ...
			out << '<' << join(",", typeParam->getChildList(), print<deref<NodePtr>>()) << '>';
		}
		return out;
	}

	std::ostream& GenericTypeVariable::printTo(std::ostream& out) const {
		// create output buffer
		out << "'" << *getVarName();

		// print parameters (or empty <> if none)
		return out << '<' << join(",", getTypeParameter()->getChildList(), print<deref<NodePtr>>()) << '>';
	}

	std::ostream& VariadicGenericTypeVariable::printTo(std::ostream& out) const {
		// create output buffer
		out << "'" << *getVarName() << "...";

		// print parameters (or empty <> if none)
		return out << '<' << join(",", getTypeParameter()->getChildList(), print<deref<NodePtr>>()) << '>';
	}

	std::ostream& FunctionType::printTo(std::ostream& out) const {
		// fetch object type if required
		TypePtr objType;
		if(isConstructor() || isDestructor() || isMemberFunction() || isVirtualMemberFunction()) {
			if(getParameterTypes().empty() || !lang::isReference(getParameterTypes()[0])) {
				objType = GenericType::get(getNodeManager(), "%error%");
			} else {
				FunctionTypePtr ft = this;
				objType = analysis::getObjectType(ft);
			}
		}

		// handle instantiation types
		if(hasInstantiationTypes()) {
			out << "<" << join(",", getInstantiationTypeList(), print<deref<NodePtr>>()) << ">";
		}

		// handle constructors
		if(isConstructor()) {
			auto paramBegin = getParameterTypes().begin() + 1;
			auto paramEnd = getParameterTypes().end();
			return out << "(" << *objType << "::(" << join(",", paramBegin, paramEnd, print<deref<NodePtr>>()) << "))";
		}

		// handle destructor
		if(isDestructor()) {
			assert_eq(1u, getParameterTypes().size());
			return out << "(~" << *objType << "::())";
		}

		// handle (virtual) member function types
		if(isMemberFunction() || isVirtualMemberFunction()) {
			auto paramBegin = getParameterTypes().begin() + 1;
			auto paramEnd = getParameterTypes().end();
			return out << "(" << *objType << "::(" << join(",", paramBegin, paramEnd, print<deref<NodePtr>>()) << ")"
					<< (isMemberFunction() ? "->" : "~>") << *getReturnType() << ")";
		}

		out << "(";
		// add parameters
		out << "(" << join(",", getParameterTypes()->getChildList(), print<deref<NodePtr>>()) << ")";

		// add result
		return out << ((isPlain()) ? "->" : "=>") << *getReturnType() << ")";
	}

	std::ostream& TagTypeBinding::printTo(std::ostream & out) const {
		return out << *getTag() << "=" << *getRecord();
	}

	namespace {

		std::ostream& printRecordTypeContents(const RecordPtr& record, std::ostream& out) {
			bool first = true;
			auto printSeparator = [&]() { out << (first ? "" : ","); first = false; };

			out << "{";

			//print fields
			if (record->getFields().size() > 0) { first = false; }
			out << join(",", record->getFields(), print<deref<NodePtr>>());

			//print constructor types
			for (const auto& constructor : record->getConstructors()) {
				printSeparator();
				const auto& parameterTypes = constructor.getType().as<FunctionTypePtr>()->getParameterTypes();
				assert_ge(parameterTypes->size(), 1) << "Invalid constructor, no parameters (should at least have this)";
				out << "ctor(" << join(",", ++parameterTypes.begin(), parameterTypes.end(), print<deref<NodePtr>>()) << ")";
			}

			//print destructor type
			printSeparator();
			out << "dtor()";

			//print member function types
			for (const auto& memberFunction : record->getMemberFunctions()) {
				printSeparator();
				const auto& implementationType = memberFunction->getImplementation().getType().as<FunctionTypePtr>();
				const auto& parameterTypes = implementationType->getParameterTypes();
				assert_ge(parameterTypes->size(), 1) << "Invalid method, no parameters (should at least have this)";
				out << *memberFunction->getName() << "(" << join(",", ++parameterTypes.begin(), parameterTypes.end(), print<deref<NodePtr>>()) << ")->"
				    << *implementationType->getReturnType();
			}

			//print pure virtual member function types
			for (const auto& memberFunction : record->getPureVirtualMemberFunctions()) {
				printSeparator();
				const auto& implementationType = memberFunction->getType();
				const auto& parameterTypes = implementationType->getParameterTypes();
				assert_ge(parameterTypes->size(), 1) << "Invalid pure virtual method, no parameters (should at least have this)";
				out << "pure virtual " << *memberFunction->getName() << "("
						<< join(",", ++parameterTypes.begin(), parameterTypes.end(), print<deref<NodePtr>>()) << ")->" << *implementationType->getReturnType();
			}

			return out << "}";
		}

	}

	std::ostream& Struct::printTo(std::ostream& out) const {
		out << "struct";
		if(!getName()->getValue().empty()) {
			out << " " << *getName();
		}
		if(!getParents()->empty()) {
			out << " : [" << join(", ", getParents(), print<deref<ParentPtr>>()) << "]";
		}
		out << " ";

		return printRecordTypeContents(this, out);
	}

	std::ostream& Union::printTo(std::ostream& out) const {
		out << "union";
		if(!getName()->getValue().empty()) {
			out << " " << *getName();
		}
		out << " ";

		return printRecordTypeContents(this, out);
	}

	namespace {

		template<template<typename T> class Ptr, typename Set>
		void appendFreeTagTypeReferences(const Ptr<const Node>& root, Set& set, bool fieldsOnly, const TagTypeReferencePtr& origin) {
			// collect free tag type references
			if (auto tag = root.template isa<Ptr<const TagTypeReference>>()) {
				set.insert(tag);
				return;
			}

			// handle nested expressions
			if (auto expr = root.template isa<Ptr<const Expression>>()) {
				// stop here if only fields should be covered
				if (fieldsOnly) return;

				// extract nested references
				if (origin) {
					// filter out origin from references in the expressions
					Set nested;
					appendFreeTagTypeReferences(Ptr<const Node>(expr), nested, false, TagTypeReferencePtr());

					// filter nested elements to exclude origin
					for(const auto& cur : nested) {
						if (*cur != *origin) set.insert(cur);
					}

					// done
					return;
				}
			}

			// skip tags in type variable bindings
			if (auto tagType = root.template isa<Ptr<const TagType>>()) {
				// if there are free tag type references ...
				if (analysis::hasFreeTagTypeReferences(tagType)) {
					// ... collect those too
					appendFreeTagTypeReferences(Ptr<const Node>(tagType->getDefinition()), set, fieldsOnly, origin);
				}
				return;
			}

			// handle nested definitions
			if (auto def = root.template isa<Ptr<const TagTypeDefinition>>()) {

				Set nested;
				for(const auto& cur : def) {
					appendFreeTagTypeReferences(Ptr<const Node>(cur->getRecord()),nested, fieldsOnly, origin);
				}

				Set filtered;
				for(const auto& cur : nested) {
					if (!def->getDefinitionOf(cur)) filtered.insert(cur);
				}

				// add nested tags to
				set.insert(filtered.begin(), filtered.end());

				// done
				return;
			}

			// descent recursively
			for(const auto& cur : root.getChildList()) {
				appendFreeTagTypeReferences(cur, set, fieldsOnly, origin);
			}
		}

		/**
		 * Collects all free tag-type references in the given code fragment by excluding the given origin-reference.
		 */
		template<template<typename T> class Ptr>
		std::set<Ptr<const TagTypeReference>> getFreeTagTypeReferences(const Ptr<const Node>& root, const TagTypeReferencePtr& origin = TagTypeReferencePtr()) {

			std::set<Ptr<const TagTypeReference>> res;
			appendFreeTagTypeReferences(root, res, false, origin);
			return res;

		}

		template<template<typename T> class Ptr>
		std::set<Ptr<const TagTypeReference>> getFreeTagTypeReferencesInFields(const Ptr<const Node>& root) {
			std::set<Ptr<const TagTypeReference>> res;
			appendFreeTagTypeReferences(root, res, true, TagTypeReferencePtr());
			return res;
		}

	}

	TagTypeDefinitionPtr TagTypeDefinition::get(NodeManager& manager, const TagTypeBindingMap& bindings) {
		vector<TagTypeBindingPtr> tagTypeBindings;
		for(auto p : bindings) {
			tagTypeBindings.push_back(TagTypeBinding::get(manager, p.first, p.second));
		}
		std::sort(tagTypeBindings.begin(), tagTypeBindings.end(), [](const TagTypeBindingPtr& a, const TagTypeBindingPtr& b) {
			return a->getTag()->getName()->getValue() < b->getTag()->getName()->getValue();
		});
		return manager.get(TagTypeDefinition(convertList(tagTypeBindings)));
	}

	const vector<TagTypeReferenceAddress>& TagTypeDefinition::getRecursiveReferences() const {

		// the annotation to store the reference list
		struct reference_list {
			vector<TagTypeReferenceAddress> list;
			bool operator==(const reference_list& other) const {
				return list == other.list;
			}
		};

		// check the attached list
		if (hasAttachedValue<reference_list>()) {
			return getAttachedValue<reference_list>().list;
		}

		// compute references
		vector<TagTypeReferenceAddress> res;
		for (const auto& binding : TagTypeDefinitionAddress(TagTypeDefinitionPtr(this))) {
			for (const auto& cur : getFreeTagTypeReferences(NodeAddress(binding->getRecord()), TagTypeReferencePtr())) {
				assert_eq(cur.getRootNode().ptr, this) << cur << " = " << *cur << " is not rooted by this definition!\n" << *this;
				res.push_back(cur);
			}
		}

		// attach the result
		attachValue(reference_list{ res });
		return getRecursiveReferences();
	}

	const vector<TagTypeReferenceAddress>& TagTypeDefinition::getRecursiveReferencesOf(const TagTypeReferencePtr& reference) const {
		static const vector<TagTypeReferenceAddress> empty;

		auto def = getDefinitionOf(reference);
		if (!def) return empty;

		// the annotation to store the reference list
		struct reference_list {
			utils::map::PointerMap<TagTypeReferencePtr,vector<TagTypeReferenceAddress>> lists;
			bool operator==(const reference_list& other) const {
				return lists == other.lists;
			}
		};

		// create annotation if necessary
		if (!hasAttachedValue<reference_list>()) {
			attachValue<reference_list>();
		}

		// get lists
		auto& lists = const_cast<reference_list&>(getAttachedValue<reference_list>()).lists;

		// find entry
		auto pos = lists.find(reference);
		if (pos != lists.end()) return pos->second;

		// compute list
		vector<TagTypeReferenceAddress>& res = lists[reference];
		for (const auto& cur : getRecursiveReferences()) {
			assert_eq(cur.getRootNode().ptr,this);
			if (*cur == *reference) res.push_back(cur);
		}

		// done
		return res;
	}



	TagTypePtr TagTypeDefinition::peelDefinition(NodeManager& manager, const TagTypeReferencePtr& tag, unsigned times) const {


		// start with unmodified definition
		TagTypeDefinitionPtr definition(this);

		// if peeling factor is 0, we are done
		if (times == 0) {
			return TagType::get(manager, tag, definition);
		}

		// get list of free tag type references
		auto tags = getFreeTagTypeReferences(NodeAddress(getDefinitionOf(tag)), tag);

		// if there are recursive references
		if (!tags.empty()) {

			// peel recursive definitions
			std::map<TagTypeReferencePtr, TagTypePtr> peeled;
			for (const auto& cur : tags) {
				auto curPtr = cur.as<TagTypeReferencePtr>();
				auto& peeledCur = peeled[curPtr];
				if (!peeledCur) {
					peeledCur = TagType::get(manager, curPtr, TagTypeDefinitionPtr(this))->peel(times - 1);
				}
			}

			// turn into a map of modifications
			std::map<NodeAddress,NodePtr> mods;
			for(const auto& cur : tags) {
				mods[cur] = peeled[cur.as<TagTypeReferencePtr>()];
			}

			// build peeled definition
			TagTypeBindingMap tm = { { tag, transform::replaceAll(manager, mods).as<RecordPtr>() } };
			definition = TagTypeDefinition::get(manager, tm);
		}

		// peel tag type using helper
		return TagType::get(manager, tag, definition);
	}

	NodePtr TagTypeDefinition::peelMember(NodeManager& manager, const NodePtr& member) const {

		// collect free tag type references in the given member
		auto positions = getFreeTagTypeReferences(NodeAddress(member));

		// skip if there is nothing to do
		if (positions.empty()) return member;

		// create replacement map
		std::map<NodeAddress, NodePtr> replacements;
		for(const auto& cur : positions) {
			replacements[cur] = TagType::get(manager, cur, this);
		}

		// conduct replacement
		return transform::replaceAll(manager, replacements);
	}

	TypePtr TagTypeDefinition::unpeel(NodeManager& mgr, const TypePtr& input) const {
		IRBuilder builder(mgr);
		// get canonical Tag Type (tag is irrelevant) and input
		TagTypePtr canonicalTT = builder.tagType(getDefinitions().front().getTag(), TagTypeDefinitionPtr(this));
		TagTypeDefinitionPtr ttDef = canonicalTT->getDefinition();
		TypePtr adjustedParam = analysis::getCanonicalType(input);

		// create replacement map and apply
		NodeMap replacements;
		for(const auto& binding : ttDef) {
			replacements[builder.tagType(binding->getTag(), ttDef)] = binding->getTag();
		}
		return transform::replaceAllGen(mgr, adjustedParam, replacements, transform::globalReplacement);
	}

	bool TagType::isRecursive() const {
		// the marker type to annotate the result of the is-recursive check
		struct RecursiveTypeMarker {
			bool value;
			bool operator==(const RecursiveTypeMarker& other) const {
				return value == other.value;
			}
		};

		// check potential annotation
		if (this->hasAttachedValue<RecursiveTypeMarker>()) {
			return this->getAttachedValue<RecursiveTypeMarker>().value;
		}

		bool res = false;
		for(const auto& cur : this->getDefinition()) {
			auto set = getFreeTagTypeReferencesInFields(cur->getRecord().as<NodePtr>());
			auto pos = set.find(getTag());
			if (pos != set.end()) {
				res = true;
				break;
			}
		}

		// attach result to node
		this->attachValue((RecursiveTypeMarker){res});

		// return result
		return res;
	}

	std::ostream& TagType::printTo(std::ostream & out) const {
		if (!isRecursive()) { return out << *getRecord(); }
		return out << "rec " << *getTag() << "." << *getDefinition();
	}

	StructPtr Struct::get(NodeManager & manager, const StringValuePtr& name, const ParentsPtr& parents, const FieldsPtr& fields) {
		IRBuilder builder(manager);
		auto thisType = builder.refType(builder.tagTypeReference(name));
		return get(manager, name, parents, fields,
				Expressions::get(manager, toVector<ExpressionPtr>(
						builder.getDefaultConstructor(thisType, parents, fields),
						builder.getDefaultCopyConstructor(thisType, parents, fields),
						builder.getDefaultMoveConstructor(thisType, parents, fields)
				)),
				builder.getDefaultDestructor(thisType),
				BoolValue::get(manager, false),
				MemberFunctions::get(manager, toVector<MemberFunctionPtr>(
						builder.getDefaultCopyAssignOperator(thisType, parents, fields),
						builder.getDefaultMoveAssignOperator(thisType, parents, fields)
				)),
				PureVirtualMemberFunctions::get(manager, PureVirtualMemberFunctionList())
			);
	}

	UnionPtr Union::get(NodeManager & manager, const StringValuePtr& name, const FieldsPtr& fields) {
		IRBuilder builder(manager);
		auto thisType = builder.refType(builder.tagTypeReference(name));
		auto parents = builder.parents(ParentList());
		return get(manager, name, fields,
				Expressions::get(manager, toVector<ExpressionPtr>(
						builder.getDefaultConstructor(thisType, parents, fields),
						builder.getDefaultCopyConstructor(thisType, parents, fields),
						builder.getDefaultMoveConstructor(thisType, parents, fields)
				)),
				builder.getDefaultDestructor(thisType),
				BoolValue::get(manager, false),
				MemberFunctions::get(manager, toVector<MemberFunctionPtr>(
						builder.getDefaultCopyAssignOperator(thisType, parents, fields),
						builder.getDefaultMoveAssignOperator(thisType, parents, fields)
				)),
				PureVirtualMemberFunctions::get(manager, PureVirtualMemberFunctionList())
			);
	}


	std::ostream& NumericType::printTo(std::ostream& out) const {
		return out << *getValue();
	}

	NumericTypePtr NumericType::get(NodeManager& manager, const LiteralPtr& value) {
		return manager.get(NumericType(value));
	}

	NumericTypePtr NumericType::get(NodeManager& manager, const VariablePtr& var) {
		return manager.get(NumericType(var));
	}

} // end namespace core
} // end namespace insieme
