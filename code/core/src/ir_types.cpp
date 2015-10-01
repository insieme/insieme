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

#include "insieme/core/ir_types.h"

#include <set>

#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/reference.h"

namespace insieme {
namespace core {


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


	std::ostream& FunctionType::printTo(std::ostream& out) const {
		// fetch object type if required
		TypePtr objType;
		if(isConstructor() || isDestructor() || isMemberFunction() || isVirtualMemberFunction()) {
			if(getParameterTypes().empty() || !lang::isReference(getParameterTypes()[0])) {
				objType = GenericType::get(getNodeManager(), "%error%");
			} else {
				objType = getObjectType();
			}
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

	std::ostream& Struct::printTo(std::ostream& out) const {
		out << "struct";
		if(!getName()->getValue().empty()) {
			out << " " << *getName();
			if(getParents()->empty()) { out << " "; }
		}
		if(!getParents()->empty()) { out << " : [" << join(", ", getParents(), print<deref<ParentPtr>>()) << "] "; }
		return out << "<" << join(",", getFields(), print<deref<NodePtr>>()) << ">";
	}


	namespace {

		class TagTypePeeler : public transform::CachedNodeMapping {
			NodeManager& manager;
			TagTypeDefinitionPtr definition;


		  public:
			TagTypePeeler(NodeManager& manager, const TagTypeDefinition& definition) : manager(manager), definition(&definition) {}

			virtual const NodePtr resolveElement(const NodePtr& ptr) {
				// check whether it is a known variable
				if(auto tag = ptr.isa<TagTypeReferencePtr>()) {

					// if there is a definition for the current variable ..
					if(definition->getDefinitionOf(tag)) {
						// .. unroll the definition
						return TagType::get(manager, tag, definition);
					}
				}

				// check whether current node is a nested expression
				if(ptr.isa<ExpressionPtr>()) {
					return ptr; // do not decent into this
				}

				// replace recursively
				auto res = ptr->substitute(manager, *this);

				// migrate annotations
				transform::utils::migrateAnnotations(ptr, res);

				// done
				return res;
			}

			RecordPtr apply(const RecordPtr& node) {
				return static_pointer_cast<const Record>(node->substitute(manager, *this));
			}
		};
	}

	TagTypePtr TagTypeDefinition::peelDefinitionOnce(NodeManager& manager, const TagTypeReferencePtr& tag) const {
		// peel tag type using helper
		return TagType::get(manager, tag,
				TagTypeDefinition::get(manager, toVector(
						TagTypeBinding::get(manager, tag, TagTypePeeler(manager, *this).apply(getDefinitionOf(tag)))
				)));
	}

	namespace {

		template<template<typename T> class Ptr, typename Set>
		void appendFreeTagTypeReferences(const Ptr<const Node>& root, Set& set) {
			// collect free tag type references
			if (auto tag = root.template isa<Ptr<const TagTypeReference>>()) {
				set.insert(tag);
				return;
			}

			// prune search descent
			if (root.template isa<Ptr<const Expression>>()) return;

			// skip tag type variable bindings
			if (auto tagType = root.template isa<Ptr<const TagType>>()) {
				appendFreeTagTypeReferences(Ptr<const Node>(tagType->getDefinition()), set);
				return;
			}

			// handle nested definitions
			if (auto def = root.template isa<Ptr<const TagTypeDefinition>>()) {

				Set nested;
				for(const auto& cur : def) {
					appendFreeTagTypeReferences(Ptr<const Node>(cur->getRecord()),nested);
				}

				for(const auto& cur : def) {
					nested.erase(cur->getTag());
				}

				// add nested tags to
				set.insert(nested.begin(), nested.end());

				// done
				return;
			}

			// descent recursively
			for(const auto& cur : root.getChildList()) {
				appendFreeTagTypeReferences(cur, set);
			}
		}

		template<template<typename T> class Ptr>
		std::set<Ptr<const TagTypeReference>> getFreeTagTypeReferences(const Ptr<const Node>& root) {
			std::set<Ptr<const TagTypeReference>> res;
			appendFreeTagTypeReferences(root, res);
			return res;
		}

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
			auto set = getFreeTagTypeReferences(cur->getRecord().as<NodePtr>());
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
		return get(manager, name, parents, fields,
				Expressions::get(manager, ExpressionList()),
				IRBuilder(manager).getDefaultDestructor(),
				MemberFunctions::get(manager, MemberFunctionList()),
				PureVirtualMemberFunctions::get(manager, PureVirtualMemberFunctionList())
			);
	}

	UnionPtr Union::get(NodeManager & manager, const StringValuePtr& name, const FieldsPtr& fields) {
		return get(manager, name, fields,
				Expressions::get(manager, ExpressionList()),
				IRBuilder(manager).getDefaultDestructor(),
				MemberFunctions::get(manager, MemberFunctionList()),
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
