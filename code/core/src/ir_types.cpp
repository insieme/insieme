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

#include "insieme/core/ir_types.h"


namespace insieme {
namespace core {
namespace new_core {


	std::ostream& GenericType::printTo(std::ostream& out) const {
		// create output buffer
		out << *getName();

		// check whether there are type parameters
		auto typeParam = getTypeParameter();
		auto intParam = getIntTypeParameter();
		if (!typeParam->empty() || !intParam->empty()) {
			// print parameters ...
			out << '<';
			out << join(",", typeParam->getChildList(), print<deref<NodePtr>>());
			if (!typeParam->empty() && !intParam->empty()) {
				out << ',';
			}
			out << join(",", intParam->getChildList(), print<deref<NodePtr>>());
			out << '>';
		}
		return out;
	}


	std::ostream& FunctionType::printTo(std::ostream& out) const {
		out << "(";
		// add parameters
		out << "(" << join(",", getParameterTypes()->getChildList(), print<deref<NodePtr>>()) << ")";

		// add result
		return out << ((isPlain())?"->":"=>") << *getReturnType() << ")";
	}



	namespace {

		class RecTypeUnroller : public NodeMapping {

			NodeManager& manager;
			RecTypeDefinitionPtr definition;


		public:

			RecTypeUnroller(NodeManager& manager, const RecTypeDefinition& definition)
				: manager(manager), definition(&definition) { }

			virtual const NodePtr mapElement(unsigned, const NodePtr& ptr) {
				// check whether it is a known variable
				if (ptr->getNodeType() == NT_TypeVariable) {
					TypeVariablePtr var = static_pointer_cast<const TypeVariable>(ptr);

					// if there is a definition for the current variable ..
					if (definition->getDefinitionOf(var)) {
						// .. unroll the definition
						return RecType::get(manager, var, definition);
					}
				}

				// replace recursively
				return ptr->substitute(manager, *this);
			}

			TypePtr apply(const TypePtr& node) {
				return static_pointer_cast<const Type>(node->substitute(manager, *this));
			}

		};

	}

	TypePtr RecTypeDefinition::unrollOnce(NodeManager& manager, const TypeVariablePtr& variable) const {
		// unroll recursive type using helper
		return RecTypeUnroller(manager, *this).apply(getDefinitionOf(variable));
	}

	namespace {

		struct name_extractor {
			typedef const string& result_type;
			result_type operator()(const NamedTypePtr& name) const {
				return name->getName()->getValue();
			}
		};

		void checkForNameCollisions(const vector<NamedTypePtr>& elements) {

			// get projection to the name
			auto start = boost::make_transform_iterator(elements.begin(), name_extractor());
			auto end = boost::make_transform_iterator(elements.end(), name_extractor());

			if (hasDuplicates(start, end)) { // nice way using projections => but crashes in GCC
				throw std::invalid_argument("No duplicates within identifiers are allowed!");
			}
		}
	}


	NamedCompositeType::NamedCompositeType(const NodeType& type, const NodeList& elements)
		: Type(type, elements) {
		checkChildList(elements);
		checkForNameCollisions(convertList<NamedType>(elements));
	}

	NamedCompositeType::NamedCompositeType(const NodeType& type, const vector<NamedTypePtr>& elements)
		: Type(type, convertList(elements)) {

		checkChildList(convertList(elements));
		checkForNameCollisions(elements);
	}



} // end namespace new_core
} // end namespace core
} // end namespace insieme

