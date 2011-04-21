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

#pragma once

#include "insieme/core/ast_node.h"
#include "insieme/core/type_utils.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {
namespace analysis {

	/**
	 * A variable mapping is a one-to-one mapping between two sets of variables (not necessarily distinct). It is the result produced
	 * by a renaming process. As a substitution, it can be applied on types - in forward and reverse directions.
	 */
	class VariableMapping : public utils::Printable {

		/**
		 * The internal storage for the forward variable mapping. It is a mirrored version of the
		 * backward substitution.
		 */
		Substitution forward;

		/**
		 * The internal storage for the backward variable mapping. It is a mirrored version of the
		 * forward substitution.
		 */
		Substitution backward;

	public:

		/**
		 * Adds a new variable mapping to this instance.
		 */
		void addMapping(const TypeVariablePtr& varA, const TypeVariablePtr& varB) {
			forward.addMapping(varA, varB);
			backward.addMapping(varB, varA);
		}

		/**
		 * Checks whether the given variable has been mapped to another variable before. Only the forward
		 * direction is checked.
		 */
		bool containsMappingFor(const TypeVariablePtr& var) const {
			return forward.containsMappingFor(var);
		}

		/**
		 * Adds a new variable mapping to this instance.
		 */
		void addMapping(const VariableIntTypeParamPtr& varA, const VariableIntTypeParamPtr& varB) {
			forward.addMapping(varA, varB);
			backward.addMapping(varB, varA);
		}

		/**
		 * Checks whether the given variable has been mapped to another variable before. Only the forward
		 * direction is checked.
		 */
		bool containsMappingFor(const VariableIntTypeParamPtr& var) const {
			return forward.containsMappingFor(var);
		}

		/**
		 * Applies the forward substitution represented by this mapping to the given type.
		 */
		TypePtr applyForward(const TypePtr& type) {
			return forward.applyTo(type);
		}

		/**
		 * Applies the reverse substitution represented by this mapping to the given type.
		 */
		TypePtr applyBackward(const TypePtr& type) {
			return backward.applyTo(type);
		}

		/**
		 * Writes a (somehow) readable string representation of this mapping to the given output stream.
		 */
		std::ostream& printTo(std::ostream& out) const {
			out << "{";
			out << join(",", forward.getMapping(),
					[](std::ostream& out, const Substitution::Mapping::value_type& cur) {
						out << *cur.first << "<->" << *cur.second;
			});
			out << "/";
			out << join(",", forward.getIntTypeParamMapping(),
					[](std::ostream& out, const Substitution::IntTypeParamMapping::value_type& cur) {
						out << *cur.first << "<->" << *cur.second;
			});
			out << "}";
			return out;
		}

	};

	/**
	 * A simple utility class for renaming type variable names and int-type-parameter variables within
	 * a type expression.
	 */
	class VariableRenamer {

		/**
		 * A counter used to generate fresh variable names.
		 */
		int varCounter;

		/**
		 * A counter used to generate fresh int-type-param names.
		 */
		int varParamCounter;

	public:

		/**
		 * Creates an new instance of this class producing substitutions using the given offsets.
		 */
		VariableRenamer(int varCounterOffset=0, int varParamCounterOffset=0)
			: varCounter(varCounterOffset), varParamCounter(varParamCounter) { }


		/**
		 * Applies the renaming to the given target type.
		 *
		 * @param target the type which's variables should be renamed
		 * @return type the resulting type with the renamed variable names
		 */
		TypePtr rename(const TypePtr& target) {
			return rename(target->getNodeManager(), target);
		}

		/**
		 * Applies the renaming of variable names and produces the resulting type.
		 *
		 * @param manager the manager to be used for producing the result
		 * @param target the type which's variables should be renamed
		 * @return type the resulting type with the renamed variable names
		 */
		TypePtr rename(NodeManager& manager, const TypePtr& target) {
			return mapVariables(target).applyForward(target);
		}

		/**
		 * Proposes a substitution where all the variables within the given type is
		 * are renamed to new, fresh names. However, it is only guaranteed that the
		 * new names a fresh considering the re-naming proposals previously computed
		 * by the same instance.
		 */
		VariableMapping mapVariables(const TypePtr& target) {
			return mapVariables(target->getNodeManager(), toVector(target));
		}

		/**
		 * A template to be capable of handling containers as inputs.
		 */
		template<class Container>
		VariableMapping mapVariables(NodeManager& manager, const Container& container) {
			return mapVariables(manager, container.begin(), container.end());
		}

		/**
		 * A template to be capable of handling ranges as inputs.
		 */
		template<class Iterator>
		VariableMapping mapVariables(NodeManager& manager, const std::pair<Iterator, Iterator>& range) {
			return mapVariables(manager, range.first, range.second);
		}

		/**
		 * This method is proposing a substitution where all the type and parameter variables within
		 * the given range of types are replaced by a consistent set of new variables.
		 */
		template<class Iterator>
		VariableMapping mapVariables(NodeManager& manager, const Iterator& begin, const Iterator& end) {

			VariableMapping res;

			// create visitor collecting the renaming information
			auto visitor = makeLambdaPtrVisitor([&](const NodePtr& node) {
				NodeType type = node->getNodeType();
				if (type == NT_TypeVariable) {
					const TypeVariablePtr& var = static_pointer_cast<const TypeVariable>(node);
					if (!res.containsMappingFor(var)) {
						res.addMapping(var, getFreshVariable(manager));
					}
				} else if (type == NT_VariableIntTypeParam) {
					const VariableIntTypeParamPtr& var = static_pointer_cast<const VariableIntTypeParam>(node);
					if (!res.containsMappingFor(var)) {
						res.addMapping(var, getFreshParameter(manager));
					}
				}
			}, true);

			// use a visitor to meet all variables and variable int parameters
			std::for_each(begin, end, [&](const TypePtr& cur) {
				visitAllOnce(cur, visitor);
			});

			// return result
			return res;
		}

		/**
		 * Can be used to reset this utility class. After the reset, variable names previously
		 * used as a replacement will be reused.
		 */
		void reset() {
			varCounter = 0;
			varParamCounter = 0;
		}

	private:

		TypeVariablePtr getFreshVariable(NodeManager& manager) {
			return TypeVariable::get(manager, format("v%d", (++varCounter)));
		}

		VariableIntTypeParamPtr getFreshParameter(NodeManager& manager) {
			return VariableIntTypeParam::get(manager, 'a' + (varParamCounter++));
		}

	};


} // end namespace analysis
} // end namespace core
} // end namespace insieme
