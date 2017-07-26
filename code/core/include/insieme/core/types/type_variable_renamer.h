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
 */

#pragma once

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace core {
namespace types {

	/**
	 * A variable mapping is a one-to-one mapping between two sets of variables (not necessarily distinct). It is the result produced
	 * by a renaming process. As a substitution, it can be applied on types - in forward and reverse directions.
	 */
	class TypeMapping : public utils::Printable {
		/**
		 * The internal storage for the forward variable mapping. It is a mirrored version of the
		 * backward substitution. The element types are NodePtr to allow the structure to be forwarded
		 * to the standard replace function.
		 */
		utils::map::PointerMap<NodePtr, NodePtr> forward;

		/**
		 * The internal storage for the backward variable mapping. It is a mirrored version of the
		 * forward substitution. The element types are NodePtr to allow the structure to be forwarded
		 * to the standard replace function.
		 */
		utils::map::PointerMap<NodePtr, NodePtr> backward;

	  public:
		TypeMapping() {}

		/**
		 * Test whether this mapping is empty.
		 */
		bool empty() const {
			return forward.empty();
		}

		/**
		 * Adds a new type mapping to this instance.
		 */
		void addMapping(const TypePtr& varA, const TypePtr& varB) {
			addMappingInternal(varA, varB);
		}

		/**
		 * Checks whether the given variable has been mapped to another variable before. Only the forward
		 * direction is checked.
		 */
		bool containsMappingFor(const TypePtr& var) const {
			return forward.find(var) != forward.end();
		}

		/**
		 * Applies the forward substitution represented by this mapping to the given type.
		 */
		TypePtr applyForward(const TypePtr& type) const {
			return applyForward(type->getNodeManager(), type);
		}

		/**
		 * Applies the forward substitution on the given list of types.
		 */
		inline TypeList applyForward(NodeManager& manager, const TypeList& list) const {
			TypeList res;
			for(auto it = list.begin(); it != list.end(); ++it) {
				res.push_back(applyForward(manager, *it));
			}
			return res;
		}

		/**
		 * Applies the forward substitution represented by this mapping to the given type using the given manager.
		 */
		TypePtr applyForward(NodeManager& manager, const TypePtr& type) const;

		/**
		 * Applies the reverse substitution represented by this mapping to the given type.
		 */
		TypePtr applyBackward(const TypePtr& type) const {
			return applyBackward(type->getNodeManager(), type);
		}

		/**
		 * Applies the forward substitution on the given list of types.
		 */
		inline TypeList applyBackward(NodeManager& manager, const TypeList& list) const {
			TypeList res;
			for(auto it = list.begin(); it != list.end(); ++it) {
				res.push_back(applyBackward(manager, *it));
			}
			return res;
		}

		/**
		 * Applies the reverse substitution represented by this mapping to the given type using the given manager.
		 */
		TypePtr applyBackward(NodeManager& manager, const TypePtr& type) const;

		/**
		 * Writes a (somehow) readable string representation of this mapping to the given output stream.
		 */
		std::ostream& printTo(std::ostream& out) const {
			out << "{";
			out << join(",", forward, [](std::ostream& out, const std::pair<NodePtr, NodePtr>& cur) { out << *cur.first << "<->" << *cur.second; });
			out << "}";
			return out;
		}

	  private:
		/**
		 * Internal method for adding a pair of nodes to be mapped to each other by this class.
		 */
		void addMappingInternal(const NodePtr& from, const NodePtr& to) {
			forward.insert(std::make_pair(from, to));
			backward.insert(std::make_pair(to, from));
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
		 * Used as default parameter (to allow passing by reference).
		 */
		static const TypeMapping emptyMapping;

	  public:
		/**
		 * A virtual destructor to support sub-classes.
		 */
		virtual ~VariableRenamer(){};

		/**
		 * Creates an new instance of this class producing substitutions using the given offsets.
		 */
		VariableRenamer(int varCounterOffset = 0) : varCounter(varCounterOffset) {}

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
		TypeMapping mapVariables(const TypePtr& target, const TypeMapping& base = emptyMapping) {
			return mapVariables(target->getNodeManager(), toVector(target), base);
		}

		/**
		 * Proposes a substitution where all the variables within the given type is
		 * are renamed to new, fresh names. However, it is only guaranteed that the
		 * new names a fresh considering the re-naming proposals previously computed
		 * by the same instance.
		 */
		TypeMapping mapVariables(NodeManager& manager, const TypePtr& target, const TypeMapping& base = emptyMapping) {
			return mapVariables(manager, toVector(target), base);
		}

		/**
		 * A template to be capable of handling containers as inputs.
		 */
		template <class Container>
		TypeMapping mapVariables(NodeManager& manager, const Container& container, const TypeMapping& base = emptyMapping) {
			return mapVariables(manager, container.begin(), container.end(), base);
		}

		/**
		 * A template to be capable of handling ranges as inputs.
		 */
		template <class Iterator>
		TypeMapping mapVariables(NodeManager& manager, const std::pair<Iterator, Iterator>& range, const TypeMapping& base = emptyMapping) {
			return mapVariables(manager, range.first, range.second, base);
		}

		/**
		 * This method is proposing a substitution where all the type and parameter variables within
		 * the given range of types are replaced by a consistent set of new variables.
		 * If a base mapping is supplied, entries from it are re-used.
		 */
		template <class Iterator>
		TypeMapping mapVariables(NodeManager& manager, const Iterator& begin, const Iterator& end, const TypeMapping& base = emptyMapping) {
			TypeMapping res;

			// TODO: the renaming should happen recursively within every new scope (e.g. a function)

			// create visitor collecting the renaming information
			auto visitor = makeLambdaVisitor([&](const NodePtr& node) {
				NodeType type = node->getNodeType();
				if(type == NT_TypeVariable) {
					const TypeVariablePtr& var = node.as<TypeVariablePtr>();
					if(base.containsMappingFor(var)) {
						res.addMapping(var, base.applyForward(manager, var));
					} else if(!res.containsMappingFor(var)) {
						res.addMapping(var, getFreshVariable(manager));
					}
				}
				if(type == NT_GenericTypeVariable) {
					const GenericTypeVariablePtr& var = node.as<GenericTypeVariablePtr>();
					if(base.containsMappingFor(var)) {
						res.addMapping(var, base.applyForward(manager, var));
					} else if(!res.containsMappingFor(var)) {
						res.addMapping(var, getFreshGenericVariable(manager, var));
					}
				}

			}, true);

			// use a visitor to meet all variables and variable int parameters
			std::for_each(begin, end, [&](const TypePtr& cur) { visitDepthFirstOnce(cur, visitor, true); });

			// return result
			return res;
		}

		/**
		 * Can be used to reset this utility class. After the reset, variable names previously
		 * used as a replacement will be reused.
		 */
		void reset() {
			varCounter = 0;
		}

	  private:

		TypeVariablePtr getFreshVariable(NodeManager& manager) {
			return TypeVariable::get(manager, format("insieme_renamed_fresh_type_var_%d", (++varCounter)));
		}

		GenericTypeVariablePtr getFreshGenericVariable(NodeManager& manager, const GenericTypeVariablePtr& var) {
			return GenericTypeVariable::get(manager, format("insieme_renamed_fresh_type_var_%d", (++varCounter)), var->getTypeParameter());
		}
	};

} // end namespace types
} // end namespace core
} // end namespace insieme
