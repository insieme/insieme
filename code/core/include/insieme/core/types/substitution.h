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

#include <boost/optional.hpp>

#include "insieme/core/ir_types.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/map_utils.h"


namespace insieme {
namespace core {
namespace types {


	// -------------------------------------------------------------------------------------------------------------------------
	//                                                    Substitution
	// -------------------------------------------------------------------------------------------------------------------------


	// forward declaration
	class Substitution;

	// define a simple name for an optional substitution - which will be the result of unification and matching algorithms.
	typedef boost::optional<Substitution> SubstitutionOpt;


	/**
	 * This class represents a substitution for type variables within types.
	 */
	class Substitution : public utils::Printable {
	  public:

		/**
		 * The definition of the data structure used to maintain type variable mappings.
		 */
		using VariableMapping = utils::map::PointerMap<TypeVariablePtr, TypePtr>;
		
		/**
		 * The definition of the data structure used to maintain variadic type variable to type variable mappings.
		 */
		using VariadicTypeVariableMapping = utils::map::PointerMap<VariadicTypeVariablePtr, TypeVariableList>;

	  private:

		/**
		 * The mapping this substitution is representing.
		 */
		VariableMapping mapping;

		VariadicTypeVariableMapping variadicVarMapping;

	  public:

		/**
		 * Creates a new, empty substitution.
		 */
		Substitution(){};

		/**
		 * Creates a single-element mapping.
		 *
		 * @param var the variable to be substituted
		 * @param type the type the variable should be substituted with
		 */
		Substitution(const TypeVariablePtr& var, const TypePtr& type);

		/**
		 * Checks whether this substitution is actually mapping any variables to some type.
		 */
		bool empty() const {
			return mapping.empty();
		}

		/**
		 * Applies this substitution to the given node.
		 * @param manager the manager to be used for creating new type node instances
		 * @param node the node to which this substitution should be applied to
		 * @return the resulting node
		 */
		NodePtr applyTo(NodeManager& manager, const NodePtr& node) const;

		/**
		 * Applies this substitution to the given node and preserves the type information.
		 *
		 * @param manager the manager to be used for creating new type node instances
		 * @param node the node to which this substitution should be applied to
		 * @return the resulting type expression
		 */
		template<typename T>
		Pointer<T> applyTo(NodeManager& manager, const Pointer<T>& node) const {
			return applyTo(manager, NodePtr(node)).template as<Pointer<T>>();
		}

		/**
		 * A special overload for type variables for which the preservation of there
		 * node type can not be ensured.
		 */
		TypePtr applyTo(NodeManager& manager, const TypeVariablePtr& node) const {
			return applyTo(manager, node.as<TypePtr>());
		}

		/**
		 * Applies this substitution to the given node using the node's node manager. A call
		 * to applyTo(node) is equivalent to applyTo(node->getNodeManager(),node).
		 *
		 * @param node the node to which this substitution should be applied to
		 * @return the resulting type expression
		 */
		template<typename T>
		Pointer<T> applyTo(const Pointer<T>& node) const {
			return applyTo(node->getNodeManager(), NodePtr(node)).template as<Pointer<T>>();
		}

		/**
		 * A special overload for type variables for which the preservation of there
		 * node type can not be ensured.
		 */
		TypePtr applyTo(const TypeVariablePtr& node) const {
			return applyTo(node.as<TypePtr>());
		}

		/**
		 * Enables substitutions to be utilized as functors.
		 */
		template<typename T>
		Pointer<T> operator()(const Pointer<T>& node) const {
			return applyTo(node);
		}

		/**
		 * Obtains the (optional) type bound to the given type variable. 
		 */
		TypePtr operator[](const TypeVariablePtr& var) const {
			auto pos = mapping.find(var);
			return (pos == mapping.end()) ? TypePtr() : pos->second;
		}

		/**
		 * Obtains the (optional) list of type variables the given variadic type variable
		 * has been expanded into.
		 * 
		 * @param var the variadic type variable to be resolved
		 * @return a list of type variables the variadic type has been expanded to or null, if not expanded.
		 */
		const TypeVariableList* operator[](const VariadicTypeVariablePtr& var) const {
			auto pos = variadicVarMapping.find(var);
			return (pos == variadicVarMapping.end()) ? nullptr : &pos->second;
		}

		/**
		 * An overload for type variables for which the preservation of the node type
		 * can not be guaranteed.
		 */
		TypePtr operator()(const TypeVariablePtr& var) const {
			return applyTo(var);
		}

		/**
		 * Extends this substitution by the given mapping. If the same variable
		 * is already mapped to some type, the current mapping will be replaced.
		 *
		 * @param the variable which should be substituted.
		 * @param the type the variable should be substituted for.
		 */
		void addMapping(const TypeVariablePtr& var, const TypePtr& type);

		/**
		 * Extends this substitution by the given mapping. If the same variable
		 * is already mapped to some type, the current mapping will be replaced.
		 *
		 * @param the variadic variable which should be substituted.
		 * @param the type variables the variadic variable should be substituted for.
		 */
		void addMapping(const VariadicTypeVariablePtr& var, const TypeVariableList& variables);

		/**
		 * Checks whether this substitution contains a mapping for the given variable.
		 *
		 * @param var the variable to be checked
		 * @return true if so, false otherwise
		 */
		bool containsMappingFor(const TypeVariablePtr& var) const;

		/**
		 * Removes the mapping stored for the given variable from this substitution.
		 * @param var the variable to be removed
		 */
		void remMappingOf(const TypeVariablePtr& var);

		/**
		* Removes all mappings associated to variables within the given variable set.
		*
		* @param variables the list of variables to be removed
		*/
		void removeMappings(const TypeVariableSet& variables);

		/**
		 * Obtains a constant reference to the type variable mapping constituting this substitution.
		 * @return a constant reference to the internally maintained type variable mapping
		 */
		const VariableMapping& getVariableMapping() const {
			return mapping;
		}

		/**
		 * Creates a copy of this substitution only covering the variables covered by the given variable sets.
		 *
		 * @param variables the list of variables to be covered by retained
		 * @param variadicVars the list of variadic variables to be covered by retained
		 * @return a copy of this substitution restricted to the given variable sets
		 */
		Substitution restrictTo(const TypeVariableSet& variables, const VariadicTypeVariableSet& variadicVars) const;

        /**
         * Creates a copy of the given substitution where all referenced types are handled by the given manger.
         *
         * @param manager the manager to be managing the nodes referenced by the resulting substitution
         * @param substitution the substitution to be copied
         * @return a copy of the given substitution instance where all nodes are maintained by the given manager
         */
		Substitution copyTo(NodeManager& manager) const;

		/**
		 * Prints this substitution to the given output stream.
		 *
		 * @param out the stream to be printed to
		 * @return the handed in stream
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

		/**
		 * A utility function to compose two substitutions. Applying the resulting substitution will have the
		 * same effect on any type expression as applying substitution a followed by b.
		 *
		 * @param manager the manager to be used for creating new type nodes if necessary
		 * @param a the first substitution to be applied
		 * @param b the second substitution to be applied
		 * @return a substitution combining the given substitutions.
		 */
		static Substitution compose(NodeManager& manager, const Substitution& a, const Substitution& b);

	};

	/**
	 * Creates a copy of the given substitution where all referenced types are handled by the given manger.
	 *
	 * @param manager the manager to be managing the nodes referenced by the resulting substitution
	 * @param substitution the substitution to be copied
	 * @return a copy of the given substitution instance where all nodes are maintained by the given manager
	 */
	inline SubstitutionOpt copyTo(NodeManager& manager, const SubstitutionOpt& substitution) {
		// check for early exit
		if(!substitution) { return substitution; }

		// copy the substitute
		return substitution->copyTo(manager);
	}


} // end namespace types
} // end namespace core
} // end namespace insieme
