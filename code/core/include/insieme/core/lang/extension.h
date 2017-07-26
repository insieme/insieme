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

#include <string>
#include <map>

#include <boost/noncopyable.hpp>

#include "insieme/utils/assert.h"
#include "insieme/utils/string_utils.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/types/match.h"

#include "insieme/core/lang/lang.h"


namespace insieme {
namespace core {
namespace lang {

	using std::string;

	/**
	 * The type utilized for producing nodes lazily.
	 */
	typedef std::function<NodePtr()> lazy_factory;

	/**
	 * The type utilized for providing a list of symbols defined by this extension
	 * mapped to functions obtaining the corresponding instance.
	 */
	typedef std::map<string,lazy_factory> symbol_map;

	/**
	 * A map for type aliases like ref<'a> being an alias for ref<'a,f,f>
	 */
	typedef std::map<GenericTypePtr, TypePtr> type_alias_map;

	/**
	 * This class represents the common base class of language extensions. Such
	 * extensions are defining new types or literals and can be used within the
	 * frontend, backends or analyses to encode information within the IR.
	 *
	 * Extensions should not directly be created. Instead, extensions should be created
	 * using the corresponding factory method of the NodeManager.
	 *
	 * One can define named IR extensions by using the macros with the _WITH_NAME suffix
	 * provided. Those can be used to create named types, literal and derived constructs.
	 * Note that names used to identify these have to be unique - not just within this
	 * extension but also across all other extensions. If an extension is created with
	 * the the macro without _WITH_NAME, its IR_NAME will be converted from camelcase to
	 * underscores and used as name. Also this name has to be unique.
	 *
	 * Each new extension should be registered with a unique name in the ExtensionRegistry.
	 * This name can then be used to reference all the named constructs defined within
	 * this extension in arbitrary IR code during parsing, as well as in other extensions.
	 */
	class Extension : private boost::noncopyable {

		/**
		 * The list of all symbols visible to the definitions of the local primitives,
		 * including the local symbols.
		 */
		mutable symbol_map symbols;

		/**
		 * A map with all named types, literals and derived constructs defined within this extension.
		 */
		mutable symbol_map local_symbols;

		/**
		 * A map of type alias active when parsing the definitions in this extension, including
		 * the all locally defined and imported aliases.
		 */
		mutable type_alias_map type_aliases;

		/**
		 * A map of locally defined type aliases.
		 */
		mutable type_alias_map local_type_aliases;

	  protected:

		/**
		 * The manager this extension is maintained by.
		 */
		NodeManager& manager;

		/**
		 * Creates a new instance of this language extension being associated to the
		 * given manager.
		 */
		Extension(NodeManager& manager) : manager(manager) {}

		/**
		 * Checks if the given name is not already in use.
		 * Will fail an assertion in case the name is already in use.
		 */
		void checkIrNameNotAlreadyInUse(const string& irName) const;

		/**
		 * Adds a new mapping to the list of named mappings in this extension.
		 */
		void addNamedIrExtension(const string& name, const lazy_factory& factory) const {
			if(!name.empty()) {
				local_symbols.insert({ name, factory });
				symbols.insert({ name, factory });
			}
		}

	  public:
		/**
		 * A virtual destructor to enable the proper destruction of derived
		 * instances.
		 */
		virtual ~Extension() {}

		/**
		 * Obtains a copy of the internally maintained reference to the node
		 * manager this extension is associated with.
		 */
		NodeManager& getNodeManager() const {
			return manager;
		}

		/**
		 * Returns a map with all the symbols defined by this and all imported extensions.
		 */
		const symbol_map& getSymbols() const {
			return symbols;
		}

		/**
		 * Returns all active type aliases in this extension, including the locally defined
		 * as well as the imported definitions.
		 */
		const type_alias_map& getTypeAliases() const {
			return type_aliases;
		}

		/**
		 * Returns a map with all the symbols defined by this extension.
		 */
		const symbol_map& getDefinedSymbols() const {
			return local_symbols;
		}

		/**
		 * Obtains a list of locally defined type aliases.
		 */
		const type_alias_map& getDefinedTypeAliases() const {
			return local_type_aliases;
		}

	  protected:

		/**
		 * Imports all symbols of another extension.
		 */
		template<typename Extension>
		bool importExtension() {
			// load extension
			auto& other = manager.getLangExtension<Extension>();
			// import symbols
			for(const auto& cur : other.getSymbols()) {
				symbols.insert(cur);
			}
			// import type aliases
			for(const auto& cur : other.getTypeAliases()) {
				type_aliases.insert(cur);
			}
			return true;	// just a dummy result
		}

		/**
		 * Registers a type alias within this language module.
		 */
		bool addAlias(const GenericTypePtr& pattern, const TypePtr& full) {
			local_type_aliases.insert({ pattern, full });
			type_aliases.insert({ pattern, full });
			return true;
		}

		/**
		 * Registers a type alias within this language module.
		 */
		bool addAlias(const string& pattern, const string& full) {
			return addAlias(
					getType(manager, pattern, getSymbols(), getTypeAliases()).as<GenericTypePtr>(),
					getType(manager, full, getSymbols(), getTypeAliases())
			);
		}

		/**
		 * A utility simplifying the creation of a type within language extensions. The
		 * given type string will be parsed and returned.
		 *
		 * @param manager the node manager to be used for creating the type
		 * @param type the string to be parsed
		 * @param definitions a map of already existing named definitions used during parsing
		 * @param aliases a map of type aliases to be considered in the parsing
		 * @return the requested type
		 */
		static TypePtr getType(NodeManager& manager, const string& type, const symbol_map& definitions = symbol_map(), const type_alias_map& aliases = type_alias_map());

		/**
		 * A utility simplifying the creation of literals within language extensions.
		 * The type of the literal is passed as a string which will internally be parsed.
		 *
		 * @param manager the node manager to be used for creating the resulting literal
		 * @param type the type of the resulting literal, encoded as a string
		 * @param value the value of the resulting literal
		 * @param definitions a map of already existing named definitions used during parsing
		 * @param aliases a map of type aliases to be considered in the parsing
		 * @return the requested literal
		 */
		static LiteralPtr getLiteral(NodeManager& manager, const string& type, const string& value,
							  const symbol_map& definitions = symbol_map(), const type_alias_map& aliases = type_alias_map());

		/**
		 * A utility simplifying the creation of expressions within language extensions.
		 *
		 * @param manager the node manager to be used for creating the resulting literal
		 * @param spec the inspire code describing the resulting construct
		 * @param definitions a map of already existing named definitions used during parsing
		 * @param aliases a map of type aliases to be considered in the parsing
		 * @return the requested literal
		 */
		static ExpressionPtr getExpression(NodeManager& manager, const string& spec, const symbol_map& definitions = symbol_map(), const type_alias_map& aliases = type_alias_map());

		/**
		 * A utility to verify whether a given node is a call to a given expression.
		 *
		 * @param candidate the node to be tested
		 * @param factory a lazy factory for the creation of the call target to be tested
		 * @return true if it is a call to the given literal, false otherwise
		 */
		static CallExprPtr isCallOf(const NodePtr& candidate, const lazy_factory& factory) {
			auto call = candidate.isa<CallExprPtr>();
			return (call && *(call->getFunctionExpr()) == *(factory())) ? call : CallExprPtr();
		}
	};


	/**
	 * Two macros to create unique identifiers for import dummy variables.
	 */
	#define __UNIQUE_MODULE_IMPORT_VAR_NAME_AUX(LINE) _module_ ## LINE ## _import
	#define __UNIQUE_MODULE_IMPORT_VAR_NAME(LINE) __UNIQUE_MODULE_IMPORT_VAR_NAME_AUX(LINE)

	/**
	 * A macro enabling the import of symbols of other language extensions.
	 */
	#define IMPORT_MODULE(NAME) \
	  private: bool __UNIQUE_MODULE_IMPORT_VAR_NAME(__LINE__) = importExtension<NAME>();


	// Two auxiliary macros to concatenate tokens
	#define __INSIEME_MACRO_ARGUMENT_CONCAT_DETAIL(X, Y) X##Y
	#define __INSIEME_MACRO_ARGUMENT_CONCAT(X, Y) __INSIEME_MACRO_ARGUMENT_CONCAT_DETAIL(X, Y)

	/**
	 * A macro adding a type alias definition to this extension.
	 */
	#define TYPE_ALIAS(PATTERN,TYPE) \
	  private : bool __INSIEME_MACRO_ARGUMENT_CONCAT(_alias_, __LINE__) = addAlias(PATTERN,TYPE);

	/**
	 * A macro supporting the simple declaration and definition of a type within a language extension
	 * implementation.
	 *
	 * @param NAME the name of the type literal to be added
	 * @param IR_NAME the name used to reference this type within this extension and in parsed code
	 * @param TYPE the IR type to be represented as a string
	 */
	#define LANG_EXT_TYPE_WITH_NAME(NAME, IR_NAME, TYPE)                                                                                                       \
	  private:                                                                                                                                                 \
		mutable insieme::core::TypePtr type_##NAME = reg##NAME();                                                                                              \
																																							   \
		const insieme::core::TypePtr reg##NAME() const {                                                                                                       \
			checkIrNameNotAlreadyInUse(IR_NAME);                                                                                                               \
			return insieme::core::TypePtr();                                                                                                                   \
		}                                                                                                                                                      \
                                                                                                                                                               \
	  public:                                                                                                                                                  \
		const insieme::core::TypePtr& get##NAME() const {                                                                                                      \
		    if(!type_##NAME) {                                                                                                                                 \
				type_##NAME = getType(getNodeManager(), TYPE, getSymbols(), getTypeAliases());                                                                 \
				assert_true(type_##NAME) << "Unable to parse IR for type " #NAME;                                                                              \
				insieme::core::lang::markAsBuiltIn(type_##NAME);                                                                                               \
			}                                                                                                                                                  \
			return type_##NAME;                                                                                                                                \
		}                                                                                                                                                      \
		const bool is##NAME(const insieme::core::NodePtr& node) const {                                                                                        \
			if(auto expr = node.isa<insieme::core::ExpressionPtr>()) return is##NAME(expr->getType());													       \
			return node && (*node == *get##NAME());                                                                                                            \
		}

	/**
	 * A macro supporting the simple declaration and definition of a type within a language extension
	 * implementation.
	 *
	 * @param NAME the name of the type literal to be added
	 * @param TYPE the IR type to be represented as a string
	 */
	#define LANG_EXT_TYPE(NAME, TYPE) LANG_EXT_TYPE_WITH_NAME(NAME, camelcaseToUnderscore(#NAME), TYPE)

	/**
	 * A macro supporting the simple declaration and definition of a literal within a language extension
	 * implementation.
	 *
	 * @param NAME the name of the literal to be added
	 * @param IR_NAME the name used to reference this literal within this extension and in parsed code
	 * @param VALUE the value of this literal
	 * @param TYPE the IR type of the literal
	 */
	#define LANG_EXT_LITERAL_WITH_NAME(NAME, IR_NAME, VALUE, TYPE)                                                                                             \
	  private:                                                                                                                                                 \
		mutable insieme::core::LiteralPtr lit_##NAME = reg##NAME();                                                                                            \
                                                                                                                                                               \
		insieme::core::LiteralPtr reg##NAME() const {                                                                                                          \
			checkIrNameNotAlreadyInUse(IR_NAME);                                                                                                               \
			addNamedIrExtension(IR_NAME, [&]()->insieme::core::NodePtr { return get##NAME(); });                                                               \
			return insieme::core::LiteralPtr();                                                                                                                \
		}                                                                                                                                                      \
                                                                                                                                                               \
	  public:                                                                                                                                                  \
		const insieme::core::LiteralPtr& get##NAME() const {                                                                                                   \
		    if(!lit_##NAME) {                                                                                                                                  \
				lit_##NAME = getLiteral(getNodeManager(), TYPE, VALUE, getSymbols(), getTypeAliases());                                                        \
				assert_true(lit_##NAME) << "Unable to parse IR for literal " #NAME;                                                                            \
				insieme::core::lang::markAsDerived(lit_##NAME, VALUE);                                                                                         \
				insieme::core::lang::markAsBuiltIn(lit_##NAME);                                                                                                \
			}                                                                                                                                                  \
			return lit_##NAME;                                                                                                                                 \
		}                                                                                                                                                      \
		const bool is##NAME(const insieme::core::NodePtr& node) const {                                                                                        \
			return node && (*node == *get##NAME());                                                                                                            \
		} 																																					   \
	    const insieme::core::CallExprPtr isCallOf##NAME(const insieme::core::NodePtr& node) const {                                                            \
			return isCallOf(node,[&]()->insieme::core::NodePtr { return get##NAME(); });                                                             		   \
		}

	/**
	 * A macro supporting the simple declaration and definition of a literal within a language extension
	 * implementation.
	 *
	 * @param NAME the name of the literal to be added
	 * @param VALUE the value of this literal
	 * @param TYPE the IR type of the literal
	 */
	#define LANG_EXT_LITERAL(NAME, VALUE, TYPE) LANG_EXT_LITERAL_WITH_NAME(NAME, VALUE, VALUE, TYPE)

	/**
	 * A macro supporting the simple declaration and definition of a derived language extension implementation.
	 *
	 * @param NAME the name of the language construct
	 * @param IR_NAME the name used to reference this derived within this extension and in parsed code
	 * @param SPEC the implementation of the derived construct (using INSPIRE)
	 */
	#define LANG_EXT_DERIVED_WITH_NAME(NAME, IR_NAME, SPEC)                                                                                                    \
	  private:                                                                                                                                                 \
		mutable insieme::core::ExpressionPtr expr_##NAME = reg##NAME();                                                                                        \
                                                                                                                                                               \
		const insieme::core::ExpressionPtr reg##NAME() const {                                                                                                 \
			checkIrNameNotAlreadyInUse(IR_NAME);                                                                                                               \
			addNamedIrExtension(IR_NAME, [&]()->insieme::core::NodePtr { return get##NAME(); });                                                               \
			return insieme::core::ExpressionPtr();                                                                                                             \
		}                                                                                                                                                      \
                                                                                                                                                               \
	  public:                                                                                                                                                  \
		const insieme::core::ExpressionPtr& get##NAME() const {                                                                                                \
		    if(!expr_##NAME) {                                                                                                                                 \
				expr_##NAME = getExpression(getNodeManager(), SPEC, getSymbols(), getTypeAliases());                                                           \
				assert_true(expr_##NAME) << "Unable to parse IR for derived " #NAME;                                                                           \
				if(expr_##NAME.isa<insieme::core::LambdaExprPtr>()) {                                                                                          \
					expr_##NAME = insieme::core::LambdaExpr::get(manager, expr_##NAME.as<insieme::core::LambdaExprPtr>()->getLambda(), IR_NAME);               \
				}                                                                                                                                              \
				insieme::core::lang::markAsDerived(expr_##NAME, IR_NAME);                                                                                      \
				insieme::core::lang::markAsBuiltIn(expr_##NAME);                                                                                               \
	        }                                                                                                                                                  \
			return expr_##NAME;                                                                                                                                \
		}                                                                                                                                                      \
		const bool is##NAME(const insieme::core::NodePtr& node) const {                                                                                        \
			if(!node) return false;                                                                                                                            \
			return *node == *get##NAME();                                                                                                                      \
		} 																																					   \
		const insieme::core::CallExprPtr isCallOf##NAME(const insieme::core::NodePtr& node) const {                                                            \
			return isCallOf(node,[&]()->insieme::core::NodePtr { return get##NAME(); });                                                            		   \
		}

	/**
	 * A macro supporting the simple declaration and definition of a derived language extension implementation.
	 *
	 * @param NAME the name of the language construct
	 * @param SPEC the implementation of the derived construct (using INSPIRE)
	 */
	#define LANG_EXT_DERIVED(NAME, SPEC) LANG_EXT_DERIVED_WITH_NAME(NAME, camelcaseToUnderscore(#NAME), SPEC)

} // end namespace lang
} // end namespace core
} // end namespace insieme
