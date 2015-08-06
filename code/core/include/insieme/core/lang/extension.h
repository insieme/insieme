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

#pragma once

#include <boost/noncopyable.hpp>

#include "insieme/utils/string_utils.h"

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/lang/lang.h"

#include <string>
#include <map>

namespace insieme {
namespace core {
namespace lang {

using std::string;

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

	NodeManager& manager;
	
	/**
	 * A map with all named types, literals and derived constructs defined within this extension
	 */
	mutable std::map<string, NodePtr> extensionIrNames;
	
protected:

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
	void addNamedIrExtension(const string name, const NodePtr node) const {
		if(!name.empty()) {
			extensionIrNames.insert(std::make_pair(name, node));
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
	 * Returns a map with all the named IR extensions defined by this extension
	 */
	const std::map<string, NodePtr>& getNamedIrExtensions() const {
		return extensionIrNames;
	}
};

/**
 * A utility simplifying the creation of a type within language extensions. The
 * given type string will be parsed and returned.
 *
 * @param manager the node manager to be used for creating the type
 * @param type the string to be parsed
 * @param definitions a map of already existing named definitions used during parsing
 * @return the requested type
 */
TypePtr getType(NodeManager& manager, const string& type, const std::map<string, NodePtr>& definitions = std::map<string, NodePtr>());

/**
 * A utility simplifying the creation of literals within language extensions.
 * The type of the literal is passed as a string which will internally be parsed.
 *
 * @param manager the node manager to be used for creating the resulting literal
 * @param type the type of the resulting literal, encoded as a string
 * @param value the value of the resulting literal
 * @param definitions a map of already existing named definitions used during parsing
 * @return the requested literal
 */
LiteralPtr getLiteral(NodeManager& manager, const string& type, const string& value,
                      const std::map<string, NodePtr>& definitions = std::map<string, NodePtr>());
                      
                      
/**
 * A macro supporting the simple declaration and definition of a type within a language extension
 * implementation.
 *
 * @param NAME the name of the type literal to be added
 * @param IR_NAME the name used to reference this type within this extension and in parsed code
 * @param TYPE the IR type to be represented as a string
 */
#define LANG_EXT_TYPE_WITH_NAME(NAME, IR_NAME, TYPE) \
		private: \
			const insieme::core::TypePtr type_##NAME = create##NAME(); \
			 \
			const insieme::core::TypePtr create##NAME() const {\
				checkIrNameNotAlreadyInUse(IR_NAME); \
				const insieme::core::TypePtr result = insieme::core::lang::getType(getNodeManager(), TYPE, getNamedIrExtensions()); \
				insieme::core::lang::markAsBuiltIn(result); \
				addNamedIrExtension(IR_NAME, result); \
				return result; \
			} \
		public: \
			const insieme::core::TypePtr& get##NAME() const { \
				return type_##NAME; \
			} \
			const bool is##NAME(const insieme::core::TypePtr& node) const { \
				return node && (*node == *get##NAME()); \
			}

/**
 * A macro supporting the simple declaration and definition of a type within a language extension
 * implementation.
 *
 * @param NAME the name of the type literal to be added
 * @param TYPE the IR type to be represented as a string
 */
#define LANG_EXT_TYPE(NAME, TYPE) \
		LANG_EXT_TYPE_WITH_NAME(NAME, camelcaseToUnderscore(#NAME), TYPE)

/**
 * A macro supporting the simple declaration and definition of a literal within a language extension
 * implementation.
 *
 * @param NAME the name of the literal to be added
 * @param IR_NAME the name used to reference this literal within this extension and in parsed code
 * @param VALUE the value of this literal
 * @param TYPE the IR type of the literal
 */
#define LANG_EXT_LITERAL_WITH_NAME(NAME, IR_NAME, VALUE, TYPE) \
		private: \
			const insieme::core::LiteralPtr lit_##NAME = create##NAME(); \
			 \
			const insieme::core::LiteralPtr create##NAME() const { \
				checkIrNameNotAlreadyInUse(IR_NAME); \
				const insieme::core::LiteralPtr result = insieme::core::lang::getLiteral(getNodeManager(), TYPE, VALUE, getNamedIrExtensions()); \
				insieme::core::lang::markAsDerived(result, VALUE); \
				insieme::core::lang::markAsBuiltIn(result); \
				addNamedIrExtension(IR_NAME, result); \
				return result; \
			} \
		public: \
			const insieme::core::LiteralPtr& get##NAME() const { \
				return lit_##NAME; \
			} \
			const bool is##NAME(const insieme::core::NodePtr& node) const { \
				return node && (*node == *get##NAME()); \
			}

/**
 * A macro supporting the simple declaration and definition of a literal within a language extension
 * implementation.
 *
 * @param NAME the name of the literal to be added
 * @param VALUE the value of this literal
 * @param TYPE the IR type of the literal
 */
#define LANG_EXT_LITERAL(NAME, VALUE, TYPE) \
		LANG_EXT_LITERAL_WITH_NAME(NAME, camelcaseToUnderscore(#NAME), VALUE, TYPE)

/**
 * A macro supporting the simple declaration and definition of a derived language extension implementation.
 *
 * @param NAME the name of the language construct
 * @param IR_NAME the name used to reference this derived within this extension and in parsed code
 * @param SPEC the implementation of the derived construct (using INSPIRE)
 */
#define LANG_EXT_DERIVED_WITH_NAME(NAME, IR_NAME, SPEC) \
		private: \
			const insieme::core::ExpressionPtr expr_##NAME = create##NAME(); \
			 \
			const insieme::core::ExpressionPtr create##NAME() const { \
				checkIrNameNotAlreadyInUse(IR_NAME); \
				insieme::core::IRBuilder builder(getNodeManager()); \
				const insieme::core::ExpressionPtr result = builder.normalize(builder.parseExpr(SPEC, getNamedIrExtensions())).as<insieme::core::ExpressionPtr>(); \
				insieme::core::lang::markAsDerived(result, #NAME); \
				insieme::core::lang::markAsBuiltIn(result); \
				addNamedIrExtension(IR_NAME, result); \
				return result; \
			} \
		public: \
			const insieme::core::ExpressionPtr& get##NAME() const { \
				return expr_##NAME; \
			} \
			const bool is##NAME(const insieme::core::NodePtr& node) const { \
				return node && (*node == *get ## NAME()); \
			}

/**
 * A macro supporting the simple declaration and definition of a derived language extension implementation.
 *
 * @param NAME the name of the language construct
 * @param SPEC the implementation of the derived construct (using INSPIRE)
 */
#define LANG_EXT_DERIVED(NAME, SPEC) \
		LANG_EXT_DERIVED_WITH_NAME(NAME, camelcaseToUnderscore(#NAME), SPEC)

} // end namespace lang
} // end namespace core
} // end namespace insieme
