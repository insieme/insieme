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

#include <boost/noncopyable.hpp>

#include "insieme/core/forward_decls.h"

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
	 */
	class Extension : private boost::noncopyable {

		NodeManager& manager;

	protected:

		/**
		 * Creates a new instance of this language extension being associated to the
		 * given manager.
		 */
		Extension(NodeManager& manager) : manager(manager) {}

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
	};

	/**
	 * A utility simplifying the creation of a type within language extensions. The
	 * given type string will be parsed and returned.
	 *
	 * @param manager the node manager to be used for creating the type
	 * @param type the string to be parsed
	 * @return the requested type
	 */
	core::TypePtr getType(core::NodeManager& manager, const string& type);

	/**
	 * A utility simplifying the creation of literals within language extensions.
	 * The type of the literal is passed as a string which will internally be parsed.
	 *
	 * @param manager the node manager to be used for creating the resulting literal
	 * @param type the type of the resulting literal, encoded as a string
	 * @param value the value of the resulting literal
	 * @return the requested literal
	 */
	core::LiteralPtr getLiteral(core::NodeManager& manager, const string& type, const string& value);


	/**
	 * A macro supporting the simple declaration and definition of a type within a language extension
	 * implementation.
	 *
	 * @param NAME the name of the type literal to be added
	 * @param TYPE the IR type to be represented as a string
	 */
	#define LANG_EXT_TYPE(NAME,TYPE) \
		private: \
			mutable insieme::core::TypePtr lit_ ## NAME; \
		public: \
			const insieme::core::TypePtr& get ## NAME () const { \
				if (!lit_##NAME) { \
					lit_ ## NAME = insieme::core::lang::getType(getNodeManager(), TYPE); \
				} \
				return lit_##NAME; \
			}

	/**
	 * A macro supporting the simple declaration and definition of a type within a language extension
	 * implementation.
	 *
	 * @param NAME the name of the type literal to be added
	 * @param TYPE the IR type to be represented as a string
	 */
	#define LANG_EXT_NAMED_TYPE(NAME,TYPE,CNAME) \
		private: \
			mutable insieme::core::TypePtr lit_ ## NAME; \
		public: \
			const insieme::core::TypePtr& get ## NAME () const { \
				if (!lit_##NAME) { \
					lit_ ## NAME = annotations::c::attachCName(insieme::core::lang::getType(getNodeManager(), TYPE), CNAME);\
				} \
				return lit_##NAME; \
			}

	/**
	 * A macro supporting the simple declaration and definition of a type within a language extension
	 * implementation.
	 *
	 * @param NAME the name of the type literal to be added
	 * @param TYPE the IR type to be represented as a string
	 */
	#define LANG_EXT_LITERAL(NAME,VALUE,TYPE) \
		private: \
			mutable insieme::core::LiteralPtr lit_ ## NAME; \
		public: \
			const insieme::core::LiteralPtr& get ## NAME () const { \
				if (!lit_##NAME) { \
					lit_ ## NAME = insieme::core::lang::getLiteral(getNodeManager(), TYPE, VALUE); \
				} \
				return lit_##NAME; \
			} \
			const bool is ## NAME (const insieme::core::NodePtr& node) const { \
				return *node == *get ## NAME(); \
			}

} // end namespace lang
} // end namespace core
} // end namespace insieme
