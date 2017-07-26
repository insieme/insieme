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


#include <functional>
#include <string>
#include <map>
#include <boost/optional.hpp>

#include "insieme/core/ir_node.h"
#include "insieme/core/lang/extension.h"


namespace insieme {
namespace core {
namespace lang {

	/**
	 * This class can be used to obtain factories by name which create language extensions.
	 *
	 * New named language extensions have to be registered in the constructor of this class.
	 */
	class ExtensionRegistry {
	  private:
		// the map storing factory functions which can be called to create a new instance of language extensions
		std::map<const std::string, std::function<const Extension&(NodeManager&)>> extensionFactories;

		// prevent external instantiation
		ExtensionRegistry();

		// prevent copies
		ExtensionRegistry(ExtensionRegistry const&) = delete;

		void operator=(ExtensionRegistry const&) = delete;

	  public:
		/*
		 * Returns the singleton instance of this ExtensionRegistry
		 *
		 * @return the singleton instance.
		 */
		static ExtensionRegistry& getInstance() {
			static ExtensionRegistry instance;
			return instance;
		}

		/**
		 * Looks up an Extension factory by name
		 *
		 * @param name the name of the extension to look up
		 * @return a factory which can be used to create the extension, given a NodeManager
		 */
		const boost::optional<std::function<const Extension&(NodeManager&)>> lookupExtensionFactory(const std::string& extensionName) const;

		/**
		 * Returns all extension factories
		 *
		 * @return all factories and names of registered extensions which can be used to create the extension, given a NodeManager
		 */
		const std::map<const std::string, std::function<const Extension&(NodeManager&)>> getExtensionFactories() const {
			return extensionFactories;
		}
	};


} // end namespace lang
} // end namespace core
} // end namespace insieme
