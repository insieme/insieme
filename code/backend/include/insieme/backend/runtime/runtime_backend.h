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

#pragma once

#include "insieme/backend/backend.h"

namespace insieme {
namespace backend {
namespace runtime {

	// A forward declaration of the sequential backend implementation
	class RuntimeBackend;
	typedef std::shared_ptr<RuntimeBackend> RuntimeBackendPtr;

	/**
	 * The facade for the backend capable of generating code to be used by the runtime backend.
	 *
	 * This backend converts the given IR representation into C99 / C++11 target code interacting with
	 * the Insieme Runtime environment.
	 */
	class RuntimeBackend : public Backend {

	  public:
		/**
		 * A constructor of this kind of backend accepting an operator table extender.
		 */
		RuntimeBackend(const BackendConfigPtr& config = std::make_shared<BackendConfig>())
		    : Backend(std::vector<AddOnPtr>(), config) {}


		/**
		 * A factory method obtaining a smart pointer referencing a
		 * fresh instance of the runtime backend using the default configuration.
		 *
		 * @return a smart pointer to a fresh instance of the runtime backend
		 */
		static RuntimeBackendPtr getDefault();


	  protected:
		/**
		 * Creates a converter instance capable of converting IR code into C / C++ code utilizing
		 * the runtime for realizing parallel constructs.
		 *
		 * @param manager the manager to be utilized for the conversion
		 * @return a converter instance conducting the code conversion
		 */
		virtual Converter buildConverter(core::NodeManager& manager) const;
	};

} // end namespace runtime
} // end namespace backend
} // end namespace insieme
