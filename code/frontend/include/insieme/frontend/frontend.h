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

#include <string>
#include <vector>
#include <map>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_program.h"

#include "insieme/frontend/compiler.h"

namespace insieme {

namespace core {
	// some forward declarations
	class NodeManager;
	class Program;
	template<typename T> class Pointer;
	typedef Pointer<const Program> ProgramPtr;
}

namespace frontend {

	using std::vector;
	using std::string;

	class ConversionJob {

	public:

		/**
		 * A list of options to adjust the print.
		 */
		enum Option {
			OpenMP			= 1<<0,
			OpenCL			= 1<<1,
			Cilk			= 1<<2
		};

		/**
		 * The default frontend configuration.
		 */
		static const unsigned DEFAULT_FLAGS;

	private:

		/**
		 * The node manager to be used for the conversion.
		 */
		core::NodeManager& manager;

		/**
		 * The translation units to be converted.
		 */
		vector<string> files;

		/**
		 * A list of include directories to be considered.
		 */
		vector<string> includeDirs;

		/**
		 * The C standard to be followed.
		 */
		string standard;

		/**
		 * A list of definitions to be passed to the preprocessor.
		 */
		vector<string> definitions;

		/**
		 * Additional flags - a bitwise boolean combination of Options (see Option)
		 */
		unsigned flags;

	public:

		/**
		 * Creates a new conversion job covering a single file.
		 */
		ConversionJob(core::NodeManager& manager, const string& file);

		/**
		 * Creates a new conversion job based on the given options.
		 */
		ConversionJob(core::NodeManager& manager, const vector<string>& files, const vector<string>& includeDirs = vector<string>());

		/**
		 * Allows to check for an option.
		 */
		bool hasOption(const Option option) const {
			return flags & option;
		}

		/**
		 * Updates the state of an option.
		 */
		void setOption(const Option option, bool status = true) {
			flags = (status)?(flags | option):( flags & ~option);
		}

		/**
		 * Updates the options set for the conversion process.
		 */
		void setOptions(unsigned options) {
			flags = options;
		}

		/**
		 * Obtains a reference to the currently defined definitions.
		 */
		const vector<string>& getDefinitions() const {
			return definitions;
		}

		/**
		 * Updates the definitions to be used by the conversion process.
		 */
		void setDefinitions(const vector<string>& definitions) {
			this->definitions = definitions;
		}

		/**
		 * Adds a pre-processor definition to this conversion job.
		 */
		void addDefinition(const string& name, const string& value);

		/**
		 * Triggers the actual conversion. The previously set up parameters will be used to attempt a conversion.
		 *
		 * @return the resulting, converted program
		 * @throws an exception if the conversion fails.
		 */
		core::ProgramPtr execute();
	};


} // end namespace frontend
} // end namespace insieme
