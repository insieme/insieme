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
#include <sstream>
#include <vector>
#include <memory>
#include <cassert>

#include <boost/algorithm/string/replace.hpp>

#include "insieme/utils/string_utils.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace simple_backend {
	
	/**
	 * A stream based on std::stringstream that keeps a level of indentation and automatically
	 * applies it at every line break.
	 */
	class CodeBuffer : public insieme::utils::Printable {
		public:

			/**
			 * A special type and constant to be use to increase the intention by one step.
			 */
			struct IndR { }; static const IndR indR;

			/**
			 * A special type and constant to be used to decrease the intention by one step.
			 */
			struct IndL { }; static const IndL indL;

		private:

			/**
			 * The output stream to be written to.
			 */
			std::stringstream ss;

			/**
			 * The current indent string.
			 */
			std::string indentString;

			/**
			 * A generic method allowing to append content to the resulting code fragment.
			 *
			 * @param param the content to be added
			 */
			template<typename T>
			void append(const T& param) {
				std::stringstream ssTmp;
				ssTmp << param;
				std::string tmp = ssTmp.str();
				boost::replace_all(tmp, "\n", std::string("\n") + indentString);
				ss << tmp;
			}

			/**
			 * A special handling for IndR instances.
			 */
			void append(IndR) {
				indentString += "\t";
			}

			/**
			 * A special handling for IndL instances.
			 */
			void append(IndL) {
				assert(indentString.length()>0 && "Trying to indent below level 0");
				indentString = indentString.substr(0, indentString.length()-1);
			}

		public:

			/**
			 * A default constructor for this stream.
			 */
			CodeBuffer() : indentString("") { }

			/**
			 * Obtains the code written into this stream as a string.
			 *
			 * @return the code written into this stream as a string.
			 */
			std::string toString() const;

			/**
			 * Prints the content of this buffer to the given output stream.
			 * @param out the stream the output should be printed to
			 */
			virtual std::ostream& printTo(std::ostream& out) const;

			/**
			 * A generic implementation of the << operator allowing code to append
			 * new content to the generated code.
			 *
			 * @param param the content to be added
			 */
			template<typename T>
			CodeBuffer& operator<<(const T& code) {
				append(code);
				return *this;
			}

	};


	// Forward declarations
	class CodeFragment;
	typedef std::shared_ptr<CodeFragment> CodeFragmentPtr;

	/**
	 * A code fragment encapsulates some generated source code (in the form of a CodeBuffer) and an
	 * (optional) list of code fragments it depends on. A code fragment might be a type definition,
	 * a function prototype, a function definition, etc.
	 */
	class CodeFragment {

			/**
			 * The code stream containing the code of this fragment.
			 */
			CodeBuffer codeBuffer;

			/**
			 * The name of this fragment. The name will be used to generate a comment within
			 */
			const std::string name;

			/**
			 * The list of code fragments this fragment is depending on. The dependencies should form
			 * a DAG and a topological order of this DAG is used when generating the resulting code.
			 */
			std::vector<CodeFragmentPtr> dependencies;

			/**
			 * A flag indicating whether this code fragment is only used for grouping other fragments via
			 * dependencies (set to true) or to represent actual code. Dummy fragments will not be printed
			 * to the output code.
			 */
			const bool dummy;

			/**
			 * Creates a new code fragment with the given name and dummy-status.
			 *
			 * @param name the name of the new fragment
			 * @param dummy a flag indicating whether the new fragment should be a dummy fragment or not
			 */
			CodeFragment(const std::string& name, bool dummy) : name(name), dummy(dummy) { }

		public:

			/**
			 * A static factory method creating a new code fragment based on the given name.
			 *
			 * @param name the name of the new fragment
			 */
			static CodeFragmentPtr createNew(const std::string& name);

			/**
			 * A static factory method creating a new dummy-code fragment based on the given name.
			 *
			 * @param name the name of the new fragment
			 */
			static CodeFragmentPtr createNewDummy(const std::string& name);

			/**
			 * Adds a dependency to this fragment.
			 *
			 * @param fragment the code fragment to be depending on
			 */
			void addDependency(const CodeFragmentPtr& fragment);

			/**
			 * Adds a list of dependencies to this fragment.
			 *
			 * @param fragments the list of fragments to be depending on
			 */
			void addDependencies(const std::vector<CodeFragmentPtr>& fragments);

			/**
			 * Obtains list of all code fragments this fragment is depending on.
			 *
			 * @return a list of all fragments depending on.
			 */
			const std::vector<CodeFragmentPtr>& getDependencies() const { return dependencies; };

			/**
			 * Obtains the name of this code fragment.
			 *
			 * @return the name of this fragment
			 */
			const std::string& getName() { return name; }

			/**
			 * Tests whether this code fragment is marked to be a dummy fragment. Dummy fragments
			 * can be used to organize code dependencies but are not considered during code generation.
			 *
			 * @return true if it is a dummy fragment, false otherwise.
			 */
			bool isDummy() const { return dummy; }

			/**
			 * Obtains a reference to the code buffer defining the body of this code fragment.
			 */
			CodeBuffer& getCodeBuffer() { return codeBuffer; }

			/**
			 * A generic implementation of the << operator allowing code to append
			 * new code to this code fragment.
			 *
			 * @param code the code to be added
			 */
			template<typename T>
			CodeFragment& operator<<(const T& code) {
				codeBuffer << code;
				return *this;
			}
	};

	/**
	 * Allows to directly write data / information into the code fragment pointer. This way, no
	 * extra dereferencing is required.
	 *
	 * @param fragment the fragment to which the new code should be added
	 * @param code the code to be added
	 * @return the same pointer which has been passed as an argument
	 */
	template<typename T>
	const CodeFragmentPtr& operator<<(const CodeFragmentPtr& fragment, const T& code) {
		(*fragment) << code;
		return fragment;
	}

} // namespace simple_backend
} // namespace insieme

/**
 * Allows code pointers to be printed to some output stream.
 */
std::ostream& operator<<(std::ostream& os, const insieme::simple_backend::CodeFragmentPtr& cp);
