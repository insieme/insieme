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

#include <memory>
#include <string>

#include "insieme/backend/backend.h"
#include "insieme/backend/c_ast/c_ast.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {

	// forward declarations
	class Node;
	template<typename T> class Pointer;
	typedef Pointer<const Node> NodePtr;

}
namespace backend {
namespace c_ast {


	// forward declaration for fragments
	class CodeFragment;
	typedef std::shared_ptr<CodeFragment> CodeFragmentPtr;


	/**
	 * A class representing a C based target code.
	 */
	class CCode : public TargetCode {

		/**
		 * The one "root" code fragment representing the resulting target code. The
		 * code represented by this instance is the transitive closure of the dependencies
		 * defined by the given code fragment.
		 */
		CodeFragmentPtr root;

	public:

		/**
		 * Creates a new C Code instance representing a conversion from the given
		 * source node to the given target code fragment.
		 *
		 * @param source the IR node the source is based on
		 * @param code the root element of the resulting target code fragment
		 */
		CCode(const core::NodePtr& source, const CodeFragmentPtr& root);

		/**
		 * Allows this code fragment to be printed to some output stream according
		 * to the Printable interface.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

	};

	/**
	 * An abstract base class for various kinds of specialized code fragments. This base class
	 * defines an interface for code fragments to be handled uniformly.
	 */
	class CodeFragment : public utils::Printable {

		/**
		 * The list of code fragments this fragment is depending on. The dependencies should form
		 * a DAG and a topological order of this DAG is used when generating the resulting code.
		 */
		std::vector<CodeFragmentPtr> dependencies;

	public:

		/**
		 * A default constructor creating a code fragment without any dependencies.
		 */
		CodeFragment() : dependencies() {}

		/**
		 * A constructor for a code fragment initializing its internal dependencies base
		 * on the given dependency vector.
		 *
		 * @param dependencies the code fragments this fragment is depending on - default: empty list
		 */
		CodeFragment(const vector<CodeFragmentPtr>& dependencies) : dependencies(dependencies) {}

		/**
		 * A virtual destructor to support proper sub-type handling.
		 */
		virtual ~CodeFragment() {};

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

	};


	/**
	 * A code fragment is a top-level piece of code within the resulting C program. It encapsulate a
	 * type / variable / function declaration or definition represented via a C_AST instance, a name
	 * for the fragment as well as dependencies on other fragments which need to be located before
	 * this fragment within the resulting target code.
	 */
	class CCodeFragment : public CodeFragment {

		/**
		 * The code encapsulated by this fragment.
		 */
		NodePtr code;

		/**
		 * The name of this fragment. The name will be used to generate a comment within the target code.
		 */
		const std::string name;

	public:

		/**
		 * Creates a new code fragment encapsulating the given code fragment.
		 *
		 * @param code the code this fragment is covering
		 * @param name the name of the new fragment
		 */
		CCodeFragment(const NodePtr& code, const std::string& name) : code(code), name(name){ }

		/**
		 * A static factory method creating a new code fragment based on the given code and name.
		 *
		 * @param code the code forming the body of the resulting fragment
		 * @param name the name of the new fragment
		 */
		static CodeFragmentPtr createNew(const NodePtr& code, const std::string& name);

		/**
		 * Obtains a reference to the code buffer defining the body of this code fragment.
		 *
		 * @return a constant reference to the represented code body
		 */
		const NodePtr& getCode() { return code; }

		/**
		 * Updates the code covered by this code fragment.
		 *
		 * @param newCode the code to replace the currently covered code
		 */
		void setCode(const NodePtr& newCode) { code = newCode; }

		/**
		 * Obtains the name of this code fragment.
		 *
		 * @return the name of this fragment
		 */
		const std::string& getName() { return name; }

		/**
		 * Prints the code covered by this fragment to the given output stream.
		 *
		 * @param out the stream to be printed to
		 * @return the out reference passed as an argument
		 */
		virtual std::ostream& printTo(std::ostream& out) const;
	};


	/**
	 * A special kind of code fragment used for aggregating dependencies. This code fragment will
	 * not contribute any output to the resulting code file.
	 */
	class DummyFragment : public CodeFragment {

	public:

		/**
		 * Creates a new dummy fragment depending on the given code fragments.
		 *
		 * @param dependencies a list of fragments this new fragment should depend on
		 */
		DummyFragment(const vector<CodeFragmentPtr>& dependencies) : CodeFragment(dependencies) {}

		/**
		 * A static factory method creating a new dummy-code fragment based on the given name.
		 *
		 * @param name the name of the new fragment
		 */
		static CodeFragmentPtr createNew(const vector<CodeFragmentPtr>& dependencies = vector<CodeFragmentPtr>());

		/**
		 * Prints a dummy code fragment (nothing to print).
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			// nothing to print
			return out;
		}
	};


} // end namespace c_ast
} // end namespace backend
} // end namespace insieme
