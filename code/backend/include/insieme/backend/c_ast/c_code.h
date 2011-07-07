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
#include <set>

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


	/**
	 * A class representing a C based target code.
	 */
	class CCode : public TargetCode {

		/**
		 * A list of seeds for the graph of fragments the represented target code is consisting of.
		 * The code represented by this instance is the transitive closure of the dependencies
		 * defined by the given code fragments.
		 */
		const vector<CodeFragmentPtr> fragments;

		/**
		 * The list of files / headers to be included by the resulting target code.
		 */
		const vector<string> includes;

	public:

		/**
		 * Creates a new C Code instance representing a conversion from the given
		 * source node to the given target code fragment.
		 *
		 * @param source the IR node this code has been generated from
		 * @param code the root element of the resulting target code fragment
		 * @param includes the list of includes to be added during code generation
		 */
		CCode(const core::NodePtr& source, const CodeFragmentPtr& root, const vector<string>& includes = vector<string>());

		/**
		 * Creates a new C Code instance representing a conversion from the given
		 * source node to the transitive closure of the given fragments.
		 *
		 * @param source the IR node this code has been generated from
		 * @param fragments seeds / entry points of the represented program
		 * @param includes the list of includes to be added during code generation
		 */
		CCode(const core::NodePtr& source, const vector<CodeFragmentPtr>& fragments, const vector<string>& includes = vector<string>());

		/**
		 * Creates a new C code instance representing a translation of the given source to the given code fragment.
		 *
		 * @param source the source of the translation, hence the internal IR representation.
		 * @param fragment the code fragment forming the root of the DAG of code fragments representing the result
		 * @param includes the list of includes to be added during code generation
		 */
		static CCodePtr createNew(const core::NodePtr& source, CodeFragmentPtr& fragment, const vector<string>& includes) {
			return std::make_shared<CCode>(source, fragment, includes);
		}

		/**
		 * Creates a new C code instance representing a translation of the given source to the given code fragments.
		 *
		 * @param source the source of the translation, hence the internal IR representation.
		 * @param fragments the code fragment forming the root elements of the DAG of code fragments representing the result
		 * @param includes the list of includes to be added during code generation
		 */
		static CCodePtr createNew(const core::NodePtr& source, const vector<CodeFragmentPtr>& fragments, const vector<string>& includes) {
			return std::make_shared<CCode>(source, fragments, includes);
		}

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
		 * The set of code fragments this fragment is depending on. The dependencies should form
		 * a DAG and a topological order of this DAG is used when generating the resulting code.
		 */
		DependencySet dependencies;

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
		CodeFragment(const DependencySet& dependencies) : dependencies(dependencies) {}

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
		 * Adds the fragment pointers present within the given container to the
		 * set of dependencies defined for this code fragment.
		 *
		 * @param fragments the list of fragments to be depending on
		 */
		template<typename Container>
		void addDependencies(const Container& fragments) {
			// just add all dependencies (the add dependency is filtering null pointer)
			for_each(fragments, [&](const CodeFragmentPtr& cur) {
				this->addDependency(cur);
			});
		}

		/**
		 * Obtains list of all code fragments this fragment is depending on.
		 *
		 * @return a list of all fragments depending on.
		 */
		const DependencySet& getDependencies() const { return dependencies; };

	};

	/**
	 * A code fragment is a top-level piece of code within the resulting C program. It encapsulate a
	 * type / variable / function declaration or definition represented via a C_AST instance, a name
	 * for the fragment as well as dependencies on other fragments which need to be located before
	 * this fragment within the resulting target code.
	 */
	class CCodeFragment : public CodeFragment {

		/**
		 * A shared pointer referencing the node manager used for maintaining the nodes
		 * forming the C AST describing the represented target code.
		 */
		const SharedCNodeManager cNodeManager;

		/**
		 * The code encapsulated by this fragment. The fragment may represent an abitrary sequence of
		 * definitions / comments.
		 */
		vector<NodePtr> code;

	public:

		/**
		 * Creates a new code fragment encapsulating the given code fragment.
		 *
		 * @param nodeManager the node manager managing the life-span of the given C AST node
		 * @param code the code this fragment is covering
		 */
		CCodeFragment(const SharedCNodeManager& nodeManager, const NodePtr& code) : cNodeManager(nodeManager), code(toVector(code)) { }

		/**
		 * Creates a new code fragment encapsulating the given code fragments.
		 *
		 * * @param nodeManager the node manager managing the life-span of the given C AST nodes
		 * @param code the code this fragment is covering
		 */
		CCodeFragment(const SharedCNodeManager& nodeManager, const vector<NodePtr>& code) : cNodeManager(nodeManager), code(code) { }

		/**
		 * A static factory method creating a new code fragment based on the given code snippets.
		 *
		 * @param nodeManager the node manager managing the life-span of the given C AST node
		 * @param code the code snippets to be combined to a code fragment
		 */
		template<typename ... Nodes>
		static CCodeFragmentPtr createNew(const SharedCNodeManager& nodeManager, const Nodes& ... code) {
			return std::make_shared<CCodeFragment>(nodeManager, toVector<NodePtr>(code...));
		}

		/**
		 * A static factory method creating a new code fragment based on the given code and name.
		 *
		 * @param nodeManager the node manager managing the life-span of the given C AST node
		 * @param code the code snippets to be combined to a code fragment
		 */
		static CCodeFragmentPtr createNew(const SharedCNodeManager& nodeManager, const vector<NodePtr>& code) {
			return std::make_shared<CCodeFragment>(nodeManager, code);
		}

		/**
		 * Obtains a reference to the node manager managing the life cycle of the contained C AST nodes.
		 *
		 * @return a shared pointer to the requested node manager
		 */
		const SharedCNodeManager getCNodeManager() const {
			return cNodeManager;
		}

		/**
		 * Obtains a reference to the code buffer defining the body of this code fragment.
		 *
		 * @return a constant reference to the represented code body
		 */
		const vector<NodePtr>& getCode() const { return code; }

		/**
		 * Obtains a reference to the code buffer defining the body of this code fragment.
		 *
		 * @return a constant reference to the represented code body
		 */
		vector<NodePtr>& getCode() { return code; }

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
		DummyFragment(const DependencySet& dependencies = DependencySet()) : CodeFragment(dependencies) {}

		/**
		 * A static factory method creating a new dummy-code fragment based on the given name.
		 *
		 * @param name the name of the new fragment
		 */
		static CodeFragmentPtr createNew(const DependencySet& dependencies = DependencySet());

		/**
		 * Prints a dummy code fragment (nothing to print).
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			// nothing to print
			return out;
		}
	};

//	/**
//	 * A special kind of code fragment representing a include directive.
//	 */
//	class IncludeFragment : public CodeFragment {
//
//		/**
//		 * The file to be included.
//		 */
//		const string file;
//
//	public:
//
//		/**
//		 * Creates a new fragment of this type including the given file name.
//		 *
//		 * @param file the file to be included.
//		 */
//		IncludeFragment(const string& file) : CodeFragment() {}
//
//		/**
//		 * Creates a new shared instance of this code fragment including the given file.
//		 *
//		 * @param file the file to be included
//		 */
//		static IncludeFragmentPtr createNew(const string& file);
//
//		/**
//		 *
//		 */
//		virtual std::ostream& printTo(std::ostream& out) const {
//			if (file[0] == '<' || file[0] == '"') {
//				return out << "#include " << file << "\n";
//			}
//			return out << "#include <" << file << ">\n";
//		}
//
//	};


} // end namespace c_ast
} // end namespace backend
} // end namespace insieme
