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
	template <typename T>
	class Pointer;
	typedef Pointer<const Node> NodePtr;
}
namespace backend {

	class PostProcessor;
	typedef std::shared_ptr<PostProcessor> PostProcessorPtr;

	namespace c_ast {

		class CodeFragmentManager : boost::noncopyable {
			SharedCNodeManager cNodeManager;

			vector<CodeFragment*> fragments;

			/**
			 * A list of special, named code fragments which can be accessed
			 * via their names. All fragments within this map have to be part
			 * of the fragments vector, hence managed by this manager.
			 */
			std::map<string, CodeFragment*> namedFragments;

		  public:
			CodeFragmentManager(const SharedCNodeManager& cNodeManager) : cNodeManager(cNodeManager) {}

			~CodeFragmentManager();

			template <typename T, typename... E>
			Ptr<T> create(E... args) {
				T* res = new T(args...);
				fragments.push_back(res);
				return Ptr<T>(res);
			}

			const SharedCNodeManager& getNodeManager() const {
				return cNodeManager;
			}

			static SharedCodeFragmentManager createShared() {
				return std::make_shared<CodeFragmentManager>(CNodeManager::createShared());
			}

			CodeFragmentPtr getFragment(const string& name) {
				auto pos = namedFragments.find(name);
				if(pos != namedFragments.end()) { return pos->second; }
				return 0;
			}

			void bindFragment(const string& name, const CodeFragmentPtr& fragment) {
				assert_true(contains(fragments, fragment.ptr)) << "Cannot bind fragment not maintained by this manager!";
				__insieme_unused auto res = namedFragments.insert(std::make_pair(name, fragment.ptr));
				assert_true(res.second) << "Name already in use!!";
			}
		};


		/**
		 * A class representing a C based target code.
		 */
		class CCode : public TargetCode {
			/**
			 * The fragment manager managing the life cycle of the maintained fragments.
			 */
			const SharedCodeFragmentManager fragmentManager;

			/**
			 * The list of code fragments in the correct order forming this program. To include all
			 * required fragments in to proper order, the transitive closure of the dependency and
			 * requirement relation between fragments and a topological order on the first relation
			 * can be computed using the getOrderedClosure() function.
			 */
			const vector<CodeFragmentPtr> fragments;

		  public:
			/**
			 * Creates a new C Code instance representing a conversion from the given
			 * source node to the given target code fragment.
			 *
			 * @param manager the fragment manager maintaining the given code fragment and all its dependencies and requirements
			 * @param source the IR node this code has been generated from
			 * @param code the root element of the resulting target code fragment (closure will be automatically computed)
			 */
			CCode(const SharedCodeFragmentManager& manager, const core::NodePtr& source, const CodeFragmentPtr& root);

			/**
			 * Creates a new C Code instance representing a conversion from the given
			 * source node to the transitive closure of the given fragments.
			 *
			 * @param manager the fragment manager maintaining the given code fragment and all its dependencies and requirements
			 * @param source the IR node this code has been generated from
			 * @param fragments the code fragments this code is consisting of
			 */
			CCode(const SharedCodeFragmentManager& manager, const core::NodePtr& source, const vector<CodeFragmentPtr>& fragments);

			/**
			 * Creates a new C code instance representing a translation of the given source to the given code fragments.
			 *
			 * @param source the source of the translation, hence the internal IR representation.
			 * @param fragments the code fragments the resulting code should be consisting of
			 */
			static CCodePtr createNew(const SharedCodeFragmentManager& manager, const core::NodePtr& source, const vector<CodeFragmentPtr>& fragments) {
				return std::make_shared<CCode>(manager, source, fragments);
			}

			/**
			 * Allows this code fragment to be printed to some output stream according
			 * to the Printable interface.
			 */
			virtual std::ostream& printTo(std::ostream& out) const;

			/**
			 * List of fragments which compose this code
			 */
			const vector<CodeFragmentPtr>& getFragments() const { return fragments; }
		};

		/**
		 * An abstract base class for various kinds of specialized code fragments. This base class
		 * defines an interface for code fragments to be handled uniformly.
		 */
		class CodeFragment : public utils::VirtualPrintable {
			/**
			 * The set of code fragments this fragment is depending on. The dependencies should form
			 * a DAG and a topological order of this DAG is used when generating the resulting code.
			 */
			FragmentSet dependencies;

			/**
			 * Additional code fragments which have to be present within the code, but this fragment is
			 * not depending on it.
			 */
			FragmentSet requirements;

			/**
			 * The list of files / headers to be included by the resulting target code.
			 */
			std::set<string> includes;

		  public:
			/**
			 * A default constructor creating a code fragment without any dependencies.
			 */
			CodeFragment() : dependencies(), includes() {}

			/**
			 * A constructor for a code fragment initializing its internal dependencies base
			 * on the given dependency vector.
			 *
			 * @param dependencies the code fragments this fragment is depending on - default: empty list
			 */
			CodeFragment(const FragmentSet& dependencies) : dependencies(dependencies) {}

			/**
			 * A copy constructor checking for cyclic dependencies.
			 */
			CodeFragment(const CodeFragment& other);

			/**
			 * A virtual destructor to support proper sub-type handling.
			 */
			virtual ~CodeFragment(){};

			/**
			 * A implementation of the assignment operator checking for cycles.
			 */
			CodeFragment& operator=(const CodeFragment& other);

			/**
			 * Adds a dependency to this fragment.
			 *
			 * @param fragment the code fragment to be depending on
			 */
			virtual void addDependency(const CodeFragmentPtr& fragment);

			/**
			 * Adds the fragment pointers present within the given container to the
			 * set of dependencies defined for this code fragment.
			 *
			 * @param fragments the list of fragments to be depending on
			 */
			template <typename Container>
			void addDependencies(const Container& fragments) {
				// just add all dependencies (the add dependency is filtering null pointer)
				for_each(fragments, [&](const CodeFragmentPtr& cur) { this->addDependency(cur); });
			}

			/**
			 * Removes a dependency to the given fragment.
			 *
			 * @param fragment the code fragment dependency to be eliminated
			 * @return true if the dependency was present, false otherwise
			 */
			bool remDependency(const CodeFragmentPtr& fragment);

			/**
			 * Obtains the list of all code fragments this fragment is depending on, hence, all fragments
			 * which have to occur before this fragment within the resulting code.
			 *
			 * @return a set of all fragments depending on.
			 */
			const FragmentSet& getDependencies() const {
				return dependencies;
			};

			/**
			 * Checks whether this code fragment is directly or indirectly depending on the given
			 * code fragment.
			 */
			bool isDependingOn(const CodeFragmentPtr& fragment);

			/**
			 * Adds a requirement to this fragment.
			 *
			 * @param fragment the code fragment required by this fragment to be present somewhere within the resulting code
			 */
			virtual void addRequirement(const CodeFragmentPtr& fragment);

			/**
			 * Removes a requirement from this fragment.
			 *
			 * @param fragment the code fragment requirement to be eliminated
			 * @return true if a requirement was removed, false otherwise
			 */
			virtual bool remRequirement(const CodeFragmentPtr& fragment);

			/**
			 * Adds the fragment pointers present within the given container to the
			 * set of requirements defined for this code fragment.
			 *
			 * @param fragments the list of fragments to be required
			 */
			template <typename Container>
			void addRequirements(const Container& fragments) {
				// just add all dependencies (the add dependency is filtering null pointer)
				for_each(fragments, [&](const CodeFragmentPtr& cur) { this->addRequirement(cur); });
			}

			/**
			 * Obtains the set of code fragments this fragment is not depending on considering the order
			 * within the resulting source file, but which are still required somewhere within the code.
			 *
			 * @return a set of all code fragments this fragment is requireing, yet not depending on
			 */
			const FragmentSet& getRequirements() const {
				return requirements;
			};

			/**
			 * Obtains the set of includes currently required by this code fragment.
			 *
			 * @return the list of includes currently required by this code fragment.
			 */
			const std::set<string>& getIncludes() const {
				return includes;
			};

			/**
			 * Add a new include file to the set of required includes.
			 *
			 * @param include the file to be included
			 */
			virtual void addInclude(const string& include) {
				// add include
				includes.insert(include);
			}

			/**
			 * Adds a list of includes to the includes required by this code fragment.
			 *
			 * @param newIncludes the includes to be added
			 */
			template <typename Container>
			void addIncludes(const Container& newIncludes) {
				// insert all includes
				includes.insert(newIncludes.begin(), newIncludes.end());
			}
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
			SharedCNodeManager cNodeManager;

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
			CCodeFragment(const SharedCNodeManager& nodeManager, const NodePtr& code) : cNodeManager(nodeManager), code(toVector(code)) {}

			/**
			 * Creates a new code fragment encapsulating the given code fragments.
			 *
			 * * @param nodeManager the node manager managing the life-span of the given C AST nodes
			 * @param code the code this fragment is covering
			 */
			CCodeFragment(const SharedCNodeManager& nodeManager, const vector<NodePtr>& code) : cNodeManager(nodeManager), code(code) {}

			/**
			 * A static factory method creating a new code fragment based on the given code and name.
			 *
			 * @param manager the node manager managing the life-span of the given C AST node and the resulting fragment
			 * @param code the code snippets to be combined to a code fragment
			 */
			static CCodeFragmentPtr createNew(const SharedCodeFragmentManager& manager, const vector<NodePtr>& code) {
				return manager->create<CCodeFragment>(manager->getNodeManager(), code);
			}

			/**
			 * A static factory method creating a new code fragment based on the given code snippets.
			 *
			 * @param manager the node manager managing the life-span of the given C AST node and the resulting fragment
			 * @param code the code snippets to be combined to a code fragment
			 */
			template <typename... Nodes>
			static CCodeFragmentPtr createNew(const SharedCodeFragmentManager& manager, const Nodes&... code) {
				return createNew(manager, toVector<NodePtr>(code...));
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
			const vector<NodePtr>& getCode() const {
				return code;
			}

			/**
			 * Obtains a reference to the code buffer defining the body of this code fragment.
			 *
			 * @return a constant reference to the represented code body
			 */
			vector<NodePtr>& getCode() {
				return code;
			}

			/**
			 * Appends the given C-Code construct to the list of constructs covered by this fragment.
			 *
			 * @param code the construct to be appended
			 */
			void appendCode(const NodePtr& code) {
				if(code) { this->code.push_back(code); }
			}

			/**
			 * Prints the code covered by this fragment to the given output stream.
			 *
			 * @param out the stream to be printed to
			 * @return the out reference passed as an argument
			 */
			virtual std::ostream& printTo(std::ostream& out) const;

			/**
			 * Instructs this C-code fragment to apply the given post-processor to
			 * itself. This will change the internal code structure.
			 *
			 * @param processor the post-processor to be applied on this fragment.
			 */
			void apply(const PostProcessorPtr& processor);
		};

		/**
		 * Obtains a the list of fragments containing all the code fragments within the transitive
		 * closure of the dependency and requirement relation ordered according to a topological
		 * order of the dependency relation. Hence, code fragments required by an element X are
		 * located before the element X within the resulting list.
		 *
		 * @param fragments the seed elements for the computation of the transitive closure
		 * @return the requested closure
		 */
		vector<CodeFragmentPtr> getOrderedClosure(const vector<CodeFragmentPtr>& fragments);


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
			DummyFragment(const FragmentSet& dependencies = FragmentSet()) : CodeFragment(dependencies) {}

			/**
			 * A static factory method creating a new dummy-code fragment based on the given name.
			 *
			 * @param manager the node manager managing the life-span of the resulting fragment
			 * @param name the name of the new fragment
			 */
			static CodeFragmentPtr createNew(const SharedCodeFragmentManager& manager, const FragmentSet& dependencies = FragmentSet());

			/**
			 * Prohibit the utilization of a dummy code fragment as a include fragment.
			 */
			virtual void addInclude(const string& include) {
				assert_fail() << "Unsupported Operation - use an IncludeFragment instead!";
				abort();
			}

			/**
			 * Prints a dummy code fragment (nothing to print).
			 */
			virtual std::ostream& printTo(std::ostream& out) const {
				// nothing to print
				return out;
			}
		};


		/**
		 * A special kind of code fragment used for representing header file dependencies. This code fragment will
		 * only create an include file entry.
		 */
		class IncludeFragment : public CodeFragment {
		  public:
			/**
			 * Creates a new include fragment including the given file.
			 *
			 * @param include the file to be included.
			 */
			IncludeFragment(const string& include) {
				addInclude(include);
			}

			/**
			 * A static factory method creating a new dummy-code fragment based on the given name.
			 *
			 * @param manager the node manager managing the life-span of the resulting fragment
			 * @param include the name of the file to be included
			 */
			static CodeFragmentPtr createNew(const SharedCodeFragmentManager& manager, const string& include);

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
