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

#include <memory>

#include "insieme/core/ir_node.h"
#include "insieme/utils/printable.h"

#include "insieme/backend/addon.h"

#include "insieme/backend/backend_config.h"

/**
 * This header file is defining the general interface to be implemented
 * by any backend implementation.
 *
 * A backend within the insieme compiler environment is capable of transforming
 * a given INSPIRE program into some executable target code. The format and programming
 * language remains undefined.
 */

namespace insieme {
namespace backend {

	/**
	 * An abstract class defining an interface for the resulting target code. While
	 * the internal organization remains back-end specific, the basic interaction
	 * with the produced code is standardized by this interface.
	 */
	class TargetCode : public utils::Printable {
		/**
		 * The INSPIRE program fragment this target code has been generated from.
		 */
		const core::NodePtr source;

	  protected:
		/**
		 * A protected constructor for this class to be used by derived versions.
		 *
		 * @param source the INSPIRE program for which this target code representation has been generated.
		 */
		TargetCode(const core::NodePtr& source) : source(source) {}

	  public:
		/**
		 * A virtual default constructor allowing subclasses to be properly destroyed.
		 */
		virtual ~TargetCode(){};

		/**
		 * Obtains the INSPIRE program code this target code is based on.
		 */
		const core::NodePtr& getSource() const {
			return source;
		}

		/**
		 * Requests this target code implementation to print the code to
		 * the given output stream.
		 *
		 * Implementations of this pure virtual function have to use their
		 * internal program representation to conduct the requested operation.
		 * Multiple (parallel) invocations of this function have to be supported.
		 *
		 * @param out the stream to be printed to.
		 * @return a reference to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const = 0;
	};

	/**
	 * A definition of the pointer type to be used for handling
	 * polymorphic objects of the abstract target code class.
	 */
	typedef std::shared_ptr<TargetCode> TargetCodePtr;


	/**
	 * The abstract base class for any kind of compiler back-end. A back-end
	 * may have may have an arbitrary, implementation dependent set of configuration
	 * flags. The only common requirement is to offer a converter function
	 * which is translating the internal INSPIRE representation of a program to
	 * some implementation specific target code.
	 */
	class Backend {
		/**
		 * The list of installed Add-Ons.
		 */
		vector<AddOnPtr> addons;

		/**
		 * This backend's configuration.
		 */
		BackendConfigPtr config;

	  public:
		/**
		 * Creates a new backend instance based on an optional list of add-ons to
		 * be installed within this backend.
		 */
		Backend(const vector<AddOnPtr>& addons = std::vector<AddOnPtr>(), const BackendConfigPtr& config = std::make_shared<BackendConfig>())
		    : addons(addons), config(config) {}

		/**
		 * A virtual default constructor allowing subclasses to be properly destroyed.
		 */
		virtual ~Backend(){};

		/**
		 * Installs an additional Add-On within this backend instance.
		 *
		 * @param addon the AddOn to be installed.
		 */
		void addAddOn(const AddOnPtr& addon) {
			addons.push_back(addon);
		}

		/**
		 * Creates and installs an additional Add-On within this backend instance.
		 *
		 * @tparam A the type of the add-on to be installed
		 * @tparam T the types of parameters required for the constructor of A
		 * @param addon the AddOn to be installed.
		 */
		template <typename A, typename... T>
		void addAddOn(T... args) {
			addons.push_back(makeAddOn<A>(args...));
		}

		/**
		 * Installs default addons shared by most backends
		 */
		void addDefaultAddons();

		/**
		 * The method performing the actual conversion of the given program into
		 * a corresponding target code fragment.
		 *
		 * The method is processing the given program and producing some implementation
		 * specific target code. Multiple (parallel) invocations of this function have to
		 * be supported.
		 */
		TargetCodePtr convert(const core::NodePtr& program) const {
			Converter converter = buildConverter(program->getNodeManager());
			installAddons(converter);
			return converter.convert(program);
		}

		/**
		 * Obtains mutable access to this backend's configuration.
		 */
		BackendConfig& getConfiguration() {
			return *config;
		}

		/**
		 * Obtains constant access to this backend's configuration.
		 */
		const BackendConfig& getConfiguration() const {
			return *config;
		}

	  protected:
		// ---------------- to be implemented by sub-classes -----------------

		/**
		 * This member function needs to be implemented by sub-types by constructing
		 * a converter instance ready to convert input codes according to the corresponding
		 * backend specification - not considering AddOns.
		 */
		virtual Converter buildConverter(core::NodeManager& manager) const = 0;

	  private:
		/**
		 * Installs all Add-Ons defined for this backend instance within the given converter.
		 */
		void installAddons(Converter& converter) const {
			for(auto cur : addons) {
				cur->installOn(converter);
			}
		}
	};

	/**
	 * A definition of the pointer type to be used for handling
	 * polymorphic objects of the abstract backend class.
	 */
	typedef std::shared_ptr<Backend> BackendPtr;


} // end: namespace backend
} // end: namespace insieme
