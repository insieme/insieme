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
#include <memory>

#include <boost/utility.hpp>

#include "insieme/core/forward_decls.h"

#include "insieme/analysis/features/feature_value.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace analysis {
namespace features {

	using std::string;

	class Feature;
	typedef std::shared_ptr<Feature> FeaturePtr;

	/**
	 * An abstract base class for all supported features. Features may be atomic, e.g.
	 * the number of arithmetic operations within a code section or the estimated size
	 * of a loop. Alternatively, features may be derived or composed. Derived features
	 * may for instance be the sum / avg / normalized value of other features. Composed
	 * features may be the combination of other features.
	 */
	class Feature : public utils::Printable {

		/**
		 * A flag determining whether this feature is an atomic or composed feature.
		 */
		bool atomic;

		/**
		 * The unique name of this feature. Features are identified by their name.
		 */
		string name;

		/**
		 * A short description of this feature.
		 */
		string description;

		/**
		 * The type of value used to represent instances of this feature.
		 */
		TypePtr valueType;

	public:

		/**
		 * Creates a new feature based on the given properties.
		 *
		 * @param atomic a flag indicating whether this is an atomic or derived feature
		 * @param name the unique name of this feature
		 * @param desc a short description of this feature
		 * @param valueType the type of value used to represent instances of this feature
		 */
		Feature(bool atomic, const string& name, const string& desc, const TypePtr& valueType)
			: atomic(atomic), name(name), description(desc), valueType(valueType) {}

		/**
		 * A virtual destructor for this virtual base class.
		 */
		virtual ~Feature() {}

		/**
		 * The function to be used for extracting a feature from the given code. By default,
		 * the feature value will be cached within an annotation of the code. Concrete feature
		 * implementations may override this behavior if requested. Internally, deriveFrom is
		 * invoked.
		 *
		 * @param code the code section from which the value of this feature should be extracted
		 * @return the value of this feature within the given code section
		 */
		virtual Value extractFrom(const core::NodePtr& code) const;

		/**
		 * Determines whether this feature is an atomic or a derived/composed feature.
		 */
		bool isAtomic() const {
			return atomic;
		}

		/**
		 * Obtains the name of this feature.
		 */
		const string& getName() const {
			return name;
		}

		/**
		 * Obtains a description for this feature.
		 */
		const string& getDescription() const {
			return description;
		}

		/**
		 * Obtains the value type of this feature.
		 */
		const TypePtr& getValueType() const {
			return valueType;
		}

		/**
		 * Compares this and another feature for equality. Two features are equivalent if
		 * the name is the same.
		 */
		bool operator==(const Feature& other) const {
			return this == &other || name == other.name;
		}

		/**
		 * Compares this and another feature for inequality. Two features are equivalent if
		 * the name is the same.
		 */
		bool operator!=(const Feature& other) const {
			return !(*this == other);
		}

		/**
		 * Tests whether this features name is lexicographically smaller than the name of the
		 * given feature.
		 */
		bool operator<(const Feature& other) const {
			return name < other.name;
		}

		/**
		 * Prints a string-representation of this feature to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << name;
		}

	protected:

		/**
		 * The internal implementation of the extraction process. This function should just compute
		 * the value of this feature for the given code section.
		 *
		 * @param code the code section for which the feature value should be evaluated
		 * @return the value of this feature within this code section
		 */
		virtual Value evaluateFor(const core::NodePtr& code) const =0;

	};

	/**
	 * A utility creating feature pointer instances.
	 */
	template<typename T, typename ... P>
	FeaturePtr make_feature(const P& ... params) {
		return std::make_shared<T>(params...);
	}


	vector<Value> extractFrom(const core::NodePtr& node, const vector<FeaturePtr>& features);


} // end namespace features
} // end namespace analysis
} // end namespace insieme
