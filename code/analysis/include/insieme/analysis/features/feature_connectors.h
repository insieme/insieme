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

#include "insieme/analysis/features/feature_value.h"
#include "insieme/analysis/features/feature.h"

namespace insieme {
namespace analysis {
namespace features {

	/**
	 * A connector between multiple features aggregating the result of multiple
	 * features into a vector.
	 */
	class FeatureVector : public Feature {

		/**
		 * The list of sub-features to be aggregated.
		 */
		vector<FeaturePtr> subFeatures;

	public:

		/**
		 * Creates a new aggregated feature vector based on the given features and description.
		 *
		 * @param features the features to be aggregated
		 * @param desc the description to be attached to the resulting feature vector
		 */
		FeatureVector(const vector<FeaturePtr>& features, const string& desc = "Aggregation of features.");

	protected:

		/**
		 * Extracts the entire feature vector from the given code section.
		 */
		virtual Value evaluateFor(const core::NodePtr& code) const;
	};

} // end namespace features
} // end namespace analysis
} // end namespace insieme
