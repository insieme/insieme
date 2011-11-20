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

#include "insieme/analysis/features/ir_features.h"

#include <sstream>

#include "insieme/analysis/features/feature_connectors.h"

namespace insieme {
namespace analysis {
namespace features {

	namespace detail {

		string getName(const vector<FeaturePtr>& features) {
			std::stringstream ss;
			ss << "[" << join(",",features,print<deref<FeaturePtr>>()) << "]";
			return ss.str();
		}

		TypePtr getVectorType(const vector<FeaturePtr>& features) {
			vector<TypePtr> res;
			for_each(features, [&](const FeaturePtr& cur) {
				res.push_back(cur->getValueType());
			});
			return tuple(res);
		}
	}


	FeatureVector::FeatureVector(const vector<FeaturePtr>& features, const string& desc)
		: Feature(false, detail::getName(features), desc, detail::getVectorType(features)), subFeatures(features) {};


	Value FeatureVector::evaluateFor(const core::NodePtr& code) const {
		vector<Value> res;
		for_each(subFeatures, [&](const FeaturePtr& cur) {
			res.push_back(cur->extractFrom(code));
		});
		return res;
	}

} //end namespace features
} //end namespace analysis
} //end namespace insieme
