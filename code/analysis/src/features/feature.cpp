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

#include "insieme/analysis/features/feature.h"

#include <map>
#include <functional>

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_node_annotation.h"

namespace insieme {
namespace analysis {
namespace features {

	namespace {

		/**
		 * An annotation used to cache feature instances within IR annotations.
		 */
		class FeatureStore : public core::NodeAnnotation {

			static const string NAME;

			static const utils::StringKey<FeatureStore> KEY;

			std::map<string, Value> values;

		public:

			virtual const utils::AnnotationKey* getKey() const {
				return &KEY;
			}

			/**
			 * Requests the name of this annotation. The name should be a constant class member.
			 *
			 * @return the name of this annotation type
			 */
			virtual const std::string& getAnnotationName() const {
				return NAME;
			}

			static Value getValue(const Feature& feature, const core::NodePtr& code, const std::function<Value()>& eval) {

				// obtain feature annotation
				auto annotation = code->getAnnotation(KEY);
				if (!annotation) {
					code->addAnnotation<FeatureStore>();
					annotation = code->getAnnotation(KEY);
				}

				// extract value
				std::map<string,Value>& values = annotation->values;
				auto pos = values.find(feature.getName());
				if (pos != values.end()) {
					return pos->second;
				}

				// not present => compute
				Value res = eval();

				// check value type
				assert(feature.getValueType()->isValid(res) && "Feature extraction produced incorrect type!");

				// save and return
				values[feature.getName()] = res;
				return res;
			}

		};

		const string FeatureStore::NAME = "FeatureStore";

		const utils::StringKey<FeatureStore> FeatureStore::KEY(FeatureStore::NAME);
	}


	Value Feature::extractFrom(const core::NodePtr& code) const {
		// extract feature lazy
		return FeatureStore::getValue(*this, code, [&](){ return this->evaluateFor(code); });
	}


} //end namespace features
} //end namespace analysis
} //end namespace insieme
