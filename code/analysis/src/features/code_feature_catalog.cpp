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

#include "insieme/analysis/features/code_feature_catalog.h"

#include "insieme/core/ir_node.h"

namespace insieme {
namespace analysis {
namespace features {

	namespace {

		void addScalarFeatures(const core::lang::BasicGenerator& basic, FeatureCatalog& catalog) {

			// create lists of considered types
			std::map<string, vector<core::TypePtr>> types;

			types["*"] = vector<core::TypePtr>();

			types["char"] = toVector(basic.getChar());

			types["int1"] = toVector(basic.getInt1());
			types["int2"] = toVector(basic.getInt2());
			types["int4"] = toVector(basic.getInt4());
			types["int8"] = toVector(basic.getInt8());
			types["int*"] = core::convertList<core::Type>(basic.getSignedIntGroup());

			types["uint1"] = toVector(basic.getUInt1());
			types["uint2"] = toVector(basic.getUInt2());
			types["uint4"] = toVector(basic.getUInt4());
			types["uint8"] = toVector(basic.getUInt8());
			types["uint*"] = core::convertList<core::Type>(basic.getUnsignedIntGroup());

			types["integer"] = core::convertList<core::Type>(basic.getIntGroup());

			types["real4"] = toVector(basic.getFloat());
			types["real8"] = toVector(basic.getDouble());
			types["real*"] = core::convertList<core::Type>(basic.getRealGroup());

			// create lists of considered operations
			std::map<string, vector<core::ExpressionPtr>> ops;

			ops["arithmetic"] = core::convertList<core::Expression>(basic.getArithOpGroup());
			ops["comparison"] = core::convertList<core::Expression>(basic.getCompOpGroup());
			ops["bitwise"] = core::convertList<core::Expression>(basic.getBitwiseOpGroup());


			// create the actual features
			for_each(types, [&](const std::pair<string, vector<core::TypePtr>>& cur_type) {
				for_each(ops, [&](const std::pair<string, vector<core::ExpressionPtr>>& cur_ops){
//					catalog.addFeature()
				});
			});
		}

		void addRealFeatures(const core::lang::BasicGenerator& basic, FeatureCatalog& catalog) {

		}


		FeatureCatalog initCatalog() {
			// the node manager managing nodes inside the catalog
			static core::NodeManager manager;
			auto& basic = manager.getLangBasic();

			FeatureCatalog catalog;

			addScalarFeatures(basic, catalog);





			return catalog;
		}

	}

	const FeatureCatalog& getFullCodeFeatureCatalog() {
		const static FeatureCatalog catalog = initCatalog();
		return catalog;
	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme
