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

#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <boost/algorithm/string/predicate.hpp>
#include <boost/program_options.hpp>
#include <boost/filesystem.hpp>

#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/ir_address.h"
#include "insieme/frontend/frontend.h"

#include "insieme/driver/handle_fetures.h"


namespace ft = insieme::analysis::features;
namespace bpo = boost::program_options;
namespace bfs = boost::filesystem;

namespace insieme {
namespace driver {

	core::NodeAddress loadCode(core::NodeManager& manager, const vector<string>& inputs, const vector<string>& includes, const vector<string>& definitions) {

		try {
			// check whether the given file is a binary file ..
			if (inputs.size() == 1u && !(boost::ends_with(inputs[0], ".c") || boost::ends_with(inputs[0], ".cpp"))) {
				// try loading the given binary file
				std::fstream in(inputs[0], std::fstream::in);
				return core::dump::binary::loadAddress(in, manager);
			}
		} catch (const core::dump::InvalidEncodingException& iee) {
			std::cerr << "Unable to decode binary input file: " << iee.what() << "\nTrying to load file using C/C++ frontend ..." << std::endl;
		}

		try {

			// use frontend to load program files
			auto job = frontend::ConversionJob(inputs, includes);
			job.setOption(frontend::ConversionJob::OpenMP);
			job.setDefinitions(definitions);
			return core::NodeAddress(job.execute(manager));

		} catch (const frontend::ClangParsingError& e) {
			std::cerr << "Unexpected error encountered: " << e.what() << std::endl;
			exit(1);
		}

		return core::NodeAddress();
	}

	vector<ft::FeaturePtr> getFeatureList() {

		// load feature catalogs
		analysis::features::FeatureCatalog catalog;
		catalog.addAll(ft::getFullCodeFeatureCatalog());
		catalog.addAll(ft::getFullCacheFeatureCatalog());

		// assemble list of features to be used
		vector<ft::FeaturePtr> features;
//		features.push_back(ft::createSimpleCodeFeature("NumLoops", "", ft::createNumForLoopSpec(ft::FeatureAggregationMode::FA_Static)));

		features.push_back(catalog.getFeature("CACHE_USAGE_64_512_2_LRU"));

		// add all features from the catalog
//		for_each(catalog, [&](const std::pair<string, ft::FeaturePtr>& cur) {
//			features.push_back(cur.second);
//		});

//		features.push_back(catalog.getFeature("SCF_NUM_any_all_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_NUM_any_all_OPs_real"));
//		features.push_back(catalog.getFeature("SCF_NUM_integer_all_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_NUM_integer_all_OPs_real"));
//		features.push_back(catalog.getFeature("SCF_NUM_real4_all_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_NUM_real4_all_OPs_real"));
//		features.push_back(catalog.getFeature("SCF_NUM_real8_all_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_NUM_real8_all_OPs_real"));
//
//		// any read
//		features.push_back(catalog.getFeature("SCF_IO_NUM_any_read_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_any_read_OPs_real"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_any_write_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_any_write_OPs_real"));
//
//		// scalar read
//		features.push_back(catalog.getFeature("SCF_IO_NUM_scalar_read_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_scalar_read_OPs_real"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_scalar_write_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_scalar_write_OPs_real"));
//
//		// scalar read
//		features.push_back(catalog.getFeature("SCF_IO_NUM_vector_read_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_vector_read_OPs_real"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_vector_write_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_vector_write_OPs_real"));
//
//		// array read
//		features.push_back(catalog.getFeature("SCF_IO_NUM_array_read_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_array_read_OPs_real"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_array_write_OPs_static"));
//		features.push_back(catalog.getFeature("SCF_IO_NUM_array_write_OPs_real"));


//		features.push_back(catalog.getFeature("SCF_NUM_any_all_OPs_polyhedral"));

		for(auto it = features.begin(); it != features.end(); ++it) {
			assert(*it && "Unset feature encountered!");
		}

		return features;
	}

} // end namespace driver
} // end namespace iniseme
