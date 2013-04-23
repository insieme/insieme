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

#include "insieme/utils/logging.h"

#include "insieme/analysis/features/code_feature_catalog.h"
#include "insieme/analysis/features/cache_feature_catalog.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/frontend/frontend.h"

#include "insieme/driver/cmd/options.h"

	/**
	 * This executable is accepting some program code and extracting a set of
	 * feature from the given program.
	 */

	using namespace std;
	using namespace insieme;
	namespace ft = insieme::analysis::features;
	namespace cmd = insieme::driver::cmd;

	ft::FeatureCatalog getFeatureCatalog() {
		ft::FeatureCatalog catalog;
		catalog.addAll(ft::getFullCodeFeatureCatalog());
		catalog.addAll(ft::getFullCacheFeatureCatalog());
		return catalog;
	}

	vector<ft::FeaturePtr> selectFeatureList(const ft::FeatureCatalog& catalog) {

		// assemble list of features to be used
		vector<ft::FeaturePtr> features;

		// add all features from the catalog
//		for_each(catalog, [&](const std::pair<string, ft::FeaturePtr>& cur) {
//			features.push_back(cur.second);
//		});

		// Optional: select features individually

		// any read
		features.push_back(catalog.getFeature("SCF_IO_NUM_any_read_OPs_static"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_any_read_OPs_real"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_any_write_OPs_static"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_any_write_OPs_real"));

		// scalar read
		features.push_back(catalog.getFeature("SCF_IO_NUM_scalar_read_OPs_static"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_scalar_read_OPs_real"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_scalar_write_OPs_static"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_scalar_write_OPs_real"));

		// scalar read
		features.push_back(catalog.getFeature("SCF_IO_NUM_vector_read_OPs_static"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_vector_read_OPs_real"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_vector_write_OPs_static"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_vector_write_OPs_real"));

		// array read
		features.push_back(catalog.getFeature("SCF_IO_NUM_array_read_OPs_static"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_array_read_OPs_real"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_array_write_OPs_static"));
		features.push_back(catalog.getFeature("SCF_IO_NUM_array_write_OPs_real"));

		return features;
	}

	/**
	 * The Insieme Optimizer entry point.
	 */
	int main(int argc, char** argv) {
		core::NodeManager manager;

		// set up logger
		Logger::get(cerr, LevelSpec<>::loggingLevelFromStr("ERROR"));

		cout << " --- Insieme Code Feature Extractor ---- \n";

		// process handle command line arguments
		cmd::Options options = cmd::Options::parse(argc, argv);
		if (!options.valid) return (options.help)?0:1;


		// loading features
		analysis::features::FeatureCatalog catalog = getFeatureCatalog();
		cout << "Supporting " << catalog.size() << " features.\n";

		// obtain list of features
		vector<ft::FeaturePtr> features = selectFeatureList(catalog);
		cout << "Selected " << features.size() << " features.\n";


		// load code fragment
		cout << "Loading input files ..." << endl;
		auto code = options.job.execute(manager);

		// print code fragment:
		//cout << "Processing Code Fragment: \n" << core::printer::PrettyPrinter(code) << "\n\n";

		// extract all features
		vector<ft::Value> values = analysis::features::extractFrom(code, features);

		// extract features
		for(std::size_t i = 0; i<features.size(); i++) {
			cout << format("%-60s %20s\n", features[i]->getName(), toString(values[i]));
		}

		// done
		cout << "Done!" << endl;
		return 0;

	}

