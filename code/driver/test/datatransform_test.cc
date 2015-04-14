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

#include <gtest/gtest.h>

#include "insieme/frontend/frontend.h"

#include "insieme/core/checks/full_check.h"

#include "insieme/annotations/data_annotations.h"

#include "insieme/transform/datalayout/aos_to_soa.h"

#include "insieme/utils/config.h"
#include "insieme/utils/logging.h"

#include "insieme/driver/cmd/insiemecc_options.h"

namespace fe = insieme::frontend;
namespace core = insieme::core;
namespace driver = insieme::driver;
namespace annot = insieme::annotations;
namespace dl = insieme::transform::datalayout;
using namespace insieme::utils::set;
using namespace insieme::utils::log;

TEST(DatatransformTest, SimplePragma) {
	Logger::get(std::cerr, ERROR);

	core::NodeManager manager;

	std::string srcDir = SRC_ROOT_DIR "driver/test/inputs/datatransform.c";

	// create and customize conversion job
	std::vector<std::string> args = {"compiler", srcDir};
	driver::cmd::Options options = driver::cmd::Options::parse(args);

	LOG(INFO) << "Converting input program '" << srcDir << "' to IR...";
	core::ProgramPtr program = options.job.execute(manager);
	LOG(INFO) << "Done.";

	EXPECT_EQ(&program->getNodeManager(), &manager);
	EXPECT_TRUE(manager.contains(program));

	core::printer::PrettyPrinter pp(program, core::printer::PrettyPrinter::OPTIONS_DETAIL);

	LOG(INFO) << "Printing the IR: " << pp;

	auto semantic = core::checks::check(program);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const core::checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});

	auto errors = semantic.getErrors();
	EXPECT_EQ(0u, errors.size()) ;

	bool foundAnnot = false;

	core::visitDepthFirst(program, [&](const core::DeclarationStmtPtr& decl) {
		if(decl->hasAnnotation(annot::DataTransformAnnotation::KEY)) {
			annot::DataTransformAnnotationPtr dta = decl->getAnnotation(annot::DataTransformAnnotation::KEY);

			EXPECT_EQ(0u, dta->getTilesize());
			EXPECT_TRUE(dta->isSoa());
			foundAnnot = true;
		}
	});

	EXPECT_TRUE(foundAnnot);
	core::NodePtr p(program);

	// do the transformation
	dl::AosToSoa ats(p, dl::findPragma);
	ats.transform();

	dumpPretty(p);
}
