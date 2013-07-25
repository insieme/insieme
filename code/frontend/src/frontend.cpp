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

#include <sstream>

#include "insieme/frontend/frontend.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/clang_config.h"
#include "insieme/frontend/omp/omp_sema.h"
#include "insieme/frontend/omp/omp_annotation.h"
#include "insieme/frontend/cilk/cilk_sema.h"
#include "insieme/frontend/ocl/ocl_host_compiler.h"

#include "insieme/frontend/tu/ir_translation_unit.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/compiler/compiler.h"

namespace insieme {
namespace frontend {


	const unsigned ConversionSetup::DEFAULT_FLAGS = PrintDiag;

	ConversionSetup::ConversionSetup(const vector<path>& includeDirs)
		: includeDirs(includeDirs),
		  stdLibIncludeDirs(::transform(insieme::utils::compiler::getDefaultCppIncludePaths(), [](const string& cur) { return path(cur); })),
		  standard(Auto),
		  definitions(),
		  flags(DEFAULT_FLAGS) {};


	bool ConversionSetup::isCxx(const path& file) const {
		static std::set<string> CxxExtensions({ ".cpp", ".cxx", ".cc", ".C" });
		return standard == Cxx03 || (standard==Auto && ::contains(CxxExtensions, boost::filesystem::extension(file)));
	}


	tu::IRTranslationUnit ConversionJob::toTranslationUnit(core::NodeManager& manager) const {

		// add definitions needed by the OpenCL frontend
		ConversionSetup setup = *this;
		if(hasOption(OpenCL)) {
			setup.addIncludeDirectory(SRC_DIR);
			setup.addIncludeDirectory(SRC_DIR "inputs");
			setup.addIncludeDirectory(SRC_DIR "../../../test/ocl/common/");  // lib_icl

			setup.setDefinition("INSIEME");
		}

		// convert files to translation units
		auto units = ::transform(files, [&](const path& file)->tu::IRTranslationUnit {
			auto res = convert(manager, file, setup);

			// apply OpenMP sema
			if (setup.hasOption(ConversionSetup::OpenMP)) {
				res = omp::applySema(res, manager);
			}

			// apply Cilk sema
			if (setup.hasOption(ConversionSetup::Cilk)) {
				res = cilk::applySema(res, manager);
			}

			// done
			return res;
		});

		// merge the translation units
		return tu::merge(manager, tu::merge(manager, libs), tu::merge(manager, units));

	}

	core::ProgramPtr ConversionJob::execute(core::NodeManager& manager, bool fullApp) const {

		// create a temporary manager
		core::NodeManager tmpMgr;		// not: due to the relevance of class-info-annotations no chaining of managers is allowed here

		// load and merge all files into a single translation unit
		auto unit = toTranslationUnit(tmpMgr);

		// convert units to a single program
		auto res = (fullApp) ? tu::toProgram(tmpMgr, unit) : tu::resolveEntryPoints(tmpMgr, unit);

		// apply OpenCL conversion
		if(hasOption(OpenCL)) {
			frontend::ocl::HostCompiler oclHostCompiler(res, *this);
			res = oclHostCompiler.compile();
		}

		// strip of OMP annotation since those may contain references to local nodes
		core::visitDepthFirstOnce(res, [](const core::NodePtr& cur) {
			cur->remAnnotation(omp::BaseAnnotation::KEY);
		});

		// return instance within global manager
		return manager.get(res);
	}

} // end namespace frontend
} // end namespace insieme
