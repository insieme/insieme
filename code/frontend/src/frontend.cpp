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

#include <sstream>

#include "insieme/frontend/frontend.h"

#include "insieme/frontend/converter.h"
#include "insieme/frontend/omp/omp_annotation.h"
#include "insieme/frontend/tu/ir_translation_unit.h"

#include "insieme/utils/config.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/annotations/naming.h"

#include "insieme/frontend/extensions/builtin_function_extension.h"
#include "insieme/frontend/extensions/cilk_extension.h"
#include "insieme/frontend/extensions/frontend_cleanup_extension.h"
#include "insieme/frontend/extensions/insieme_pragma_extension.h"
#include "insieme/frontend/extensions/instrumentation_region_extension.h"
#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/frontend/extensions/omp_frontend_extension.h"
#include "insieme/frontend/extensions/semantic_check_extension.h"
#include "insieme/frontend/extensions/significance_frontend_extension.h"
#include "insieme/frontend/extensions/test_pragma_extension.h"

namespace insieme {
namespace frontend {


	const unsigned ConversionSetup::DEFAULT_FLAGS = PrintDiag;

	ConversionSetup::ConversionSetup(const vector<path>& includeDirs)
	    : includeDirs(includeDirs),
	      systemHeaderSearchPath(::transform(insieme::utils::compiler::getDefaultCppIncludePaths(), [](const string& cur) { return path(cur); })),
	      standard(Auto), definitions(), interceptedNameSpacePatterns({"std::.*", "__gnu_cxx::.*", "_m_.*", "_mm_.*", "__mm_.*", "__builtin_.*"}),
	      interceptedHeaderDirs(), flags(DEFAULT_FLAGS){};


	bool ConversionSetup::isCxx(const path& file) const {
		static std::set<string> CxxExtensions({".cpp", ".cxx", ".cc", ".C"});
		return standard == Cxx03 || standard == Cxx98 || (standard == Auto && ::contains(CxxExtensions, boost::filesystem::extension(file)));
	}

	// This method calls the lambdas that were collected
	// when the flags were registered. The lambda will decide
	// if the extension is kept in the list or drops out.
	// In a second pass the prerequisites are checked
	// and an error message may be printed.
	void ConversionJob::frontendExtensionInit() {
		// reset extensions list in case extensions changed, we do not want duplicates
		extensionList.clear();

		// register all extensions that should be registered
		for(auto it = extensions.begin(); it != extensions.end(); ++it) {
			if(it->second(*this)) {
				extensionList.push_back(it->first);

				// collect the kidnapped headers and add them to the list
				for(auto kidnappedHeader : it->first->getKidnappedHeaderList()) {
					addSystemHeadersDirectory(kidnappedHeader);
				}

				// add additional include directories
				for(auto includeDir : it->first->getIncludeDirList()) {
					addIncludeDirectory(includeDir);
				}
			}
		}

		// check pre-requisites
		for(auto ext : getExtensions()) {
			auto isMissing = ext->isPrerequisiteMissing(*this);
			if(isMissing) {
				std::cerr << "Prerequisite for an extension is missing:\n" << *isMissing << std::endl;
				assert_fail() << "Aborting due to frontend extension prerequisite error.";
			}
		}
	}

	void ConversionSetup::setStandard(const Standard& standard) {
		this->standard = standard;
	}


	tu::IRTranslationUnit ConversionJob::toIRTranslationUnit(core::NodeManager& manager) {
		// extension initialization
		frontendExtensionInit();

		// convert files to translation units
		auto units = ::transform(files, [&](const path& file) -> tu::IRTranslationUnit {
			auto res = convert(manager, file, *this);

			// maybe a visitor wants to manipulate the IR program
			for(auto extension : getExtensions())
				res = extension->IRVisit(res);

			// done
			return res;
		});

		// merge the translation units
		auto singleTu = tu::merge(manager, tu::merge(manager, libs), tu::merge(manager, units));

		// forward the C++ flag
		singleTu.setCXX(this->isCxx());
		return singleTu;
	}

	core::ProgramPtr ConversionJob::execute(core::NodeManager& manager, core::ProgramPtr& program) {
		// strip of OMP annotation since those may contain references to local nodes
		core::visitDepthFirstOnce(program, [](const core::NodePtr& cur) { cur->remAnnotation(omp::BaseAnnotation::KEY); });

		// maybe a visitor wants to manipulate the IR translation unit
		for(auto extension : getExtensions()) {
			program = extension->IRVisit(program);
		}

		// return instance within global manager
		return core::transform::utils::migrate(program, manager);
	}

	core::ProgramPtr ConversionJob::execute(core::NodeManager& manager) {
		// extension initialization
		frontendExtensionInit();

		// create a temporary manager
		core::NodeManager& tmpMgr = manager; // for performance we are just using the same manager
		//		core::NodeManager tmpMgr;		// not: due to the relevance of class-info-annotations no chaining of managers is allowed here

		// load and merge all files into a single translation unit
		auto unit = toIRTranslationUnit(tmpMgr);
		core::ProgramPtr res;

		if(unit.getEntryPoints().size() > 1) {
			res = tu::resolveEntryPoints(tmpMgr, unit);
		} else {
			res = tu::toProgram(tmpMgr, unit);
		}

		return execute(manager, res);
	}

	core::ProgramPtr ConversionJob::execute(core::NodeManager& manager, bool fullApp) {
		// extension initialization
		frontendExtensionInit();

		// create a temporary manager
		core::NodeManager& tmpMgr = manager; // for performance we are just using the same manager
		//		core::NodeManager tmpMgr;		// not: due to the relevance of class-info-annotations no chaining of managers is allowed here

		// load and merge all files into a single translation unit
		auto unit = toIRTranslationUnit(tmpMgr);

		// convert units to a single program
		auto res = (fullApp) ? tu::toProgram(tmpMgr, unit) : tu::resolveEntryPoints(tmpMgr, unit);

		return execute(manager, res);
	}

	bool ConversionJob::isCxx() const {
		if(getStandard() == Standard::Cxx03 || getStandard() == Cxx98 || getStandard() == Standard::Cxx11) { return true; }
		if(getStandard() == Standard::C99) { return false; }

		bool cppFile = any(files, [&](const path& cur) { return static_cast<const ConversionSetup&>(*this).isCxx(cur); });

		bool cppLibs = any(libs, [&](const tu::IRTranslationUnit& tu) -> bool { return tu.isCXX(); });

		return cppFile || cppLibs;
	}

	void ConversionJob::registerExtensionFlags(boost::program_options::options_description& options) {
		// register all plugins

		// interceptor wants to be first
		registerFrontendExtension<extensions::InterceptorExtension>(options);

		registerFrontendExtension<extensions::BuiltinFunctionExtension>(options);
		registerFrontendExtension<extensions::InstrumentationRegionExtension>(options);
		registerFrontendExtension<extensions::TestPragmaExtension>(options);
		registerFrontendExtension<extensions::InsiemePragmaExtension>(options);
		registerFrontendExtension<extensions::OmpFrontendExtension>(options);
		registerFrontendExtension<extensions::SignificanceFrontendExtension>(options);
		registerFrontendExtension<extensions::CilkFrontendExtension>(options);

		// FE cleanup wants to be second-last
		registerFrontendExtension<extensions::FrontendCleanupExtension>(options);
	}

	std::ostream& ConversionJob::printTo(std::ostream& out) const {
		out << "~~~~~~CONVERSION SETUP~~~~~~\n";
		out << "input files: \n" << files << std::endl;
		out << "flags: \n"
		    << "PrintDiag " << hasOption(ConversionSetup::PrintDiag) << "\n"
		    << "WinCrossCompile " << hasOption(ConversionSetup::WinCrossCompile) << "\n"
		    << "TAG_MPI " << hasOption(ConversionSetup::TAG_MPI) << "\n"
		    << "ProgressBar " << hasOption(ConversionSetup::ProgressBar) << "\n"
		    << "NoWarnings " << hasOption(ConversionSetup::NoWarnings) << "\n"
		    << "NoDefaultExtensions " << hasOption(ConversionSetup::NoDefaultExtensions) << "\n" << std::endl;
		out << "interceptions: \n" << getInterceptedNameSpacePatterns() << std::endl;
		out << "crosscompilation dir: \n" << getCrossCompilationSystemHeadersDir() << std::endl;
		out << "include dirs: \n" << getIncludeDirectories() << std::endl;
		out << "definitions: \n" << getDefinitions() << std::endl;
		out << "libraries: \n" << libs << std::endl;
		out << "standard: \n" << getStandard() << std::endl;
		out << "number of registered extensions: \n" << getExtensions().size() << std::endl;
		out << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		return out;
	}

} // end namespace frontend
} // end namespace insieme
