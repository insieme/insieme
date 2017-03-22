/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include <sstream>

#include "insieme/frontend/frontend.h"

#include "insieme/frontend/converter.h"
#include "insieme/frontend/omp/omp_annotation.h"
#include "insieme/frontend/utils/file_extensions.h"

#include "insieme/utils/config.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/tu/ir_translation_unit.h"

#include "insieme/frontend/extensions/builtin_function_extension.h"
#include "insieme/frontend/extensions/cilk_extension.h"
#include "insieme/frontend/extensions/frontend_cleanup_extension.h"
#include "insieme/frontend/extensions/insieme_pragma_extension.h"
#include "insieme/frontend/extensions/instrumentation_region_extension.h"
#include "insieme/frontend/extensions/intrinsic_support_extension.h"
#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/frontend/extensions/malloc_extension.h"
#include "insieme/frontend/extensions/omp_frontend_extension.h"
#include "insieme/frontend/extensions/opencl_frontend_extension.h"
#include "insieme/frontend/extensions/significance_frontend_extension.h"
#include "insieme/frontend/extensions/test_pragma_extension.h"
#include "insieme/frontend/extensions/variable_argument_list_extension.h"
#include "insieme/frontend/extensions/variable_length_array_extension.h"
#include "insieme/frontend/extensions/while_to_for_extension.h"

namespace insieme {
namespace frontend {


	const unsigned ConversionSetup::DEFAULT_FLAGS = PrintDiag;

	ConversionSetup::ConversionSetup(const vector<path>& includeDirs)
	    : includeDirs(includeDirs), standard(Auto), definitions(), interceptedHeaderDirs(), flags(DEFAULT_FLAGS){};


	bool ConversionSetup::isCxx(const path& file) const {
		if(standard == Cxx11 || standard == Cxx14) { return true; }
		if(standard == C99) { return false; }
		if(standard == Auto) { return ::contains(frontend::utils::cxxExtensions, boost::filesystem::extension(file)); }
		assert_fail() << "Unable to determine whether given language standard is C++ or not";
		return false;
	}

	vector<path>& ConversionSetup::getSystemHeaderSearchPathInternal() {
		if(!systemHeaderSearchPath.isEvaluated()) {
			systemHeaderSearchPath.setValue(::transform(insieme::utils::compiler::getDefaultCppIncludePaths(), [](const string& cur) { return path(cur); }));
		}
		return systemHeaderSearchPath;
	}

	// This method calls the lambdas that were collected
	// when the flags were registered. The lambda will decide
	// if the extension is kept in the list or drops out.
	// In a second pass the prerequisites are checked
	// and an error message may be printed.
	// also this method will collect include paths of all kinds gathered from the extensions in this ConversionJob
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

				std::stringstream ss;
				for(const auto& extPtr : getExtensions()) {
					const auto& feExt = *extPtr;
					ss << typeid(feExt).name() << "\n";
				}
				std::cerr << "Loaded extensions:\n" << ss.str() << std::endl;

				assert_fail() << "Aborting due to frontend extension prerequisite error.";
			}
		}
	}

	void ConversionSetup::setStandard(const Standard& standard) {
		this->standard = standard;
	}


	core::tu::IRTranslationUnit ConversionJob::toIRTranslationUnit(core::NodeManager& manager) {
		// extension initialization
		frontendExtensionInit();

		// convert files to translation units
		auto units = ::transform(files, [&](const path& file) -> core::tu::IRTranslationUnit {
			auto res = convert(manager, file, *this);

			// maybe a visitor wants to manipulate the IR program
			for(auto extension : getExtensions())
				res = extension->IRVisit(res);

			// done
			return res;
		});

		// merge the translation units
		auto singleTu = core::tu::merge(manager, core::tu::merge(manager, libs), core::tu::merge(manager, units));

		// forward the C++ flag
		singleTu.setCXX(this->isCxx());
		return singleTu;
	}

	core::ProgramPtr ConversionJob::applyPostProcessing(core::NodeManager& manager, core::ProgramPtr& program) const {
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
			res = core::tu::resolveEntryPoints(tmpMgr, unit);
		} else {
			res = core::tu::toProgram(tmpMgr, unit);
		}

		return applyPostProcessing(manager, res);
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
		auto res = (fullApp) ? core::tu::toProgram(tmpMgr, unit) : core::tu::resolveEntryPoints(tmpMgr, unit);

		return applyPostProcessing(manager, res);
	}

	bool ConversionJob::isCxx() const {
		bool cppFile = any(files, [&](const path& cur) { return ConversionSetup::isCxx(cur); });

		bool cppLibs = any(libs, [](const core::tu::IRTranslationUnit& tu) { return tu.isCXX(); });

		return cppFile || cppLibs;
	}

	void ConversionJob::registerExtensionFlags(boost::program_options::options_description& options) {
		// register all plugins

		// interceptor wants to be first
		registerFrontendExtension<extensions::InterceptorExtension>(options);

		// intrinsics support wants to come up front too, to get high priority for kidnapped headers
		registerFrontendExtension<extensions::IntrinsicSupportExtension>(options);

		registerFrontendExtension<extensions::BuiltinFunctionExtension>(options);
		registerFrontendExtension<extensions::InstrumentationRegionExtension>(options);
		registerFrontendExtension<extensions::TestPragmaExtension>(options);
		registerFrontendExtension<extensions::InsiemePragmaExtension>(options);
		registerFrontendExtension<extensions::WhileToForExtension>(options);
		registerFrontendExtension<extensions::OmpFrontendExtension>(options);
		registerFrontendExtension<extensions::SignificanceFrontendExtension>(options);
		registerFrontendExtension<extensions::CilkFrontendExtension>(options);
		registerFrontendExtension<extensions::MallocExtension>(options);

		registerFrontendExtension<extensions::VariableArgumentListExtension>(options);
		registerFrontendExtension<extensions::VariableLengthArrayExtension>(options);
		registerFrontendExtension<extensions::OpenCLFrontendExtension>(options);

		registerFrontendExtension<extensions::FrontendCleanupExtension>(options);
	}

	void ConversionJob::registerDefaultExtensions()	{
		boost::program_options::options_description opt;
		registerExtensionFlags(opt);
	}

	std::ostream& ConversionJob::printTo(std::ostream& out) const {
		out << "~~~~~~CONVERSION SETUP~~~~~~\n";
		out << "input files: \n" << files << std::endl;
		out << "flags: \n"
		    << "PrintDiag " << hasOption(ConversionSetup::PrintDiag) << "\n"
		    << "NoWarnings " << hasOption(ConversionSetup::NoWarnings) << "\n"
		    << "NoDefaultExtensions " << hasOption(ConversionSetup::NoDefaultExtensions) << "\n" << std::endl;
		out << "interceptions: \n" << getInterceptedHeaderDirs() << std::endl;
		out << "interceptionWhitelist: \n" << getInterceptionWhitelist() << std::endl;
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
