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
#include "insieme/utils/config.h"
#include "insieme/frontend/omp/omp_sema.h"
#include "insieme/frontend/omp/omp_annotation.h"
#include "insieme/frontend/cilk/cilk_sema.h"
#include "insieme/frontend/ocl/ocl_host_compiler.h"

#include "insieme/frontend/tu/ir_translation_unit.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/compiler/compiler.h"

#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/annotations/naming.h"

#include "insieme/frontend/extensions/cpp11_extension.h"
#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/frontend/extensions/variadic_arguments_extension.h"
#include "insieme/frontend/extensions/asm_extension.h"
#include "insieme/frontend/extensions/cpp_refs.h"
#include "insieme/frontend/extensions/frontend_cleanup.h"
#include "insieme/frontend/extensions/ocl_host_extension.h"
#include "insieme/frontend/extensions/semantic_check_extension.h"
#include "insieme/frontend/extensions/builtin_function_extension.h"
#include "insieme/frontend/extensions/gemsclaim_extension.h"

namespace insieme {
namespace frontend {


	const unsigned ConversionSetup::DEFAULT_FLAGS = PrintDiag;

	ConversionSetup::ConversionSetup(const vector<path>& includeDirs)
		: includeDirs(includeDirs),
		  systemHeaderSearchPath(::transform(insieme::utils::compiler::getDefaultCppIncludePaths(), [](const string& cur) { return path(cur); })),
		  standard(Auto),
		  definitions(),
		  interceptions( { "std::.*", "__gnu_cxx::.*", "_m_.*", "_mm_.*", "__mm_.*", "__builtin_.*" } ),
		  interceptedHeaderDirs(),
		  flags(DEFAULT_FLAGS) {
    };


	bool ConversionSetup::isCxx(const path& file) const {
		static std::set<string> CxxExtensions({ ".cpp", ".cxx", ".cc", ".C" });
		return standard == Cxx03 || (standard==Auto && ::contains(CxxExtensions, boost::filesystem::extension(file)));
	}

    //register frontend plugins
    void ConversionSetup::frontendPluginInit() {
        registerFrontendPlugin<extensions::InterceptorPlugin>(getInterceptions());
        registerFrontendPlugin<VariadicArgumentsPlugin>();
        registerFrontendPlugin<extensions::ASMExtension>();
        registerFrontendPlugin<CppRefsCleanup>();   //FIXME: make it only if cpp
        registerFrontendPlugin<extensions::BuiltinFunctionExtension>();

        if(hasOption(ConversionJob::GemCrossCompile)) {
            registerFrontendPlugin<GemsclaimPlugin>();
			setDefinition("_GEM");
        }

        if (hasOption(ConversionSetup::StrictSemanticChecks)) {
            registerFrontendPlugin<extensions::SemanticCheckPlugin>();
        }

        if(flags & OpenCL) {
        	registerFrontendPlugin<extensions::OclHostPlugin>(includeDirs);
		}
       
        if(flags & icl_lib) {
        	registerFrontendPlugin<extensions::IclHostPlugin>(includeDirs);
		}

      	registerFrontendPlugin<FrontendCleanup>();

	    for(auto plugin : getPlugins()) {
            for(auto kidnappedHeader : plugin->getKidnappedHeaderList()) {
                addSystemHeadersDirectory(kidnappedHeader);
            }
        }

    }

    void ConversionSetup::setStandard(const Standard& standard) {
        this->standard = standard;
        if(standard == Cxx11)
                registerFrontendPlugin<extensions::Cpp11Plugin>();
    }


	tu::IRTranslationUnit ConversionJob::toIRTranslationUnit(core::NodeManager& manager) const {
	    ConversionSetup setup = *this;

		// plugin initialization
		setup.frontendPluginInit();

		// add definitions needed by the OpenCL frontend
		if(hasOption(OpenCL)) {
			setup.addIncludeDirectory(CLANG_SRC_DIR);
			setup.addIncludeDirectory(CLANG_SRC_DIR "inputs");
			setup.addIncludeDirectory(CLANG_SRC_DIR "../../../test/ocl/common/");  // lib_icl

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

			//FIXME: who takes care of applying MPI sema/OCL

            // maybe a visitor wants to manipulate the IR program
            for(auto plugin : setup.getPlugins())
                res = plugin->IRVisit(res);

			// done
			return res;
		});

		// merge the translation units
		auto singleTu = tu::merge(manager, tu::merge(manager, libs), tu::merge(manager, units));

		// forward the C++ flag
		singleTu.setCXX(this->isCxx());
		return singleTu;
	}

	core::ProgramPtr ConversionJob::execute(core::NodeManager& manager, bool fullApp) const {
	    ConversionSetup setup = *this;


		// plugin initialization
            setup.frontendPluginInit();


		// create a temporary manager
	    core::NodeManager& tmpMgr = manager;	// for performance we are just using the same manager
//		core::NodeManager tmpMgr;		// not: due to the relevance of class-info-annotations no chaining of managers is allowed here

		// load and merge all files into a single translation unit
		auto unit = toIRTranslationUnit(tmpMgr);

		// convert units to a single program
		auto res = (fullApp) ? tu::toProgram(tmpMgr, unit) : tu::resolveEntryPoints(tmpMgr, unit);

		// apply OpenCL conversion
/*		if(hasOption(OpenCL)) {
			frontend::ocl::HostCompiler oclHostCompiler(res, *this);
			res = oclHostCompiler.compile();
		}
*/
		// strip of OMP annotation since those may contain references to local nodes
		core::visitDepthFirstOnce(res, [](const core::NodePtr& cur) {
			cur->remAnnotation(omp::BaseAnnotation::KEY);
		});

        // maybe a visitor wants to manipulate the IR translation unit
        for(auto plugin : setup.getPlugins())
            res = plugin->IRVisit(res);

		// return instance within global manager
		return core::transform::utils::migrate(res, manager);
	}

	bool ConversionJob::isCxx() const {
		if (getStandard() == Standard::Cxx03 || getStandard() == Standard::Cxx11) return true;
		if (getStandard() == Standard::C99) return false;

		bool cppFile = any(files, [&](const path& cur) {
			return static_cast<const ConversionSetup&>(*this).isCxx(cur);
		});

		bool cppLibs = any(libs, [&](const tu::IRTranslationUnit& tu) -> bool { return tu.isCXX(); } );

		return cppFile || cppLibs;
	}

	void ConversionJob::printConversionJob() const {
        std::cout << "~~~~~~CONVERSION SETUP~~~~~~\n";
        std::cout << "input files: \n" << this->files << std::endl;
        std::cout << "flags: \n" <<
			"PrintDiag " << hasOption(ConversionSetup::PrintDiag) << "\n" <<
			"OpenMP " << hasOption(ConversionSetup::OpenMP) << "\n" <<
			"OpenCL " << hasOption(ConversionSetup::OpenCL) << "\n" <<
			"Cilk " << hasOption(ConversionSetup::Cilk) << "\n" <<
			"WinCrossCompile " << hasOption(ConversionSetup::WinCrossCompile) << "\n" <<
			"GemCrossCompile " << hasOption(ConversionSetup::GemCrossCompile) << "\n" <<
			"TAG_MPI " << hasOption(ConversionSetup::TAG_MPI) << "\n" <<
			"ProgressBar " << hasOption(ConversionSetup::ProgressBar) << "\n" <<
			"NoWarnings " << hasOption(ConversionSetup::NoWarnings) << "\n" <<
			"StrictSemanticChecks " << hasOption(ConversionSetup::StrictSemanticChecks) << "\n" << std::endl;
        std::cout << "interceptions: \n" << this->getInterceptions() << std::endl;
        std::cout << "include dirs: \n" << this->getIncludeDirectories() << std::endl;
        std::cout << "libraries: \n" << this->libs << std::endl;
        std::cout << "standard: \n" << this->getStandard() << std::endl;
        std::cout << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
	}

} // end namespace frontend
} // end namespace insieme
