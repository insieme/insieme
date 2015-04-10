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

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/config.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/normalize.h"

#include "insieme/core/annotations/naming.h"
#include "insieme/annotations/meta_info/meta_infos.h"

#include "insieme/utils/logging.h"

namespace fe = insieme::frontend;
namespace core = insieme::core;
using namespace insieme::utils::set;
using namespace insieme::utils::log;
using namespace core;

LambdaExprPtr getEntryPoint(const core::ProgramPtr& prog, const std::string& entry)
{
	for(auto it=prog->getEntryPoints().begin(); it!=prog->getEntryPoints().end(); it++)
	{
		LambdaExprPtr fun = (*it).as<LambdaExprPtr>();

		if(core::annotations::hasNameAttached(fun) && core::annotations::getAttachedName(fun).compare(entry) == 0) {
			return analysis::normalize(fun);
		}
	}

	return LambdaExprPtr();
}

TEST(OMPx, SimpleRegion) {

	Logger::get(std::cerr, INFO, 0);

	core::NodeManager manager;
	core::IRBuilder builder(manager);

	// C source code compilation

	fe::ConversionJob job(CLANG_SRC_DIR "inputs/omp+_region.c");
	job.addIncludeDirectory(CLANG_SRC_DIR "inputs");
	job.setOption(fe::ConversionJob::OpenMP);

	LOG(INFO) << "Converting input program '" << std::string(CLANG_SRC_DIR) << "inputs/omp+_region.c" << "' to IR...";

	core::ProgramPtr prog = job.execute(manager, false);
	ASSERT_TRUE(prog);

	LambdaExprPtr progEntry = getEntryPoint(prog, "simpleRegion");
	ASSERT_TRUE(progEntry);

	LOG(DEBUG) << "Printing the IR: " << core::printer::PrettyPrinter(progEntry, core::printer::PrettyPrinter::OPTIONS_DETAIL);

	// Target IR code

	LOG(INFO) << "Parsing reference IR code...";

	// Main function with region
/*
	auto res = analysis::normalize(builder.parseProgram(
			"let fun000 = ()->unit {"
				"ref<int<4>> v1 = var(3);"
			"};"

			"int<4> main() {"
				"ref<int<4>> v1 = var(0);"
				"ref<int<4>> v5 = var(0);"
				"{"
					"parallel(job([1-1], fun000()));"
				"};"
				"return 0;"
			"}"));
	ASSERT_TRUE(res);
	auto resEntry = *(res->getEntryPoints().begin());
	ASSERT_TRUE(resEntry);

	LOG(DEBUG) << "Printing the IR: " << core::printer::PrettyPrinter(resEntry, core::printer::PrettyPrinter::OPTIONS_DETAIL);

	LOG(INFO) << "Comparing results...";

	// print program using pretty printer
	EXPECT_EQ(toString(core::printer::PrettyPrinter(resEntry)), toString(core::printer::PrettyPrinter(progEntry)));
	*/
	EXPECT_EQ("AP(rec v0.{v0=fun() {ref<int<4>> v1 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return type<'a>;}}(v1), memloc.stack); ref.assign(v2, v1); return v2;}}(0); ref<int<4>> v2 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return type<'a>;}}(v1), memloc.stack); ref.assign(v2, v1); return v2;}}(0); {parallel(job [] (bind(){rec v0.{v0=fun() {ref<int<4>> v1 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return type<'a>;}}(v1), memloc.stack); ref.assign(v2, v1); return v2;}}(3);}}()}));}; return 0;}})", toString(progEntry));
}

void dumpObjectiveMetaInfo(insieme::annotations::ompp_objective_info info) {
    LOG(INFO) << "[" << info.region_id << "] { " << info.energy_weight << ", (" << info.energy_min << ", " << info.energy_max << "), " <<
        info.power_weight << ", (" << info.power_min << ", " << info.power_max << "), " <<
        info.time_weight << ", (" << info.time_min << ", " << info.time_max << ") }";
}

TEST(OMPx, Objective) {

	Logger::get(std::cerr, INFO, 0);

	core::NodeManager manager;
	core::IRBuilder builder(manager);

	// C source code compilation

	fe::ConversionJob job(CLANG_SRC_DIR "inputs/omp+_region.c");
	job.addIncludeDirectory(CLANG_SRC_DIR "inputs");
	job.setOption(fe::ConversionJob::OpenMP);

	LOG(INFO) << "Converting input program '" << std::string(CLANG_SRC_DIR) << "inputs/omp+_region.c" << "' to IR...";

	core::ProgramPtr prog = job.execute(manager, false);
	ASSERT_TRUE(prog);

	LambdaExprPtr progEntry = getEntryPoint(prog, "objective");
	ASSERT_TRUE(progEntry);

    std::vector<insieme::annotations::ompp_objective_info> infos;
    core::visitDepthFirst(progEntry, [&] (const core::NodePtr& cur) {
        if(cur->hasAttachedValue<insieme::annotations::ompp_objective_info>()) {
            infos.push_back(cur->getAttachedValue<insieme::annotations::ompp_objective_info>());
        }
        
    });

    ASSERT_TRUE(infos.size() == 3);

	//#pragma omp region objective(0.1*E+0.3*P+0.6*T)
    ASSERT_TRUE(infos[0].energy_weight == 0.1f);
    ASSERT_TRUE(infos[0].energy_min == -1.f);
    ASSERT_TRUE(infos[0].energy_max == -1.f);
    ASSERT_TRUE(infos[0].power_weight == 0.3f);
    ASSERT_TRUE(infos[0].power_min == -1.f);
    ASSERT_TRUE(infos[0].power_max == -1.f);
    ASSERT_TRUE(infos[0].time_weight == 0.6f);
    ASSERT_TRUE(infos[0].time_min == -1.f);
    ASSERT_TRUE(infos[0].time_max == -1.f);

	//#pragma omp task objective(:E<10)
    ASSERT_TRUE(infos[1].energy_weight == 0.f);
    ASSERT_TRUE(infos[1].energy_min == -1.f);
    ASSERT_TRUE(infos[1].energy_max == 10.f);
    ASSERT_TRUE(infos[1].power_weight == 0.f);
    ASSERT_TRUE(infos[1].power_min == -1.f);
    ASSERT_TRUE(infos[1].power_max == -1.f);
    ASSERT_TRUE(infos[1].time_weight == 0.f);
    ASSERT_TRUE(infos[1].time_min == -1.f);
    ASSERT_TRUE(infos[1].time_max == -1.f);
    ASSERT_TRUE(infos[1].region_id == infos[0].region_id +1);
	
    //#pragma parallel objective(0.1*E+0.2*P+0.7*T:T<3;P>22)
    ASSERT_TRUE(infos[2].energy_weight == 0.1f);
    ASSERT_TRUE(infos[2].energy_min == -1.f);
    ASSERT_TRUE(infos[2].energy_max == -1.f);
    ASSERT_TRUE(infos[2].power_weight == 0.2f);
    ASSERT_TRUE(infos[2].power_min == 22.f);
    ASSERT_TRUE(infos[2].power_max == -1.f);
    ASSERT_TRUE(infos[2].time_weight == 0.7f);
    ASSERT_TRUE(infos[2].time_min == -1.f);
    ASSERT_TRUE(infos[2].time_max == 3.f);
    ASSERT_TRUE(infos[2].region_id == infos[0].region_id +2);
}

TEST(OMPx, Target) {

	Logger::get(std::cerr, INFO, 0);

	core::NodeManager manager;
	core::IRBuilder builder(manager);

	// C source code compilation

	fe::ConversionJob job(CLANG_SRC_DIR "inputs/omp+_region.c");
	job.addIncludeDirectory(CLANG_SRC_DIR "inputs");
	job.setOption(fe::ConversionJob::OpenMP);

	LOG(INFO) << "Converting input program '" << std::string(CLANG_SRC_DIR) << "inputs/omp+_region.c" << "' to IR...";

	core::ProgramPtr prog = job.execute(manager, false);
	ASSERT_TRUE(prog);

	LambdaExprPtr progEntry = getEntryPoint(prog, "target");
	ASSERT_TRUE(progEntry);
}

TEST(OMPx, Param) {

	Logger::get(std::cerr, INFO, 0);

	core::NodeManager manager;
	core::IRBuilder builder(manager);
    auto& basic = manager.getLangBasic();

	// C source code compilation

	fe::ConversionJob job(CLANG_SRC_DIR "inputs/omp+_region.c");
	job.addIncludeDirectory(CLANG_SRC_DIR "inputs");
	job.setOption(fe::ConversionJob::OpenMP);

	LOG(INFO) << "Converting input program '" << std::string(CLANG_SRC_DIR) << "inputs/omp+_region.c" << "' to IR...";

	core::ProgramPtr prog = job.execute(manager, false);
	ASSERT_TRUE(prog);

	LambdaExprPtr progEntry = getEntryPoint(prog, "param");
	ASSERT_TRUE(progEntry);

	LOG(DEBUG) << "Printing the IR: " << core::printer::PrettyPrinter(progEntry, core::printer::PrettyPrinter::OPTIONS_DEFAULT);

	// Target IR code

	LOG(INFO) << "Parsing reference IR code...";

    std::map<string,NodePtr> symbols;
    symbols["inf"] = builder.literal("inf", basic.getIntInf());
    auto vectorTy = builder.vectorType(basic.getInt4(), builder.concreteIntTypeParam(static_cast<size_t>(3)));
    symbols["vector"] = builder.callExpr(basic.getUndefined(), builder.getTypeLiteral(vectorTy));
/*
	auto res = analysis::normalize(builder.parseProgram(
			"let fun000 = (ref<int<4>> v1) -> unit {"
                "v1 = pickInRange(((10-0)/2))*2+0;"
                "ref<int<4>> v2 = var(( *v1));"
            "};"

            "let fun001 = (ref<int<4>> v1, ref<vector<int<4>,3>> v2) -> unit {"
                "v1 = v2[pickInRange(3ul)];"
                "{"
                    "ref<int<4>> v3 =  var(v1);"
                "};"
                "mergeAll();"
            "};"

            "int<4> main() {"
                "ref<int<4>> v1 = var(0);"
                "ref<int<4>> v2 = var(0);"
                "ref<vector<int<4>,3>> v3 = var(vector<int<4>,3>);"
                "{"
                    "parallel(job([1-1], fun000(v1)));"
                "};"
                "{"
                    "merge(parallel(job([1-inf], fun001(v1,v3))));"
                "};"
                "return 0;"
			"}", symbols));
	ASSERT_TRUE(res);
	auto resEntry = *(res->getEntryPoints().begin());
	ASSERT_TRUE(resEntry);

	LOG(DEBUG) << "Printing the IR: " << core::printer::PrettyPrinter(resEntry, core::printer::PrettyPrinter::OPTIONS_DETAIL);

	LOG(INFO) << "Comparing results...";

	// print program using pretty printer
	EXPECT_EQ(toString(core::printer::PrettyPrinter(resEntry)), toString(core::printer::PrettyPrinter(progEntry)));
	*/
	EXPECT_EQ("AP(rec v0.{v0=fun() {ref<int<4>> v1 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return type<'a>;}}(v1), memloc.stack); ref.assign(v2, v1); return v2;}}(0); ref<int<4>> v2 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return type<'a>;}}(v1), memloc.stack); ref.assign(v2, v1); return v2;}}(0); ref<vector<int<4>,3>> v3 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return type<'a>;}}(v1), memloc.stack); ref.assign(v2, v1); return v2;}}(undefined(type<vector<int<4>,3>>)); {parallel(job [] (bind(){rec v0.{v0=fun(ref<int<4>> v1) {ref.assign(v1, int.add(int.mul(pickInRange(0, int.div(int.sub(10, 0), 2), 0, 0, 0), 2), 0)); ref<int<4>> v2 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return type<'a>;}}(v1), memloc.stack); ref.assign(v2, v1); return v2;}}(ref.deref(v1));}}(v1)}));}; {merge(parallel(job [] (bind(){rec v0.{v0=fun(ref<int<4>> v1, ref<vector<int<4>,3>> v2) {ref.assign(v1, ref.deref(rec v0.{v0=fun(ref<vector<'elem,#l>> v1, uint<8> v2) {return ref.narrow(v1, dp.element(dp.root, v2), type<'elem>);}}(v2, pickInRange(1, 3ul, 0, 0, 0)))); {ref<int<4>> v3 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return type<'a>;}}(v1), memloc.stack); ref.assign(v2, v1); return v2;}}(ref.deref(v1));}; mergeAll();}}(v1, v3)})));}; return 0;}})", toString(progEntry));
}

TEST(OMPx, FirstLocal) {

	Logger::get(std::cerr, INFO, 0);

	core::NodeManager manager;
	core::IRBuilder builder(manager);

	// C source code compilation

	fe::ConversionJob job(CLANG_SRC_DIR "inputs/omp+_region.c");
	job.addIncludeDirectory(CLANG_SRC_DIR "inputs");
	job.setOption(fe::ConversionJob::OpenMP);

	LOG(INFO) << "Converting input program '" << std::string(CLANG_SRC_DIR) << "inputs/omp+_region.c" << "' to IR...";

	core::ProgramPtr prog = job.execute(manager, false);
	ASSERT_TRUE(prog);

	LambdaExprPtr progEntry = getEntryPoint(prog, "firstLocal");
	ASSERT_TRUE(progEntry);

	LOG(DEBUG) << "Printing the IR: " << core::printer::PrettyPrinter(progEntry, core::printer::PrettyPrinter::OPTIONS_DETAIL);

	// Target IR code

	LOG(INFO) << "Parsing reference IR code...";
/*
	auto res = analysis::normalize(builder.parseProgram(
			"let fun000 = (int<4> v0)->unit {"
				"ref<int<4>> v2 = loc(v0);"
				"{"
					"ref<int<4>> v3 = var(*v2 + 3);"
				"};"
				"delete(v2);"
			"};"

			"int<4> main() {"
				"ref<int<4>> v1 = var(5);"
				"{"
					"int<4> v2 = *v1;"
					"parallel(job([1-1], fun000(v2)));"
				"};"
				"return 0;"
			"}"));
	ASSERT_TRUE(res);
	auto resEntry = *(res->getEntryPoints().begin());
	ASSERT_TRUE(resEntry);

	LOG(DEBUG) << "Printing the IR: " << core::printer::PrettyPrinter(resEntry, core::printer::PrettyPrinter::OPTIONS_DETAIL);

	LOG(INFO) << "Comparing results...";

	// print program using pretty printer
	EXPECT_EQ(toString(core::printer::PrettyPrinter(resEntry)), toString(core::printer::PrettyPrinter(progEntry)));
	*/
	EXPECT_EQ("AP(rec v0.{v0=fun() {ref<int<4>> v1 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return type<'a>;}}(v1), memloc.stack); ref.assign(v2, v1); return v2;}}(5); {int<4> v2 = ref.deref(v1); parallel(job [] (bind(){rec v0.{v0=fun(int<4> v1) {ref<int<4>> v2 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return type<'a>;}}(v1), memloc.local); ref.assign(v2, v1); return v2;}}(v1); {ref<int<4>> v3 = rec v0.{v0=fun('a v1) {ref<'a> v2 = ref.alloc(rec v0.{v0=fun('a v1) {return type<'a>;}}(v1), memloc.stack); ref.assign(v2, v1); return v2;}}(int.add(ref.deref(v2), 3));}; ref.delete(v2);}}(v2)}));}; return 0;}})", toString(progEntry));
}
