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

#include <gtest/gtest.h>

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/frontend/clang_config.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/compiler.h"
#include "insieme/frontend/utils/source_locations.h"

#include "insieme/frontend/analysis/global_variables.h"


using namespace insieme::frontend;
using namespace insieme::frontend::pragma;
using namespace insieme::core;
namespace fe = insieme::frontend;

using namespace insieme::frontend::analysis;

TEST(GlobalCollectorTest, GlobalStorage) {

	NodeManager manager;
	ConversionJob job;

	insieme::frontend::Program prog(manager, job);

	ConversionJob file = job;
	file.setFile( SRC_DIR "/inputs/globals1.cpp" );
	prog.addTranslationUnit( file );

	//collect globals, analyze all translation units
	fe::analysis::GlobalVarCollector globalsCollector;
	globalsCollector(prog.getTranslationUnits()[0]);

	//std::cout << "****************************************" << std::endl;

	std::map<std::string, GlobalVarCollector::VarStorage> solution = 
	{
		{"global_array", GlobalVarCollector::VS_GLOBAL},
		{"global_globalInOther", GlobalVarCollector::VS_EXTERN},
		{"global_pointer", GlobalVarCollector::VS_GLOBAL},
		{"global_pureExtern", GlobalVarCollector::VS_EXTERN},
		{"global_var", GlobalVarCollector::VS_GLOBAL},
		{"static_diffName2", GlobalVarCollector::VS_STATIC},
		{"static_var1", GlobalVarCollector::VS_STATIC},
		{"Obj__staticMem_member", GlobalVarCollector::VS_GLOBAL},
		{"global_instance", GlobalVarCollector::VS_GLOBAL},
		{"Obj2__staticMem_a", GlobalVarCollector::VS_GLOBAL},
		{"static_ctorGlobal0",  GlobalVarCollector::VS_STATIC}
	};
		
	for (auto it = globalsCollector.begin(); it != globalsCollector.end(); ++it){
		EXPECT_TRUE (solution.find (it.name()) != solution.end()) << "  not found: at var: "+it.name();
		EXPECT_EQ (solution[it.name()], it.storage()) << " at var: "+it.name()+"\n";
	}

	//std::cout << "****************************************" << std::endl;

	file.setFile( SRC_DIR "/inputs/globals2.cpp" );
	prog.addTranslationUnit( file );
	globalsCollector(prog.getTranslationUnits()[1]);

	//std::cout << "****************************************" << std::endl;
	
	
	std::map<std::string, GlobalVarCollector::VarStorage> solution2 = 
	{
		{"global_a", GlobalVarCollector::VS_GLOBAL},
		{"global_array", GlobalVarCollector::VS_GLOBAL},
		{"global_globalInOther", GlobalVarCollector::VS_GLOBAL},
		{"global_pointer", GlobalVarCollector::VS_GLOBAL},
		{"global_pureExtern", GlobalVarCollector::VS_EXTERN},
		{"global_var", GlobalVarCollector::VS_GLOBAL},
		{"static_a3", GlobalVarCollector::VS_STATIC},
		{"static_diffName2", GlobalVarCollector::VS_STATIC},
		{"static_var1", GlobalVarCollector::VS_STATIC},
		{"Obj__staticMem_member", GlobalVarCollector::VS_GLOBAL},
		{"global_instance", GlobalVarCollector::VS_GLOBAL},
		{"Obj2__staticMem_a", GlobalVarCollector::VS_GLOBAL},
		{"static_ctorGlobal0",  GlobalVarCollector::VS_STATIC}
	};

	for (auto it = globalsCollector.begin(); it != globalsCollector.end(); ++it){
		EXPECT_TRUE (solution2.find (it.name()) != solution.end()) << "  not found: at var: "+it.name();
		EXPECT_EQ (solution2[it.name()], it.storage()) << " at var: "+it.name()+"\n";
	}
	//std::cout << "****************************************" << std::endl;
	
//	globalsCollector.dump();
}

