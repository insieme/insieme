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

#include "insieme/annotations/data_annotations.h"
#include "insieme/annotations/loop_annotations.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/compiler.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/clang_config.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/omp/omp_pragma.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/printer/pretty_printer.h"

#include "clang/AST/Expr.h"
#include "clang/AST/Type.h"

using namespace insieme::frontend;
using namespace insieme::frontend::pragma;
using namespace insieme::core;
using namespace insieme::annotations;

TEST(PragmaDatarangeTest, HandleDatarange) {
//	CommandLineOptions::Verbosity = 2;

	NodeManager manager;

	ConversionJob job;

	insieme::frontend::Program prog(manager, job);
	TranslationUnit& tu = prog.addTranslationUnit( ConversionJob( SRC_DIR "/inputs/insieme_datarange.c" ) );

	const PragmaList& pl = tu.getPragmaList();

	EXPECT_FALSE(pl.empty());
/*
	std::cout << "PragmaList " << std::endl;
	for(auto I = pl.begin(); I != pl.end(); ++I)
		std::cout << "P: " << (*I)->toStr(comp.getSourceManager()) << std::endl;
*/
	ProgramPtr program;

	LOG(INFO) << "Converting input program '" << std::string(SRC_DIR) << "/inputs/insieme_datarange.c" << "' to IR...";

	program = prog.convert();
	size_t cnt = 0, cntLoopAnnot = 0;


	auto lookForAnnot = makeLambdaVisitor([&](const NodePtr& node) {
/*		if(node->getNodeType() == NT_CompoundStmt)
			std::cout << "\ncompound " << node->hasAnnotation(DataRangeAnnotation::KEY) << " " << node << std::endl;
*/
		if(node->hasAnnotation(DataRangeAnnotation::KEY)) {
			++cnt;
//			std::cout << node << std::endl << *node->getAnnotation(DataRangeAnnotation::KEY) << std::endl;
		}

		if(node->hasAnnotation(LoopAnnotation::KEY)) {
			++cntLoopAnnot;
			insieme::annotations::LoopAnnotationPtr lAnnot = node->getAnnotation(LoopAnnotation::KEY);
			EXPECT_EQ(10u, lAnnot->getIterations());
		}
	});

	visitDepthFirst(program, lookForAnnot);

	EXPECT_EQ(2u, cnt);
	EXPECT_EQ(1u, cntLoopAnnot);

	printer::PrettyPrinter pp(program, printer::PrettyPrinter::OPTIONS_DETAIL);

	LOG(INFO) << "Printing the IR: " << pp;


}
