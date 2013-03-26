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

#include "insieme/driver/pragma_info.h"

#include <fstream>

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_builder.h"

#include "insieme/annotations/info.h"
#include "insieme/annotations/c/naming.h"

#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/modeling/cache.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/constraint.h"

using namespace insieme;
using namespace insieme::analysis;
using namespace insieme::analysis::polyhedral;

namespace insieme { namespace driver {

namespace {

void handleScopInfo(const core::NodePtr& node) {

	if (boost::optional<Scop> scop = scop::ScopRegion::toScop(node) ) {
		LOG(INFO) << *scop;

		return;
	}
	LOG(WARNING) << "Marked region is not a SCoP.";
}


void sampling(	int 				id,
				std::ostream& 		out,
				CtxPtr<> 			ctx, 
				core::NodeManager& 	mgr,
				PiecewisePtr<> 		model, 
				int				 	rank, 
				size_t 				arr_size) 
{


	IterationVector iv = model->getIterationVector(mgr);
	assert(iv.getIteratorNum() == 0);

	for_each(iv.param_begin(), iv.param_end(), [&](const Parameter& cur) { 
		SetPtr<> domain = makeSet(ctx, IterationDomain(iv));
		const core::ExpressionPtr& exp = cur.getExpr();

		// look if there is an annotation name 
		if (std::shared_ptr<annotations::c::CNameAnnotation> ann_ptr = 
				exp->getAnnotation(annotations::c::CNameAnnotation::KEY) ) 
		{
		
			if (ann_ptr->getName() == "rank") {
				// If the parameter is 'rank' then we should set it to the value provided by the input arguments of this
				// function 
				domain = makeSet(ctx, makeVarRange(iv, cur.getExpr(), 
						core::IRBuilder(mgr).intLit(rank) )
				);
			} else {
				// Otherwise set this parameter to be an array size 
				domain = makeSet(ctx, makeVarRange(iv, cur.getExpr(), 
						core::IRBuilder(mgr).intLit(arr_size) )
				);
			}
		}
		model*= domain;
	});

	utils::Piecewise<core::arithmetic::Formula> pw = model->toPiecewise(mgr);
	out << id << ", " << arr_size << ", " 
		<< (pw.empty() ? core::arithmetic::Formula(0) : pw.begin()->second) 
		<< std::endl;

}

void handleCacheInfo(const core::NodePtr& node, int id, const cmd_options::CommandLineOptions& options) {

	using insieme::core::arithmetic::Formula;
	
	core::NodeManager& mgr = node->getNodeManager();

	// LOG(INFO) << insieme::core::printer::PrettyPrinter(node);

	// Write the estimation in a file called "cache_model.csv"
	CtxPtr<> ctx = makeCtx();

	std::fstream file("cache_model.csv", std::fstream::app | std::fstream::out);

	PiecewisePtr<> misses = 
		analysis::modeling::getCacheMisses(ctx, node, 
				options.CacheLineSize,
				options.CacheSize,
				options.CacheAssociativity
			);
	
	// LOG(INFO) << "Cache misses model for this code is: " << std::endl << *misses;
	file << "#  Sampling the model for sender process" << std::endl;
	file << "# id, arr_size, cache_misses" << std::endl;

	for(size_t size=32*1024; size <= static_cast<unsigned>(options.CacheSize*4); size *= 2) {
		sampling(id, file, ctx, mgr, misses, 0, size);
	}
	file.close();
}

} // end anonymous namespace 



core::ProgramPtr handlePragmaInfo(const core::ProgramPtr& program, const cmd_options::CommandLineOptions& options) {

	typedef annotations::Info::StrValueVect StrValueVect;

	auto&& printer = [&]( const core::NodePtr& cur ) { 

		if(std::shared_ptr<annotations::Info> info = cur->getAnnotation( annotations::Info::KEY ) ) {	
			
			for_each(*info, [&](const StrValueVect::value_type& what) {
				
				LOG(INFO) << "PragmaInfo.Handling -- ID: " << info->getId() << " (" << what << ")";
				if (what == "scop") { return handleScopInfo(cur); }
				if (what == "cache") { return handleCacheInfo(cur, info->getId(), options); }

			});
		}
	};

	core::visitDepthFirstOnce(program, core::makeLambdaVisitor( printer ) );
	return program;
}

} // end driver namespace
} // end insieme namespace
