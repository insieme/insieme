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

#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/pragma/matcher.h"
#include "insieme/frontend/convert.h"

#include "insieme/annotations/transform.h"
#include "insieme/annotations/info.h"
#include "insieme/annotations/data_annotations.h"
#include "insieme/annotations/c/location.h"

#include "insieme/core/ir_expressions.h"

#include "insieme/utils/numeric_cast.h"

#include "clang/Basic/FileManager.h"

namespace insieme {
namespace frontend {

using namespace insieme::frontend::pragma;
using namespace insieme::frontend::pragma::tok;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TestPragma ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TestPragma::TestPragma(const clang::SourceLocation& startLoc, 
					   const clang::SourceLocation& endLoc,
					   const std::string& 			type, 
					   const pragma::MatchMap& 		mmap) 

	: Pragma(startLoc, endLoc, type) 
{

	pragma::MatchMap::const_iterator fit = mmap.find("expected");
	if(fit != mmap.end()) {
		expected = *fit->second.front()->get<std::string*>();
	}
}

void TestPragma::registerPragmaHandler(clang::Preprocessor& pp) {

	pp.AddPragmaHandler(
		PragmaHandlerFactory::CreatePragmaHandler<TestPragma>(
			pp.getIdentifierInfo("test"), string_literal["expected"] >> eod
		)
	);

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ InsiemePragma ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
InsiemePragma::InsiemePragma(const clang::SourceLocation& 	startLoc, 
							 const clang::SourceLocation& 	endLoc,
							 const std::string& 			type, 
							 const pragma::MatchMap&		mmap) 
	: Pragma(startLoc, endLoc, type) { }

void InsiemePragma::registerPragmaHandler(clang::Preprocessor& pp) {
    // some utilities
	auto range              = ~l_paren >> var["var"] >> ~equal >> expr["lb"] >> ~colon >> expr["ub"] >> ~r_paren;
	// range *(, range)
	auto range_list   		= range >> *(~comma >> range);

	// define a PragmaNamespace for insieme
	clang::PragmaNamespace* insieme = new clang::PragmaNamespace("insieme");
	pp.AddPragmaHandler(insieme);

	// Add an handler for insieme mark pargma:
	// #pragma insieme mark new-line
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeMark>(
			pp.getIdentifierInfo("mark"), eod, "insieme")
		);

	// Add an handler for insieme ignore pragma:
	// #pragma insieme ignore new-line
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeIgnore>(
			pp.getIdentifierInfo("ignore"), eod, "insieme")
		);

    insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeKernelFile>(
            pp.getIdentifierInfo("kernelFile"), string_literal  >> eod, "insieme")
        );

    insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeDatarange>(
            pp.getIdentifierInfo("datarange"), range_list["ranges"] >> eod, "insieme")
        );

//*************************************************************************************************
// Insieme Pragmas for Transformations 
//************************************************************************************************/
	
	// Loop Strip Mining: takes a single integer indicating the strip amount 
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeTransform<STRIP>>(
    	pp.getIdentifierInfo("strip"), 
			l_paren >> (tok::numeric_constant >> ~comma >> tok::numeric_constant)["values"] >> r_paren >> eod, "insieme")
    );

	// Loop Interchange: contains the index of the loop being interchanged
	// it must be exactly 2:
	// 	#pragma insieme interchange (0,2)
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeTransform<INTERCHANGE>>(
    	pp.getIdentifierInfo("interchange"), 
			l_paren >> (tok::numeric_constant >> ~comma >> 
						tok::numeric_constant)["values"] >> 
			r_paren >> eod, "insieme")
    );

	// Loop Tiling: takes a list of integers constants which specifies the size of the tile size for
	// each of the dimensions which should be tiled in the loop
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeTransform<TILE>>(
    	pp.getIdentifierInfo("tile"), 
			l_paren >> (tok::numeric_constant >> *(~comma >> (tok::numeric_constant)))["values"] >> 
			r_paren >> eod, "insieme")
    );

	// Loop Unrolling: takes a single integer constant which specifies the unrolling factor
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeTransform<UNROLL>>(
    	pp.getIdentifierInfo("unroll"), 
			l_paren >> tok::numeric_constant["values"] >> r_paren >> eod, "insieme")
    );

	// Loop Fusion: takes a list of integers constants which specifies the index of the loops 
	// being fused, the loop needs to be at the same level and the pragma applyied to outer scopes
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeTransform<FUSE>>(
    	pp.getIdentifierInfo("fuse"), 
			l_paren >> (tok::numeric_constant >> *(~comma >> (tok::numeric_constant)))["values"] >> 
			r_paren >> eod, "insieme")
    );

	// Loop Fission: takes a list of integers constants which specifies the index of the stmts 
	// inside the loop which should be placed in different loops stmts
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeTransform<SPLIT>>(
    	pp.getIdentifierInfo("split"), 
			l_paren >> (tok::numeric_constant >> *(~comma >> (tok::numeric_constant)))["values"] >> 
			r_paren >> eod, "insieme")
    );

	// Loop Fission: takes a list of integers constants which specifies the index of the stmts 
	// inside the loop which should be placed in different loops stmts
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeTransform<STAMP>>(
    	pp.getIdentifierInfo("stamp"), 
			l_paren >> (tok::numeric_constant >> *(~comma >> (tok::numeric_constant)))["values"] >> 
			r_paren >> eod, "insieme")
    );

	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeTransform<RESCHEDULE>>(
    	pp.getIdentifierInfo("reschedule"), l_paren >> tok::numeric_constant >> r_paren >> eod, "insieme")
    );

	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeTransform<PARALLELIZE>>(
    	pp.getIdentifierInfo("parallelize"), l_paren >> tok::numeric_constant >>r_paren >> eod, "insieme")
    );

	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeTransform<RSTRIP>>(
    	pp.getIdentifierInfo("rstrip"),
					l_paren >> tok::numeric_constant["values"] >> r_paren >> eod, "insieme")
    );


	//===========================================================================================================
	// Insieme Info
	//===========================================================================================================
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeInfo>(
    	pp.getIdentifierInfo("info"), kwd("id") >> colon >> tok::numeric_constant["id"] >>
					l_paren >> (identifier >> *(~comma >> identifier))["values"] >>r_paren >> eod, "insieme")
    );

}


void attatchDatarangeAnnotation(const core::StatementPtr& irNode, const clang::Stmt* clangNode, frontend::conversion::ConversionFactory& convFact) {


    insieme::core::NodeAnnotationPtr annot;

    // check if there is a datarange annotation
    const PragmaStmtMap::StmtMap& pragmaStmtMap = convFact.getPragmaMap().getStatementMap();
    std::pair<PragmaStmtMap::StmtMap::const_iterator, PragmaStmtMap::StmtMap::const_iterator> iter = pragmaStmtMap.equal_range(clangNode);

   std::for_each(iter.first, iter.second,
        [ & ](const PragmaStmtMap::StmtMap::value_type& curr){
            const frontend::InsiemeDatarange* dr = dynamic_cast<const frontend::InsiemeDatarange*>( &*(curr.second) );
            if(dr) {

            	pragma::MatchMap mmap = dr->getMatchMap();

            	auto ranges = mmap.find("ranges");
            	if(ranges == mmap.end())
            		return;

            	annotations::DataRangeAnnotation dataRanges;


				for(auto I = ranges->second.begin(); I != ranges->second.end(); ++I){
            		core::VariablePtr var = static_pointer_cast<core::VariablePtr>(
						convFact.convertStmt(((*I)->get<clang::Stmt*>()))
					);
            		core::ExpressionPtr lowerBound = static_pointer_cast<core::ExpressionPtr>(
						convFact.convertStmt(((*++I)->get<clang::Stmt*>()))
					);
            		core::ExpressionPtr upperBound = static_pointer_cast<core::ExpressionPtr>(
						convFact.convertStmt(((*++I)->get<clang::Stmt*>()))
					);

            		dataRanges.addRange(annotations::Range(var, lowerBound, upperBound ));
            	}
                annot = std::make_shared<annotations::DataRangeAnnotation>((dataRanges));
            }
    });

    if(annot)
        irNode->addAnnotation(annot);

}

namespace {

using namespace insieme::annotations;

c::SourceLocation convertClangSrcLoc(clang::SourceManager& sm, const clang::SourceLocation& loc) {
	clang::FileID&& fileId = sm.getFileID(loc);
	const clang::FileEntry* fileEntry = sm.getFileEntryForID(fileId);
	return annotations::c::SourceLocation(
			fileEntry->getName(), 
			sm.getSpellingLineNumber(loc), 
			sm.getSpellingColumnNumber(loc)
		);
};

} // end anonymous namespace

unsigned extractIntegerConstant(const pragma::ValueUnionPtr& val) {
	std::string intLit = *val->get<std::string*>();
	return insieme::utils::numeric_cast<unsigned>( intLit.c_str() );
}

void attach(const clang::SourceLocation& 	startLoc,
			const clang::SourceLocation 	endLoc,
			const TransformationType& 		trans, 
			const ValueVect& 				values,
			const core::NodePtr& 			node, 
			conversion::ConversionFactory& 	fact) 
{
	
	annotations::TransformationHint::Type type;
	switch( trans ) {
		case INTERCHANGE: type = annotations::TransformationHint::LOOP_INTERCHANGE;
						  break;
		case STRIP:		  type = annotations::TransformationHint::LOOP_STRIP;
						  break;
		case TILE:		  type = annotations::TransformationHint::LOOP_TILE;
						  break;
		case UNROLL:	  type = annotations::TransformationHint::LOOP_UNROLL;
						  break;
		case FUSE:		  type = annotations::TransformationHint::LOOP_FUSE;
						  break;
		case SPLIT:		  type = annotations::TransformationHint::LOOP_SPLIT;
						  break;
		case STAMP:		  type = annotations::TransformationHint::LOOP_STAMP;
						  break;
		case RESCHEDULE:  type = annotations::TransformationHint::LOOP_RESCHEDULE;
						  break;
		case PARALLELIZE: type = annotations::TransformationHint::LOOP_PARALLELIZE;
						  break;
		
		case RSTRIP:	  type = annotations::TransformationHint::REGION_STRIP;
						  break;

		default:
						  assert(false && "Transformation name not handled");
	}

	if (!node->hasAnnotation( annotations::TransformAnnotation::KEY ) ) {
		node->addAnnotation( std::make_shared<annotations::TransformAnnotation>() );
	}

	annotations::TransformAnnotation& ann = *node->getAnnotation( annotations::TransformAnnotation::KEY );
	ann.getAnnotationList().push_back( std::make_shared<annotations::TransformationHint>(type, values) );
	
	// we also attach information related to the current position of the statement in a way
	// we are able to point back the user to the pragma location if for example the transformation
	// failed to be applied for any reason
	std::pair<clang::SourceLocation, clang::SourceLocation>&& loc = std::make_pair(startLoc, endLoc);

	node->addAnnotation( std::make_shared<annotations::c::CLocAnnotation>(
			convertClangSrcLoc(fact.getCurrentSourceManager(), loc.first),
			convertClangSrcLoc(fact.getCurrentSourceManager(), loc.second))
	);

}

void attach(const clang::SourceLocation& startLoc,
			const clang::SourceLocation endLoc,
			unsigned id,
		    const StrValueVect& values,
			const core::NodePtr& node, 
			conversion::ConversionFactory& fact) 
{
	node->addAnnotation( std::make_shared<annotations::Info>(id, values) );
}



} // end frontend namespace
} // end insieme namespace
