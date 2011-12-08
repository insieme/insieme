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
#include "insieme/annotations/c/location.h"

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
	auto range              = var >> equal >> expr["lb"] >> colon >> expr["ub"];
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

    insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeKernelFile>(
            pp.getIdentifierInfo("datarange"), range_list >> eod, "insieme")
        );

//*************************************************************************************************
// Insieme Pragmas for Transformations 
//************************************************************************************************/
	
	// Loop Interchange: contains the index of the loop being interchanged
	// it must be exactly 2:
	// 	#pragma insieme interchange (0,2)
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeInterchange>(
    	pp.getIdentifierInfo("interchange"), 
			l_paren >> (tok::numeric_constant >> ~comma >> 
						tok::numeric_constant)["idx"] >> 
			r_paren >> eod, "insieme")
    );

	// Loop Tiling: takes a list of integers constants which specifies the size of the tile size for
	// each of the dimensions which should be tiled in the loop
	insieme->AddPragma(pragma::PragmaHandlerFactory::CreatePragmaHandler<InsiemeTile>(
    	pp.getIdentifierInfo("tile"), 
			l_paren >> (tok::numeric_constant >> *(~comma >> (tok::numeric_constant)))["idx"] >> 
			r_paren >> eod, "insieme")
    );
}


namespace {

int extractIntegerConstant(const pragma::ValueUnionPtr& val) {
	std::string intLit = *val->get<std::string*>();
	return utils::numeric_cast<int>( intLit.c_str() );
}

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

InsiemeInterchange::InsiemeInterchange(const clang::SourceLocation&  startLoc, 
									   const clang::SourceLocation&  endLoc, 
								  	   const std::string& 			 type, 
					  				   const pragma::MatchMap& 	 	 mmap)

		: pragma::Pragma(startLoc, endLoc, type) 
{
	auto fit = mmap.find("idx");
	assert(fit != mmap.end() && fit->second.size() == 2);

	srcIdx = extractIntegerConstant(fit->second[0]);
	destIdx = extractIntegerConstant(fit->second[1]);
}

core::NodePtr InsiemeInterchange::attachTo(const core::NodePtr& node, conversion::ConversionFactory& fact) const {

	node->addAnnotation( std::make_shared<annotations::Interchange>(srcIdx, destIdx) );
	// we also attach information related to the current position of the statement in a way
	// we are able to point back the user to the pragma location if for example the transformation
	// failed to be applied for any reason
	std::pair<clang::SourceLocation, clang::SourceLocation>&& loc = 
				std::make_pair(getStartLocation(), getEndLocation());

	node->addAnnotation( std::make_shared<annotations::c::CLocAnnotation>(
			convertClangSrcLoc(fact.getCurrentSourceManager(), loc.first),
			convertClangSrcLoc(fact.getCurrentSourceManager(), loc.second))
	);
	return node;

}

InsiemeTile::InsiemeTile(const clang::SourceLocation&  startLoc, 
					     const clang::SourceLocation&  endLoc, 
						 const std::string& 			  type, 
					  	 const pragma::MatchMap& 	  mmap)

		: pragma::Pragma(startLoc, endLoc, type) 
{
	auto fit = mmap.find("idx");
	assert(fit != mmap.end() && fit->second.size() > 1);
	
	for_each(fit->second, [&](const ValueUnionPtr& cur) {
		tileSizes.push_back( extractIntegerConstant(cur) );
	});

}

core::NodePtr InsiemeTile::attachTo(const core::NodePtr& node, conversion::ConversionFactory& fact) const {

	node->addAnnotation( std::make_shared<annotations::Tiling>(tileSizes) );
	// we also attach information related to the current position of the statement in a way
	// we are able to point back the user to the pragma location if for example the transformation
	// failed to be applied for any reason
	std::pair<clang::SourceLocation, clang::SourceLocation>&& loc = 
				std::make_pair(getStartLocation(), getEndLocation());

	node->addAnnotation( std::make_shared<annotations::c::CLocAnnotation>(
			convertClangSrcLoc(fact.getCurrentSourceManager(), loc.first),
			convertClangSrcLoc(fact.getCurrentSourceManager(), loc.second))
	);
	return node;

}

} // end frontend namespace
} // end insieme namespace
