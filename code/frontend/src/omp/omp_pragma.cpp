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

#include "omp/omp_pragma.h"

#include "pragma_handler.h"
#include "pragma_matcher.h"

#include <clang/Lex/Pragma.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Expr.h>
#include "conversion.h"

#include <glog/logging.h>

#include <iostream>

using namespace std;

using namespace insieme::c_info::omp;

namespace insieme {
namespace frontend {
namespace omp {

OmpPragma::OmpPragma(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const string& name, const MatchMap& mmap):
	Pragma(startLoc, endLoc, name, mmap), mMap(mmap) {

//	LOG(INFO) << "~~~PRAGMA~~~" << std::endl;
//	for(MatchMap::const_iterator i = mmap.begin(), e = mmap.end(); i!=e; ++i) {
//		LOG(INFO) << "KEYWORD: " << i->first << ":" << std::endl;
//		for(ValueList::const_iterator i2=i->second.begin(), e2=i->second.end(); i2!=e2; ++i2)
//			LOG(INFO) << (*i2)->toStr() << ", ";
//		LOG(INFO) << std::endl;
//	}
}

void OmpPragma::RegisterPragmaHandlers(clang::Preprocessor& pp) {
	using namespace insieme::frontend;
	using namespace insieme::frontend::tok;

	// if(scalar-expression)
	auto if_expr 		   	= kwd("if") >> l_paren >> tok::expr["if"] >> r_paren;

	// default(shared | none)
	auto def			   	= Tok<clang::tok::kw_default>() >> l_paren >> ( kwd("shared") | kwd("none") )["default"] >> r_paren;

	// identifier *(, identifier)
	auto identifier_list   	= identifier >> *(~comma >> identifier);

	// private(list)
	auto private_clause    	=  kwd("private") >> l_paren >> identifier_list["private"] >> r_paren;

	// firstprivate(list)
	auto firstprivate_clause = kwd("firstprivate") >> l_paren >> identifier_list["firstprivate"] >> r_paren;

	// lastprivate(list)
	auto lastprivate_clause = kwd("lastprivate") >> l_paren >> identifier_list["lastprivate"] >> r_paren;


	auto op 			  	= tok::plus | tok::minus; // TODO: add more

	// reduction(operator: list)
	auto reduction_clause 	= kwd("reduction") >> l_paren >> op >> colon >> identifier_list >> r_paren;

	auto parallel_clause =  ( 	// if(scalar-expression)
								if_expr
							| 	// num_threads(integer-expression)
								(kwd("num_threads") >> l_paren >> expr["num_threads"] >> r_paren)
							|	// default(shared | none)
								def
							|	// private(list)
								private_clause
							|	// firstprivate(list)
								firstprivate_clause
							|	// shared(list)
								(kwd("shared") >> l_paren >> identifier_list["shared"] >> r_paren)
							|	// copyin(list)
								(kwd("copyin") >> l_paren >> identifier_list["copyin"] >> r_paren)
							|	// reduction(operator: list)
								reduction_clause
							);

	auto kind 			=   Tok<clang::tok::kw_static>() | kwd("dynamic") | kwd("guided") | kwd("auto") | kwd("runtime");

	auto for_clause 	=	(	private_clause
							|	firstprivate_clause
							|	lastprivate_clause
							|	reduction_clause
							|	(kwd("schedule") >> l_paren >> kind["schedule"] >> !( comma >> expr["chunk_size"] ) >> r_paren)
							|	(kwd("collapse") >> l_paren >> expr["collapse"] >> r_paren)
							|	kwd("nowait")
							);

	auto for_clause_list = !(for_clause >> *( !comma >> for_clause ));

	auto sections_clause =  ( 	// private(list)
								private_clause
							| 	// firstprivate(list)
								firstprivate_clause
							|	// lastprivate(list)
								lastprivate_clause
							|	// reduction(operator: list)
								reduction_clause
							| 	// nowait
								kwd("nowait")
							);

	auto sections_clause_list = !(sections_clause >> *( !comma >> sections_clause ));

	// [clause[ [, ]clause] ...] new-line
	auto parallel_for_clause_list = (parallel_clause | for_clause | sections_clause) >> *( !comma >> (parallel_clause | for_clause | sections_clause) );

	auto parallel_clause_list = !( 	(Tok<clang::tok::kw_for>("for") >> !parallel_for_clause_list)
								 |  (kwd("sections") >> !parallel_for_clause_list)
								 | 	(parallel_clause >> *(!comma >> parallel_clause))
								 );

	auto single_clause 	= 	(	// private(list)
								private_clause
							|	// firstprivate(list)
								firstprivate_clause
							|	// copyprivate(list)
							 	kwd("copyprivate") >> l_paren >> identifier_list["copyprivate"] >> r_paren
							|	// nowait
								kwd("nowait")
							);

	auto single_clause_list = !(single_clause >> *( !comma >> single_clause ));

	auto task_clause	 = 	(	// if(scalar-expression)
								if_expr
							|	// untied
								kwd("untied")
							|	// default(shared | none)
								def
							|	// private(list)
								private_clause
							| 	// firstprivate(list)
								firstprivate_clause
							|	// shared(list)
								kwd("shared") >> l_paren >> identifier_list["shared"] >> r_paren
							);

	auto task_clause_list = !(task_clause >> *( !comma >> task_clause ));

	// threadprivate(list)
	auto threadprivate_clause = l_paren >> identifier_list["thread_private"] >> r_paren;

	// define a PragmaNamespace for omp
	clang::PragmaNamespace* omp = new clang::PragmaNamespace("omp");
	pp.AddPragmaHandler(omp);

	// Add an handler for pragma omp parallel:
	// #pragma omp parallel [clause[ [, ]clause] ...] new-line
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpParallel>(pp.getIdentifierInfo("parallel"), parallel_clause_list >> tok::eom, "omp"));

	// omp for
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpFor>(pp.getIdentifierInfo("for"), for_clause_list >> tok::eom, "omp"));

	// #pragma omp sections [clause[[,] clause] ...] new-line
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpSections>(pp.getIdentifierInfo("sections"), sections_clause_list >> tok::eom, "omp"));

	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpSection>(pp.getIdentifierInfo("section"), tok::eom, "omp"));

	// omp single
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpSingle>(pp.getIdentifierInfo("single"), single_clause_list >> tok::eom, "omp"));

	// #pragma omp task [clause[[,] clause] ...] new-line
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpTask>(pp.getIdentifierInfo("task"), task_clause_list >> tok::eom, "omp"));

	// #pragma omp master new-line
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpMaster>(pp.getIdentifierInfo("master"), tok::eom, "omp"));

	// #pragma omp critical [(name)] new-line
	omp->AddPragma( PragmaHandlerFactory::CreatePragmaHandler<OmpCritical>(pp.getIdentifierInfo("critical"),
					!(l_paren >> identifier /*TODO */ >> r_paren) >> tok::eom, "omp"));

	//#pragma omp barrier new-line
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpBarrier>(pp.getIdentifierInfo("barrier"), tok::eom, "omp"));

	// #pragma omp taskwait newline
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpTaskWait>(pp.getIdentifierInfo("taskwait"), tok::eom, "omp"));

	// #pragma omp atimic newline
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpAtomic>(pp.getIdentifierInfo("atomic"), tok::eom, "omp"));

	// #pragma omp flush [(list)] new-line
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpFlush>(pp.getIdentifierInfo("flush"), !identifier_list["flush"] >> tok::eom, "omp"));

	// #pragma omp ordered new-line
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpOrdered>(pp.getIdentifierInfo("ordered"), tok::eom, "omp"));

	// #pragma omp threadprivate(list) new-line
	omp->AddPragma(PragmaHandlerFactory::CreatePragmaHandler<OmpThreadPrivate>(pp.getIdentifierInfo("threadprivate"), threadprivate_clause >> tok::eom, "omp"));
}

OmpAnnotationPtr handlePrivateClause(const MatchMap& mmap, conversion::ConversionFactory& fact) {

	auto fit = mmap.find("private");
	if(fit == mmap.end())
		return OmpAnnotationPtr();

	const ValueList& vars = fit->second;

	OmpPrivate::VarList varList;
	for(ValueList::const_iterator it = vars.begin(), end = vars.end(); it != end; ++it) {
		clang::Stmt* varIdent = (*it)->take<clang::Stmt*>();
		assert(varIdent && "Private clause not containing var exps");
		clang::DeclRefExpr* refVarIdent = dyn_cast<clang::DeclRefExpr>(varIdent);
		assert(refVarIdent && "Private clause not containing a DeclRefExpr");
		varList.push_back( core::dynamic_pointer_cast<const core::VarExpr>(fact.ConvertExpr( *refVarIdent )) );
	}
	return OmpAnnotationPtr( new OmpPrivate(varList) );
}

OmpAnnotationPtr OmpParallel::toAnnotation(conversion::ConversionFactory& fact) const {

	return OmpAnnotationPtr();

}

OmpAnnotationPtr OmpFor::toAnnotation(conversion::ConversionFactory& fact) const {

	// We need to check if the
	return OmpAnnotationPtr();
}

OmpAnnotationPtr OmpSections::toAnnotation(conversion::ConversionFactory& fact) const {

	// We need to check if the
	return OmpAnnotationPtr();
}

OmpAnnotationPtr OmpSection::toAnnotation(conversion::ConversionFactory& fact) const {

	// We need to check if the
	return OmpAnnotationPtr();
}

OmpAnnotationPtr OmpSingle::toAnnotation(conversion::ConversionFactory& fact) const {

	// We need to check if the
	return OmpAnnotationPtr();
}

OmpAnnotationPtr OmpTask::toAnnotation(conversion::ConversionFactory& fact) const {

	// We need to check if the
	return OmpAnnotationPtr();
}

OmpAnnotationPtr OmpMaster::toAnnotation(conversion::ConversionFactory& fact) const {
	return OmpAnnotationPtr( new c_info::omp::OmpMaster );
}

OmpAnnotationPtr OmpCritical::toAnnotation(conversion::ConversionFactory& fact) const {

	// We need to check if the
	return OmpAnnotationPtr();
}

OmpAnnotationPtr OmpBarrier::toAnnotation(conversion::ConversionFactory& fact) const {
	return OmpAnnotationPtr( new c_info::omp::OmpBarrier );
}

OmpAnnotationPtr OmpTaskWait::toAnnotation(conversion::ConversionFactory& fact) const {

	// We need to check if the
	return OmpAnnotationPtr();
}

OmpAnnotationPtr OmpAtomic::toAnnotation(conversion::ConversionFactory& fact) const {

	// We need to check if the
	return OmpAnnotationPtr();
}

OmpAnnotationPtr OmpFlush::toAnnotation(conversion::ConversionFactory& fact) const {

	// We need to check if the
	return OmpAnnotationPtr();
}

OmpAnnotationPtr OmpOrdered::toAnnotation(conversion::ConversionFactory& fact) const {

	// We need to check if the
	return OmpAnnotationPtr();
}

OmpAnnotationPtr OmpThreadPrivate::toAnnotation(conversion::ConversionFactory& fact) const {

	// We need to check if the
	return OmpAnnotationPtr();
}

} // End omp namespace
} // End frontend namespace
} // End insieme namespace
