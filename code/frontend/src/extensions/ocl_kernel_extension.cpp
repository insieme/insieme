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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#pragma GCC diagnostic ignored "-Wuninitialized"
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
	#include "clang/AST/StmtVisitor.h"
	#include <clang/AST/Expr.h>

	#include <clang/Basic/FileManager.h>
#pragma GCC diagnostic pop

#include "insieme/frontend/clang.h"

#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/extensions/ocl_kernel_extension.h"
#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/frontend/stmt_converter.h"




using namespace insieme::frontend;

namespace {


}

namespace insieme {
namespace frontend {
namespace extensions {


//////////////////////////////////////////////////////////////////////////////////////
//               adding OpenCL kernel annotation
void OclKernelPlugin::PostVisit(const clang::Decl* decl, conversion::Converter& convFact) {

	// check function decls:
	if (const clang::FunctionDecl* funcDecl = llvm::dyn_cast<clang::FunctionDecl>(decl)){
		// check Attributes of the function definition
		annotations::ocl::BaseAnnotation::AnnotationList kernelAnnotation;

		if (funcDecl->hasAttrs()) {
			const clang::AttrVec attrVec = funcDecl->getAttrs();

			for (clang::AttrVec::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
				if (clang::AnnotateAttr * attr = llvm::dyn_cast<clang::AnnotateAttr>(*I)) {
					//get annotate string
					llvm::StringRef&& sr = attr->getAnnotation();

					//check if it is an OpenCL kernel function
					if ( sr == "__kernel" ) {
						VLOG(1) << "is OpenCL kernel function";
						kernelAnnotation.push_back( std::make_shared<annotations::ocl::KernelFctAnnotation>() );
					}
				}
				else if ( clang::ReqdWorkGroupSizeAttr* attr = llvm::dyn_cast<clang::ReqdWorkGroupSizeAttr>(*I) ) {
					kernelAnnotation.push_back(
							std::make_shared<annotations::ocl::WorkGroupSizeAnnotation>( attr->getXDim(), attr->getYDim(), attr->getZDim() )
					);
				}
			}
		}

	// if OpenCL related annotations have been found, create OclBaseAnnotation and add it to the funciton's attribute
		if (!kernelAnnotation.empty()) {
			core::ExpressionPtr lambda = convFact.getLambdaFromCache(funcDecl);
			lambda->addAnnotation( std::make_shared<annotations::ocl::BaseAnnotation>(kernelAnnotation) );
			convFact.addToLambdaCache(funcDecl, lambda);
		}
	}

	// check type Decls
	else if (const clang::TypeDecl* typeDecl = llvm::dyn_cast<clang::TypeDecl>(decl)){
		typeDecl->dump();
	}

	// check var Decls
	else if (const clang::VarDecl* varDecl = llvm::dyn_cast<clang::VarDecl>(decl)){
		varDecl->dump();
	}

	// check any other decl
	//else if (const clang::WHATEVER* funcDecl = llvm::dyn_cast<clang::WHATEVER>(decl)){
	//}
}

stmtutils::StmtWrapper OclKernelPlugin::PostVisit(const clang::Stmt* stmt, const stmtutils::StmtWrapper& irStmt,
                                                     insieme::frontend::conversion::Converter& convFact) {
	// Function declarations have only one single statement
	if(!irStmt.isSingleStmt())
		return irStmt;

	if(!llvm::isa<clang::AttributedStmt>(stmt))
		return irStmt;

	insieme::annotations::ocl::BaseAnnotation::AnnotationList kernelAnnotation;
	const clang::AttributedStmt* funcDecl = llvm::dyn_cast<clang::AttributedStmt>(stmt);
	const llvm::ArrayRef<const clang::Attr*> attrVec = funcDecl->getAttrs();

	for (llvm::ArrayRef<const clang::Attr*>::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
		if (const clang::AnnotateAttr * attr = llvm::dyn_cast<clang::AnnotateAttr>(*I)) {
			//get annotate string
			llvm::StringRef&& sr = attr->getAnnotation();

			//check if it is an OpenCL kernel function
			if ( sr == "__kernel" ) {
					VLOG(1) << "is OpenCL kernel function";
					kernelAnnotation.push_back( std::make_shared<insieme::annotations::ocl::KernelFctAnnotation>() );
			}
		}

    }

	assert(false && "post visit ");
	return irStmt;
}

//////////////////////////////////////////////////////////////////////////////////////
//               opencl kernel file post processing

insieme::core::ProgramPtr OclKernelPlugin::IRVisit(insieme::core::ProgramPtr& prog) {
	insieme::core::NodeManager& mgr(prog->getNodeManager());
	// call OpenCL kernel post processing
	ocl::Compiler oclCompiler(prog, mgr);
	return oclCompiler.lookForOclAnnotations();
}

} //namespace plugin
} //namespace frontnt
} //namespace extensions
