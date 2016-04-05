/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#pragma once

/**
 * This header file should be included only by cpp files depending on clang.
 * Within header files, forward declarations should be sufficient.
 */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#pragma GCC diagnostic ignored "-Wuninitialized"

#if defined (__clang__) && (__clang_major__ > 3 || (__clang_major__ == 3 && __clang_minor__ >= 7))
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wredundant-move"
#endif

#include <clang/Frontend/ASTUnit.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/CompilerInvocation.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>

#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/CXXInheritance.h>
#include <clang/AST/Decl.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/DeclVisitor.h>
#include <clang/AST/Expr.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/RecursiveASTVisitor.h>
#include <clang/AST/StmtVisitor.h>
#include <clang/AST/TemplateBase.h>
#include <clang/AST/TypeVisitor.h>

#include <clang/Basic/Diagnostic.h>
#include <clang/Basic/FileManager.h>
#include <clang/Basic/SourceLocation.h>
#include <clang/Basic/SourceManager.h>
#include <clang/Basic/TargetInfo.h>
#include <clang/Basic/Version.h>

#include <clang/Lex/Token.h>
#include <clang/Lex/Preprocessor.h>

#include <clang/Sema/Lookup.h>
#include <clang/Sema/Ownership.h>
#include <clang/Sema/Sema.h>
#include <clang/Sema/SemaConsumer.h>

#include <clang/Parse/Parser.h>

#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Support/Casting.h>
#include <clang/Frontend/Utils.h>
#include <clang/Lex/HeaderSearch.h>

#if defined (__clang__) && (__clang_major__ > 3 || (__clang_major__ == 3 && __clang_minor__ >= 7))
#pragma clang diagnostic pop
#endif
#pragma GCC diagnostic pop

template <typename ClangType>
static inline std::string dumpClang(ClangType* node) {
	static_assert(std::is_base_of<clang::Stmt, ClangType>::value || std::is_base_of<clang::Decl, ClangType>::value
	                  || std::is_base_of<clang::Type, ClangType>::value,
	              "Can only dump clang nodes");
	std::string ret;
	llvm::raw_string_ostream strostream(ret);
	node->dump(strostream);
	return ret;
}

template <typename ClangType>
static inline std::string dumpClang(ClangType* node, clang::SourceManager& sm) {
	static_assert(std::is_base_of<clang::Stmt, ClangType>::value || std::is_base_of<clang::Decl, ClangType>::value
	                  || std::is_base_of<clang::Type, ClangType>::value,
	              "Can only dump clang nodes");
	std::string ret;
	llvm::raw_string_ostream strostream(ret);
	node->dump(strostream, sm);
	return ret;
}
