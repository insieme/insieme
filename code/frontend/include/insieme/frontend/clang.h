/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#pragma once

/**
 * This header file should be included only by cpp files depending on clang.
 * Within header files, forward declarations should be sufficient.
 */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#pragma GCC diagnostic ignored "-Wuninitialized"
#pragma GCC diagnostic ignored "-Wclass-memaccess"

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
