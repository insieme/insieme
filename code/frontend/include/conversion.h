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

#pragma once

#include "pragma_handler.h"
#include "programs.h"

#include "clang/AST/ASTConsumer.h"

// Forward declarations
namespace clang {
class ASTContext;
class DeclGroupRef;
}

namespace insieme {

class ClangStmtConverter;
class ClangTypeConverter;

struct TypeWrapper {
	core::TypePtr ref;
	TypeWrapper(): ref(core::TypePtr(NULL)) { }
	TypeWrapper(core::TypePtr type): ref(type) { }
};

struct StmtWrapper {
	core::StatementPtr ref;
	StmtWrapper(): ref(core::StatementPtr(NULL)) { }
	StmtWrapper(core::StatementPtr stmt): ref(core::StatementPtr(stmt)) { }
};

class ConversionFactory {
	ClangStmtConverter* stmtConv;
	ClangTypeConverter* typeConv;

	ConversionFactory(core::NodeManager& mgr);

	static ConversionFactory& get(core::Program::SharedDataManager mgr = core::Program::SharedDataManager()) {
		static ConversionFactory theConversionFactory(*mgr);
		return theConversionFactory;
	}
public:
	static void init(core::Program::SharedDataManager mgr) { get(mgr); }
	static TypeWrapper ConvertType(const clang::Type& type);
	static StmtWrapper ConvertStmt(const clang::Stmt& stmt);

	~ConversionFactory();
};

// ------------------------------------ InsiemeIRConsumer ---------------------------

class InsiemeIRConsumer: public clang::ASTConsumer {
	clang::ASTContext* mCtx;
	insieme::core::Program::SharedDataManager mDataMgr;

public:
	InsiemeIRConsumer(const insieme::core::Program::SharedDataManager& dataMgr) : mCtx(NULL), mDataMgr(dataMgr){
		ConversionFactory::init(mDataMgr);
	}

	virtual void Initialize(clang::ASTContext &Context) { mCtx = &Context; }
	virtual void HandleTopLevelDecl(clang::DeclGroupRef D);
	virtual void HandleTranslationUnit(clang::ASTContext &Ctx);
};



}

