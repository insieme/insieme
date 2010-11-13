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

#include "insieme/core/program.h"
#include "insieme/core/ast_builder.h"

#include "insieme/frontend/pragma_handler.h"

// Forward declarations
namespace clang {
class ASTContext;
class DeclGroupRef;
class FunctionDecl;
class InitListExpr;
namespace idx {
class Indexer;
class Program;
} // End idx namespace
} // End clang namespace


namespace {
typedef vector<insieme::core::StatementPtr>  StatementList;
typedef vector<insieme::core::ExpressionPtr> ExpressionList;

#define GET_TYPE_PTR(type) (type)->getType().getTypePtr()
}

namespace insieme {
namespace frontend {
namespace conversion {

class ASTConverter;

// ------------------------------------ ConversionFactory ---------------------------
/**
 * A factory used to convert clang AST nodes (i.e. statements, expressions and types) to Insieme IR nodes.
 */
class ConversionFactory : public boost::noncopyable {

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							ConversionContext
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Keeps all the information gathered during the conversion process.
	// Maps for variable names, cached resolved function definitions and so on...
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	struct ConversionContext {

		// Maps Clang variable declarations (VarDecls and ParmVarDecls) to an
		// IR variable.
		typedef std::map<const clang::VarDecl*, core::VariablePtr> VarDeclMap;
		VarDeclMap varDeclMap;

		// Map for resolved lambda functions
		typedef std::map<const clang::FunctionDecl*, insieme::core::ExpressionPtr> LambdaExprMap;
		LambdaExprMap lambdaExprCache;

		// Maps a function with the variable which has been introduced to represent
		// the function in the recursive definition
		typedef std::map<const clang::FunctionDecl*, insieme::core::VariablePtr> RecVarExprMap;
		RecVarExprMap recVarExprMap;

		bool isRecSubFunc;
		bool isResolvingRecFuncBody;
		core::VariablePtr currVar;

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// 						Recursive Type resolution
		typedef std::map<const clang::Type*, insieme::core::TypeVariablePtr> TypeRecVarMap;
		TypeRecVarMap recVarMap;
		bool isRecSubType;

		typedef std::map<const clang::Type*, insieme::core::TypePtr> RecTypeMap;
		RecTypeMap recTypeCache;

		bool isResolvingFunctionType;

		// Gloabal and static variables
		// map which stores, for each static or global variable, the identifier which will be used
		// as identification within the global data structure and the initialization value
		core::VariablePtr   globalVar;

		typedef std::set<const clang::FunctionDecl*> UseGlobalFuncMap;
		UseGlobalFuncMap	globalFuncMap;
		core::VariablePtr	currGlobalVar;

		core::StructTypePtr globalStructType;
		core::StructExprPtr	globalStructExpr;

		ConversionContext(): isRecSubFunc(false), isResolvingRecFuncBody(false), isRecSubType(false), isResolvingFunctionType(false) { }
	};

	ConversionContext 		ctx;

	// PIMPL pattern
	class ClangStmtConverter;
	static ClangStmtConverter* makeStmtConverter(ConversionFactory& fact);
	std::auto_ptr<ClangStmtConverter> stmtConv;

	class ClangTypeConverter;
	static ClangTypeConverter* makeTypeConverter(ConversionFactory& fact);
	std::auto_ptr<ClangTypeConverter> typeConv;

	class ClangExprConverter;
	static ClangExprConverter* makeExprConverter(ConversionFactory& fact);
	std::auto_ptr<ClangExprConverter> exprConv;

	core::SharedNodeManager mgr;
	const core::ASTBuilder  builder;
    Program& 				program;
    PragmaStmtMap 	 		pragmaMap;
    const TranslationUnit*	currTU;

	core::ExpressionPtr lookUpVariable(const clang::VarDecl* varDecl);
	core::ExpressionPtr convertInitializerList(const clang::InitListExpr* initList, const core::TypePtr& type) const;
	void attachFuncAnnotations(core::ExpressionPtr& node, const clang::FunctionDecl* funcDecl);

	friend class ASTConverter;
public:
	ConversionFactory(core::SharedNodeManager mgr, Program& program, const PragmaList& pList = PragmaList());

	const core::ASTBuilder& getASTBuilder() const { return builder; }
	core::SharedNodeManager getNodeManager() const { return mgr; }

	const PragmaStmtMap& getPragmaMap() const { return pragmaMap; }

	core::TypePtr 		convertType(const clang::Type* type) const;
	core::StatementPtr 	convertStmt(const clang::Stmt* stmt) const;
	core::ExpressionPtr convertExpr(const clang::Expr* expr) const;

	core::ExpressionPtr 	 convertFunctionDecl(const clang::FunctionDecl* funcDecl);
	core::DeclarationStmtPtr convertVarDecl(const clang::VarDecl* funcDecl);
	core::ExpressionPtr	 	 defaultInitVal(const core::TypePtr& type) const;
	core::ExpressionPtr 	 convertInitExpr(const clang::Expr* expr, const core::TypePtr& type) const ;

	core::AnnotationPtr convertAttribute(const clang::VarDecl* varDecl) const;

	core::ExpressionPtr tryDeref(const core::ExpressionPtr& expr) const;
	void setTranslationUnit(const TranslationUnit& tu) { currTU = &tu; }

	core::CallExprPtr createCallExpr(const StatementList& body, core::TypePtr retTy, bool useCapture=false) const;

};

// ------------------------------------ ASTConverter ---------------------------
/**
 *
 */
class ASTConverter {
	Program& mProg;
	ConversionFactory    mFact;
	core::ProgramPtr     mProgram;

public:
	ASTConverter(Program& prog, const core::SharedNodeManager& mgr, const PragmaList& pList) :
		mProg(prog), mFact(mgr, prog, pList), mProgram(prog.getProgram()) { }

	core::ProgramPtr getProgram() const { return mProgram; }

	core::ExpressionPtr handleFunctionDecl(const clang::FunctionDecl* funcDecl, const TranslationUnit& tu) {
		mFact.currTU = &tu;
		return mFact.convertFunctionDecl(funcDecl);
	}
	core::LambdaExprPtr	handleBody(const clang::Stmt* body, const TranslationUnit& tu);
	core::ProgramPtr 	handleTranslationUnit(const clang::DeclContext* declCtx, const TranslationUnit& tu);
};


} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
