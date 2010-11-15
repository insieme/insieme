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
	struct ConversionContext :  public boost::noncopyable {

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// 						Recursive Function resolution
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Maps Clang variable declarations (VarDecls and ParmVarDecls) to IR variables.
		typedef std::map<const clang::VarDecl*, core::VariablePtr> VarDeclMap;
		VarDeclMap varDeclMap;

		// Stores the generated IR for function declarations
		typedef std::map<const clang::FunctionDecl*, insieme::core::ExpressionPtr> LambdaExprMap;
		LambdaExprMap lambdaExprCache;

		// Maps a function with the variable which has been introduced to represent
		// the function in the recursive definition
		typedef std::map<const clang::FunctionDecl*, insieme::core::VariablePtr> RecVarExprMap;
		RecVarExprMap recVarExprMap;

		// When set this variable tells the frontend to resolve eventual recursive function call
		// using the mu variables which has been previously placed in the recVarExprMap
		bool isRecSubFunc;

		// It tells the frontend the body of a recursive function is being resolved and
		// eventual functions which are already been resolved should be not converted again
		// but read from the map
		bool isResolvingRecFuncBody;

		// This variable points to the current mu variable representing the start of the
		// recursion
		core::VariablePtr currVar;

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// 						Recursive Type resolution
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		typedef std::map<const clang::Type*, insieme::core::TypeVariablePtr> TypeRecVarMap;
		TypeRecVarMap recVarMap;
		bool isRecSubType;

		typedef std::map<const clang::Type*, insieme::core::TypePtr> RecTypeMap;
		RecTypeMap recTypeCache;

		bool isResolvingFunctionType;

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// 						Global variables utility
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Gloabal and static variables
		// map which stores, for each static or global variable, the identifier which will be used
		// as identification within the global data structure and the initialization value
		core::VariablePtr   globalVar;

		typedef std::set<const clang::FunctionDecl*> UseGlobalFuncMap;
		UseGlobalFuncMap	globalFuncMap;
		core::VariablePtr	currGlobalVar;

		core::StructTypePtr globalStructType;
		core::StructExprPtr	globalStructExpr;

		std::unordered_map<insieme::core::VariablePtr, insieme::core::VariablePtr, hash_target<insieme::core::VariablePtr>,
			equal_target<insieme::core::VariablePtr>> needRef;

		ConversionContext(): isRecSubFunc(false), isResolvingRecFuncBody(false), isRecSubType(false), isResolvingFunctionType(false) { }
	};

	ConversionContext 		ctx;

	/**
	 * Converts a Clang statements into an IR statements.
	 */
	class ClangStmtConverter;
	// Instantiates the statement converter
	static ClangStmtConverter* makeStmtConverter(ConversionFactory& fact);
	std::auto_ptr<ClangStmtConverter> stmtConv; // PIMPL pattern

	/**
	 * Converts a Clang types into an IR types.
	 */
	class ClangTypeConverter;
	// Instantiates the type converter
	static ClangTypeConverter* makeTypeConverter(ConversionFactory& fact);
	std::auto_ptr<ClangTypeConverter> typeConv; // PIMPL pattern

	/**
	 * Converts a Clang expression into an IR expression.
	 */
	class ClangExprConverter;
	// Instantiates the expression converter
	static ClangExprConverter* makeExprConverter(ConversionFactory& fact);
	std::auto_ptr<ClangExprConverter> exprConv; // PIMPL pattern

	core::NodeManager& 		mgr;
	const core::ASTBuilder  builder;
    Program& 				program;

    /**
     * Maps of statements to pragmas.
     */
    PragmaStmtMap 	 		pragmaMap;

    /**
     * A pointer to the translation unit which is currently used to resolve symbols, i.e. literals
     * Every time a function belonging to a different translation unit is called this pointer
     * is set to translation unit containing the function definition.
     */
    const TranslationUnit*	currTU;

    /**
     * Returns a reference to the IR data structure used to represent a variable of the input C program.
     *
     * The function guarantees that the same variable in the input program is always represented in the
     * IR with the same generated Variable and in the case of access to global variables, a reference
     * to a member of the global data structure is returned.
     */
	core::ExpressionPtr lookUpVariable(const clang::VarDecl* varDecl);
	core::ExpressionPtr convertInitializerList(const clang::InitListExpr* initList, const core::TypePtr& type) const;

	/**
	 * Attach annotations to a C function of the input program.
	 */
	void attachFuncAnnotations(core::ExpressionPtr& node, const clang::FunctionDecl* funcDecl);

	friend class ASTConverter;
public:
	ConversionFactory(core::NodeManager& mgr, Program& program);

	const core::ASTBuilder& getASTBuilder() const { return builder; }
	core::NodeManager& 	getNodeManager() const { return mgr; }

	const PragmaStmtMap& getPragmaMap() const { return pragmaMap; }

	core::TypePtr 		convertType(const clang::Type* type) const;
	core::StatementPtr 	convertStmt(const clang::Stmt* stmt) const;
	core::ExpressionPtr convertExpr(const clang::Expr* expr) const;

	core::ExpressionPtr 	 convertFunctionDecl(const clang::FunctionDecl* funcDecl, bool isEntryPoint=false);
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
	core::NodeManager&	mgr;
	Program& 			mProg;
	ConversionFactory   mFact;
	core::ProgramPtr    mProgram;

public:
	ASTConverter(core::NodeManager& mgr, Program& prog) : mgr(mgr), mProg(prog), mFact(mgr, prog), mProgram(prog.getProgram()) { }

	core::ProgramPtr getProgram() const { return mProgram; }

	core::ProgramPtr 	handleFunctionDecl(const clang::FunctionDecl* funcDecl, bool isMain=false);
	core::LambdaExprPtr	handleBody(const clang::Stmt* body, const TranslationUnit& tu);

};


} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
