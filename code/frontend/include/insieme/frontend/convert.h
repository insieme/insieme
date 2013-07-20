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

#include <set>
#include <memory>
#include <functional>

#include "insieme/frontend/frontend.h"
#include "insieme/frontend/program.h"
#include "insieme/frontend/utils/interceptor.h"
#include "insieme/frontend/pragma/handler.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/map_utils.h"

// FIXME: cleanup includes and stuff, find tradeof between compilation time and code complexity
// Forward declarations
namespace clang {
class FunctionDecl;
class InitListExpr;
} // End clang namespace

namespace {

typedef vector<insieme::core::StatementPtr> StatementList;
typedef vector<insieme::core::ExpressionPtr> ExpressionList;

#define GET_TYPE_PTR(type) (type)->getType().getTypePtr()


} // end anonymous namespace

namespace insieme {
namespace frontend {

/**
 * This function converts a clang translation unit into an IR translation unit.
 *
 * @param manager the manager to be used for managing the resulting IR nodes
 * @param unit the translation unit to be processed
 * @param setup the setup for the conversion process to be respected
 * @return the resulting translation unit
 */
tu::IRTranslationUnit convert(core::NodeManager& manager, const path& unit, const ConversionSetup& setup = ConversionSetup());

/**
 * This function converts a list of translation units into a list of IR translation units.
 *
 * @param manager the manager to be used for managing the resulting IR nodes
 * @param units the translation units to be processed
 * @param setup the setup for the conversion process to be respected
 * @return the resulting translation units
 */
vector<tu::IRTranslationUnit> convert(core::NodeManager& manager, const vector<path>& units, const ConversionSetup& setup = ConversionSetup());


namespace conversion {

// ------------------------------------ Converter ---------------------------
/**
 * The main unit orchestraiting the clang AST => IR Translation Unit conversion processes.
 */
class Converter :  boost::noncopyable {

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 					Cache of already converted elements
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	 * Maps Clang variable declarations (VarDecls and ParmVarDecls) to IR variables.
	 */
	typedef std::map<const clang::ValueDecl*, core::ExpressionPtr> VarDeclMap;
	VarDeclMap varDeclMap;

	/**
	 * Stores the generated IR for function declarations
	 */
	typedef std::map<const clang::FunctionDecl*, insieme::core::ExpressionPtr> LambdaExprMap;
	LambdaExprMap lambdaExprCache;

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 						Recursive Function resolution
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	 * Maps a function with the variable which has been introduced to represent
	 * the function in the recursive definition
	 */
	typedef std::map<const clang::FunctionDecl*, insieme::core::VariablePtr> RecVarExprMap;
	RecVarExprMap recVarExprMap;

	/**
	 * This variable stores the list of parameters passed as an argument to the currently processed
	 * function.
	 */
	// TODO: remove this - not required
	typedef const vector<core::VariablePtr>* ParameterList;
	ParameterList curParameter;


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 						Specifically marked Objects
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	std::set<const clang::VarDecl*> thread_private;

	/*
	 * Every time an input parameter of a function of type 'a is improperly used as a ref<'a>
	 * a new variable is created in function body and the value of the input parameter assigned to it
	 */
	typedef insieme::utils::map::PointerMap<insieme::core::VariablePtr, insieme::core::VariablePtr> WrapRefMap;
	WrapRefMap wrapRefMap;

	/*	FIXME: rename --> takes care of TagDecl not ClassDecl!
		TagDecl are for struct/union/class/enum --> used in C and CXX */
	// maps the resulting type pointer to the declaration of a class
	typedef std::map<const clang::TagDecl*, core::TypePtr> ClassDeclMap;
	ClassDeclMap classDeclMap;


	/*
	 * Keeps the CXXTemporaries together with their IR declaration stmt
	 */
	typedef std::map<const clang::CXXTemporary*, core::DeclarationStmtPtr> TemporaryInitMap;
	TemporaryInitMap tempInitMap;


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 						Diagnosis system
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	 * list of warnings up to this point
	 */
	std::set<std::string> warnings;


	//	GlobalIdentMap globalIdentMap;                                  ////////////////////////////////
	core::NodeManager& mgr;
	const core::IRBuilder builder;

	/**
	 * Converts a Clang statements into an IR statements.
	 */
	class StmtConverter;
	class CStmtConverter;
	class CXXStmtConverter;
	std::shared_ptr<StmtConverter> stmtConvPtr;

	/**
	 * Converts a Clang types into an IR types.
	 */
	class TypeConverter;
	class CTypeConverter;
	class CXXTypeConverter;
	std::shared_ptr<TypeConverter> typeConvPtr;

	/**
	 * Converts a Clang expression into an IR expression.
	 */
	class ExprConverter;
	class CExprConverter;
	class CXXExprConverter;
	std::shared_ptr<ExprConverter> exprConvPtr;

	const Program& program;

	/**
	 * Maps of statements to pragmas.
	 */
	pragma::PragmaStmtMap pragmaMap;

	tu::IRTranslationUnit irTranslationUnit;

	bool used;

	/**
	 * Attach annotations to a C function of the input program.
	 *
	 * returns the a MarkerExprPtr if a marker node has to be added and the passed node else
	 */
	core::ExpressionPtr attachFuncAnnotations(const core::ExpressionPtr& node,
			const clang::FunctionDecl* funcDecl);

public:

	Converter(core::NodeManager& mgr, const Program& program);

	// should only be run once
	tu::IRTranslationUnit convert();

	// Getters & Setters
	const core::IRBuilder& getIRBuilder() const {
		return builder;
	}
	core::NodeManager& getNodeManager() const {
		return mgr;
	}
	const Program& getProgram() const {
		return program;
	}

	const ClangCompiler& getCompiler() const {
		return program.getCompiler();
	}

	clang::Preprocessor& getPreprocessor() const {
		return getCompiler().getPreprocessor();
	}

	clang::SourceManager& getSourceManager() const {
		return getCompiler().getSourceManager();
	}

	tu::IRTranslationUnit& getIRTranslationUnit() {
		return irTranslationUnit;
	}

	const tu::IRTranslationUnit& getIRTranslationUnit() const {
		return irTranslationUnit;
	}

	/**
	 * Determines the definition of the given generic type pointer within the
	 * internally maintained IR Translation Unit. If non is present, the given
	 * type will be returned.
	 */
	const core::TypePtr lookupTypeDetails(const core::GenericTypePtr& type) const {
		core::TypePtr res = getIRTranslationUnit()[type];
		return (res)?res:type;
	}

	/**
	 * Determines the definition of the given generic type pointer within the
	 * internally maintained IR Translation Unit. If non is present, the given
	 * type will be returned.
	 */
	const core::TypePtr lookupTypeDetails(const core::TypePtr& type) const {
		return (type.isa<core::GenericTypePtr>()) ? lookupTypeDetails(type.as<core::GenericTypePtr>()) : type;
	}

	/**
	 * Returns a reference to the IR data structure used to represent a variable of the input C program.
	 *
	 * The function guarantees that the same variable in the input program is always represented in the
	 * IR with the same generated Variable and in the case of access to global variables, a reference
	 * to a member of the global data structure is returned.
	 */
	core::ExpressionPtr lookUpVariable(const clang::ValueDecl* valDecl);

	/**
	 * Returns a map which associates a statement of the clang AST to a pragma (if any)
	 * @return The statement to pragma multimap
	 */
	const pragma::PragmaStmtMap& getPragmaMap() const {
		return pragmaMap;
	}

	/**
	 * Entry point for converting function to the right type
	 * @param dcl declaration of the function
	 * @return the corresponding IR type
	 */
	core::FunctionTypePtr convertFunctionType(const clang::FunctionDecl* dcl);

	/**
	 * Entry point for converting clang types into an IR types
	 * @param type is a clang type
	 * @return the corresponding IR type
	 */
	core::TypePtr convertType(const clang::Type* type);

	/**
	 * Entry point for converting clang statements into IR statements
	 * @param stmt is a clang statement of the AST
	 * @return the corresponding IR statement
	 */
	core::StatementPtr convertStmt(const clang::Stmt* stmt) const;

	/**
	 * Entry point for converting clang expressions to IR expressions
	 * @param expr is a clang expression of the AST
	 * @return the corresponding IR expression
	 */
	core::ExpressionPtr convertExpr(const clang::Expr* expr) const;

	/**
	 * Looks for eventual attributes attached to the clang variable declarations (used for OpenCL implementation)
	 * and returns corresponding IR annotations to be attached to the IR corresponding declaration node.
	 * @param varDecl clang Variable declaration AST node
	 * @return IR annotation
	 */
	core::NodeAnnotationPtr convertAttribute(const clang::ValueDecl* valDecl) const;

	/**
	 * Converts a function declaration into an IR lambda.
	 * @param funcDecl is a clang FunctionDecl which represent a definition for the function
	 * @param isEntryPoint determine if this function is an entry point of the generated IR
	 * @return Converted lambda
	 */
	core::ExpressionPtr convertFunctionDecl(const clang::FunctionDecl* funcDecl);

	/**
	 * Converts variable declarations into IR an declaration statement. This method is also responsible
	 * to map the generated IR variable with the translated variable declaration, so that later uses
	 * of the variable can be mapped to the same IR variable (see lookupVariable method).
	 * @param varDecl a clang variable declaration
	 * @return The IR translation of the variable declaration
	 */
	core::StatementPtr convertVarDecl(const clang::VarDecl* varDecl);

	/**
	 * Returns the default initialization value of the IR type passed as input.
	 * @param type is the IR type
	 * @return The default initialization value for the IR type
	 */
	// TODO: should most likely be subsituted / merged by with zero
	core::ExpressionPtr defaultInitVal(const core::TypePtr& type) const;

	
	/**
	 * converts the initilaization of a variable
	 * @param clangType the clang type of the whole expression, used to retrieve array sizes
	 * @param expr the clang expression to convert
	 * @param type the inspire type exprected for the structure
	 * @param zeroInit should the values be initialized to 0?
	 * @return the IR expression ready to be coupled with the decl
	 */
	// TODO: make it private where it is used
	core::ExpressionPtr convertInitExpr(const clang::Type* clangType, const clang::Expr* expr,
												const core::TypePtr& type, const bool zeroInit) ;



	/**
	 * Utility function which tries to apply the deref operation. If the input expression is not a of ref type
	 * the same expression is returned.
	 * @param expr IR expression which could be of ref or non-ref type
	 * @return a non RefType IR expression
	 */
	// TODO: use the builder equivalent
	core::ExpressionPtr tryDeref(const core::ExpressionPtr& expr) const;

	/**
	 * Utility function which tries to return the derefed type. If the input tyoe is not a of ref type
	 * the same type is returned.
	 * @param type IR type which could be of ref or non-ref type
	 * @return a non RefType IR type
	 */
	// TODO: use the builder equivalent
	core::TypePtr tryDeref(const core::TypePtr& type) const;

	/**
	 * Allows access to the set of threadprivates stored in the context.
	 */
	// TODO: try remove - apparently not used anywhere
	const std::set<const clang::VarDecl*>& getThreadprivates() const {
		return thread_private;
	}

	/**
	 * Provides access to the set of threadprivates stored in the context.
	 */
	std::set<const clang::VarDecl*>& getThreadprivates() {
		return thread_private;
	}


	
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  some helper tools   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
   /**
	* Creates the variable which should be used as a placeholder for invoking the iven
	* function call and isert it in the map (recVarExprMap) used to store such ariables
	* which are valid during the conversion of the given recursive function cycle
	*/
	// TODO: make private to the place where it is used
	core::StatementPtr materializeReadOnlyParams(const core::StatementPtr& body, const vector<core::VariablePtr>& params);


	/**
	 * print diagnosis errors, warnings stored during translation
	 * sometimes we can not retrieve a location to attach the error to, we'll store it
	 * and it will be printed as soon as a location can be used
	 * @param loc: the location this warning will be attached to
	 */
	void printDiagnosis(const clang::SourceLocation& loc);

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  CPP STUFF   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	// FIXME: where here and not expr visitor????
	core::ExpressionPtr convertInitializerList(
			const clang::InitListExpr* initList,
			const core::TypePtr& type) ;
};

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
