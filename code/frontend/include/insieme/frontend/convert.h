/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/translation_unit.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/utils/frontend_ir.h"


#include "insieme/core/ir_program.h"
#include "insieme/core/frontend_ir_builder.h"

#include "insieme/utils/map_utils.h"

#include "insieme/frontend/clang_forward.h"

namespace {
	typedef vector<insieme::core::StatementPtr> StatementList;
	typedef vector<insieme::core::ExpressionPtr> ExpressionList;

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
tu::IRTranslationUnit convert(core::NodeManager& manager, const path& unit, const ConversionSetup& setup=ConversionSetup());

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

	/**
	 * stores converted types
	 */
	struct CompareQualType { // QualType comparison function
		bool operator()(const clang::QualType& a, const clang::QualType& b) { return a.getAsOpaquePtr () > b.getAsOpaquePtr ();  }
	};
	typedef std::map<clang::QualType, insieme::core::TypePtr, CompareQualType> TypeCache;
	TypeCache typeCache;

    /**
     * Stores static variable names
     **/
    typedef std::map<const clang::VarDecl*, std::string> StaticVarDeclMap;
    StaticVarDeclMap staticVarDeclMap;
    int staticVarCount;
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 					Function resolution
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


	/**
	 * list of warnings up to this point
	 */
	std::set<std::string> warnings;



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

	const TranslationUnit& translationUnit;
	const ConversionSetup& convSetup;

	/**
	 * Maps of statements to pragmas.
	 */
	pragma::PragmaStmtMap pragmaMap;

	tu::IRTranslationUnit irTranslationUnit;

	/**
	 * 	signle used flag to avoid duplicated instantiation
	 * 	this is wrong...
	 */
	bool used;

	//////////////////////////////////////////////////
	// IR building and managing tools
	core::NodeManager& mgr;
	const core::FrontendIRBuilder builder;
	const frontend::ir::FrontendIr feIR;

	/**
	 * Attach annotations to a C function of the input translation unit.
	 *
	 * returns the a MarkerExprPtr if a marker node has to be added and the passed node else
	 */
	core::ExpressionPtr attachFuncAnnotations(const core::ExpressionPtr& node,
			const clang::FunctionDecl* funcDecl);

	/**
	 *  keeps track of the last point a source location to the input code could be found
	 */
	std::stack<clang::SourceLocation> lastTrackableLocation;

	frontend::utils::HeaderTagger headerTagger;

     void convertFunctionDeclImpl(const clang::FunctionDecl* funcDecl);

public:

	Converter(core::NodeManager& mgr, const TranslationUnit& translationUnit, const ConversionSetup& setup = ConversionSetup());

	// should only be run once
	tu::IRTranslationUnit convert();

	// Getters & Setters
	const core::FrontendIRBuilder& getIRBuilder() const {
		return builder;
	}
	core::NodeManager& getNodeManager() const {
		return mgr;
	}
	const TranslationUnit& getTranslationUnit() const {
		return translationUnit;
	}
	const frontend::ir::FrontendIr& getFrontendIR() const {
		return feIR;
	}

	const ClangCompiler& getCompiler() const {
		return translationUnit.getCompiler();
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

    std::shared_ptr<ExprConverter> getExprConverter() const {
        return exprConvPtr;
    }

    std::shared_ptr<StmtConverter> getStmtConverter() const {
        return stmtConvPtr;
    }

    std::shared_ptr<TypeConverter> getTypeConverter() const {
        return typeConvPtr;
    }

    const ConversionSetup& getConversionSetup() const {
        return convSetup;
    }

    void addToTypeCache(const clang::QualType type, core::TypePtr ptr) {
        typeCache[type] = ptr;
    }

    void addToLambdaCache(const clang::FunctionDecl* decl, core::ExpressionPtr ptr) {
        lambdaExprCache[decl] = ptr;
    }

    void addToVarDeclMap(const clang::ValueDecl* decl, core::ExpressionPtr ptr) {
    	varDeclMap[decl] = ptr;
	}

    core::ExpressionPtr getLambdaFromCache(const clang::FunctionDecl* decl) {
        return lambdaExprCache[decl];
    }

    /**
	 * Determines the definition of the given generic type pointer within the
	 * internally maintained IR Translation Unit. If non is present, the given
	 * type will be returned.
	 */
	const core::TypePtr lookupTypeDetails(const core::GenericTypePtr& type) const {
		core::TypePtr res = getIRTranslationUnit()[type];
		if (!res) return type;
		if (res.isa<core::GenericTypePtr>() && type != res){
			return lookupTypeDetails(res);
		}
		return res;
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
	 * Returns the complete function definition to the aliased Literal
	 */
	const core::ExpressionPtr lookupFunctionImpl(const core::LiteralPtr& expr) const{
		core::ExpressionPtr res = getIRTranslationUnit()[expr];
		return (res)? res:expr.as<core::ExpressionPtr>();
	}

	/**
	 * Returns the complete function definition to the aliased Literal
	 */
	const core::ExpressionPtr lookupFunctionImpl(const core::ExpressionPtr& expr) const{
		return (expr.isa<core::LiteralPtr>()) ? lookupFunctionImpl(expr.as<core::LiteralPtr>()) : expr;
	}

	/**
	 * Returns a reference to the IR data structure used to represent a variable of the input C code.
	 *
	 * The function guarantees that the same variable in the input code is always represented in the
	 * IR with the same generated Variable and in the case of access to global variables, a reference
	 * to a member of the global data structure is returned.
	 */
	core::ExpressionPtr lookUpVariable(const clang::ValueDecl* valDecl);


    /**
     * Returns a reference to the IR data structure used to represent a variable of the input C code.
     *
     * The function guarantees that the same variable in the input code is always represented in the
     * IR with the same generated Variable and in the case of access to global variables, a reference
     * to a member of the global data structure is returned.
     */
    core::ExpressionPtr lookUpVariableInWrapRefMap(const core::ExpressionPtr variable);


	/**
	 * Returns a map which associates a statement of the clang AST to a pragma (if any)
	 * @return The statement to pragma multimap
	 */
	const pragma::PragmaStmtMap& getPragmaMap() const {
		return pragmaMap;
	}

	/**
	 * convert type declaration, it triggers the extensions and converts the represented type
	 * fills the translation unit with this type
	 * @param typeDecl: the type declaration itself
	 */
	void convertTypeDecl(const clang::TypeDecl* decl);

	/**
	 * Entry point for converting clang types into an IR types
	 * @param type is a clang type
	 * @return the corresponding IR type
	 */
	core::TypePtr convertType(const clang::QualType& type);

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
	 * @return Converted lambda
	 */
	core::ExpressionPtr convertFunctionDecl(const clang::FunctionDecl* funcDecl, bool symbolic=false);

	/**
	 * retrieves the symbol asociated with a function without the need of triggering the translation
	 * of the function. The function itself should be translated by the clang declaration traverser,
	 * and stored in the translation unit.
	 * @param functionDecl the function decl
	 * */
	core::ExpressionPtr getCallableExpression(const clang::FunctionDecl* funcDecl);

	/**
	 * Entry point for converting function to the right type
	 * @param dcl declaration of the function
	 * @return the corresponding IR type
	 */
	core::FunctionTypePtr convertFunctionType(const clang::FunctionDecl* dcl);

	/**
	 * this function takes care of the initialization expression of variables.
	 * it recursively descends the type tree to match everithing very carefully, like a grandma
	 * it handles several differen cases:
	 * 	- stack variable: ref<T> var = expr (typed ref<T> ) as well
	 *	- global variable ref<T> var := expr (typed T!!!)
	 *		this is why a T typed expression will be returned to be fixed by the variable
	 *		declaration to the apropiate form.
	 *	C++ exceptions:
	 *		- a constructor expression returns already the ref<T> expression, refvar should be
	 *		ignored
	 *		- cpp references create aliases to other ref, do not create a refvar to avoid copy
	 *
	 *	@param elemType, T in the ref<T> to be match the return type
	 *	@init, the converted init expression (notice that this is as the visitor returns it, and
	 *	might not be correct due to initialization lists that need to be fixed in the recursion)
	 *	@return the "ready to use" init expression as explained before
	 */
	core::ExpressionPtr getInitExpr (const core::TypePtr& elemType, const core::ExpressionPtr& init);

	/**
	 * Converts variable declarations into IR an declaration statement. This method is also responsible
	 * to map the generated IR variable with the translated variable declaration, so that later uses
	 * of the variable can be mapped to the same IR variable (see lookupVariable method).
	 * @param varDecl a clang variable declaration
	 * @return The IR translation of the variable declaration
	 */
	core::StatementPtr convertVarDecl(const clang::VarDecl* varDecl);

	/**
	 * Converts a enumConstant declaration into an IR literal with the correct enumType, checks if
	 * the enumType is intercepted, from a system header or user provided.
	 * @param enumConstantDecl a clang enumConstantDecl
	 * @return the IR translation of the enumeration constant -- a literal
	 */
	core::ExpressionPtr convertEnumConstantDecl(const clang::EnumConstantDecl* enumConstantDecl);


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
	 * extracts a list of statements and converts the scope into an function call
	 * @param a statement to be extracted
	 * @param the type the whole thing should return
	 * @param whenever the evaluation needs to be lazy
	 * @return a call expression to the generated labda
	 */
core::ExpressionPtr createCallExprFromBody(const core::StatementPtr& stmt, const core::TypePtr& retType, bool lazy = false);
	/**
	 * extracts a list of statements and converts the scope into an function call
	 * @param the list of statements to be converted
	 * @param the type the whole thing should return
	 * @param whenever the evaluation needs to be lazy
	 * @return a call expression to the generated labda
	 */
	core::ExpressionPtr createCallExprFromBody(const stmtutils::StmtWrapper& irStmts, const core::TypePtr& retType, bool lazy = false);


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

	/**
	 * Returns a variable representing the "this" ( always v0)
	 * @param thisType an IR type of the "this"
	 * @return a VariablePtr representing the "thisVar"
	 */
	core::VariablePtr thisVariable(const core::TypePtr& thisType) {
		return builder.variable(thisType, 0);
	}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  some helper tools   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	/**
	 * retrives the header tagger to annotate headers
	 */
	const frontend::utils::HeaderTagger&  getHeaderTagger() const;

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

	/**
	 *  keeps track of the last point a source location to the Declaration
	 *  this might be different depending of what we are dealing with.
	 *  Template spetialization might have 2 locations, template and instantiation location, both of those
	 *  are not the location retrieved by the getLocation method in Decl
	 */
	void trackSourceLocation (const clang::Decl* decl);

	/**
	 *  keeps track of the last point a source location to the Statement
	 *  this might be different depending of what we are dealing with
	 */
	void trackSourceLocation (const clang::Stmt* stmt);

	/**
	 * unstacks last tracked location
	 */
	void untrackSourceLocation ();

	/**
	 *  returns readable location of the last registered source location
	 */
	std::string getLastTrackableLocation() const;

    /**
     *  returns the filename and path of the translation unit
     */
	const boost::filesystem::path& getTUFileName() const {
        return getTranslationUnit().getFileName();
	}

};

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
