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

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/utils/map_utils.h"
#include <memory>
#include <set>
#include <functional>

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

typedef vector<insieme::core::StatementPtr> StatementList;
typedef vector<insieme::core::ExpressionPtr> ExpressionList;

#define GET_TYPE_PTR(type) (type)->getType().getTypePtr()

} // end anonymous namespace

namespace insieme {
namespace frontend {

namespace cpp {
	class TemporaryHandler;
} // end cpp namespace


namespace conversion {

class ASTConverter;
class CASTConverter;
class CXXASTConverter;
class ConversionFactory;
class CXXConversionFactory;

// ------------------------------------ ConversionFactory ---------------------------
/**
 * A factory used to convert clang AST nodes (i.e. statements, expressions and types) to Insieme IR nodes.
 */
class ConversionFactory: public boost::noncopyable {

protected:
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							ConversionContext
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Keeps all the information gathered during the conversion process.
	// Maps for variable names, cached resolved function definitions and so on...
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	struct ConversionContext: public boost::noncopyable {

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// 						Recursive Function resolution
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// Maps Clang variable declarations (VarDecls and ParmVarDecls) to IR variables.
		typedef std::map<const clang::ValueDecl*, core::VariablePtr> VarDeclMap;
		VarDeclMap varDeclMap;

		// Stores the generated IR for function declarations
		typedef std::map<const clang::FunctionDecl*,
				insieme::core::ExpressionPtr> LambdaExprMap;
		LambdaExprMap lambdaExprCache;

		/*
		 * Maps a function with the variable which has been introduced to represent
		 * the function in the recursive definition
		 */
		typedef std::map<const clang::FunctionDecl*, insieme::core::VariablePtr> RecVarExprMap;
		RecVarExprMap recVarExprMap;

		/*
		 * When set this variable tells the frontend to resolve eventual recursive function call
		 * using the mu variables which has been previously placed in the recVarExprMap
		 */
		bool isRecSubFunc;

		// It tells the frontend the body of a recursive function is being resolved and
		// eventual functions which are already been resolved should be not converted again
		// but read from the map
		bool isResolvingRecFuncBody;

		// This variable points to the current mu variable representing the start of the recursion
		core::VariablePtr currVar;

		// This variable stores the list of parameters passed as an argument to the currently processed
		// function.
		typedef const vector<core::VariablePtr>* ParameterList;
		ParameterList curParameter;

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// 						Recursive Type resolution
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		typedef std::map<const clang::Type*, insieme::core::TypeVariablePtr> TypeRecVarMap;
		TypeRecVarMap recVarMap;
		bool isRecSubType;

		typedef std::map<const clang::Type*, insieme::core::TypePtr> RecTypeMap;
		RecTypeMap recTypeCache;

		bool isResolvingFunctionType;

		typedef std::map<const clang::Type*, insieme::core::TypePtr> TypeCache;
		TypeCache typeCache;
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		// 						Global variables utility
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		typedef std::pair<core::StructTypePtr, core::StructExprPtr> GlobalStructPair;
		// Keeps the type and initialization of the global variables within the entry point
		GlobalStructPair globalStruct;

		// Global and static variables
		core::VariablePtr globalVar;

		std::set<const clang::VarDecl*> thread_private;
		std::set<const clang::VarDecl*> volatiles;

		/*
		 * Set of the function which need access to global variables, every time such a
		 * function is converted the data structure containing global variables has to
		 * be correctly forwarded by using the capture list
		 */
		typedef std::set<const clang::FunctionDecl*> UseGlobalFuncMap;
		UseGlobalFuncMap globalFuncMap;

		typedef std::map<const clang::VarDecl*, core::StringValuePtr> GlobalIdentMap;
		GlobalIdentMap globalIdentMap;

		/*
		 * Every time an input parameter of a function of type 'a is improperly used as a ref<'a>
		 * a new variable is created in function body and the value of the input parameter assigned to it
		 */
		typedef insieme::utils::map::PointerMap<insieme::core::VariablePtr, insieme::core::VariablePtr> WrapRefMap;
		WrapRefMap wrapRefMap;
/*
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//						Dtor handling
		//				maps, variables for destructor handling
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		typedef std::stack<core::VariablePtr> ScopeObjects;
		ScopeObjects scopeObjects;
		ScopeObjects downStreamScopeObjects;

		typedef std::map<const clang::FunctionDecl*,
				vector<insieme::core::VariablePtr>> FunToTemporariesMap;
		FunToTemporariesMap fun2TempMap;
		typedef std::map <core::VariablePtr,clang::CXXRecordDecl*> ObjectMap;
				ObjectMap objectMap;

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//						Polymorphic Classes
		//				maps, variables for virtual function tables
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		typedef std::pair<unsigned int, unsigned int> ClassFuncPair;
		typedef std::map<const clang::CXXRecordDecl*, ClassFuncPair> PolymorphicClassMap;
		PolymorphicClassMap polymorphicClassMap;

		typedef std::map<const clang::CXXMethodDecl*, unsigned int> VirtualFunctionIdMap;
		VirtualFunctionIdMap virtualFunctionIdMap;

		typedef std::map< std::pair<const clang::CXXRecordDecl*, const clang::CXXRecordDecl*>, int > OffsetMap;
		OffsetMap offsetMap;

		typedef std::map<const clang::CXXRecordDecl*, vector<std::pair<const clang::CXXMethodDecl*, const clang::CXXMethodDecl*>>> FinalOverriderMap;
		FinalOverriderMap finalOverriderMap;

		core::ExpressionPtr offsetTableExpr;	//access offsetTable via globalVar
		core::ExpressionPtr vFuncTableExpr;		//access offsetTable via globalVar

		core::ExpressionPtr thisStack2; // not only of type core::Variable - in nested classes
		core::ExpressionPtr thisVar; // used in Functions as reference

		typedef std::map<const clang::SourceLocation, core::VariablePtr> ThisMap;
		ThisMap thisMap;

		// current Type of class
		core::TypePtr curTy;

		bool useClassCast;

		// for operators
		bool isCXXOperator;

		// type on which the operator is called
		core::TypePtr operatorTy;

		core::ExpressionPtr lhsThis;
		core::ExpressionPtr rhsThis;

		// maps a constructor declaration to the call expression with memory allocation - such
		// a call expression returns a pointer to the allocated and initialized object
		LambdaExprMap lambdaExprCacheNewObject;

		// maps the values of each constructor initializer to its declaration, e.g. A() a(0) {} => a...field, 0...value
		typedef std::map<const clang::FieldDecl*, core::ExpressionPtr> CtorInitializerMap;
		CtorInitializerMap ctorInitializerMap;
*/
		/*	FIXME: rename --> takes care of TagDecl not ClassDecl!
			TagDecl are for struct/union/class/enum --> used in C and CXX */
		// maps the resulting type pointer to the declaration of a class
		typedef std::map<const clang::TagDecl*, core::TypePtr> ClassDeclMap;
		ClassDeclMap classDeclMap;

		ConversionContext() :
				isRecSubFunc(false), isResolvingRecFuncBody(false), curParameter(
						0), isRecSubType(false), isResolvingFunctionType(false) {
		}

	};

	ConversionContext ctx;

	/**
	 * Converts a Clang statements into an IR statements.
	 */
	class StmtConverter;
	class CStmtConverter;
	std::shared_ptr<StmtConverter> stmtConvPtr;

	/**
	 * Converts a Clang types into an IR types.
	 */
	class TypeConverter;
	class CTypeConverter;
	std::shared_ptr<TypeConverter> typeConvPtr;

	/**
	 * Converts a Clang expression into an IR expression.
	 */
	class ExprConverter;
	class CExprConverter;
	std::shared_ptr<ExprConverter> exprConvPtr;

	//	GlobalIdentMap globalIdentMap;                                  ////////////////////////////////
	core::NodeManager& mgr;
	const core::IRBuilder builder;
	Program& program;

	/**
	 * Maps of statements to pragmas.
	 */
	pragma::PragmaStmtMap pragmaMap;

	/**
	 * A pointer to the translation unit which is currently used to resolve symbols, i.e. literals
	 * Every time a function belonging to a different translation unit is called this pointer
	 * is set to translation unit containing the function definition.
	 */
	const TranslationUnit* currTU;

	/**
	 * Returns a reference to the IR data structure used to represent a variable of the input C program.
	 *
	 * The function guarantees that the same variable in the input program is always represented in the
	 * IR with the same generated Variable and in the case of access to global variables, a reference
	 * to a member of the global data structure is returned.
	 */
	core::ExpressionPtr lookUpVariable(const clang::ValueDecl* valDecl);
	core::ExpressionPtr convertInitializerList(
			const clang::InitListExpr* initList,
			const core::TypePtr& type) const;

	/**
	 * Attach annotations to a C function of the input program.
	 *
	 * returns the a MarkerExprPtr if a marker node has to be added and the passed node else
	 */
	core::ExpressionPtr attachFuncAnnotations(const core::ExpressionPtr& node,
			const clang::FunctionDecl* funcDecl);

	core::FunctionTypePtr addGlobalsToFunctionType(	const core::IRBuilder& builder,
													const core::TypePtr& globals,
													const core::FunctionTypePtr& funcType);

	friend class ASTConverter;
	friend class CASTConverter;

public:

	typedef std::pair<clang::FunctionDecl*, clang::idx::TranslationUnit*> TranslationUnitPair;

	ConversionFactory(core::NodeManager& mgr, Program& program);
	ConversionFactory(core::NodeManager& mgr, Program& program,
					std::shared_ptr<StmtConverter> stmtConvPtr,
					std::shared_ptr<TypeConverter> typeConvPtr,
					std::shared_ptr<ExprConverter> exprConvPtr);

	virtual ~ConversionFactory();

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

	clang::SourceManager& getCurrentSourceManager() const {
		assert(currTU && "FATAL: Translation unit not correctly set");
		return currTU->getCompiler().getSourceManager();
	}

	const ClangCompiler& getCurrentCompiler() const {
		assert(currTU && "FATAL: Translation unit not correctly set");
		return currTU->getCompiler();
	}
	/**
	 * Force the current translation.
	 * @param tu new translation unit
	 */
	void setTranslationUnit(const TranslationUnit& tu) {
		currTU = &tu;
	}

	/**
	 * Because when literals are read from a function declaration we need to
	 * set manually the translation unit which contains the definition of the
	 * function, this method helps in setting the translation unit correctly.
	 *
	 * Returns the previous translation unit in the case it has to be set back. 
	 */
	const clang::idx::TranslationUnit* getTranslationUnitForDefinition(
			clang::FunctionDecl*& fd);

	/**
	 * Returns a map which associates a statement of the clang AST to a pragma (if any)
	 * @return The statement to pragma multimap
	 */
	const pragma::PragmaStmtMap& getPragmaMap() const {
		return pragmaMap;
	}

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
	 * Entry point for converting clang expressions to IR expres    Number of Shared Nodes: 200
	 Number of Addressable Nodes: 1068
	 Share Ratio: 5.34
	 Height of tree: 30 sions
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
	core::NodeAnnotationPtr convertAttribute(
			const clang::ValueDecl* valDecl) const;

	/**
	 * Utility function which tries to apply the deref operation. If the input expression is not a of ref type
	 * the same expression is returned.
	 * @param expr IR expression which could be of ref or non-ref type
	 * @return a non RefType IR expression
	 */
	core::ExpressionPtr tryDeref(const core::ExpressionPtr& expr) const;

	/**
	 * Utility function which tries to return the derefed type. If the input tyoe is not a of ref type
	 * the same type is returned.
	 * @param type IR type which could be of ref or non-ref type
	 * @return a non RefType IR type
	 */
	core::TypePtr tryDeref(const core::TypePtr& type) const;

	/**
	 * Allows access to the set of threadprivates stored in the context
	 * @return IR annotation
	 */
	const std::set<const clang::VarDecl*>& getThreadprivates() const {
		return ctx.thread_private;
	}

	const std::set<const clang::VarDecl*>& getVolatiles() const {
		return ctx.volatiles;
	}

	// typedef std::function<core::ExpressionPtr (core::NodeManager&, const clang::CallExpr*)> CustomFunctionHandler;
	/**
	 * Registers a handler for call expressions. When a call expression to the provided function declaration 
	 * is encountered by the frontend, the provided handler is invoked. The handler produces an IR expression
	 * which will be used to replace the call expression in the generated IR program
	 */
	// void registerCallExprHandler(const clang::FunctionDecl* funcDecl, CustomFunctionHandler& handler);

// private:
//	typedef std::map<const clang::FunctionDecl*, CustomFunctionHandler> CallExprHandlerMap;
//	CallExprHandlerMap callExprHanlders;

	/**
	 * Converts a function declaration into an IR lambda.
	 * @param funcDecl is a clang FunctionDecl which represent a definition for the function
	 * @param isEntryPoint determine if this function is an entry point of the generated IR
	 * @return Converted lambda
	 */
	virtual core::NodePtr convertFunctionDecl(const clang::FunctionDecl* funcDecl,
			bool isEntryPoint = false);

	/**
	 * Converts variable declarations into IR an declaration statement. This method is also responsible
	 * to map the generated IR variable with the translated variable declaration, so that later uses
	 * of the variable can be mapped to the same IR variable (see lookupVariable method).
	 * @param varDecl a clang variable declaration
	 * @return The IR translation of the variable declaration
	 */
	virtual core::DeclarationStmtPtr convertVarDecl(const clang::VarDecl* varDecl);

	/**
	 * Returns the default initialization value of the IR type passed as input.
	 * @param type is the IR type
	 * @return The default initialization value for the IR type
	 */
	virtual core::ExpressionPtr defaultInitVal(const core::TypePtr& type) const;

	virtual core::ExpressionPtr convertInitExpr(const clang::Expr* expr,
			const core::TypePtr& type, const bool zeroInit) const;

	virtual void collectGlobalVar(const clang::FunctionDecl* funcDecl);
};

struct GlobalVariableDeclarationException: public std::runtime_error {
	GlobalVariableDeclarationException() :
			std::runtime_error("") {
	}
};


// ------------------------------------ CXXConversionFactory ---------------------------
/**
 * A factory used to convert clang AST nodes (i.e. statements, expressions and types) to Insieme IR nodes.
 */
class CXXConversionFactory: public ConversionFactory {

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							ConversionContext
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Keeps all the information gathered during the conversion process.
	// Maps for variable names, cached resolved function definitions and so on...
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	struct CXXConversionContext: public boost::noncopyable  {

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//						Dtor handling
		//				maps, variables for destructor handling
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		typedef std::stack<core::VariablePtr> ScopeObjects;
		ScopeObjects scopeObjects;
		ScopeObjects downStreamScopeObjects;

		typedef std::map<const clang::FunctionDecl*,
				vector<insieme::core::VariablePtr>> FunToTemporariesMap;
		FunToTemporariesMap fun2TempMap;
		typedef std::map <core::VariablePtr,clang::CXXRecordDecl*> ObjectMap;
				ObjectMap objectMap;

		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
		//						Polymorphic Classes
		//				maps, variables for virtual function tables
		//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		typedef std::pair<unsigned int, unsigned int> ClassFuncPair;
		typedef std::map<const clang::CXXRecordDecl*, ClassFuncPair> PolymorphicClassMap;
		PolymorphicClassMap polymorphicClassMap;

		typedef std::map<const clang::CXXMethodDecl*, unsigned int> VirtualFunctionIdMap;
		VirtualFunctionIdMap virtualFunctionIdMap;

		typedef std::map< std::pair<const clang::CXXRecordDecl*, const clang::CXXRecordDecl*>, int > OffsetMap;
		OffsetMap offsetMap;

		typedef std::map<const clang::CXXRecordDecl*, vector<std::pair<const clang::CXXMethodDecl*, const clang::CXXMethodDecl*>>> FinalOverriderMap;
		FinalOverriderMap finalOverriderMap;

		core::ExpressionPtr offsetTableExpr;	//access offsetTable via globalVar
		core::ExpressionPtr vFuncTableExpr;		//access offsetTable v{ia globalVar

		core::ExpressionPtr thisStack2; // not only of type core::Variable - in nested classes
		core::ExpressionPtr thisVar; // used in Functions as reference

		// current Type of class
		core::TypePtr curTy;

		bool useClassCast;

		// for operators
		bool isCXXOperator;

		// type on which the operator is called
		core::TypePtr operatorTy;

		core::ExpressionPtr lhsThis;
		core::ExpressionPtr rhsThis;

		// maps the values of each constructor initializer to its declaration, e.g. A() a(0) {} => a...field, 0...value
//		typedef std::map<const clang::FieldDecl*, core::ExpressionPtr> CtorInitializerMap;
//		CtorInitializerMap ctorInitializerMap;

		CXXConversionContext() : useClassCast(false), isCXXOperator(false) {
		}
	};

	CXXConversionContext cxxCtx;

	/**
	 * Converts a Clang statements into an IR statements.
	 */
	class CXXStmtConverter;

	/**
	 * Converts a Clang types into an IR types.
	 */
	class CXXTypeConverter;

	/**
	 * Converts a Clang expression into an IR expression.
	 */
	class CXXExprConverter;


	//virtual function support: update classId
	vector<core::StatementPtr> updateClassId(	const clang::CXXRecordDecl* recDecl,
												core::ExpressionPtr expr,
												unsigned int classId);

	// virtual function support: create initializations statments for the offsetTable
	vector<core::StatementPtr> initOffsetTable();

	// virtual function support: create initializations statments for the vFuncTable
	vector<core::StatementPtr> initVFuncTable();

	//create/update access vfunc offset table
	void updateVFuncOffsetTableExpr();

	//create/update access vfunc table
	void updateVFuncTableExpr();

	core::FunctionTypePtr addThisArgToFunctionType(	const core::IRBuilder& builder,
													const core::TypePtr& structTy,
													const core::FunctionTypePtr& funcType);

	friend class CXXASTConverter;
	friend class cpp::TemporaryHandler;
public:

	CXXConversionFactory(core::NodeManager& mgr, Program& program);
	virtual ~CXXConversionFactory();

	/**
	 * Converts a function declaration into an IR lambda.
	 * @param funcDecl is a clang FunctionDecl which represent a definition for the function
	 * @param isEntryPoint determine if this function is an entry point of the generated IR
	 * @return Converted lambda
	 */
	virtual core::NodePtr convertFunctionDecl(const clang::FunctionDecl* funcDecl,
			bool isEntryPoint = false);

	/**
	 * Converts variable declarations into IR an declaration statement. This method is also responsible
	 * to map the generated IR variable with the translated variable declaration, so that later uses
	 * of the variable can be mapped to the same IR variable (see lookupVariable method).
	 * @param varDecl a clang variable declaration
	 * @return The IR translation of the variable declaration
	 */
	virtual core::DeclarationStmtPtr convertVarDecl(const clang::VarDecl* varDecl);

	/**
	 * Returns the default initialization value of the IR type passed as input.
	 * @param type is the IR type
	 * @return The default initialization value for the IR type
	 */
	virtual core::ExpressionPtr defaultInitVal(const core::TypePtr& type) const;

	virtual core::ExpressionPtr convertInitExpr(const clang::Expr* expr,
			const core::TypePtr& type, const bool zeroInit) const;

	virtual void collectGlobalVar(const clang::FunctionDecl* funcDecl);
};

// ------------------------------------ ASTConverter ---------------------------
class ASTConverter {
protected:
	core::NodeManager& mgr;
	Program& mProg;
	ConversionFactory& mFact;
	core::ProgramPtr mProgram;

public:
	ASTConverter(core::NodeManager& mgr, Program& prog, ConversionFactory& fact) :
			mgr(mgr),
			mProg(prog),
			mFact(fact),
			mProgram(prog.getProgram()) {
	}

	core::ProgramPtr getProgram() const {
		return mProgram;
	}

	core::ProgramPtr handleFunctionDecl(const clang::FunctionDecl* funcDecl,
			bool isMain = false);

	core::LambdaExprPtr handleBody(const clang::Stmt* body,
			const TranslationUnit& tu);
};

// ----------------------------------- CASTConverter ---------------------------
class CASTConverter : public ASTConverter {
	ConversionFactory mFact;

public:
	CASTConverter(core::NodeManager& mgr, Program& prog) :
		ASTConverter(mgr, prog, mFact), mFact(mgr, prog) {
	}
};

// --------------------------------- CXXASTConverter ---------------------------
class CXXASTConverter : public ASTConverter {
	CXXConversionFactory mFact;

public:
	CXXASTConverter(core::NodeManager& mgr, Program& prog) :
		ASTConverter(mgr, prog, mFact), mFact(mgr, prog) {
	}
};


} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
