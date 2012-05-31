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

#include "insieme/frontend/convert.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/utils/map_utils.h"
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

namespace utils {
	class CXXCallExprVisitor;
	class FunctionDepenencyGraph;
}

namespace cpp {
	class TemporaryHandler;
} // end cpp namespace

namespace conversion {

class CXXASTConverter;

// ------------------------------------ ConversionFactory ---------------------------
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
	struct CXXConversionContext: public ConversionContext {

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

		// type on which the operator is called
		core::TypePtr operatorTy;

		core::ExpressionPtr lhsThis;
		core::ExpressionPtr rhsThis;

		// maps the resulting type pointer to the declaration of a class
		typedef std::map<const clang::TagDecl*, core::TypePtr> ClassDeclMap;
		ClassDeclMap classDeclMap;

		// maps the values of each constructor initializer to its declaration, e.g. A() a(0) {} => a...field, 0...value
		typedef std::map<const clang::FieldDecl*, core::ExpressionPtr> CtorInitializerMap;
		CtorInitializerMap ctorInitializerMap;

		CXXConversionContext() : ConversionContext() {
		}
	};

	CXXConversionContext ctx;

	/**
	 * Converts a Clang statements into an IR statements.
	 */
	class CXXClangStmtConverter;
	// Instantiates the statement converter
	static CXXClangStmtConverter* makeStmtConvert(CXXConversionFactory& fact);
	// clean the memory
	static void cleanStmtConvert(CXXClangStmtConverter* stmtConv);
	CXXClangStmtConverter* stmtConv; // PIMPL pattern

	/**
	 * Converts a Clang types into an IR types.
	 */
	class CXXClangTypeConverter;
	// Instantiates the type converter
	static CXXClangTypeConverter* makeTypeConvert(CXXConversionFactory& fact,
			Program& program);
	// clean the memory
	static void cleanTypeConvert(CXXClangTypeConverter* typeConv);
	CXXClangTypeConverter* typeConv; // PIMPL pattern

	/**
	 * Converts a Clang expression into an IR expression.
	 */
	class CXXClangExprConverter;
	// Instantiates the expression converter
	static CXXClangExprConverter* makeExprConvert(CXXConversionFactory& fact,
			Program& program);
	// clean the memory
	static void cleanExprConvert(CXXClangExprConverter* exprConv);
	CXXClangExprConverter* exprConv; // PIMPL pattern

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

	typedef std::pair<clang::FunctionDecl*, clang::idx::TranslationUnit*> TranslationUnitPair;

	CXXConversionFactory(core::NodeManager& mgr, Program& program);
	~CXXConversionFactory();

};

//struct GlobalVariableDeclarationException: public std::runtime_error {
//	GlobalVariableDeclarationException() :
//			std::runtime_error("") {
//	}
//};

// --------------------------------- CXXASTConverter ---------------------------
/**
 *
 */
class CXXASTConverter : public ASTConverter {
//	TO BE DELETED
//	core::NodeManager& mgr;
//	Program& mProg;
	CXXConversionFactory mFact;
//	core::ProgramPtr mProgram;

public:
	CXXASTConverter(core::NodeManager& mgr, Program& prog) : ASTConverter(mgr, prog), mFact(mgr, prog) {
//		mFact = CXXConversionFactory(mgr, prog);
		//globColl = CXXGlobalVarCollector
	}
	virtual ~CXXASTConverter();

};

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
