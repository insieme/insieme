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

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"

#include "clang/AST/RecursiveASTVisitor.h"
#include "insieme/frontend/program.h"

#include "insieme/frontend/utils/indexer.h"

#include <set>
#include <map>
#include <stack>

/* clang [3.0]
 * namespace clang {
namespace idx {:w

class Indexer;
class TranslationUnit;
} // end idx namespace
} */

namespace insieme {
namespace frontend {

namespace conversion {
class ConversionFactory;
class CXXConversionFactory;
}

namespace analysis {

/**
 * Collects variables marked as global (i.e. global and static variables) within the input
 * program (which may consist of multiple translation units). The process is lazy as only
 * used variable belonging to the call graph will be gathered.
 *
 * It generates a the data structure and appropriate initializer which holds the entire set of
 * global variables used in the program.
 */
class GlobalVarCollector : public clang::RecursiveASTVisitor<GlobalVarCollector> {
public:
	/*
	 * List of found global variables. The boolean value is used to decide whether the
	 * variable has to be initialized or it was defined as external and a reference
	 * to  the existing value has to be generated
	 */
	typedef std::set<const clang::VarDecl*> GlobalVarSet;

	// clang [3.0] typedef std::map<const clang::VarDecl*, const clang::idx::TranslationUnit*> VarTUMap;
	typedef std::map<const clang::VarDecl*, const insieme::frontend::TranslationUnit*> VarTUMap;

	/*
	 * Set of functions already visited, this avoid the solver to loop in the
	 * case of recursive function calls
	 */
	typedef std::set<const clang::FunctionDecl*> VisitedFuncSet;

	// A call stack of functions created during the visit of the input code.
	typedef std::stack<const clang::FunctionDecl*> FunctionStack;

	/*
	 * Set of functions which need access to global variables. This structure will
	 * be used to decide whether the data structure containing the global variables
	 * has to be forwarded through this function via the capture list
	 */
	typedef std::set<const clang::FunctionDecl*> UseGlobalFuncMap;

	typedef std::pair<core::StructTypePtr, core::StructExprPtr> GlobalStructPair;
	typedef std::map<const clang::VarDecl*, core::StringValuePtr> GlobalIdentMap;

	GlobalVarCollector(
		conversion::ConversionFactory& 		convFact,
		// clang [3.0] const clang::idx::TranslationUnit* 	currTU, 
		const insieme::frontend::TranslationUnit* 	currTU, 
		// FIXME find an indexer
		//clang::idx::Indexer& 				indexer,
		insieme::frontend::utils::Indexer& 				indexer,
		UseGlobalFuncMap& 					globalFuncMap)
	: 
	  convFact(convFact),
	  currTU(currTU), 
	  indexer(indexer), 
	  usingGlobals(globalFuncMap) {
	}
	virtual ~GlobalVarCollector() {};

	bool VisitStmt(clang::Stmt* stmt);
	bool VisitVarDecl(clang::VarDecl* decl);
	void VisitExternVarDecl(clang::VarDecl* decl);
	bool VisitDeclRefExpr(clang::DeclRefExpr* decl);
	bool VisitCallExpr(clang::CallExpr* callExpr);

	/* CXX specific methods -- not support in C version
	 * implemented/used in CXXGlobalVarCollector*/
	virtual bool VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* callExpr) { 
		assert(false && "not supported in GlobalVarCollector"); 
	}
	virtual bool VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* callExpr) { 
		assert(false && "not supported in GlobalVarCollector"); 
	}
	virtual bool VisitCXXDeleteExpr(clang::CXXDeleteExpr* deleteExpr) { 
		assert(false && "not supported in GlobalVarCollector"); 
	}
	virtual bool VisitCXXNewExpr(clang::CXXNewExpr* newExpr) { 
		assert(false && "not supported in GlobalVarCollector"); 
	}
	virtual bool VisitCXXConstructExpr(clang::CXXConstructExpr* ctorExpr) { 
		assert(false && "not supported in GlobalVarCollector"); 
	}

	void operator()(const clang::Decl* decl);
	void operator()(const Program::TranslationUnitSet& tus);

	/**
	 * Returns the list of collected global variables. For each variable
	 * a boolean flag indicating if the variable needs to be initialized
	 * or not (in the case the global variable is marked as external)
	 */
	inline const GlobalVarSet& getGlobals() const { return globals; }

	/**
	 * Returns the list of functions which needs access (directly or
	 * indirectly) to the global struct which will be passed accordingly
	 * @return
	 */
	const UseGlobalFuncMap& getUsingGlobals() const { return usingGlobals; }
	
	const GlobalIdentMap& getIdentifierMap() const { return varIdentMap; }

	void dump(std::ostream& out) const ;

	virtual GlobalStructPair createGlobalStruct();

protected:

	core::StringValuePtr
	buildIdentifierFromVarDecl(const clang::VarDecl* varDecl, const clang::FunctionDecl* func = NULL ) const;

	conversion::ConversionFactory& 		convFact;
	GlobalVarSet						globals;
	VarTUMap							varTU;
	GlobalIdentMap						varIdentMap;
	// clang [3.0] const clang::idx::TranslationUnit* 	currTU;
	const insieme::frontend::TranslationUnit* 	currTU;
	VisitedFuncSet 						visited;
	FunctionStack						funcStack;

	//clang::idx::Indexer& 				indexer;
	insieme::frontend::utils::Indexer& 				indexer;
	UseGlobalFuncMap& 					usingGlobals;
};


////////////////////////////////////////////////////////////////////////////////////
///      CXX global var collector
////////////////////////////////////////////////////////////////////////////////////
class CXXGlobalVarCollector : public GlobalVarCollector {
	public:

		//virtual function stuff
		typedef std::pair<unsigned int, unsigned int> ClassFuncPair; //first = classId, second = count of virtual functions
		typedef std::map<const clang::CXXRecordDecl*, ClassFuncPair> PolymorphicClassMap;
		typedef std::map<const clang::CXXMethodDecl*, unsigned int> VirtualFunctionIdMap;
		typedef std::map<const clang::CXXRecordDecl*, vector<std::pair<const clang::CXXMethodDecl*, const clang::CXXMethodDecl*>>> FinalOverriderMap;
		typedef std::map< std::pair<const clang::CXXRecordDecl*, const clang::CXXRecordDecl*>, int > OffsetMap;

		CXXGlobalVarCollector(
				conversion::ConversionFactory& 		convFact,
				// clang [3.0] const clang::idx::TranslationUnit* 	currTU,
				const insieme::frontend::TranslationUnit* 	currTU,
				insieme::frontend::utils::Indexer& 				indexer,
				UseGlobalFuncMap& 					globalFuncMap,
				PolymorphicClassMap& 				polymorphicClassMap,
				OffsetMap&							offsetMap,
				VirtualFunctionIdMap&				virtualFunctionIdMap,
				FinalOverriderMap&					finalOverriderMap)
	:
	  //GlobalVarCollector(convFact, currTU, indexer, globalFuncMap),
	  GlobalVarCollector(convFact, currTU, indexer, globalFuncMap),
	/*convFact(convFact),
	  currTU(currTU),
	  indexer(indexer),
	  usingGlobals(globalFuncMap),
	*/
	  polymorphicClassMap(polymorphicClassMap),
	  offsetMap(offsetMap),
	  virtualFunctionIdMap(virtualFunctionIdMap),
	  finalOverriderMap(finalOverriderMap) {
		maxFunctionCounter = -1;
	}
	virtual ~CXXGlobalVarCollector() {};

	virtual bool VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* callExpr);
	virtual bool VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* callExpr);
	virtual bool VisitCXXDeleteExpr(clang::CXXDeleteExpr* deleteExpr);
	virtual bool VisitCXXNewExpr(clang::CXXNewExpr* newExpr);
	virtual bool VisitCXXConstructExpr(clang::CXXConstructExpr* ctorExpr);
	vector<clang::CXXRecordDecl*> getAllDynamicBases(const clang::CXXRecordDecl* recDeclCXX );

	virtual GlobalStructPair createGlobalStruct();

private:

	void collectVTableData(const clang::CXXRecordDecl* recDecl);

	//used for virtual functions
	PolymorphicClassMap& 				polymorphicClassMap;
	OffsetMap&							offsetMap;
	VirtualFunctionIdMap&				virtualFunctionIdMap;
	FinalOverriderMap&					finalOverriderMap;
	int									maxFunctionCounter;
};

} // end analysis namespace
} // end frontend namespace
} // end insieme namespace

namespace std {
std::ostream& operator<<(std::ostream& out, const insieme::frontend::analysis::GlobalVarCollector& globals);
} // end std namespace
