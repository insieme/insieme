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

#include <sstream>
#include <assert.h>
#include <unordered_map>
#include <memory>

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/statements.h"
#include "insieme/core/program.h"
#include "insieme/core/types.h"
#include "insieme/core/lang_basic.h"

#include "insieme/c_info/naming.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/hash_utils.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/simple_backend/code_management.h"

namespace insieme {
namespace simple_backend {

using namespace core;


/** Generates unique names for anonymous AST nodes when required.
 ** Uses a simple counting system. Not thread safe, and won't necessarily generate the same name
 ** for the same node in different circumstances. Names will, however, stay the same for unchanged 
 ** programs over multiple runs of the compiler.
 ** */
class NameGenerator {
	unsigned long num;

public:
	NameGenerator() : num(0) { }

	std::unordered_map<NodePtr, string, hash_target<NodePtr>, equal_target<NodePtr>> nameMap; 

	string getName(const NodePtr& ptr, const string fragment = "");

	string getVarName(const VariablePtr& var);
};

// TODO more sane dependency handling / move forward declaration
class ConversionContext;

/** Manages C type generation and lookup for IR types.
 ** */
class TypeManager : public ASTVisitor<string> {
	ConversionContext& cc;
	bool visitStarting, declVisit;

public:
	TypeManager(ConversionContext& conversionContext) : cc(conversionContext) { }

	string getTypeName(const core::TypePtr type, bool inDecl = false);
	string getTypeDecl(const core::TypePtr type);
	CodePtr getTypeDefinition(const core::TypePtr type);

	string visitRefType(const RefTypePtr& ptr);
	string visitGenericType(const GenericTypePtr& ptr);
	string visitStructType(const StructTypePtr& ptr);
	string visitVectorType(const VectorTypePtr& ptr);
};

/** Manages C function generation and lookup for named lambda expressions.
 ** */
class FunctionManager {
	ConversionContext& cc;

public:
	typedef std::unordered_map<ExpressionPtr, CodePtr, hash_target<ExpressionPtr>, equal_target<ExpressionPtr>> FunctionMap;

private:
	FunctionMap functionMap;

public:
	FunctionManager(ConversionContext& conversionContext) : cc(conversionContext) { }

	CodePtr getFunction(const core::LambdaExprPtr& lambda);
	CodePtr getFunction(const core::RecLambdaExprPtr& lambda, const CodePtr& surrounding);
	CodePtr getFunctionLiteral(const LiteralPtr& literal);
	void writeFunctionCall(const Identifier& funId, const LambdaExprPtr& ptr);
};


class VariableManager {

public:

	enum MemoryLocation {
		NONE, 	/* < in case the variable is not referencing a memory cell */
		STACK, 	/* < the variable references a memory cell on the stack */
		HEAP 	/* < the variable references a memory cell on the heap */
	};

	struct VariableInfo {
		MemoryLocation location;
	};

private:

	typedef std::unordered_map<VariablePtr, VariableInfo, hash_target<VariablePtr>, equal_target<VariablePtr>> VariableInfoMap;

	VariableInfoMap variableMap;

public:

	VariableManager() : variableMap() {};

	const VariableInfo& getInfo(const VariablePtr& variable) const;
	void addInfo(const VariablePtr& variable, const VariableInfo& info);
	void removeInfo(const VariablePtr& variable);
	bool hasInfoFor(const VariablePtr& variable) const;

};

/** A map from Entry points to Code sections returned by ConversionContext::convert.
 ** Can be printed to any output stream
 ** */
class ConvertedCode : public std::unordered_map<ExpressionPtr, CodePtr, hash_target<ExpressionPtr>, equal_target<ExpressionPtr>> { 
	ProgramPtr fromProg;
public:
	ConvertedCode(const ProgramPtr& fromProg) : std::unordered_map<ExpressionPtr, CodePtr, hash_target<ExpressionPtr>, equal_target<ExpressionPtr>>(), 
		fromProg(fromProg) { }
	const ProgramPtr& getProgram() const;
};


/** Stores the persistent state objects required to perform a simple_backend conversion.
 ** This includes a NameGenerator, a FunctionManager and a TypeManager.
 ** */
class ConversionContext {
	NameGenerator nameGen;
	TypeManager typeMan;
	FunctionManager funcMan;
	NodeManager nodeManager;
	VariableManager varManager;

public:
	// The following may produce warnings, but the use of the this pointer in this case is well specified
	// (the base class initializers do not dereference it)
	ConversionContext() : typeMan(*this), funcMan(*this) { }
	
	//typedef std::unordered_map<ExpressionPtr, CodePtr, hash_target<ExpressionPtr>, equal_target<ExpressionPtr>> ConvertedCode;

	NameGenerator& getNameGen() { return nameGen; }
	TypeManager& getTypeMan() { return typeMan; }
	FunctionManager& getFuncMan() { return funcMan; }

	ConvertedCode convert(const core::ProgramPtr& prog);

	NodeManager& getNodeManager() { return nodeManager; }
	VariableManager& getVariableManager() { return varManager; }
};

/** Central simple_backend conversion class, visits IR nodes and generates C code accordingly.
 ** */
class ConvertVisitor : public ASTVisitor<> {
	ConversionContext& cc;
	NameGenerator& nameGen;
	VariableManager& varManager;
	CodePtr defCodePtr;
	CodeStream& cStr;
	
	template<typename Functor>
	void runWithCodeStream(CodeStream& cs, Functor f) {
		CodeStream& oldCodeStream = cStr;
		cStr = cs;
		f();
		cs = oldCodeStream;
	}

public:
	ConvertVisitor(ConversionContext& conversionContext) : cc(conversionContext), nameGen(cc.getNameGen()), 
		varManager(cc.getVariableManager()), defCodePtr(std::make_shared<CodeFragment>()), cStr(defCodePtr->getCodeStream()) { };
	ConvertVisitor(ConversionContext& conversionContext, const CodePtr& cptr) : cc(conversionContext), nameGen(cc.getNameGen()), 
		varManager(cc.getVariableManager()), defCodePtr(cptr), cStr(defCodePtr->getCodeStream()) { };

	CodePtr getCode() { return defCodePtr; }

	void visitNode(const NodePtr& node) {
		cStr << "<?>" << toString(*node) << "</?>";
	}

	void visitProgram(const ProgramPtr&) {
		assert(0 && "ConvertVisitor should never encounter program node");
	}
	
	////////////////////////////////////////////////////////////////////////// Statements

	void visitBreakStmt(const BreakStmtPtr&) {
		cStr << "break";
	}

	void visitCompoundStmt(const CompoundStmtPtr& ptr);

	void visitContinueStmt(const ContinueStmtPtr&) {
		cStr << "continue";
	}

	void visitDeclarationStmt(const DeclarationStmtPtr& ptr);

	void visitForStmt(const ForStmtPtr& ptr) {
		auto decl = ptr->getDeclaration();
		auto var = decl->getVariable();
		string ident = cc.getNameGen().getVarName(var);
		cStr << "for(";
		visit(decl);
		cStr << "; " << ident << " < ";
		visit(ptr->getEnd());
		cStr << "; " << ident << " += "; 
		visit(ptr->getStep());
		cStr << ") ";
		visit(ptr->getBody());
	}

	void visitIfStmt(const IfStmtPtr& ptr) {
		cStr << "if(";
		visit(ptr->getCondition());
		cStr << ") ";
		visit(ptr->getThenBody());
		cStr << " else ";
		visit(ptr->getElseBody());
	}

	void visitReturnStmt(const ReturnStmtPtr& ptr);

	void visitSwitchStmt(const SwitchStmtPtr& ptr);

	void visitWhileStmt(const WhileStmtPtr& ptr) {
		cStr << "while(";
		visit(ptr->getCondition());
		cStr << ") ";
		visit(ptr->getBody());
	}

	////////////////////////////////////////////////////////////////////////// Expressions

	void visitCallExpr(const CallExprPtr& ptr);

	void visitCastExpr(const CastExprPtr& ptr);

	void visitJobExpr(const JobExprPtr& ptr) {
		//// check if local decls exist, if so generate struct to hold them and populate it
		//auto localDecls = ptr->getLocalDecls();
		//if(localDecls.size() > 0) {
		//	string structName = nameGen.getName(ptr, "jobLocalDecls");
		//	string structVarName = structName + "__var";
		//	CodePtr structCode = defCodePtr->addDependency(structName);
		//	CodeStream& sCStr = structCode->getCodeStream();
		//	// definition
		//	sCStr << "struct " << structName << CodeStream::indR << " {\n";
		//	// variable declaration
		//	cStr << "struct " << structName << "* " << structVarName << " = new " << structName << ";";
		//	for_each(localDecls, [&](const DeclarationStmtPtr& cur) {
		//		auto varExp = cur->getVarExpression();
		//		// generate definition
		//		sCStr << this->printTypeName(varExp->getType()) << " " << varExp->getIdentifier().getName() << ";";
		//		// populate entry
		//		cStr << structVarName << "." << varExp->getIdentifier().getName() << " = ";
		//		this->visit(cur->getInitialization());
		//		cStr << ";";
		//	});
		//	sCStr << CodeStream::indL << "};";
		//	// TODO finish job generation (when runtime lib available)
		//}
	}

	void visitLambdaExpr(const LambdaExprPtr& ptr);

	void visitRecLambdaExpr(const RecLambdaExprPtr& ptr);

	void visitLiteral(const LiteralPtr& ptr);

public:

	void visitStructExpr(const StructExprPtr& ptr) {
		//cStr << "struct ";
		//internalVisitComposite(ptr);
	}

	void visitUnionExpr(const UnionExprPtr& ptr) {
		//
	}

	void visitTupleExpr(const TupleExprPtr& ptr) {
		// TODO check when to use ref()/cref()
		cStr << "std::make_tuple(";
		auto exps = ptr->getExpressions();
		if(exps.size() > 0) {
			visit(exps.front());
			for_each(exps.cbegin()+1, exps.cend(), [&](const ExpressionPtr& cur) {
				cStr << ", ";
				this->visit(cur);
			});
		}
		cStr << ")";
	}

	void visitVariable(const VariablePtr& ptr);

	void processArgument(const ExpressionPtr& argument);
};

} // namespace simple_backend
} // namespace insieme

std::ostream& operator<<(std::ostream& out, const insieme::simple_backend::ConvertedCode& code);
