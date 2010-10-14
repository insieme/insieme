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

#include "ast_node.h"
#include "ast_visitor.h"
#include "statements.h"
#include "program.h"
#include "types.h"
#include "lang_basic.h"
#include "naming.h"

#include "container_utils.h"
#include "hash_utils.h"
#include "functional_utils.h"

#include "code_management.h"

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
	std::unordered_map<NodePtr, string, hash_target<NodePtr>, equal_target<NodePtr>> nameMap; 

	string getName(const NodePtr& ptr, const char* fragment) {
		auto it = nameMap.find(ptr);
		if(it != nameMap.end()) return string("__insieme_") + fragment + "_" + it->second;
		// generate a new name string
		std::stringstream name;
		switch(ptr->getNodeType()) {
			case NT_RecTypeDefinition:
			case NT_RecLambdaDefinition:
				name << "supp"; break;
			case NT_ArrayType:
			case NT_ChannelType:
			case NT_GenericType:
			case NT_FunctionType:
			case NT_RecType:
			case NT_RefType:
			case NT_StructType:
			case NT_TupleType:
			case NT_UnionType:
			case NT_VectorType:
			case NT_TypeVariable:
				name << "type"; break;
			case NT_CallExpr:
			case NT_CastExpr:
			case NT_JobExpr:
			case NT_LambdaExpr:
			case NT_ParamExpr:
			case NT_RecLambdaExpr:
			case NT_Literal:
			case NT_StructExpr:
			case NT_TupleExpr:
			case NT_UnionExpr:
			case NT_VarExpr:
			case NT_VectorExpr:
				name << "expr"; break;
			case NT_BreakStmt:
			case NT_CompoundStmt:
			case NT_ContinueStmt:
			case NT_DeclarationStmt:
			case NT_ForStmt:
			case NT_IfStmt:
			case NT_ReturnStmt:
			case NT_SwitchStmt:
			case NT_WhileStmt:
				name << "stat"; break;
			case NT_Program:
				name << "prog"; break;
		}
		name << "_" << num++;
		nameMap.insert(make_pair(ptr, name.str()));
		return getName(ptr, fragment);
	} 
};

/** Converts simple IR types to their corresponding C(++) representations.
 ** Examples of "simple" types are integers, booleans, reals and strings.
 ** */
class SimpleTypeConverter : public ASTVisitor<string> {
	NameGenerator& nameGen;
	bool firstRef;

public:
	SimpleTypeConverter(NameGenerator& nameGen) : nameGen(nameGen), firstRef(true) { }

	string visitRefType(const RefTypePtr& ptr);

	string visitGenericType(const GenericTypePtr& ptr);

	string visitStructType(const StructTypePtr& ptr);
};

// TODO more sane dependency handling / move forward declaration
class ConversionContext;

/** Manages C type generation and lookup for IR types.
 ** */
class TypeManager {
	ConversionContext& cc;

public:
	TypeManager(ConversionContext& conversionContext) : cc(conversionContext) { }

	string getTypeName(const core::TypePtr type);
	string getTypeDecl(const core::TypePtr type);
	CodePtr getTypeDefinition(const core::TypePtr type);
};

/** Manages C function generation and lookup for named lambda expressions.
 ** */
class FunctionManager {
	ConversionContext& cc;

public:
	typedef std::unordered_map<Identifier, CodePtr, boost::hash<Identifier>> FunctionMap;

private:
	FunctionMap functionMap;

public:
	FunctionManager(ConversionContext& conversionContext) : cc(conversionContext) { }

	CodePtr getFunction(const core::LambdaExprPtr& lambda, const Identifier& ident);
	CodePtr getFunctionLiteral(const core::FunctionTypePtr& type, const string& name);
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

public:
	// The following may produce warnings, but the use of the this pointer in this case is well specified
	// (the base class initializers do not dereference it)
	ConversionContext() : typeMan(*this), funcMan(*this) { }
	
	//typedef std::unordered_map<ExpressionPtr, CodePtr, hash_target<ExpressionPtr>, equal_target<ExpressionPtr>> ConvertedCode;

	NameGenerator& getNameGen() { return nameGen; }
	TypeManager& getTypeMan() { return typeMan; }
	FunctionManager& getFuncMan() { return funcMan; }

	ConvertedCode convert(const core::ProgramPtr& prog);
};

/** Central simple_backend conversion class, visits IR nodes and generates C code accordingly.
 ** */
class ConvertVisitor : public ASTVisitor<> {
	ConversionContext& cc;
	NameGenerator& nameGen;
	CodePtr defCodePtr;
	CodeStream& cStr;
	
	string printTypeName(const TypePtr& typ) {
		return cc.getTypeMan().getTypeName(typ);
	}

	template<typename Functor>
	void runWithCodeStream(CodeStream& cs, Functor f) {
		CodeStream& oldCodeStream = cStr;
		cStr = cs;
		f();
		cs = oldCodeStream;
	}

public:
	ConvertVisitor(ConversionContext& conversionContext) : cc(conversionContext), nameGen(cc.getNameGen()), 
		defCodePtr(std::make_shared<CodeFragment>()), cStr(defCodePtr->getCodeStream()) { };
	ConvertVisitor(ConversionContext& conversionContext, const CodePtr& cptr) : cc(conversionContext), nameGen(cc.getNameGen()), 
		defCodePtr(cptr), cStr(defCodePtr->getCodeStream()) { };

	CodePtr getCode() { return defCodePtr; }

	//void visitNode(const NodePtr& node) {
	//	std::cout << *node << std::endl;
	//}

	void visitProgram(const ProgramPtr& ptr) {
		assert(0 && "ConvertVisitor should never encounter program node");
	}
	
	////////////////////////////////////////////////////////////////////////// Statements

	void visitBreakStmt(const BreakStmtPtr&) {
		cStr << "break";
	}

	void visitCompoundStmt(const CompoundStmtPtr& ptr) {
		cStr << "{" << CodeStream::indR << "\n";
		for_each(ptr->getChildList(), [&, this](const NodePtr& ptr) { 
			this->visit(ptr); 
			cStr << ";\n"; // TODO remove this if stmt/expr handling better
		});
		cStr << CodeStream::indL << "\n}" << "\n";
	}

	void visitContinueStmt(const ContinueStmtPtr& ptr) {
		cStr << "continue";
	}

	void visitDeclarationStmt(const DeclarationStmtPtr& ptr) {
		cStr << printTypeName(ptr->getVarExpression()->getType()) << " " << ptr->getVarExpression()->getIdentifier().getName() << " = ";
		visit(ptr->getInitialization());
	}

	void visitForStmt(const ForStmtPtr& ptr) {
		auto decl = ptr->getDeclaration();
		const string& ident = decl->getVarExpression()->getIdentifier().getName();
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
		cStr << "else ";
		visit(ptr->getElseBody());
	}

	void visitReturnStmt(const ReturnStmtPtr& ptr);

	void visitSwitchStmt(const SwitchStmtPtr& ptr) {
		cStr << "switch(";
		visit(ptr->getSwitchExpr());
		cStr << ") {\n";
		for_each(ptr->getCases(), [&](const SwitchStmt::Case& curCase) { // GCC sucks
			this->visit(curCase.first);
			this->cStr << ":" << CodeStream::indR << "\n";
			this->visit(curCase.second);
			this->cStr << "break;" << CodeStream::indL << "\n";
		});
		cStr << "}";
	}

	void visitWhileStmt(const WhileStmtPtr& ptr) {
		cStr << "while(";
		visit(ptr->getCondition());
		cStr << ") ";
		visit(ptr->getBody());
	}

	////////////////////////////////////////////////////////////////////////// Expressions

	void visitCallExpr(const CallExprPtr& ptr);

	void visitCastExpr(const CastExprPtr& ptr) {
		cStr << "((" << printTypeName(ptr->getType()) << ")(";
		visit(ptr->getSubExpression());
		cStr << "))";
	}

	void visitJobExpr(const JobExprPtr& ptr) {
		// check if local decls exist, if so generate struct to hold them and populate it
		auto localDecls = ptr->getLocalDecls();
		if(localDecls.size() > 0) {
			string structName = nameGen.getName(ptr, "jobLocalDecls");
			string structVarName = structName + "__var";
			CodePtr structCode = defCodePtr->addDependency(structName);
			CodeStream& sCStr = structCode->getCodeStream();
			// definition
			sCStr << "struct " << structName << CodeStream::indR << " {\n";
			// variable declaration
			cStr << "struct " << structName << "* " << structVarName << " = new " << structName << ";";
			for_each(localDecls, [&](const DeclarationStmtPtr& cur) {
				auto varExp = cur->getVarExpression();
				// generate definition
				sCStr << this->printTypeName(varExp->getType()) << " " << varExp->getIdentifier().getName() << ";";
				// populate entry
				cStr << structVarName << "." << varExp->getIdentifier().getName() << " = ";
				this->visit(cur->getInitialization());
				cStr << ";";
			});
			sCStr << CodeStream::indL << "};";
			// TODO finish job generation (when runtime lib available)
		}
	}

	void visitLambdaExpr(const LambdaExprPtr& ptr);

	void visitRecLambdaExpr(const LambdaExprPtr& ptr) {
		// TODO when cname annotations are standardized
	}

	void visitLiteral(const LiteralPtr& ptr);

private:
	void internalVisitComposite(const NamedCompositeExprPtr& ptr) {
		//auto members = ptr->getMembers();
		//cStr << CodeStream::indR << "{\n"
		//for_each(members, [&](const NamedCompositeExpr::Member& cur) {
		//	
		//};
	}
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

	void visitVarExpr(const VarExprPtr& ptr) {
		cStr << ptr->getIdentifier().getName();
	}
};

} // namespace simple_backend
} // namespace insieme

std::ostream& operator<<(std::ostream& out, const insieme::simple_backend::ConvertedCode& code);
