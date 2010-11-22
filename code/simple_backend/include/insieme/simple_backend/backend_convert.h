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

#include "insieme/c_info/naming.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/hash_utils.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/simple_backend/code_management.h"
#include "insieme/simple_backend/type_manager.h"
#include "insieme/simple_backend/name_generator.h"
#include "insieme/simple_backend/function_manager.h"

namespace insieme {
namespace simple_backend {

using namespace core;


// TODO more sane dependency handling / move forward declaration
class ConversionContext;


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
	VariableManager varManager;
	FunctionManager funcMan;
	NodeManager& nodeManager;

public:

	// a lang basic code generator reference
	const lang::BasicGenerator& basic;

	// The following may produce warnings, but the use of the this pointer in this case is well specified
	// (the base class initializers do not dereference it)
	ConversionContext(const NodePtr& target);
	
	//typedef std::unordered_map<ExpressionPtr, CodePtr, hash_target<ExpressionPtr>, equal_target<ExpressionPtr>> ConvertedCode;

	NameGenerator& getNameGen() { return nameGen; }
	TypeManager& getTypeMan() { return typeMan; }
	FunctionManager& getFuncMan() { return funcMan; }

	ConvertedCode convert(const core::ProgramPtr& prog);

	NodeManager& getNodeManager() { return nodeManager; }
	VariableManager& getVariableManager() { return varManager; }
};

// a forward declaration of the convert visitor
class ConvertVisitor;

namespace detail {

	/**
	 * This class allows the conversion visitor to use special formats when printing the call of a specific function.
	 * For instance, this formatter allows to write the add operation + in infix notation.
	 */
	class Formatter {
	public:

		/**
		 * Performs the actual code formating. This method is pure abstract and
		 * has to be implemented within sub-classes.
		 *
		 * @param visitor the visitor and its context using this formatter
		 * @param call the call expression to be handled
		 */
		virtual void format(ConvertVisitor& visitor, CodeStream& cStr, const CallExprPtr& call) =0;

	};

	/**
	 * Since formatter instances are polymorthic, they need to be handled via pointer or
	 * references. Further, the memory management needs to be considered. Therefore, formatter
	 * should be passed using this pointer type, which is based on a shared pointer.
	 */
	typedef std::shared_ptr<Formatter> FormatterPtr;

	/**
	 * The Lambda Formatter is a concrete generic implementation of the Formatter class. It uses
	 * a lambda expression passed in during the construction to format the actual output.
	 */
	template<typename Lambda>
	class LambdaFormatter : public Formatter {

		/**
		 * The lambda used to perform the formatting.
		 */
		Lambda lambda;

	public:

		/**
		 * Creates a new instance of this type printing the given literal using the
		 * given lambda during the formating.
		 *
		 * @param literal the literal to be handled by this formatter
		 * @param lambda the lambda performing the actual formatting
		 */
		LambdaFormatter(Lambda lambda) : lambda(lambda) {}

		/**
		 * Conducts the actual formatting of the given call expression.
		 *
		 * @param visitor the visitor and its context using this formatter
		 * @param call the call expression to be handled
		 */
		virtual void format(ConvertVisitor& visitor, CodeStream& cStr, const CallExprPtr& call) {
			lambda(visitor, cStr, call);
		}
	};

	/**
	 * A utility function to create LiteralFormatter instances without the need of
	 * specifying generic types. Those types will be inferred automatically.
	 *
	 * @param literal the literal to be handled by the requested formatter
	 * @return a new formatter handling the call expressions using the given lambda
	 */
	template<typename Lambda>
	FormatterPtr make_formatter(Lambda lambda) {
		return std::make_shared<LambdaFormatter<Lambda>>(lambda);
	}

	// define a type for handling formatter
	//typedef std::unordered_map<LiteralPtr, FormatterPtr, hash_target<LiteralPtr>, equal_target<LiteralPtr>> FormatTable;
	typedef std::unordered_map<string, FormatterPtr> FormatTable;

	// a forward declaration for a method assembling formater tables
	FormatTable initFormatTable(const lang::BasicGenerator&);

}

/** Central simple_backend conversion class, visits IR nodes and generates C code accordingly.
 ** */
class ConvertVisitor : public ASTVisitor<> {
	ConversionContext& cc;
	NameGenerator& nameGen;
	VariableManager& varManager;
	CodePtr defCodePtr;
	CodeStream& cStr;
	
	/**
	 * The table handling operator specific formatting rules.
	 */
	detail::FormatTable formats;

	template<typename Functor>
	void runWithCodeStream(CodeStream& cs, Functor f) {
		CodeStream& oldCodeStream = cStr;
		cStr = cs;
		f();
		cs = oldCodeStream;
	}

public:
	ConvertVisitor(ConversionContext& conversionContext) : cc(conversionContext), nameGen(cc.getNameGen()),
		varManager(cc.getVariableManager()), defCodePtr(std::make_shared<CodeFragment>()), cStr(defCodePtr->getCodeStream()),
		formats(detail::initFormatTable(cc.basic)) { };
	ConvertVisitor(ConversionContext& conversionContext, const CodePtr& cptr) : cc(conversionContext), nameGen(cc.getNameGen()),
		varManager(cc.getVariableManager()), defCodePtr(cptr), cStr(defCodePtr->getCodeStream()),
		formats(detail::initFormatTable(cc.basic)) { };

	CodePtr getCode() { return defCodePtr; }

	ConversionContext& getConversionContext() { return cc; }

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

	void visitIfStmt(const IfStmtPtr& ptr);

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

	void visitCaptureInitExpr(const CaptureInitExprPtr& ptr);

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

	void visitLiteral(const LiteralPtr& ptr);

public:

	void visitStructExpr(const StructExprPtr& ptr);

	void visitUnionExpr(const UnionExprPtr& ptr);

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

	void visitMemberAccessExpr(const MemberAccessExprPtr& ptr);

	void visitVariable(const VariablePtr& ptr);

	void visitVectorExpr(const VectorExprPtr& ptr);

};

} // namespace simple_backend
} // namespace insieme

std::ostream& operator<<(std::ostream& out, const insieme::simple_backend::ConvertedCode& code);
