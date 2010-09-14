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

#include <boost/algorithm/string/replace.hpp>

#include "ast_node.h"
#include "ast_visitor.h"
#include "statements.h"
#include "program.h"

#include "container_utils.h"
#include "hash_utils.h"

namespace insieme {
namespace simple_backend {

using namespace ::insieme::core;

/**
 * A stream based on std::stringstream that keeps a level of indentation and automatically
 * applies it at every line break.
 */
class CodeStream {
public:
	struct IndR { }; static IndR indR;
	struct IndL { }; static IndL indL;

private:
	std::stringstream ss;
	string indentString;

	template<typename T>
	friend CodeStream& operator<<(CodeStream& cstr, const T& param);
	
	template<typename T>
	void append(const T& param) {
		std::stringstream ssTmp;
		// TODO fix operator lookup
		ssTmp << param;
		string tmp = ssTmp.str();
		boost::replace_all(tmp, "\n", string("\n") + indentString);
		ss << tmp;
	}
	void append(IndR param) {
		indentString += "\t";
	}
	void append(IndL param) {
		assert(indentString.length()>0 && "Trying to indent below level 0");
		indentString = indentString.substr(0, indentString.length()-1);
	}

public:
	CodeStream() : indentString("") {
	}

	string getString() {
		return ss.str();
	}
};

template<typename T>
CodeStream& operator<<(CodeStream& cstr, const T& param) {
	cstr.append(param);
	return cstr;
}


// Forward declarations
class CodeFragment;
typedef std::shared_ptr<CodeFragment> CodePtr;

/**
 * A code fragment encapsulates some generated source code (in the form of a CodeStream) and an 
 * (optional) list of code fragments it depends on.
 */
class CodeFragment {

	CodeStream cStream;
	string name;
	std::vector<CodePtr> dependencies;

public:
	CodeFragment(const string& name = "unnamed") : name(name) {	}

	CodePtr addDependency(const string& name = "unnamed") {
		CodePtr newDep(new CodeFragment(name));
		dependencies.push_back(newDep);
		return newDep;
	}

	CodeStream& getCodeStream() { return cStream; }
};

class TypeConverter : public ASTVisitor<TypeConverter, string> {
public:
	string visitStructType(const StructTypePtr& ptr) {
		string ret = "struct " + ptr->getName() + "{\n";
		for_each(ptr->getEntries(), [](const NamedCompositeType::Entry& entry) {
			//entry.first
		});
		return ret;
	}
};

/**
 * Generates unique names for anonymous AST nodes when required.
 * Uses a simple counting system. Not thread safe, and won't necessarily generate the same name
 * for the same node in different circumstances. Names will, however, stay the same for unchanged 
 * programs over multiple runs of the compiler.
 */
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
		case SUPPORT:		name << "supp"; break;
		case TYPE:			name << "type"; break;
		case EXPRESSION:	name << "expr"; break;
		case STATEMENT:		name << "stat"; break;
		case DEFINITION:	name << "defi"; break;
		case PROGRAM:		name << "prog"; break;
		}
		name << "_" << num++;
		nameMap.insert(make_pair(ptr, name.str()));
		return getName(ptr, fragment);
	} 
};

class ConvertVisitor : public ASTVisitor<ConvertVisitor> {
	CodeFragment defCodeFrag;
	CodeStream& cStr;
	TypeConverter typeConv;
	NameGenerator nameGen;
	
	string printTypeName(const TypePtr& typ) {
		// TODO print C type name for specified type to cStr
		return "!Type";
	}

	template<typename Functor>
	void runWithCodeStream(CodeStream& cs, Functor f) {
		CodeStream& oldCodeStream = cStr;
		cStr = cs;
		f();
		cs = oldCodeStream;
	}

public:
	ConvertVisitor() : cStr(defCodeFrag.getCodeStream()) { };

	string getCode() {
		return cStr.getString();
	}

	//void visitNode(const NodePtr& node) {
	//	std::cout << *node << std::endl;
	//}

	void visitProgram(const ProgramPtr& ptr) {
		for_each(ptr->getDefinitions(), [](const DefinitionPtr& def) {
			
		});
	}
	
	////////////////////////////////////////////////////////////////////////// Statements

	void visitBreakStmt(const BreakStmtPtr&) {
		cStr << "break;\n";
	}

	void visitCompoundStmt(const CompoundStmtPtr& ptr) {
		cStr << "{" << CodeStream::indR << "\n";
		for_each(ptr->getChildList(), [&, this](const NodePtr& ptr) { this->visit(ptr); });
		cStr << CodeStream::indL << "}" << "\n";
	}

	void visitContinueStmt(const ContinueStmtPtr& ptr) {
		cStr << "continue;\n";
	}

	void visitDeclarationStmt(const DeclarationStmtPtr& ptr) {
		cStr << printTypeName(ptr->getVarExpression()->getType()) << " " << ptr->getVarExpression()->getIdentifier().getName() << " = ";
		visit(ptr->getInitialization());
		cStr << ";\n";
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

	void visitReturnStmt(const ReturnStmtPtr& ptr) {
		cStr << "return;\n";
	}

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
		cStr << "}\n";
	}

	void visitWhileStmt(const WhileStmtPtr& ptr) {
		cStr << "while(";
		visit(ptr->getCondition());
		cStr << ") ";
		visit(ptr->getBody());
	}

	////////////////////////////////////////////////////////////////////////// Expressions

	void visitCallExpr(const CallExprPtr& ptr) {
		const std::vector<ExpressionPtr>& args = ptr->getArguments();
		visit(ptr->getFunctionExpr());
		cStr << "(";
		if(args.size()>0) {
			visit(args.front());
			for_each(args.cbegin()+1, args.cend(), [&, this](const ExpressionPtr& curArg) {
				this->cStr << ", ";
				this->visit(curArg);
			});
		}
		cStr << ")";
	}

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
			CodePtr structCode = defCodeFrag.addDependency(structName);
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

	void visitLambdaExpr(const LambdaExprPtr& ptr) {
		// TODO when cname annotations are standardized
	}

	void visitRecLambdaExpr(const LambdaExprPtr& ptr) {
		// TODO when cname annotations are standardized
	}

	void visitLiteral(const LiteralPtr& ptr) {
		cStr << ptr->getValue();
	}

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
