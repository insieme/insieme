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

#include "insieme/analysis/cfg.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/printer/pretty_printer.h"

#include <stack>
#include <tuple>

using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::cfg;

namespace {

struct Scope {
	NodePtr  root;
	CFG::VertexTy entry;
	CFG::VertexTy exit;

	Scope(const NodePtr& root, const CFG::VertexTy& entry, const CFG::VertexTy& exit) :
		root(root), entry(entry), exit(exit) { }
};

/**
 * ScopeStack: represent a stack of scopes
 */
struct ScopeStack : std::vector<Scope> {

	ScopeStack() { }

	void push(const Scope& scope) { push_back(scope); }
	const Scope& top() const { return back(); }
	void pop() { pop_back(); }

	const Scope& getEnclosingLoop() const {
		auto it = std::find_if(rbegin(), rend(), [](const Scope& curr){ return !!dynamic_pointer_cast<const ForStmt>(curr.root); });
		assert(it != rend()); // if the IR is well formed a continue statement is only allowed if an enclosing loop exists
		return *it;
	}
};

typedef std::tuple<bool, CFG::VertexTy> BlockBounds;

struct CFGBuilder: public ASTVisitor< BlockBounds > {

	CFG& cfg;
	CFG::VertexTy head;
	ScopeStack scopeStack;

	CFGBuilder(CFG& cfg) : cfg(cfg) { }

	BlockBounds visitIfStmt(const IfStmtPtr& ifStmt) {
		cfg::Block ifBlock;
		ifBlock.setTerminal(ifStmt);
		CFG::VertexTy&& src = cfg.addNode( ifBlock );
		CFG::VertexTy sink = head;

		head = cfg.addNode();
		cfg.addEdge(head, sink);
		// push scope into the stack for this compound statement
		BlockBounds&& thenCfg = visit(ifStmt->getThenBody());
		cfg.addEdge(src, std::get<0>(thenCfg) ? std::get<1>(thenCfg) : head);

		head = cfg.addNode();
		cfg.addEdge(head, sink);
		BlockBounds&& elseCfg = visit(ifStmt->getElseBody());
		cfg.addEdge(src, std::get<0>(elseCfg) ? std::get<1>(elseCfg) : head);

		// move the pointer to the head of the CFG to the
		// terminal statement associated to this if stmt
		head = src;

		return std::make_tuple(false, src);
	}

	BlockBounds visitContinueStmt(const ContinueStmtPtr& continueStmt) {
		CFG::VertexTy target = scopeStack.getEnclosingLoop().entry;
		return std::make_tuple(true, target);
	}

	BlockBounds visitBreakStmt(const BreakStmtPtr& breakStmt) {
		CFG::VertexTy target = scopeStack.getEnclosingLoop().exit;
		return std::make_tuple(true, target);
	}

	BlockBounds visitMarkerStmt(const MarkerStmtPtr& markerStmt) {
		return visit( markerStmt->getSubStatement() );
	}

	BlockBounds visitForStmt(const ForStmtPtr& forStmt) {
		cfg::Block forBlock;
		forBlock.setTerminal(forStmt);
		CFG::VertexTy&& src = cfg.addNode( forBlock );
		CFG::VertexTy&& sink = cfg.addNode( cfg::Block() );

		cfg.addEdge(src, head);
		cfg.addEdge(sink, src);
		// push scope into the stack for this compound statement
		scopeStack.push( Scope(forStmt, src, head) );
		head = sink;

		visit(forStmt->getBody());
		cfg.addEdge(src, head);
		scopeStack.pop();

		head = src;
		return std::make_tuple(false, src);
	}

	BlockBounds visitCompoundStmt(const CompoundStmtPtr& compStmt) {
		// add a temporary node as sink of this compound stmt,
		// later on, the proper CFGEleemnt will be written
		const std::vector<StatementPtr>& body = compStmt->getStatements();
		if(body.empty())
			return std::make_tuple(false, CFG::VertexTy());

		// we are sure there is at least 1 element in this compound statement
		for_each(body.rbegin(), body.rend(),
			[ this ](const StatementPtr& curr) {
				BlockBounds&& currNode = this->visit(curr);
				// if the curr statement is not empty, is being appended to the graph
				if(std::get<0>(currNode)) {
					this->cfg.addEdge(std::get<1>(currNode), this->head);
					this->head = std::get<1>(currNode);
				}
			}
		);

		return std::make_tuple(true, head);
	}

//	BlockBounds visitCallExpr(const CallExprPtr& callExpr) {
//		if(callExpr->getFunctionExpr()->getNodeType() == NT_LambdaExpr) {
//			cfg::Block callBlock;
//			callBlock.setTerminal(callExpr);
//			CFG::VertexTy&& src = cfg.addNode( callBlock );
//			CFG::VertexTy&& sink = cfg.addNode( cfg::Block() );
//			cfg.addEdge(sink, head);
//			head = sink;
//			visit(callExpr->getFunctionExpr());
//			cfg.addEdge(src, head);
//			head = src;
//			return std::make_tuple(false, head);
//		}
//		return visitStatement(callExpr);
//	}

	BlockBounds visitLambdaExpr(const LambdaExprPtr& lambda) {
		CFG::VertexTy sink = head;

		scopeStack.push( Scope(lambda, CFG::VertexTy(), sink) );
		BlockBounds&& body = visit(lambda->getBody());
		scopeStack.pop();

		head = std::get<1>(body);

		return std::make_tuple(true, head);
	}

	BlockBounds visitProgram(const ProgramPtr& program) {
		CFG::VertexTy&& src = cfg.addNode( cfg::Block() );
		CFG::VertexTy&& sink = cfg.addNode( cfg::Block() );

		std::for_each(program->getEntryPoints().begin(), program->getEntryPoints().end(),
			[ this, src, sink ]( const ExpressionPtr& curr ) {
				this->head = sink;
				BlockBounds&& currBlock = this->visit(curr);
				this->cfg.addEdge(src, std::get<1>(currBlock));
			}
		);
		head = src;

		return std::make_tuple(true, src);
	}

	BlockBounds visitStatement(const StatementPtr& stmt) {
		// CFG::VertexTy&& v = cfg.addNode( cfg::Element(stmt, cfg::Element::BLOCK) );
		cfg.getNode(head).appendStmt(stmt);
		return std::make_tuple(false, head);
	}

};


} // end anonymous namespace

namespace insieme {
namespace analysis {

namespace cfg {

//core::ExpressionPtr Element::getCond() const {
//	// assert(type == Element::TERMINAL);
//	switch(stmt->getNodeType()) {
//	case core::NT_IfStmt:
//		return static_pointer_cast<const IfStmt>(stmt)->getCondition();
//	case core::NT_ForStmt:
//		return static_pointer_cast<const ForStmt>(stmt)->getEnd();
//	default:
//		assert(false);
//	}
//}

} // end cfg namespace

CFG CFG::buildCFG(const ProgramPtr& rootNode) {
	CFG cfg;
	CFGBuilder builder(cfg);
	builder.visit(rootNode);
	return cfg;
}

} // end analysis namespace
} // end insieme namespace

namespace std {

std::ostream& operator<<(std::ostream& out, const insieme::analysis::CFG& cfg) {
	using namespace insieme::analysis;

	CFG::ConstNodePropertyMapTy&& node = get(&CFG::NodeProperty::block, cfg.graph);
	boost::write_graphviz(out, cfg.graph, CFG::LabelWriter<CFG::ConstNodePropertyMapTy>(node));
	return out;
}

std::ostream& operator<<(std::ostream& out, const insieme::analysis::cfg::Block& block) {
	if(block.getSize() > 0 || !!block.getTerminator()) {
		out << "[shape=box,label=\"";
		size_t num = 0;
		std::for_each(block.stmt_begin(), block.stmt_end(), [ &out, &num ](const insieme::analysis::cfg::Element& curr) {
			out << num++ << ": " << insieme::core::printer::PrettyPrinter( static_pointer_cast<const Statement>(curr), 1<<5 ) << "\\l";
		});
		if(!!block.getTerminator())
			out << "T: " << block.getTerminator();

		return out << "\"]";
	}
	return out << "[shape=circle,label=\"\"]";
}

std::ostream& operator<<(std::ostream& out, const insieme::analysis::cfg::Terminator& term) {
	// assert(type == Element::TERMINAL);
	switch(term->getNodeType()) {
	case insieme::core::NT_IfStmt:
		return out << "IF(" << insieme::core::printer::PrettyPrinter( static_pointer_cast<const IfStmt>(term)->getCondition(), 1<<5 ) << ")\\l";
	case insieme::core::NT_ForStmt: {
		ForStmtPtr forStmt = static_pointer_cast<const ForStmt>(term);
		return out << "FOR( " << printer::PrettyPrinter( forStmt->getDeclaration(), 1<<5) << "; "
				<< printer::PrettyPrinter( forStmt->getDeclaration()->getVariable(), 1<<5 ) << " < " << printer::PrettyPrinter(forStmt->getEnd(), 1<<5 ) << "; "
				<< printer::PrettyPrinter( forStmt->getDeclaration()->getVariable(), 1<<5 ) << " += " << printer::PrettyPrinter( forStmt->getStep(), 1<<5) << ")\\l";
	}
	default:
		assert(false);
	}
}

} // end std namespace

