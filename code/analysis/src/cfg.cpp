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

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

#include <tuple>

using namespace insieme::core;
using namespace insieme::utils;
using namespace insieme::analysis;
using namespace insieme::analysis::cfg;

namespace {

/**
 * Scope - Store the entry and exit blocks of program scopes. This is used in case of continue or break statements
 * to jump to the corresponding block.
 */
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
struct ScopeStack : public std::vector<Scope> {

	// Push the current scope on top of the stack
	void push(const Scope& scope) { push_back(scope); }

	// Returns the current top of the stack
	const Scope& top() const { return back(); }

	// Remove the element on top of the stack
	void pop() { pop_back(); }

	const Scope& getEnclosingLambda() const {
		// in the case we have jump outside a function we have to look for the innermost enclosing lambda expression
		auto filter = [](const Scope& curr) -> bool { return curr.root->getNodeType() == NT_LambdaExpr; };
		return getEnclosingBlock(filter);
	}

	// Returns target block of a continue statement placed in the innermost scope
	const Scope& getContinueTarget() const {
		auto filter = [](const Scope& curr) -> bool {
			NodeType&& nt = curr.root->getNodeType();
			return (nt == NT_WhileStmt || nt == NT_ForStmt);
		};
		return getEnclosingBlock(filter);
	}

	// Returns target block of a break statement placed in the innermost scope
	const Scope& getBreakTarget() const {
		auto filter = [](const Scope& curr) -> bool {
			NodeType&& nt = curr.root->getNodeType();
			return (nt == NT_WhileStmt || nt == NT_ForStmt || NT_SwitchStmt);
		};
		return getEnclosingBlock(filter);
	}

private:
	/**
	 * Search for enclosing scope to be returned. The filter class is used to filter out scopes which are not of
	 * interested because neglected by the particular control flow statement (e.g. continue statmente only looks
	 * for surrounding loops, not eventual switch statements)
	 * @param filter Filters the scopes
	 * @return The enclosing scope at which the control statement is referring to
	 */
	template <class Filter>
	const Scope& getEnclosingBlock(const Filter& filter) const {
		auto it = std::find_if(rbegin(), rend(), filter);
		// if the IR is well formed a continue statement is only allowed if an enclosing loop exists
		assert(it != rend());
		return *it;
	}
};

/**
 * Builder of the Control Flow Graph. Traverses the IR and creates blocks appending them to the CFG.
 * The visit is done in reverse order in a way the number of CFG nodes is minimized.
 */
struct CFGBuilder: public ASTVisitor< void > {

	CFG& cfg;
	cfg::Block* currBlock;

	bool isPending;

	CFG::VertexTy entry, exit, succ;
	ScopeStack 	scopeStack;

	// bool subExpr;
	// static insieme::utils::map::PointerMap<LambdaExprPtr, CFGPtr> cfgMap;

	CFGBuilder(CFG& cfg) :
			ASTVisitor<void>(false),
			cfg(cfg),
			currBlock(NULL),
			isPending(false),
			entry( cfg.addBlock( new cfg::Block ) ),
			exit( cfg.addBlock( new cfg::Block ) ),
			succ(exit) { }

	void createBlock() {
		// if we already have a block allocated and the block is empty we return it
		if ( isPending || (currBlock && currBlock->empty()) )
			return;

		// we have to make sure the currBlock is not containing elements already
		assert( !isPending && !currBlock && "CFG block lost during CFG creation" );
		currBlock = new cfg::Block;
		isPending = true;
	}

	void appendPendingBlock() {
		if ( isPending && currBlock && !currBlock->empty() ) {
			CFG::VertexTy&& node = cfg.addBlock(currBlock);
			cfg.addEdge(node, succ);
			succ = node;
			currBlock = NULL;
			isPending = false;
		}

		if ( !isPending && currBlock ) {
			currBlock = NULL;
		}
	}

	void visitIfStmt(const IfStmtPtr& ifStmt) {
		cfg::Block* ifBlock = new cfg::Block;
		ifBlock->appendElement( cfg::Element(ifStmt->getCondition(), cfg::Element::CtrlCond) );
		ifBlock->setTerminal(ifStmt);
		CFG::VertexTy&& src = cfg.addBlock( ifBlock );

		// the current node needs to be append to the graph (if not empty)
		appendPendingBlock();
		CFG::VertexTy sink = succ;

		createBlock();
		// push scope into the stack for this compound statement
		visit(ifStmt->getThenBody());
		appendPendingBlock();
		cfg.addEdge(src, succ, cfg::Edge("T")); // FIXME

		// reset the successor for the thenBody
		succ = sink;

		createBlock();
		// push scope into the stack for this compound statement
		visit(ifStmt->getElseBody());
		appendPendingBlock();
		cfg.addEdge(src, succ, cfg::Edge("F")); // FIXME

		succ = src;
		currBlock = ifBlock;
		isPending = false;
	}

	void visitContinueStmt(const ContinueStmtPtr& continueStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));

		createBlock();
		currBlock->setTerminal(continueStmt);
		succ = scopeStack.getContinueTarget().entry;
	}

	void visitBreakStmt(const BreakStmtPtr& breakStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));

		createBlock();
		currBlock->setTerminal(breakStmt);
		succ = scopeStack.getBreakTarget().exit;
	}

	void visitReturnStmt(const ReturnStmtPtr& retStmt) {
		assert(!currBlock || (currBlock && currBlock->empty()));

		createBlock();
		currBlock->setTerminal(retStmt);
		currBlock->appendElement( cfg::Element(retStmt->getReturnExpr()) );
		succ = scopeStack.getEnclosingLambda().exit;
	}

	void visitMarkerStmt(const MarkerStmtPtr& markerStmt) {
		visit( markerStmt->getSubStatement() );
	}

	void visitForStmt(const ForStmtPtr& forStmt) {
		cfg::Block* forBlock = new cfg::Block;
		forBlock->setTerminal(forStmt);
		forBlock->appendElement( cfg::Element(forStmt->getEnd(), cfg::Element::CtrlCond) );
		CFG::VertexTy&& src = cfg.addBlock( forBlock );

		appendPendingBlock();
		CFG::VertexTy sink = succ;

		// increment expression
		cfg::Block* incBlock = new cfg::Block;
		incBlock->appendElement( cfg::Element(forStmt, cfg::Element::LoopIncrement) );
		CFG::VertexTy&& inc = cfg.addBlock( incBlock );
		cfg.addEdge(inc, src);

		succ = inc;

		createBlock();
		// push scope into the stack for this compound statement
		scopeStack.push( Scope(forStmt, src, sink) );
		visit( forStmt->getBody() );
		scopeStack.pop();

		appendPendingBlock();

		cfg.addEdge(src, succ, cfg::Edge("T")); // FIXME
		cfg.addEdge(src, sink, cfg::Edge("F")); // FIXME

		succ = src;
		// decl stmt of the for loop needs to be part of the incoming block
		createBlock();
		currBlock->appendElement( cfg::Element(forStmt, cfg::Element::LoopInit) );
	}

	void visitWhileStmt(const WhileStmtPtr& whileStmt) {
		cfg::Block* whileBlock = new cfg::Block;
		whileBlock->appendElement( cfg::Element(whileStmt->getCondition()) );
		whileBlock->setTerminal(whileStmt);
		CFG::VertexTy&& src = cfg.addBlock( whileBlock );

		// the current node needs to be append to the graph (if not empty)
		appendPendingBlock();
		CFG::VertexTy sink = succ;

		succ = src;
		scopeStack.push( Scope(whileStmt, src, sink) );
		createBlock();
		// push scope into the stack for this compound statement
		visit(whileStmt->getBody());
		scopeStack.pop();

		appendPendingBlock();
		cfg.addEdge(src, succ, cfg::Edge("T")); // FIXME
		cfg.addEdge(src, sink, cfg::Edge("F")); // FIXME

		succ = src;
	}

	void visitSwitchStmt(const SwitchStmtPtr& switchStmt) {
		cfg::Block* switchBlock = new cfg::Block;
		switchBlock->appendElement( cfg::Element(switchStmt->getSwitchExpr()) );
		switchBlock->setTerminal(switchStmt);
		CFG::VertexTy&& src = cfg.addBlock( switchBlock );

		// the current node needs to be appendend to the graph (if not empty)
		appendPendingBlock();
		CFG::VertexTy sink = succ;

		scopeStack.push( Scope(switchStmt, src, sink) );
		const std::vector<SwitchStmt::Case>& cases = switchStmt->getCases();
		std::for_each(cases.begin(), cases.end(), [this, &src, &sink](const SwitchStmt::Case& curr){
			this->succ = sink;
			this->createBlock();
			// push scope into the stack for this compound statement
			this->visit(curr.second);

			appendPendingBlock();
			this->cfg.addEdge(src, succ, cfg::Edge( curr.first->toString() ));
		});

		succ = sink;
		createBlock();
		// Default case
		this->visit(switchStmt->getDefaultCase());
		appendPendingBlock();
		cfg.addEdge(src, succ);

		scopeStack.pop();
		succ = src;
		currBlock = switchBlock;
		isPending = false;
	}

	void visitCompoundStmt(const CompoundStmtPtr& compStmt) {
		const std::vector<StatementPtr>& body = compStmt->getStatements();

		createBlock();
		// we are sure there is at least 1 element in this compound statement
		for_each(body.rbegin(), body.rend(),
			[ this ](const StatementPtr& curr) { this->visit(curr); }
		);
	}

//	void visitCallExpr(const CallExprPtr& callExpr) {
//		VLOG(1) << "Visiting CallExpr: " << printer::PrettyPrinter(callExpr, 1<<5);
//		if(callExpr->getFunctionExpr()->getNodeType() == NT_LambdaExpr) {
//			const LambdaExprPtr& lambdaExpr = static_pointer_cast<const LambdaExpr>(callExpr->getFunctionExpr());
//			auto fit = cfgMap.find(lambdaExpr);
//			if(fit == cfgMap.end()) {
//				VLOG(1) << "Building CFG for function";
//				cfgMap.insert( std::make_pair(lambdaExpr, CFG::buildCFG(lambdaExpr)) );
//			}
//		}
//
//		if(!subExpr) {
//			if(!currBlock)
//				currBlock = new cfg::Block;
//			currBlock->appendElement(StatementPtr(callExpr));
//		}
//		subExpr = true;
//		const vector<ExpressionPtr>& args = callExpr->getArguments();
//		std::for_each(args.begin(), args.end(), [this](const ExpressionPtr& curr){ this->visit(curr); });
//		subExpr = false;
//	}

	void visitLambdaExpr(const LambdaExprPtr& lambda) {
		scopeStack.push( Scope(lambda, CFG::VertexTy(), succ) );
		visit(lambda->getBody());
		scopeStack.pop();
		// cfgMap.insert( std::make_pair(lambda, CFGPtr(&cfg)) );
		appendPendingBlock();
	}

	void visitProgram(const ProgramPtr& program) {
		const Program::EntryPointList& entryPoints = program->getEntryPoints();
		std::for_each(entryPoints.begin(), entryPoints.end(),
			[ this ]( const ExpressionPtr& curr ) {
				this->succ = this->exit;
				this->visit(curr);
				// connect the resulting block with the entry point
				this->cfg.addEdge( this->entry, this->succ );
			}
		);
		succ = entry;
	}

	void visitStatement(const StatementPtr& stmt) {
		assert(currBlock);
		currBlock->appendElement(stmt);
	}

	void completeGraph() {
		if (entry == succ)
			return;
		if (currBlock)
			appendPendingBlock();

		cfg.addEdge(entry, succ);
		succ = entry;
	}

};

// insieme::utils::map::PointerMap<LambdaExprPtr, CFGPtr> CFGBuilder::cfgMap = insieme::utils::map::PointerMap<LambdaExprPtr, CFGPtr>();
} // end anonymous namespace

namespace insieme {
namespace analysis {


CFGPtr CFG::buildCFG(const NodePtr& rootNode) {
	CFGPtr cfg = std::make_shared<CFG>();
	CFGBuilder builder(*cfg);
	builder.visit(rootNode);
	builder.completeGraph();

	// print stats
//	VLOG(2) << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~";
//	VLOG(2) << "Num of CFG graphs: " << CFGBuilder::cfgMap.size();
//	std::for_each(CFGBuilder::cfgMap.begin(), CFGBuilder::cfgMap.end(),
//		[](insieme::utils::map::PointerMap<LambdaExprPtr, CFGPtr>::value_type curr){
//			VLOG(2) << "number of nodes: " << curr.second->getSize();
//		}
//	);
//	VLOG(2) << "Main: " << cfg->getSize();
	return cfg;
}

CFG::VertexTy CFG::addBlock(cfg::Block* block) {
	CFG::VertexTy&& v = boost::add_vertex(CFG::NodeProperty(block), graph);
	block->setBlockID(v);
	boost::property_map< CFG::ControlFlowGraph, size_t CFG::NodeProperty::* >::type&& blockID =
			get(&CFG::NodeProperty::id, graph);
	put(blockID, v, currId++);
	return v;
}

CFG::~CFG() {
	boost::graph_traits<ControlFlowGraph>::vertex_iterator vi, vi_end;
	for (boost::tie(vi, vi_end) = vertices(graph); vi != vi_end; ++vi) {
		const cfg::Block* block = &getBlock(*vi);
		delete block;
	}
}

} // end analysis namespace
} // end insieme namespace

namespace std {

std::ostream& operator<<(std::ostream& out, const insieme::analysis::CFG& cfg) {
	using namespace insieme::analysis;

	CFG::ConstNodePropertyMapTy&& node = get(&CFG::NodeProperty::block, cfg.graph);
	CFG::ConstEdgePropertyMapTy&& edge = get(&CFG::EdgeProperty::edge, cfg.graph);

	boost::write_graphviz(out, cfg.graph,
			CFG::BlockLabelWriter<CFG::ConstNodePropertyMapTy>(node),
			CFG::EdgeLabelWriter<CFG::ConstEdgePropertyMapTy>(edge), boost::default_writer(),
			get(&CFG::NodeProperty::id, cfg.graph)
	);
	return out;
}

std::ostream& operator<<(std::ostream& out, const insieme::analysis::cfg::Block& block) {
	if(!block.empty()) {
		out << "[shape=box,label=\"";
		out << "[B" << block.getBlockID() << "]\\l";
		size_t num = 0;
		std::for_each(block.stmt_begin(), block.stmt_end(), [ &out, &num ](const insieme::analysis::cfg::Element& curr) {
			out << num++ << ": ";
			switch(curr.getType()) {
			case cfg::Element::None:
				out << printer::PrettyPrinter( static_pointer_cast<const Statement>(curr), 1<<5 );
				break;
			case cfg::Element::CtrlCond:
				out << printer::PrettyPrinter( static_pointer_cast<const Statement>(curr), 1<<5 ) << " <CTRL>";
				break;
			case cfg::Element::LoopInit: {
				out << printer::PrettyPrinter( static_pointer_cast<const ForStmt>(curr)->getDeclaration(), 1<<5 ) << " <LOOP_INIT>";
				break;
			}
			case cfg::Element::LoopIncrement: {
				const ForStmtPtr& forStmt = static_pointer_cast<const ForStmt>(curr);
				out << printer::PrettyPrinter(forStmt->getDeclaration()->getVariable()) << " += "
				    << printer::PrettyPrinter( forStmt->getStep(), 1<<5 ) << " <LOOP_INC>";
				break;
			}
			default:
				break;
			}
			out << "\\l";
		});
		if(!!block.getTerminator())
			out << "T: " << block.getTerminator();

		return out << "\"]";
	}
	// assert(block.getBlockID() == 0 || block.getBlockID() == 1); // entry and exit block are the only allowed empty blocks
	return out << "[shape=diamond,label=\""<< (block.getBlockID() == 0 ? "ENTRY" : "EXIT") << "\"]";
}

std::ostream& operator<<(std::ostream& out, const insieme::analysis::cfg::Terminator& term) {
	// assert(type == Element::TERMINAL);
	switch(term->getNodeType()) {
	case NT_IfStmt:
		return out << "IF(...)\\l";
	case NT_ForStmt: {
		ForStmtPtr forStmt = static_pointer_cast<const ForStmt>(term);
		return out << "FOR( " << "... ; "
				<< printer::PrettyPrinter( forStmt->getDeclaration()->getVariable(), 1<<5 ) << " < "
				<< printer::PrettyPrinter(forStmt->getEnd(), 1<<5 ) << "; ...)\\l";
	}
	case NT_WhileStmt:
		return out << "WHILE(...)\\l";
	case NT_SwitchStmt:
		return out << "SWITCH(...)\\l";
	case NT_ContinueStmt:
		return out << "CONTINUE\\l";
	case NT_BreakStmt:
		return out << "BREAK\\l";
	case NT_ReturnStmt:
		return out << "RETURN\\l";
	default:
		assert(false);
	}
}

} // end std namespace

