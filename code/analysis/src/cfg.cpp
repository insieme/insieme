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

#include <boost/graph/strong_components.hpp>
#include <boost/graph/graphviz.hpp>

#include <boost/graph/breadth_first_search.hpp>

#include <boost/graph/depth_first_search.hpp>

#include "insieme/analysis/cfg.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_expressions.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

#include <tuple>
#include <stack>

using namespace insieme::core;
using namespace insieme::utils;
using namespace insieme::analysis;
using namespace insieme::analysis::cfg;

typedef std::vector<ExpressionAddress> ExpressionAddressList;

namespace {

/**
 * Scope - Store the entry and exit blocks of program scopes. This is used in case of continue or
 * break statements to jump to the corresponding block.
 */
struct Scope {
	NodePtr root;         // The IR node which defines this scope
	CFG::VertexTy entry;   // The CFG node where the scope begins
	CFG::VertexTy exit;    // The CFG node where the scope ends

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
		// in the case we have jump outside a function we have to look for the innermost enclosing
		// lambda expression
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
	 * Search for enclosing scope to be returned. The filter class is used to filter out scopes
	 * which are not of interested because neglected by the particular control flow statement (e.g.
	 * continue statmente only looks for surrounding loops, not eventual switch statements)
	 *
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

class EmptyBlockException : public std::logic_error {
public:
	EmptyBlockException() : std::logic_error("block empty")  { }
};

class NotConnectedException : public std::logic_error {
public:
	NotConnectedException() : std::logic_error("block not connected")  { }
};


struct BlockManager {

public:

	class BlockInfo : public std::tuple<std::unique_ptr<cfg::Block>, bool, bool, CFG::VertexTy> {

		/**
		 * Release the pointer from the unique_ptr container so it does not get removed
		 */
		inline void release_block_ptr() {
			assert (!pending() && "Cannot release, the blcok has not been inserted into the CFG");
			std::get<0>(*this).release();
		}

	public:
		BlockInfo(const CFGPtr& cfg) : 
			std::tuple<
				std::unique_ptr<cfg::Block>, 
				bool, 
				bool, 
				CFG::VertexTy
			> (std::unique_ptr<cfg::Block>(new cfg::Block(*cfg)), true, false, CFG::VertexTy()) { }

		BlockInfo(BlockInfo&& other) : 
			std::tuple<std::unique_ptr<cfg::Block>, bool, bool, CFG::VertexTy>(std::move(other)) { }

		inline BlockInfo& operator=(BlockInfo&& other) {

			assert (((pending() && curr_block()->empty()) || !pending()) && "Blcok is dirty!" ) ;
			if (!pending()) { release_block_ptr(); }

			std::tuple<std::unique_ptr<cfg::Block>, bool, bool, CFG::VertexTy>::operator=(std::move(other));
			return *this;
		}

		inline cfg::Block* curr_block() const { 
			return std::get<0>(*this).get(); 
		}
	
		inline const bool& pending() const { 
			return std::get<1>(*this); 
		}
		inline bool& pending() { 
			return std::get<1>(*this); 
		}

		inline const bool& connected() const { return std::get<2>(*this); }
		inline bool& connected() { return std::get<2>(*this); }

		inline const CFG::VertexTy& block_id() const { return std::get<3>(*this); }
		inline CFG::VertexTy& block_id() { return std::get<3>(*this); }

		~BlockInfo() {
			// If the block has been used in the CFG then do not delete it 
			if (!pending()) { release_block_ptr(); }
		}

	};

	BlockManager(const CFGPtr& cfg) : cfg(cfg), bInfo(cfg) { }

	// Pass the ownership of the block info externally
	BlockInfo get() { 
		BlockInfo b ( std::move(bInfo) ); 
		assert(b.curr_block());
		// allocate a new block
		bInfo = BlockInfo(cfg);
		return std::move(b);
	}

	inline void set(BlockInfo&& block) { bInfo = std::move(block); }

	inline cfg::Block* operator->() const { return bInfo.curr_block(); }

	/**
	 * Close the current block and creates a new one for new content. 
	 * If the block contains no statements this function has no effects
	 */
	void close() { 

		// Reuse it
		if (bInfo.pending() && !bInfo.connected() && bInfo.curr_block()->empty()) { return; }

		if (!bInfo.connected()) throw NotConnectedException();

		bInfo = BlockInfo(cfg);
	}

	CFG::VertexTy connectTo(const CFG::VertexTy& node, const cfg::Edge& e = cfg::Edge()) {
		if (bInfo.pending()) { append(); }
	
		cfg->addEdge(bInfo.block_id(), node, e);
		bInfo.connected()=true;

		return bInfo.block_id();
	}

	/** 
	 * Append the block to the CFG but it keeps the block as current
	 */
	CFG::VertexTy append() {
		if (!bInfo.pending())
			return bInfo.block_id();

		if (bInfo.pending() && !bInfo.curr_block()->empty()) {
			bInfo.block_id() = cfg->addBlock(bInfo.curr_block());
			bInfo.pending()=false;
			return bInfo.block_id();
		} 

		// throw an exception
		throw EmptyBlockException();
	}

private:

	CFGPtr cfg;
	BlockInfo bInfo;

};


/**
 * Builder of the Control Flow Graph. Traverses the IR and creates blocks appending them to the CFG.
 * The visit is done in reverse order in a way the number of CFG nodes is minimized.
 */
template < CreationPolicy CP >
struct CFGBuilder: public IRVisitor< void, Address > {

	CFGPtr 			cfg;
	IRBuilder 		builder;

	CFG::VertexTy 	entry, exit, succ, head;
	ScopeStack 		scopeStack;
	BlockManager 	blockMgr;
	
	VariablePtr 	retVar;

	CFGBuilder(CFGPtr cfg, const NodeAddress& root) : 
		IRVisitor<void, Address>(false), 
		cfg(cfg), 
		builder(root->getNodeManager()), 
		blockMgr(cfg)
	{
		assert( !cfg->hasSubGraph(root) && "CFG for this root node already being built");
		CFG::GraphBounds&& bounds = cfg->addSubGraph(root);
		// initialize the entry/exit blocks for this CFG
		entry = std::get<1>(bounds);
		head = entry;

		exit = std::get<2>(bounds);
		succ = exit;

		// set the exit variable to be used to store the return value of the
		// function 
		retVar = std::get<0>(bounds);
		
		visit( root ); 				// Visit the IR

		blockMgr.close();

		// Performs the final steps to finalize the CFG
		if ( entry == succ )	return;

		//if ( cfg->getBlock(succ).empty() ) {
		//	// If the first statement of a root element is a function call we end up with an empty
		//	// statement at the top of the CFG, we want to remove that block and connect the
		//	// outgoing edges to the entry node
		//	cfg->replaceNode(succ, entry);
		//	return;
		//}	
		cfg->addEdge(entry, succ);	// connect the entry with the top node
	}

	/**
	 * When a continue statement is encountered we jump to the closest enclosing loop scope
	 * (i.e. for or while stmt)
	 */
	void visitContinueStmt(const ContinueStmtAddress& continueStmt) {
		blockMgr->terminator() = cfg::Terminator(continueStmt);
		succ = blockMgr.connectTo(scopeStack.getContinueTarget().entry);
	}

	/**
	 * When a break statement is encountered we jump right after the closest enclosing loop scope
	 * (i.e. for or while stmt)
	 */
	void visitBreakStmt(const BreakStmtAddress& breakStmt) {
		blockMgr->terminator() = cfg::Terminator( breakStmt );
		succ = blockMgr.connectTo(scopeStack.getBreakTarget().exit);
	}

	/**
	 * When a return statement is encountered we jump to the exit block of the closest enclosing function scope
	 * (i.e. lambda expression)
	 */
	void visitReturnStmt(const ReturnStmtAddress& retStmt) {
		blockMgr->terminator() = cfg::Terminator(retStmt);
		succ = blockMgr.connectTo(scopeStack.getEnclosingLambda().exit);
		
		if (!builder.getLangBasic().isUnit(retStmt->getReturnExpr()->getType())) {

			if (retVar) {
				cfg->getTmpVarMap().storeTmpVar(retStmt->getReturnExpr(), retVar);
			}

			visit( retStmt->getReturnExpr() );
		}
	}

	void visitMarkerStmt(const MarkerStmtAddress& markerStmt) {
		visit( markerStmt->getSubStatement() );
	}

	void visitMarkerExpr(const MarkerExprAddress& markerExpr) {
		visit( markerExpr->getSubExpression() );
	}

	void visitIfStmt(const IfStmtAddress& ifStmt) {

		// append any pending block before we fork the CFG for inserting the for stmt
		blockMgr.close();

		// Store the current head of the CFG (stored in succ)
		CFG::VertexTy sink = succ;
		
		// the THEN body of the IF stmt
		visit( ifStmt->getThenBody() );
		blockMgr.close();
		CFG::VertexTy thenBlock = succ;
	
		succ = sink;

		// the ELSE body of the IF stmt
		visit( ifStmt->getElseBody() );
		blockMgr.close();
		CFG::VertexTy elseBlock = succ;

		// Build the block representing the entry of the IF stmt
		blockMgr->terminator() = cfg::Terminator(ifStmt);
		CFG::VertexTy src = blockMgr.append();

		// Connect the thenBlock with the head of the CFG 
		blockMgr.connectTo(thenBlock, cfg::Edge( builder.getLangBasic().getTrue() )); 

		// Connect the else block with the head of the CFG 
		blockMgr.connectTo(elseBlock, cfg::Edge( builder.getLangBasic().getFalse() ));

		// this is the successor now
		succ = src;

		// Visit the condition of the if stmt
		visit( ifStmt->getCondition() );
	}

	void visitSwitchStmt(const SwitchStmtAddress& switchStmt) {

		blockMgr.close();
		
		blockMgr->terminator() = cfg::Terminator(switchStmt);
		CFG::VertexTy src = blockMgr.append();
		BlockManager::BlockInfo saveBlock = blockMgr.get();

		CFG::VertexTy sink = succ;
		
		std::vector< std::pair<ExpressionPtr, CFG::VertexTy> > casesBlocks;

		scopeStack.push( Scope(switchStmt, src, sink) );
		for ( const auto& curr : switchStmt->getCases() ) {
			succ = sink;

			visit(curr->getBody());
			blockMgr.close();

			if (succ != sink) { 
				casesBlocks.push_back( {curr->getGuard(), succ} ); 
			}
		}

		succ = sink;

		// Default case
		visit(switchStmt->getDefaultCase());
		blockMgr.close();

		casesBlocks.push_back( { builder.stringLit("default"), succ } );
		scopeStack.pop();

		// reset the src block
		blockMgr.set(std::move(saveBlock));

		// connect the case blocks
		for_each(casesBlocks, 
				[&](const std::pair<ExpressionPtr, CFG::VertexTy>& cur) { 
					this->blockMgr.connectTo(cur.second, cur.first); 
				} );

		succ = src;

		visit( switchStmt->getSwitchExpr() );
	}

	void visitWhileStmt(const WhileStmtAddress& whileStmt) {
		
		blockMgr.close();
		
		blockMgr->terminator() = cfg::Terminator(whileStmt);
		CFG::VertexTy src = blockMgr.append();

		BlockManager::BlockInfo saveBlock = blockMgr.get();

		CFG::VertexTy sink = succ;

		succ = src;
		scopeStack.push( Scope(whileStmt, src, sink) );
		visit(whileStmt->getBody());
		scopeStack.pop();

		blockMgr.close();

		// reset the src block
		blockMgr.set(std::move(saveBlock));

		blockMgr.connectTo(succ, cfg::Edge( builder.getLangBasic().getTrue() )); 
		blockMgr.connectTo(sink, cfg::Edge( builder.getLangBasic().getFalse() ));

		// this is the new successor not
		succ = src;

		visit( whileStmt->getCondition() );
	}

	void visitForStmt(const ForStmtAddress& forStmt) {
	
		blockMgr.close();

		blockMgr->terminator() = cfg::Terminator(forStmt);
		CFG::VertexTy forHead = blockMgr.append();

		CFG::VertexTy sink = succ;
		succ = blockMgr.connectTo(succ, cfg::Edge( builder.boolLit(false) ));

		// Visit expressions in the End conditions 
		visit( forStmt->getEnd() );
		blockMgr.close();

		CFG::VertexTy src = succ;

		// increment expression
		blockMgr->appendElement( cfg::Element(forStmt, cfg::Element::LOOP_INCREMENT) );
		succ = blockMgr.connectTo(succ);
		blockMgr.close();

		// push scope into the stack for this compound statement
		scopeStack.push( Scope(forStmt, src, sink) );
		visit( forStmt->getBody() );
		scopeStack.pop();
		
		blockMgr.close();

		// reset the src block
		cfg->addEdge(forHead, succ, cfg::Edge( builder.boolLit(true) )); 

		succ = src;

		// decl stmt of the for loop needs to be part of the incoming block
		visit( forStmt->getDeclaration() );
	}

	void visitCompoundStmt(const CompoundStmtAddress& compStmt) {

		std::vector<StatementAddress> body(compStmt->getStatements());
		if ( body.empty() ) { return; }

		// we are sure there is at least 1 element in this compound statement
		std::for_each(body.rbegin(), body.rend(),
			[ & ](const StatementAddress& curr) { visit(curr); }
		);
	}

	/**
	 * Given an expression, this function takes care of checking whether it is necessary to
	 * introduce a temporary variable to hold its result. In that case a temporary variable is
	 * introduced and registred in the tmpVarMap. 
	 *
	 * Otherwise the same expression is returned, the boolean value is true when a temporary
	 * variable is introduced, false otherwise
	 */
	std::pair<bool,ExpressionPtr> storeTemp(const ExpressionAddress& cur ) {

		if ( cur->getNodeType() == NT_CallExpr || 
			 cur->getNodeType() == NT_CastExpr || 
			 cur->getNodeType() == NT_StructExpr ||
			 cur->getNodeType() == NT_UnionExpr ||
			 cur->getNodeType() == NT_TupleExpr ||
			 cur->getNodeType() == NT_VectorExpr ||
			 cur->getNodeType() == NT_MarkerExpr) 
		{
			return {true, cfg->getTmpVarMap().createTmpFor(cur)};
		} 
		return {false, cur.getAddressedNode()};
	}

	ExpressionPtr normalize(const ExpressionAddress& expr, std::vector<NodeAddress>& idxs) {

		struct NormalizeVisitor: public IRVisitor<ExpressionPtr, Address> {

			CFGBuilder& cfgBuilder;
			IRBuilder& builder;
			std::vector<NodeAddress>& idxs;

			NormalizeVisitor(CFGBuilder& cfgBuilder, std::vector<NodeAddress>& idxs) : 
				IRVisitor<ExpressionPtr, Address>(false), 
				cfgBuilder(cfgBuilder), 
				builder(cfgBuilder.builder), 
				idxs(idxs) { }

			ExpressionPtr visitCallExpr(const CallExprAddress& callExpr) {
				// analyze the arguments of this call expression
				vector<ExpressionPtr> newArgs;
				for (const auto& arg : callExpr->getArguments()) {
					auto&& ret = cfgBuilder.storeTemp(arg);
					newArgs.push_back( ret.second );
					if (ret.first) { idxs.push_back( arg ); }
				}
				return builder.callExpr(callExpr->getFunctionExpr(), newArgs);
			}

			ExpressionPtr visitCastExpr(const CastExprAddress& castExpr) {
				// analyze the arguments of this call expression
				auto&& newArg = cfgBuilder.storeTemp(castExpr->getSubExpression());

				if (newArg.first) { idxs.push_back(castExpr->getSubExpression()); }

				return newArg.first ? 
					cfgBuilder.builder.castExpr(castExpr->getType(), newArg.second) :
					castExpr.getAddressedNode();
			}

			ExpressionPtr visitMarkerExpr(const MarkerExprAddress& markerExpr) {
				return visit(markerExpr->getSubExpression());
			}

			ExpressionPtr visitStructExpr(const StructExprAddress& structExpr) {
				StructExpr::Members members; 

				for(const auto& member : structExpr->getMembers()) {
					auto&& tmpVar = cfgBuilder.storeTemp(member->getValue());
					members.push_back( 
						NamedValue::get(structExpr->getNodeManager(), member->getName(), tmpVar.second) 
					);
					if (tmpVar.first) {
						idxs.push_back( member->getValue() );
					}
				}
				return builder.structExpr( members );
			}

			ExpressionPtr visitUnionExpr(const UnionExprAddress& unionExpr) {

				auto&& tmpVar = cfgBuilder.storeTemp(unionExpr->getMember());
				if (tmpVar.first) {
					idxs.push_back( unionExpr->getMember() );
				}
				return builder.unionExpr( unionExpr->getType(), unionExpr->getMemberName(), tmpVar.second );
			}

			ExpressionPtr visitVectorExpr(const VectorExprAddress& vectorExpr) {
				std::vector<ExpressionPtr> exprs; 

				for(const auto& expr : vectorExpr->getExpressions()) {
					auto&& tmpVar = cfgBuilder.storeTemp(expr);
					exprs.push_back( tmpVar.second );
					if (tmpVar.first) {
						idxs.push_back( expr );
					}
				}
				return builder.vectorExpr( exprs );
			}

			ExpressionPtr visitTupleExpr(const TupleExprAddress& tupleExpr) {
				std::vector<ExpressionPtr> exprs; 

				for(const auto& expr : tupleExpr->getExpressions()) {
					auto&& tmpVar = cfgBuilder.storeTemp(expr);
					exprs.push_back( tmpVar.second );
					if (tmpVar.first) {
						idxs.push_back( expr );
					}
				}
				return builder.tupleExpr( exprs );
			}

			ExpressionPtr visitExpression(const ExpressionAddress& expr) {
				return expr.getAddressedNode();
			}

		};

		return NormalizeVisitor(*this, idxs).visit(expr);
	}

	// In the case the passed address needs to be saved in one of the predisposed temporary
	// variables, this function will generate the corresponding declaration stmt
	StatementPtr assignTemp(const ExpressionAddress& expr, const ExpressionPtr& currExpr) {
		auto var = cfg->getTmpVarMap().lookupImmediateAlias(expr);
		return var ? static_cast<StatementPtr>(builder.declarationStmt( var, currExpr )) : currExpr;
	}

	void visitArgument(const ExpressionAddress& arg) {
		
		// Visit the Arguments
		if ( arg->getNodeType() == NT_CallExpr || 
			 arg->getNodeType() == NT_CastExpr ||
			 arg->getNodeType() == NT_StructExpr ||
			 arg->getNodeType() == NT_UnionExpr ||
			 arg->getNodeType() == NT_TupleExpr ||
			 arg->getNodeType() == NT_VectorExpr ||
			 arg->getNodeType() == NT_MarkerExpr ) 
		{
			visit(arg);
		}

	}

	void visitDeclarationStmt(const DeclarationStmtAddress& declStmt) {

		ExpressionAddress init = declStmt->getInitialization();

		std::vector<NodeAddress> idxs;
		// analyze the arguments of this call expression
		auto initExpr = normalize(init, idxs);
		
		bool isLambda = init->getNodeType() == NT_CallExpr && 
						init.as<CallExprAddress>()->getFunctionExpr()->getNodeType() == NT_LambdaExpr;

		if (isLambda) { initExpr = storeTemp(init).second; }

		blockMgr->appendElement( cfg::Element(builder.declarationStmt(declStmt->getVariable(), initExpr), declStmt) );

		append();

		// If it was introduced a temporary variable for the initialization value then we have to
		// recur over the initialization expression 
		if (isLambda) { visit(init); return; }

		for (const auto& addr : idxs) { visit(addr); }
	}

	void visitCastExpr(const CastExprAddress& castExpr) {
		ExpressionAddress subExpr = castExpr->getSubExpression();
	
		std::vector<NodeAddress> idxs;
		// analyze the arguments of this call expression
		auto subExprMod = normalize(subExpr,idxs);

		bool isLambda = subExpr->getNodeType() == NT_CallExpr && 
						subExpr.as<CallExprAddress>()->getFunctionExpr()->getNodeType() == NT_LambdaExpr;

		if (isLambda) { subExprMod = storeTemp(subExpr).second; }

		blockMgr->appendElement( cfg::Element( assignTemp(castExpr,subExprMod), castExpr) );
		append();

		if (isLambda) { visit(subExpr); return; }

		if (!idxs.empty()) { visit(idxs.front()); }
	}

	
	void visitInitializerExpr(const ExpressionAddress& exprAddr) {
		std::vector<NodeAddress> idxs;
		// analyze the arguments of this call expression
		auto repMod = normalize(exprAddr,idxs);

		blockMgr->appendElement( cfg::Element( assignTemp(exprAddr,repMod), exprAddr) );
		append();
		
		for (const auto& addr : idxs) { visit(addr); }
	}

	void visitStructExpr(const StructExprAddress& structExpr) { visitInitializerExpr(structExpr); }
	void visitTupleExpr(const TupleExprAddress& tupleExpr) { visitInitializerExpr(tupleExpr); }
	void visitUnionExpr(const UnionExprAddress& unionExpr) { visitInitializerExpr(unionExpr); }
	void visitVectorExpr(const VectorExprAddress& vecExpr) { visitInitializerExpr(vecExpr); }

	void visitCallExpr(const CallExprAddress& callExpr) {
	
		vector<ExpressionPtr> newArgs;
		for (const auto& arg : callExpr->getArguments()) {
			newArgs.push_back( storeTemp(arg).second );
		}
		
		// LOG(INFO) << *callExpr->getFunctionExpr();
		ExpressionPtr toAppendStmt = builder.callExpr(callExpr->getFunctionExpr(), newArgs);

		if ( callExpr->getFunctionExpr()->getNodeType() == NT_LambdaExpr ) {
			const LambdaExprAddress& lambdaExpr = static_address_cast<const LambdaExpr>(callExpr->getFunctionExpr());

			if ( !cfg->hasSubGraph(lambdaExpr) ) {
				// In the case the body has not been visited yet, proceed with the graph construction
				// TODO: This can be executed in a separate thread (if necessary)
				CFG::buildCFG<CP>(lambdaExpr.getAddressedNode(), cfg);
			}

			blockMgr.close();

			// get the bounds 
			CFG::GraphBounds&& bounds = cfg->getNodeBounds(lambdaExpr);

			// A call expression creates 2 blocks, 1 spawning the function call and the second one
			// collecting the return value
			cfg::CallBlock* call = new cfg::CallBlock(*cfg);
			cfg::RetBlock* ret = new cfg::RetBlock(*cfg);

			// we interconnect the two blocks so that if we want to have intra-procedural analysis
			// we can jump directly to the return block without visiting the body of the function
			call->setReturnBlock( *ret );
			// call->appendElement( cfg::Element(callExpr) );

			ret->setCallBlock(*call);

			CFG::VertexTy&& callVertex = cfg->addBlock( call );

			const auto& params = lambdaExpr->getParameterList();
			for(size_t idx=0; idx<params.size(); ++idx) {
				const auto& param = params[idx];
				const auto& arg = newArgs[idx];

				call->appendElement( cfg::Element(builder.declarationStmt(param, arg), param) );
			}

			// lookup the retVar introduced for this lambdaexpr
			auto retVar = std::get<0>(bounds);

			if (auto tmpVar = cfg->getTmpVarMap().lookupImmediateAlias(callExpr)) {
				ret->appendElement( cfg::Element(builder.declarationStmt( tmpVar, retVar ), callExpr) );
			}

			cfg->addEdge(callVertex, std::get<1>(bounds)); // CALL -> Function Entry

			CFG::VertexTy&& retVertex = cfg->addBlock( ret );
			cfg->addEdge(std::get<2>(bounds), retVertex); // Function Exit -> RET
			cfg->addEdge(retVertex, succ );

			succ = callVertex;
			
		} else {
			blockMgr->appendElement( cfg::Element(assignTemp(callExpr,toAppendStmt), callExpr) );

			append();
		}

		// Visit the Arguments
		for (const auto& cur : callExpr->getArguments()) { visitArgument(cur); }
	}

	//void visitCallExpr(const CallExprAddress& callExpr) {
		//// if the call expression is calling a lambda the body of the lambda is processed and the
		//// sub graph is built
		//if ( callExpr->getFunctionExpr()->getNodeType() == NT_LambdaExpr ) {
			//const LambdaExprAddress& lambdaExpr = static_address_cast<const LambdaExpr>(callExpr->getFunctionExpr());

			//if ( !cfg->hasSubGraph(lambdaExpr) ) {
				//// In the case the body has not been visited yet, proceed with the graph construction
				//// TODO: This can be executed in a separate thread (if necessary)
				//CFG::buildCFG<CP>(lambdaExpr.getAddressedNode(), cfg);
			//}

			//appendPendingBlock();

			//CFG::GraphBounds&& bounds = cfg->getNodeBounds(lambdaExpr);
			//// A call expression creates 2 blocks, 1 spawning the function call and the second one
			//// collecting the return value
			//cfg::CallBlock* call = new cfg::CallBlock(*cfg);
			//cfg::RetBlock* ret = new cfg::RetBlock(*cfg);

			//// we interconnect the two blocks so that if we want to have intra-procedural analysis
			//// we can jump directly to the return block without visiting the body of the function
			//call->setReturnBlock( *ret );
			//// call->appendElement( cfg::Element(callExpr) );

			//ret->setCallBlock(*call);

			//CFG::VertexTy&& callVertex = cfg->addBlock( call );

			//const auto& params = lambdaExpr->getParameterList();
			//const auto& args = callExpr->getArguments();
			//assert(params.size() == args.size());

			//for(size_t idx=0; idx<args.size(); ++idx) {
				//const auto& param = params[idx];
				//const auto& arg = storeTemp(args[idx]);

				//call->appendElement( cfg::Element(builder.declarationStmt(param, arg.second), param) );
			//}

			//// lookup the retVar introduced for this lambdaexpr
			//auto retVar = std::get<0>(bounds);

			//// lookup for whether we need to introduce a temporary var for this
			//// callExpr
			//auto callIt = tmpVarMap.find(callExpr);
			//if (callIt != tmpVarMap.end()) {
				//ret->appendElement( cfg::Element(builder.declarationStmt( callIt->second, retVar ), callExpr) );
			//}
			//cfg->addEdge(callVertex, std::get<1>(bounds)); // CALL -> Function Entry

			//CFG::VertexTy&& retVertex = cfg->addBlock( ret );
			//cfg->addEdge(std::get<2>(bounds), retVertex); // Function Exit -> RET
			//cfg->addEdge(retVertex, succ );

			//succ = callVertex;
			//resetCurrBlock();

		//} else {
			//// we are in the multistmt per block mode we should not append and create a new block
			//// here
			//assert(currBlock);

			//// Analyze the call expression and introduce temporary variables 
			//const vector<ExpressionAddress>& args = callExpr->getArguments();

			//vector<ExpressionPtr> newArgs;
			//std::for_each(args.begin(), args.end(), [ & ] (const ExpressionAddress& curr) {
					//newArgs.push_back( this->storeTemp(curr).second );
				//});

			//StatementPtr toAppendStmt = builder.callExpr(callExpr->getFunctionExpr(), newArgs);
			//auto fit = tmpVarMap.find(callExpr);
			//if (fit!=tmpVarMap.end()) {
				//toAppendStmt = builder.declarationStmt( fit->second, toAppendStmt.as<ExpressionPtr>() );
			//}

			//currBlock->appendElement( cfg::Element(toAppendStmt, callExpr) );

			//if (CP == OneStmtPerBasicBlock)
				//appendPendingBlock();
		//}

		//bool hasAllocated=false;
		//if ( !hasHead ) {
			//assert(!spawnBlock);
			//spawnBlock = new cfg::Block( *cfg, cfg::Block::DEFAULT );
			//head = cfg->addBlock( spawnBlock );
			//hasHead = true;
			//hasAllocated = true;
			//maxSpawnedArg = 0;
		//}

		//CFG::VertexTy sink = succ;

		//size_t spawnedArgs = 0;
		//const auto& args = callExpr->getArguments();
		//argNumStack.push(0);
		//std::for_each(args.begin(), args.end(), [ this, sink, &spawnedArgs ] (const ExpressionAddress& curr) {

			//// in the case the argument is a call expression, we need to allocate a separate block
			//// in order to perform the inter-procedural function call
			//if ( curr->getNodeType() == NT_CallExpr || 
				 //curr->getNodeType() == NT_CastExpr ||
				 //curr->getNodeType() == NT_MarkerExpr) 
			//{
				//this->createBlock();
				//this->visit( curr );
				//this->appendPendingBlock();
				
				//if ( this->succ != sink ) { ++spawnedArgs;	}

				//this->succ = sink;
			//}
			//this->argNumStack.top()++;

		//});
		//argNumStack.pop();
	
		//if(spawnedArgs > maxSpawnedArg) {
			//maxSpawnedArg = spawnedArgs;
		//}
		//// In the case a spawnblock has been created to capture arguments of the callExpr but no
		//// arguments were call expressions, therefore the created spawnblock is not necessary. 
		//if ( maxSpawnedArg<2 && hasAllocated ) {
			//if (spawnedArgs == 1) {
				//succ = **cfg->successors_begin(head);
			//}

			//// remove the spawned block from the CFG 
			//cfg->removeBlock( head );
			//// delete spawnBlock;
			//spawnBlock = NULL;

			//// set the head to false (for next calls to this function)
			//hasHead = false;
			//isPending = false;
			//return;
		//}

		//if ( spawnedArgs==0 && !hasAllocated ) {
			//cfg->addEdge(head, succ);
		//}

		//if ( hasAllocated ) {
			//succ = head;
			//currBlock = spawnBlock;
			//isPending = false;
			//hasHead = false;
			//spawnBlock = NULL;
			//hasAllocated=false;
		//}
	//}

	

	void visitLambdaExpr(const LambdaExprAddress& lambda) {
		scopeStack.push( Scope(lambda, CFG::VertexTy(), succ) );
		visit(lambda->getBody());
		scopeStack.pop();
	}

	void visitProgram(const ProgramAddress& program) {
		const ExpressionAddressList& entryPoints = program->getEntryPoints();
		std::for_each(entryPoints.begin(), entryPoints.end(),
			[ & ]( const ExpressionAddress& curr ) {
				succ = exit;

				visit(curr);
				// connect the resulting block with the entry point
				this->cfg->addEdge( entry, succ );
			}
		);
		succ = entry;
	}

	void append() {
		CFG::VertexTy bId = blockMgr.append();
		if (bId != succ)
			succ = blockMgr.connectTo(succ);

		if (CP == OneStmtPerBasicBlock) 
			blockMgr.close();

	}

	void visitStatement(const StatementAddress& stmt) {
		
		StatementPtr toAppendStmt = stmt.getAddressedNode();
		if (ExpressionAddress expr = dynamic_address_cast<const Expression>(stmt)) {
			toAppendStmt = assignTemp(expr, toAppendStmt.as<ExpressionPtr>()); 
		}

		blockMgr->appendElement( cfg::Element(toAppendStmt, stmt) );
		append();	
	}

};

} // end anonymous namespace

namespace insieme {
namespace analysis {

template <>
CFGPtr CFG::buildCFG<OneStmtPerBasicBlock>(const NodePtr& rootNode, CFGPtr cfg) {
	CFGBuilder<OneStmtPerBasicBlock> builder(cfg, NodeAddress(rootNode));
	return cfg;
}

template <>
CFGPtr CFG::buildCFG<MultiStmtPerBasicBlock>(const NodePtr& rootNode, CFGPtr cfg) {
	CFGBuilder<MultiStmtPerBasicBlock> builder(cfg, NodeAddress(rootNode));
	return cfg;
}

CFG::VertexTy CFG::addBlock(cfg::Block* block) {
	assert(block);
	CFG::VertexTy&& v = boost::add_vertex(graph);
	
	block->setVertexID(v);
	graph[v] = std::shared_ptr<cfg::Block>(block);

	// Set the index appropriately
	BlockIDPropertyMapTy&& blockID = get(boost::vertex_index, graph);
	put(blockID, v, currId++);

	return v;
}

void CFG::removeBlock(const CFG::VertexTy& v) {
	boost::clear_in_edges(v, graph);
	boost::clear_out_edges(v, graph);
	boost::remove_vertex(v, graph);
}

std::tuple<VariablePtr, CFG::VertexTy,CFG::VertexTy> CFG::addSubGraph(const NodePtr& root) {
	CFG::VertexTy&& entry = addBlock( new cfg::Block(*this, cfg::Block::ENTRY) );
	CFG::VertexTy&& exit = addBlock( new cfg::Block(*this, cfg::Block::EXIT) );

	if (subGraphs.empty()) {
		entry_block = entry; exit_block = exit;
	}

	VariablePtr var;
	if(root->getNodeType() == core::NT_LambdaExpr) {
		var = IRBuilder(root->getNodeManager()).variable(
				root.as<LambdaExprPtr>()->getType().as<FunctionTypePtr>()->getReturnType()
			);
	}
	return subGraphs.insert( std::make_pair(root, std::make_tuple(var, entry, exit)) ).first->second;
}

core::NodePtr CFG::getRootNode() const {
	auto it = std::find_if(subGraphs.begin(), subGraphs.end(), 
		[&] (const SubGraphMap::value_type& curr) { 
			return entry_block == std::get<1>(curr.second) && 
				   exit_block  == std::get<2>(curr.second); 
		});
	assert(it != subGraphs.end() && "Root node of the CFG not correctly stored");
	return it->first;
}

void CFG::replaceNode(const CFG::VertexTy& oldNode, const CFG::VertexTy& newNode) {
	// collect the outgoing edges
	std::vector<VertexTy> dest;
	AdjacencyIterator vi, vi_end;
	tie(vi, vi_end) = adjacent_vertices(oldNode, graph);
	std::copy( vi, vi_end, std::back_inserter(dest) );

	std::vector<cfg::Edge> edges;

	OutEdgeIterator ei, ei_end;
	boost::tie(ei, ei_end) = boost::out_edges(oldNode, graph);

	std::for_each( ei, ei_end, [this, &edges](const EdgeTy& curr) {
		edges.push_back( graph[curr] );
	});

	assert(dest.size() == edges.size() && "Number of outgoing edges and children of the node should be the same");

	boost::clear_in_edges(oldNode, graph);
	boost::clear_out_edges(oldNode, graph);
	remove_vertex(oldNode, graph);

	std::vector<cfg::Edge>::const_iterator eit = edges.begin();
	for ( std::vector<VertexTy>::const_iterator vit=dest.begin(), end=dest.end(); vit != end; ++vit, ++eit) {
		this->addEdge(newNode, *vit, *eit);
	}
}

bool CFG::isEntry(const cfg::BlockPtr& block) const { 
	return block->getVertexID() == entry_block;
}

bool CFG::isExit(const cfg::BlockPtr& block) const { 
	return block->getVertexID() == exit_block;
}

CFG::EdgeTy CFG::addEdge(const VertexTy& src, const VertexTy& dest, const cfg::Edge& edge) {
	std::pair<EdgeTy, bool>&& edgeDesc = boost::add_edge(src, dest, graph);
	// we don't allow to insert the same edge twice, if this happens something 
	// is wrong in the construction of the CFG graph
	assert( edgeDesc.second && "Tried to insert a duplicated edge, forbidden!");

	graph[edgeDesc.first] = edge;
	return edgeDesc.first;
}

// Returns the Edge object associated to a graph edge connecting src and dest vertices 
const cfg::Edge& CFG::getEdge(const CFG::VertexTy& src, const CFG::VertexTy& dest) const { 
	auto edgeDescriptor = boost::edge(src, dest, graph);
	assert(edgeDescriptor.second && "No edge exists between the two selected vertices");
	return graph[edgeDescriptor.first];
}

void CFG::printStats(std::ostream& out) {
	out << "# of CFGs:        " << subGraphs.size() << std::endl;
	out << "# of total nodes: " << boost::num_vertices(graph) << std::endl;
	out << "# of total edges: " << boost::num_edges(graph) << std::endl;
}

// Prints the CFG in a DOT fromat
std::ostream& CFG::printTo(std::ostream& out) const {
	using namespace insieme::analysis;

	/**
	 * Lambda for the production of a DOT graph from the boost::graph.
	 * This class takes care of printing the content of CFG Blocks to the output stream.
	 */
	auto node_printer = [&](std::ostream& out, const VertexTy& v) {
		out << *graph[v];
	};

	/**
	 * Lambda for the production of a DOT graph from the boost::graph.
	 * This class takes care of printing the content of CFG Edges to the output stream.
	 */
	auto edge_printer = [&](std::ostream& out, const EdgeTy& edge) {
		const cfg::Edge& e = graph[edge];
		if (e.getEdgeExpr()) {
			out << "[label=\"";
			if (e.getEdgeExpr()) { out << *e.getEdgeExpr(); }
			out << "\"]";
		}
	};

	boost::write_graphviz(out, graph, node_printer, edge_printer);
	return out;
}


int CFG::getStrongComponents() {
	// Map kept to register the connected components,
	// Each vertex is associated to a component which is identified with an intereger number 
	typedef std::map<VertexTy, int> component_type;
	component_type component;
	boost::associative_property_map<component_type> component_map(component);

	typedef std::map<VertexTy, VertexTy> root_type;
	root_type root;
	boost::associative_property_map<root_type> root_map(root);

	typedef std::map<VertexTy, boost::default_color_type> color_type;
	color_type color;
	boost::associative_property_map<color_type> color_map(color);

	component_type discover;
	boost::associative_property_map<component_type> discover_map(discover);

	int num = strong_components(graph, component_map, 
			boost::root_map(root_map).
			color_map(color_map).
			discover_time_map(discover_map));
	

	// FIXME: add Analysis of Connected Components here
	return num;

	// Debug info
	// std::cout << "Total number of components: " << num << std::endl;
	//
	//BlockIDPropertyMapTy&& blockID = get(boost::vertex_index, graph);
	//for_each ( component.begin(), component.end(), [&] (const component_type::value_type& cur) {
	//	std::cout << "Vertex " << blockID[cur.first] <<" is in component " << cur.second << std::endl;
	//});
}

std::pair<cfg::BlockPtr,size_t> CFG::find(const core::NodeAddress& node) const {

	cfg::BlockPtr found;
	size_t stmtIdx=0;
	auto&& block_visitor = [&] (const cfg::BlockPtr& block) -> void {

		stmtIdx=0;
		for_each(block->stmt_begin(), block->stmt_end(), [&](const cfg::Element& cur) {
			
			// if the block has been found, just return 
			if (found) return;

			core::NodeAddress src = cur.getStatementAddress();
			core::NodeAddress trg = node;

			if(isChildOf(src,trg)) {
				assert(!found && "Another node already matched the requrested node");
				found = block;
				return; 
			}

			++stmtIdx;
		});
	};

	// Stop the depth search visitor once we find the node 
	auto terminator = [&] (const CFG::VertexTy& v, const CFG::ControlFlowGraph& g) {
		return found;
	};

	typedef std::map<VertexTy, boost::default_color_type> color_type;
	color_type color;
	boost::associative_property_map<color_type> color_map(color);

	boost::depth_first_visit( graph, 
			entry_block, 
			boost::make_dfs_visitor( BlockVisitor(block_visitor) ),
			color_map, 
			terminator
		);

	return { found, stmtIdx };
}

namespace cfg {


namespace {

//~~~~~ DOT FILE PRINTING UTILITIES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Functions invoked by the boost dot visitor to print the CFG in a dot file 
	
// print IR nodes to a string and removes eventual new lines and leading spaces
std::string getPrettyPrinted(const NodePtr& node) {
	std::ostringstream ss;
	ss << printer::PrettyPrinter(node, printer::PrettyPrinter::OPTIONS_DETAIL | 1 << 5);

	// Remove new lines and leading spaces
	std::vector<char> res;
	std::string prettyPrint = ss.str();
	for(auto it = prettyPrint.begin(), end = prettyPrint.end(); it != end; ++it)
		if(!(*it == '\n' || (it + 1 != end && *it == ' ' && *(it+1) == ' '))) {
			if(*it != '\"')
				res.push_back(*it);
			else {
				res.push_back('\\');
				res.push_back('\"');
			}
		}
	return std::string(res.begin(), res.end());
}

} // end anonymous namespace


// FOLLOWING FUNCTION NEEDS REFACTORING!!!! PLEASE REFACTOR ME!
std::ostream& Block::printTo(std::ostream& out) const {

	switch ( type() ) {

	case Block::DEFAULT:
	case Block::CALL:
	case Block::RET:
	{
		// CFG Blocks have box shape by default
		out << "[shape=box,label=\"";
		// The first line state the id of this block 
		out << "[B" << getBlockID() << "]";

		if (type() == Block::CALL) { out << ":CALL"; }
		if (type() == Block::RET) { out << ":RET"; }
		out << "\\l";

		size_t num = 0;
		std::for_each(stmt_begin(), stmt_end(), [&](const Element& curr) {
			out << num++ << ": ";
			core::StatementPtr currStmt = curr.getAnalysisStatement();
			switch(curr.getType()) {
			case cfg::Element::NONE:
				// out << getPrettyPrinted( currStmt );
				out << *currStmt;
				break;

			case cfg::Element::CTRL_COND:
				out << getPrettyPrinted( currStmt ) << " <CTRL>";
				break;

			case cfg::Element::LOOP_INIT: {
				out << getPrettyPrinted( 
						static_pointer_cast<const ForStmt>(currStmt)->getStart() 
					) << " <LOOP_INIT>";
				break;
			}
			case cfg::Element::LOOP_INCREMENT: {
				const ForStmtPtr& forStmt = static_pointer_cast<const ForStmt>(currStmt);
				out << printer::PrettyPrinter( forStmt->getIterator() ) << " += "
					<< printer::PrettyPrinter( forStmt->getStep(), 10001 ) << " <LOOP_INC>";
				break;
			}
			default: break;
			}
			out << "\\l";
		});

		if(hasTerminator())
			out << "T: " << terminator();

		return out << "\"]";
	}

	case cfg::Block::ENTRY:
		return out << "[shape=diamond,label=\"ENTRY\"]";

	case cfg::Block::EXIT:
		return out << "[shape=diamond,label=\"EXIT\"]";

	default:
		return out;
	}
}

std::ostream& Terminator::printTo(std::ostream& out) const {
	// assert(type == Element::TERMINAL);
	core::StatementPtr stmt = getAnalysisStatement();
	switch ( stmt->getNodeType() ) {

	case NT_IfStmt: 		return out << "IF(...)\\l";

	case NT_ForStmt: {
		ForStmtPtr forStmt = static_pointer_cast<const ForStmt>( stmt );
		return out << "FOR( " << "... ; "
				<< printer::PrettyPrinter( forStmt->getIterator(), 10001 ) << " < "
				<< printer::PrettyPrinter( forStmt->getEnd(), 10001 ) << "; ...)\\l";
	}

	case NT_WhileStmt:		return out << "WHILE(...)\\l";

	case NT_SwitchStmt:		return out << "SWITCH(...)\\l";

	case NT_ContinueStmt:	return out << "CONTINUE\\l";

	case NT_BreakStmt:		return out << "BREAK\\l";

	case NT_ReturnStmt: 	return out << "RETURN\\l";

	default:
		assert(false && "Terminator statement is not supported");
	}
}

} // end cfg namespace 
} // end analysis namespace
} // end insieme namespace

