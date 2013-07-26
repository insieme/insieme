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

#include <memory>
#include <sstream>
#include <map>

#include "insieme/frontend/sema.h"
#include "insieme/frontend/pragma/matcher.h"

#include <clang/Basic/SourceLocation.h>
#include <clang/Lex/Pragma.h>

#include <clang/Parse/Parser.h>

// forward declaration
namespace clang {
class Stmt;
class Decl;
class Expr;
}

namespace insieme {

namespace core {

template <class T>
class Pointer;

class Node;

typedef Pointer<const Node> NodePtr;

} // end core namespace

namespace frontend {

namespace conversion {
class Converter;
} // end convert namespace 

namespace pragma {

/** 
 * Defines an interface which pragmas which would like to be automatically transferred to the
 * generated IR must implement. If not the user is responsable of handling the attachment of pragmas
 * to the IR nodes.
 */
struct AutomaticAttachable {

	virtual core::NodePtr attachTo(const core::NodePtr& node, conversion::Converter& fact) const = 0;

	virtual ~AutomaticAttachable() { }
};

// ------------------------------------ Pragma ---------------------------
/**
 * Defines a generic pragma which contains the location (start,end), and the
 * target node
 */
class Pragma {
	/**
	 * Attach the pragma to a statement. If the pragma is already bound to a
	 * statement or location, a call to this method will produce an error.
	 */
	void setStatement(clang::Stmt const* stmt);

	/**
	 * Attach the pragma to a declaration. If the pragma is already bound to a
	 * statement or location, a call to this method will produce an error.
	 */
	void setDecl(clang::Decl const* decl);

	friend class insieme::frontend::InsiemeSema;
public:

	/**
	 * Type representing the target node which could be wither a statement
	 * or a declaration
	 */
	typedef llvm::PointerUnion<clang::Stmt const*, clang::Decl const*> PragmaTarget;

	/**
	 * Creates an empty pragma starting from source location startLoc and ending
	 * ad endLoc.
	 */
	Pragma(const clang::SourceLocation& startLoc, 
		   const clang::SourceLocation& endLoc, 
		   const std::string& 			type) :
		mStartLoc(startLoc), mEndLoc(endLoc), mType(type) { }

	/**
	 * Creates a pragma starting from source location startLoc and ending ad endLoc
	 * by passing the content of the map which associates, for each key defined in
	 * the pragma_matcher, the relative parsed list of values.
	 *
	 */
	Pragma(const clang::SourceLocation& startLoc, 
		   const clang::SourceLocation& endLoc, 
		   const std::string& 			type, 
		   const MatchMap& 				mmap) : mStartLoc(startLoc), mEndLoc(endLoc), mType(type) { }

	const clang::SourceLocation& getStartLocation() const { return mStartLoc; }
	const clang::SourceLocation& getEndLocation() const { return mEndLoc; }
	/**
	 * Returns a string which identifies the pragma
	 */
	const std::string& getType() const { return mType; }

	clang::Stmt const* getStatement() const;
	clang::Decl const* getDecl() const;

	/**
	 * Returns true if the AST node associated to this pragma is a statement (clang::Stmt)
	 */
	bool isStatement() const { 
		return !mTargetNode.isNull() && mTargetNode.is<clang::Stmt const*> ();	
	}

	/**
	 * Returns true if the AST node associated to this pragma is a declaration (clang::Decl)
	 */
	bool isDecl() const { 
		return !mTargetNode.isNull() && mTargetNode.is<clang::Decl const*> (); 
	}

	/**
	 * Writes the content of the pragma to standard output
	 */
	virtual void dump(std::ostream& out, const clang::SourceManager& sm) const;

	/**
	 * Returns a string representation of the pragma
	 */
	std::string toStr(const clang::SourceManager& sm) const;

	virtual ~Pragma() {	}

private:
	clang::SourceLocation mStartLoc, mEndLoc;
	std::string mType;
	PragmaTarget mTargetNode;
};

typedef std::shared_ptr<Pragma> PragmaPtr;
typedef std::vector<PragmaPtr> 	PragmaList;

// ------------------------------------ PragmaStmtMap ---------------------------
/**
 * Maps statements and declarations to a Pragma.
 */
class PragmaStmtMap {
public:
	typedef std::multimap<const clang::Stmt*, const PragmaPtr> StmtMap;
	typedef std::multimap<const clang::Decl*, const PragmaPtr> DeclMap;

	template <class IterT>
	PragmaStmtMap(const IterT& begin, const IterT& end) {
		std::for_each(begin, end, [ this ](const typename IterT::value_type& pragma){
			if(pragma->isStatement())
				this->stmtMap.insert( std::make_pair(pragma->getStatement(), pragma) );
			else
				this->declMap.insert( std::make_pair(pragma->getDecl(), pragma) );
		});
	}

	const StmtMap& getStatementMap() const { return stmtMap; }
	const DeclMap& getDeclarationMap() const { return declMap; }

private:
	StmtMap stmtMap;
	DeclMap declMap;
};

// -------------------------------- BasicPragmaHandler<T> ---------------------------
/**
 * Defines a generic pragma handler which uses the pragma_matcher. Pragmas which are syntactically
 * correct are then instantiated and associated with the following node (i.e. a Stmt or
 * Declaration). If an error occurs, the error message is printed out showing the location and the
 * list of tokens the parser was expecting at that location.
 */
template<class T>
class BasicPragmaHandler: public clang::PragmaHandler {
	pragma::node* pragma_matcher;
	std::string base_name;

public:
	BasicPragmaHandler(clang::IdentifierInfo* 	name, 
					   const node& 				pragma_matcher, 
					   const std::string& 		base_name = std::string()) 
		: PragmaHandler(name->getName().str()), 
		  pragma_matcher(pragma_matcher.copy()), base_name(base_name) { }

	void HandlePragma(clang::Preprocessor& 			PP, 
					  clang::PragmaIntroducerKind 	kind, 
					  clang::Token& 				FirstToken) 
	{
		// '#' symbol is 1 position before
		clang::SourceLocation&& startLoc = 
			ParserProxy::get().CurrentToken().getLocation().getLocWithOffset(-1);

		MatchMap mmap;
		ParserStack errStack;

		if ( pragma_matcher->MatchPragma(PP, mmap, errStack) ) {
			// the pragma type is formed by concatenation of the base_name and identifier, for
			// example the type for the pragma:
			//		#pragma omp barrier
			// will be "omp::barrier", the string is passed to the pragma constructur which store
			// the value
			std::ostringstream pragma_name;
			if(!base_name.empty())
				pragma_name << base_name << "::";
			if(!getName().empty())
				pragma_name << getName().str();

			clang::SourceLocation endLoc = ParserProxy::get().CurrentToken().getLocation();
			// the pragma has been successfully parsed, now we have to instantiate the correct type
			// which is associated to this pragma (T) and pass the matcher map in order for the
			// pragma to initialize his internal representation. The framework will then take care
			// of associating the pragma to the following node (i.e. a statement or a declaration).
			static_cast<InsiemeSema&>(ParserProxy::get().getParser()->getActions()).
				ActOnPragma<T>( pragma_name.str(), mmap, startLoc, endLoc );
			return;
		}
		// In case of error, we report it to the console using the clang Diagnostics.
		errorReport(PP, startLoc, errStack);
		PP.DiscardUntilEndOfDirective();
	}

	~BasicPragmaHandler() {	delete pragma_matcher; }
};

// ------------------------------------ PragmaHandlerFactory ---------------------------
struct PragmaHandlerFactory {

	template<class T>
	static clang::PragmaHandler* CreatePragmaHandler(
			clang::IdentifierInfo* name, node 
			const& re, 
			const std::string& base_name = std::string())
	{
		return new BasicPragmaHandler<T> (name, re, base_name);
	}
};

// Handle the automatic attaching of annotations (coming from user pragmas) to generated IR nodes 
core::NodePtr  attachPragma( const core::NodePtr& 			node, 
		  				     const clang::Stmt* 				clangNode, 
						     conversion::Converter& 	fact );

core::NodePtr  attachPragma( const core::NodePtr& 			node, 
		  				     const clang::Decl* 				clangDecl, 
						     conversion::Converter& 	fact );
} // end pragma namespace
} // End frontend namespace
} // End insieme namespace


