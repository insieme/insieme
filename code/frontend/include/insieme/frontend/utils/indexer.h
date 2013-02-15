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

#include <assert.h>
#include <string>
#include <map>

#include "insieme/frontend/compiler.h"
#include "insieme/frontend/program.h"

#include "clang/AST/Decl.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/DeclBase.h"



namespace insieme{
namespace frontend{
namespace utils{


class indexerASTConsumer;


//////////////////////////////////////////////////////////////////
// the indexer generates an index of 
class Indexer{
public:
	typedef std::pair<clang::Decl*, insieme::frontend::TranslationUnit*> tStored;

private:
	typedef std::map<std::string, tStored> tIndex; 

	tIndex   mIndex;
	tStored  voidPair;

public:

	////////////////////////////////////////////////
	// the context will be the owner of all generated AST nodes
	Indexer();

	////////////////////////////////////////////////
	//
	void indexTU(insieme::frontend::TranslationUnit* tu);

	////////////////////////////////////////////////
	///
	tStored getDefAndTUforDefinition (const std::string &symbol) const;

	////////////////////////////////////////////////
	//
	clang::Decl* getDefinitionFor (const std::string &symbol) const;

	////////////////////////////////////////////////
	//
	clang::Decl* getDefinitionFor (const clang::Decl* decl) const;

	////////////////////////////////////////////////
	///
	tStored getDefAndTUforDefinition (const clang::Decl* decl) const;

	////////////////////////////////////////////////
	//
	void dump() const;


	friend class indexerASTConsumer;
};







} // end namespace utils
} // end namespace frontend
} // end namespace insieme
