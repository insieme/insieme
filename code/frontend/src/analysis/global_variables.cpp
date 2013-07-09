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



// defines which are needed by LLVM
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#include <clang/AST/Decl.h>
#pragma GCC diagnostic pop

#include "insieme/frontend/analysis/global_variables.h"
#include "insieme/frontend/utils/indexer.h"


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

using namespace insieme::frontend;



namespace {
/**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * traverses the translation unit to find all declarations, 
 * mark those which are globals, extern, or static
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
class GlobalsVisitor{

	private:
		analysis::GlobalVarCollector& collector;

	public:
	GlobalsVisitor	(analysis::GlobalVarCollector& coll) :
		collector(coll)
												 
	{ }

	void analizeDecl(clang::Decl* decl, bool local){

		// === named DECL ====
		// this might be anything with a name
		if (llvm::isa<clang::NamedDecl>(decl)) {
			// === Function Decl ===
			if (clang::FunctionDecl* fdecl = llvm::dyn_cast<clang::FunctionDecl>(decl)) {
				if (fdecl->hasBody()){
					analyzeDeclContext(llvm::cast<clang::DeclContext>(fdecl), true);
				}
			}
			// === tag Decl ===
			else if (const clang::TagDecl* tag = llvm::dyn_cast<clang::TagDecl>(decl)) {

				switch (tag->getTagKind ()){
					case clang::TagDecl::TagKind::TTK_Struct :
					case clang::TagDecl::TagKind::TTK_Union 	:
					case clang::TagDecl::TagKind::TTK_Class 	:
						{
							analyzeDeclContext(llvm::cast<clang::DeclContext>(decl), false);
						}
						break;
					case clang::TagDecl::TagKind::TTK_Enum 	:

						break;

					case clang::TagDecl::TagKind::TTK_Interface :
						// FIXME: do we need this??
						break;
				}
			}
			// === Namespace Decl ===
			else if (llvm::isa<clang::NamespaceDecl>(decl)){
				analyzeDeclContext(llvm::cast<clang::DeclContext>(decl), local);
			} 
			// === templDecl Decl ===
			else if(const clang::TemplateDecl* templDecl = llvm::dyn_cast<clang::TemplateDecl>(decl)) {
				analizeDecl(templDecl->getTemplatedDecl(), local);
			}
			// === variable Decl ===
			else if (const clang::VarDecl* varDecl = llvm::dyn_cast<clang::VarDecl>(decl)){

				// this a variable, might be global, static or even extern.
				// BUT it might be also a class declaration which makes use of globals inside
				if (varDecl->isThisDeclarationADefinition() == clang::VarDecl::Definition)
					collector.addVar(varDecl, local);

				const clang::Type* type = varDecl->getType().getTypePtr();
				if (const clang::RecordType* rec = llvm::dyn_cast<clang::RecordType>(type)){
					analyzeDeclContext(llvm::cast<clang::DeclContext>(rec->getDecl()), local);
				}
			}
		} 
		// === linkage spec DECL ====
		else if (llvm::isa<clang::LinkageSpecDecl>(decl)) {
			analyzeDeclContext(llvm::cast<clang::DeclContext>(decl), local);
		}
		// === default ====
		else {
			//default case -- if DeclContext, try to analyze it
			if(clang::DeclContext* declContext = llvm::dyn_cast<clang::DeclContext>(decl)) {
				analyzeDeclContext(declContext, local);
			}
		}
				
		// if it does not have a name, it is another artifact
	}

	void analyzeDeclContext(clang::DeclContext* ctx, bool local){
		clang::DeclContext::decl_iterator it = ctx->decls_begin();
		clang::DeclContext::decl_iterator end = ctx->decls_end();
		for (; it != end; it++){
			analizeDecl(llvm::cast<clang::Decl>(*it), local);
		}
	}
};

} // end anonymous namespace

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

namespace insieme {
namespace frontend {
namespace analysis {

//////////////////////////////////////////////////////////////////
//
void GlobalVarCollector::operator()(const TranslationUnitPtr& tu){
	
	VLOG(1) << " ************* analyze: " << tu->getFileName() << " for globals  ***************";
	const ClangCompiler& compiler = tu->getCompiler();

	clang::TranslationUnitDecl* tuDecl = compiler.getASTContext().getTranslationUnitDecl();
	assert(tuDecl && "AST has not being build");

	clang::DeclContext* ctx= clang::TranslationUnitDecl::castToDeclContext (tuDecl);
	assert(ctx && "AST has no decl context");

	GlobalsVisitor vis( *this);
	vis.analyzeDeclContext(ctx, false);
}

//////////////////////////////////////////////////////////////////
//
void GlobalVarCollector::addVar(const clang::VarDecl* var, bool local){

	if (!var->hasGlobalStorage())
		return;

	std::string name = var->getQualifiedNameAsString();
	// FIXME: how should this be??
//	if (!local) 	name = "global_"+name;
//	else			name = "static_"+name;
	if (local) 	name = "static_"+name;

	VarStorage st;
	if (local){
		assert(!var->hasExternalStorage());
		st = VS_STATIC;

		auto fit = staticNames.find(var);
		if (fit == staticNames.end()){
			name.append( std::to_string (staticCount++));
			staticNames[var] = name;
		}
		else
			name = fit->second;
	}
	else if (var->hasExternalStorage()){
		st = VS_EXTERN;
	}
	else {
		st = VS_GLOBAL;
	}

	VLOG(2) << " var: " << name << " in " << (local? "static" : "global");

	// if already exists a previous version, and is marked as extern update with non extern if
	// possible
	auto fit = globalsMap.find(name);
	if (fit != globalsMap.end()){

		// is a global storage or just another extern ref?
		if (!var->hasExternalStorage()){
			VLOG(2) << "update variable storage which was previously external";
			fit->second.second = st;
		}
		// if is the decl which contains the definition, we update the record
		if (var->getDefinition()){
			fit->second.first = var;
		}
	}
	else{
		// never existed? create new one

		tGlobalDecl gd = {var,st};
		globalsMap[name] = gd;
	}
}

//////////////////////////////////////////////////////////////////
//
bool GlobalVarCollector::isExtern (const clang::VarDecl* var){
	std::string&& name = getName(var);
	auto fit = globalsMap.find(name);
	if (fit != globalsMap.end()){
		return fit->second.second == VS_EXTERN;
	}
	return false;
}

//////////////////////////////////////////////////////////////////
//
bool GlobalVarCollector::isStatic (const clang::VarDecl* var){
	std::string&& name = getName(var);
	auto fit = globalsMap.find(name);
	if (fit != globalsMap.end()){
		return fit->second.second == VS_STATIC;
	}
	return false;
}


//////////////////////////////////////////////////////////////////
//
std::string GlobalVarCollector::getName (const clang::VarDecl* var){
	auto fit = staticNames.find(var);
	if (fit != staticNames.end())
		return fit->second;
	else
	{
		// append global_ before the name, but after the qualification of 
		// namespaces
		std::string qualName = var->getQualifiedNameAsString();
		std::string name     = var->getNameAsString();
		std::string newName  = "global_"+name;

		qualName.replace( qualName.end()-name.size(), qualName.end(), newName);
		return qualName;
	}
}


//////////////////////////////////////////////////////////////////
//
void GlobalVarCollector::dump(){
	VLOG(2) << " === Globals: === ";
	VLOG(2) << join("\n" ,globalsMap);
	VLOG(2) << " === Statics: === ";
	VLOG(2) << join("\n" ,staticNames);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
//tGlobalDecl::iterator curr;
//const tInitMap& initMap;

const std::string&    GlobalVarCollector::init_it::name() const{
	return curr->first;
}
const clang::VarDecl* GlobalVarCollector::init_it::decl() const{
	return curr->second.first;
}
const clang::Expr*    GlobalVarCollector::init_it::init() const{
	if (const clang::VarDecl* definition = curr->second.first->getDefinition()){
		return definition->getInit();
	}
	else return nullptr;
}
const clang::Type*    GlobalVarCollector::init_it::type() const{
	return  curr->second.first->getType().getTypePtr();
//	if (const clang::VarDecl* definition = curr->second.first->getDefinition()){
//		return definition->getType().getTypePtr();
//	}
//	else return nullptr;
}

GlobalVarCollector::init_it GlobalVarCollector::init_it::operator++() {
	++curr;
	return *this;
}

GlobalVarCollector::init_it GlobalVarCollector::init_it::operator++(int) {
	++curr;
	return *this;
}

bool GlobalVarCollector::init_it::operator!=(const init_it& o) const {
	return curr != o.curr;
}


} // end analysis namespace
} // end frontend namespace
} // end insieme namespace
