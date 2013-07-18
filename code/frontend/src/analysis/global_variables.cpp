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
#include "clang/AST/DeclTemplate.h"
#pragma GCC diagnostic pop

#include "insieme/frontend/analysis/global_variables.h"
#include "insieme/frontend/utils/indexer.h"

#include <boost/algorithm/string/replace.hpp>

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

using namespace insieme::frontend;


namespace {

bool isIncompleteTemplate(const clang::VarDecl* var){
	const clang::Type* ty= var->getType().getTypePtr();
	return llvm::isa<clang::TemplateTypeParmType>(ty);
}

/**~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * append global_ before the name, but after the qualification of 
 * namespaces
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
std::string buildGlobalName(const clang::VarDecl* var, const std::string& storage){
		std::string qualName = var->getQualifiedNameAsString();
		std::string name     = var->getNameAsString();
		std::string newName  = storage+name;
		boost::replace_all(qualName, "::", "__");
		boost::replace_all(qualName, "<", "_");
		boost::replace_all(qualName, ">", "_");
		qualName.replace( qualName.end()-name.size(), qualName.end(), newName);
		return qualName;
}

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

	void analizeDecl(const clang::Decl* decl, bool inFunc){

		// === named DECL ====
		// this might be anything with a name
		if (const clang::NamedDecl* nm = llvm::dyn_cast<clang::NamedDecl>(decl)) {
			//std::cout << nm->getNameAsString() << std::endl;
			// === Function Decl ===
			if (const clang::FunctionDecl* fdecl = llvm::dyn_cast<clang::FunctionDecl>(nm)) {
				if (fdecl->hasBody() && !collector.getInterceptor().isIntercepted(fdecl)){
					analyzeDeclContext(llvm::cast<clang::DeclContext>(fdecl));
				}
			}
			// === tag Decl ===
			else if (const clang::TagDecl* tag = llvm::dyn_cast<clang::TagDecl>(decl)) {

				switch (tag->getTagKind ()){
					case clang::TagDecl::TagKind::TTK_Struct :
					case clang::TagDecl::TagKind::TTK_Union 	:
					case clang::TagDecl::TagKind::TTK_Class 	:
						{
							analyzeDeclContext(llvm::cast<clang::DeclContext>(decl));
						}
						break;
					case clang::TagDecl::TagKind::TTK_Enum 	:
					case clang::TagDecl::TagKind::TTK_Interface :
						break;
				}
			}
			// === Namespace Decl ===
			else if (const clang::NamespaceDecl* namespaceDecl = llvm::dyn_cast<clang::NamespaceDecl>(decl)){
				if(!collector.getInterceptor().isIntercepted(namespaceDecl->getQualifiedNameAsString())){
					analyzeDeclContext(llvm::cast<clang::DeclContext>(decl));
				}
			} 
		//	// === templDecl Decl ===
		//	else if(const clang::TemplateDecl* templDecl = llvm::dyn_cast<clang::TemplateDecl>(decl)) {
		//		analizeDecl(templDecl->getTemplatedDecl(), false);
		//	}
			// === variable Decl ===
			else if (const clang::VarDecl* varDecl = llvm::dyn_cast<clang::VarDecl>(decl)){
				// this a variable, might be global, static or even extern.
				collector.addVar(varDecl, inFunc);

				// BUT it might be also a class declaration which makes use of globals inside
				const clang::Type* type = varDecl->getType().getTypePtr();
				if (const clang::RecordType* rec = llvm::dyn_cast<clang::RecordType>(type)){
					analyzeDeclContext(llvm::cast<clang::DeclContext>(rec->getDecl()));
				}
			}
		} 
		// === linkage spec DECL ====
		else if (llvm::isa<clang::LinkageSpecDecl>(decl)) {
			analyzeDeclContext(llvm::cast<clang::DeclContext>(decl));
		}
		// === default ====
		else {
			//default case -- if DeclContext, try to analyze it
			if(const clang::DeclContext* declContext = llvm::dyn_cast<clang::DeclContext>(decl)) {
				analyzeDeclContext(declContext);
			}
		}
				
		// if it does not have a name, it is another artifact
	}

	void analyzeDeclContext(const clang::DeclContext* ctx){
		clang::DeclContext::decl_iterator it = ctx->decls_begin();
		clang::DeclContext::decl_iterator end = ctx->decls_end();
		for (; it != end; it++){
			analizeDecl(llvm::cast<clang::Decl>(*it), llvm::isa<clang::FunctionDecl>(ctx));
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
void GlobalVarCollector::operator()(const TranslationUnit& tu){
	
	VLOG(1) << " ************* analyze: " << tu.getFileName() << " for globals  ***************";
	const ClangCompiler& compiler = tu.getCompiler();

	clang::TranslationUnitDecl* tuDecl = compiler.getASTContext().getTranslationUnitDecl();
	assert(tuDecl && "AST has not being build");

	clang::DeclContext* ctx= clang::TranslationUnitDecl::castToDeclContext (tuDecl);
	assert(ctx && "AST has no decl context");

	(*this)(ctx);
}

void GlobalVarCollector::operator()(const clang::DeclContext* ctx){
	GlobalsVisitor vis( *this);
	vis.analyzeDeclContext(ctx);
}

//////////////////////////////////////////////////////////////////
//
void GlobalVarCollector::addVar(const clang::VarDecl* var, bool inFunc){

		//FIXME:: idenfify scope and visibility with clang
		
//	std::cout << "***************************************************************" << std::endl;
//	std::cout << "collectiong: " << var->getQualifiedNameAsString() << std::endl;
//	std::cout << "******************" << std::endl;
//	std::cout << "   isStaticLocal () " << var->isStaticLocal() << std::endl;
//	std::cout << "   hasExternalStorage () " 		<< var->hasExternalStorage () << std::endl;
//	std::cout << "   hasGlobalStorage () " 		<< var->hasGlobalStorage () << std::endl;
//	std::cout << "   isExternC () " 				<< var->isExternC () << std::endl;
//	std::cout << "   isLocalVarDecl () " 			<< var->isLocalVarDecl () << std::endl		;
//	std::cout << "   isFunctionOrMethodVarDecl () "<< var->isFunctionOrMethodVarDecl () << std::endl;
//	std::cout << "   isStaticDataMember () " 		<< var->isStaticDataMember () << std::endl ; 
	
	if (!var->hasGlobalStorage())
		return;

	if (isIncompleteTemplate(var))
		return;


	std::string name;
	VarStorage st;
	// not enough with var->isStaticLocal(), this will be true as well for class static members
	if (inFunc){
		name = buildGlobalName(var, "static_");
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
	else {
		if (var->isStaticDataMember())	name = buildGlobalName(var, "staticMem_");
		else  						  	name = buildGlobalName(var, "global_");

		if (var->hasExternalStorage()) st = VS_EXTERN;
		else st = VS_GLOBAL;
	}

	VLOG(2) << " var: " << name << " \t\t\t storage:" << st;

	// it might be a spetialization, initialization will be in the original template
	if (var->isStaticDataMember()){
		const clang::VarDecl* spetialization = var->getInstantiatedFromStaticDataMember ();
		if (spetialization  && spetialization->hasInit()){
			globalInitializations[name] = spetialization;
		}
	}

	// store in the ordered list for future initialization
	if (var->hasDefinition() && var->hasInit()){
		if (st == VS_STATIC) 	staticInitializations.push_back(var);
		else globalInitializations[name] = var;
	}

	// if already exists a previous version, and is marked as extern update with non extern if
	// possible
	auto fit = globalsMap.find(name);
	if (fit != globalsMap.end()){

		// is a global storage or just another extern ref?
		if (!var->hasExternalStorage()){
			VLOG(2) << "update variable storage which was previously external";
			fit->second = st;
		}
	}
	else{
		// never existed? create new one
		globalsMap[name] = st;
	}
}

//////////////////////////////////////////////////////////////////
//
bool GlobalVarCollector::isExtern (const clang::VarDecl* var){
	std::string&& name = getName(var);
	auto fit = globalsMap.find(name);
	if (fit != globalsMap.end()){
		return fit->second == VS_EXTERN;
	}
	return false;
}

//////////////////////////////////////////////////////////////////
//
bool GlobalVarCollector::isStatic (const clang::VarDecl* var){
	std::string&& name = getName(var);
	auto fit = globalsMap.find(name);
	if (fit != globalsMap.end()){
		return fit->second == VS_STATIC;
	}
	return false;
}


//////////////////////////////////////////////////////////////////
//
std::string GlobalVarCollector::getName (const clang::VarDecl* var){
	std::string name;
	auto fit = staticNames.find(var);
	if (fit != staticNames.end())
		name =  fit->second;
	else{
		if (var->isStaticDataMember())	name = buildGlobalName(var, "staticMem_");
		else  						  	name = buildGlobalName(var, "global_");
	}
	return name;
}


//////////////////////////////////////////////////////////////////
//
void GlobalVarCollector::dump(){
	std::cout << " === Globals: === " << std::endl;
	std::cout << join("\n" ,globalsMap) << std::endl;
	std::cout << " === Statics: === " << std::endl;
	std::cout << join("\n" ,staticNames) << std::endl;
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
//tGlobalDecl::iterator curr;
//const tInitMap& initMap;

const std::string&    GlobalVarCollector::init_it::name() const{
	return curr->first;
}

const GlobalVarCollector::VarStorage      GlobalVarCollector::init_it::storage() const{
	return curr->second;
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

////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////
std::ostream& operator<< (std::ostream& out, const GlobalVarCollector::VarStorage storage){
	switch (storage){
		case GlobalVarCollector::VS_GLOBAL:
			return out << "VS_GLOBAL";
		case GlobalVarCollector::VS_STATIC: 
			return out << "VS_STATIC";
		case GlobalVarCollector::VS_EXTERN:
			return out << "VS_EXTERN";
	}
	return out << "unkown type";
}

} // end analysis namespace
} // end frontend namespace
} // end insieme namespace
