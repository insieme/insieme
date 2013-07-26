
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


namespace insieme{
namespace frontend{
namespace analysis{


/** 
 *		This is our own implementation of the a clang::Decl tree for the insieme compiler,
 *		it is a prunable visitor, which means that we can prune branches but continue the 
 *		traversal in other. This fucntionality was not found on clang 3.2
 *
 * we need to do spetial enfasis on:
 * 	for types definition	
 * 		- TypeDefs
 * 		- Classes
 *	for globas collection
 *		- VarDecl
 *	for function translation
 *		- FunctionDecl
 *	in this version only those objects will be consider
 *
 *	NOTES:
 *		- non named declarations will be completelly ignored
 *		- 
 */
template <typename BASE, bool visitTemplates=false>
class PrunableDeclVisitor{

	/**
	 * Default implementation, overide to add functionality
	 */
	void VisitFunctionDecl(const clang::FunctionDecl* funcDecl) {
	}
	/**
	 * Default implementation, overide to add functionality
	 */
	void VisitRecordDecl(const clang::RecordDecl* typeDecl) {
	}
	/**
	 * Default implementation, overide to add functionality
	 */
	void VisitTypedefDecl(const clang::TypedefDecl* typeDecl) {
	}
	/**
	 * Default implementation, overide to add functionality
	 */
	void VisitVarDecl(const clang::VarDecl* var) {
	}


	/**
	 */
	void dispatchDecl(const clang::Decl* decl){
		//std::cout << "disp: " << decl->getDeclKindName() << std::endl;
		switch (decl->getKind()){
			case clang::Decl::Namespace:
				{
					traverseDeclCtx (llvm::cast<clang::DeclContext>(decl));
					break;
				}
			case clang::Decl::Record:
				{
					static_cast<BASE*>(this)->VisitRecordDecl(llvm::cast<clang::RecordDecl>(decl));
					traverseDeclCtx (llvm::cast<clang::DeclContext>(decl));

					break;
				}
			case clang::Decl::CXXRecord:
				{
					if(llvm::cast<clang::TagDecl>(decl)->isDependentType() && !visitTemplates) break;
					//if (decl->isTemplateDecl ()){ assert(false && " temmplate"); };
					//if (llvm::cast<clang::CXXRecordDecl>(decl)->hasDefinition ()){ assert(false && " dependent base"); };
					//if (llvm::cast<clang::CXXRecordDecl>(decl)->hasAnyDependentBases ()){ assert(false && " dependent base"); };
					//if (llvm::isa<clang::ClassTemplateDecl>(decl)){ assert(false); };
					//if (llvm::isa<clang::ClassTemplatePartialSpecializationDecl>(decl)) assert(false && "partial");
					//if (llvm::isa<clang::RedeclarableTemplateDecl>(decl)) assert(false && "redecl templ");
					//if (llvm::cast<clang::CXXRecordDecl>(decl)->getDescribedClassTemplate ()) assert(false && " describes template");
					//if (llvm::cast<clang::CXXRecordDecl>(decl)->getInstantiatedFromMemberClass ()) assert(false && " instantiated");
					static_cast<BASE*>(this)->VisitRecordDecl(llvm::cast<clang::RecordDecl>(decl));
					traverseDeclCtx (llvm::cast<clang::DeclContext>(decl));

					break;
				}
			case clang::Decl::Var:
				{
					if (llvm::cast<clang::VarDecl>(decl)->getType().getTypePtr()->isDependentType() && !visitTemplates) break;
					static_cast<BASE*>(this)->VisitVarDecl(llvm::cast<clang::VarDecl>(decl));
					break;
				}
			case clang::Decl::CXXDestructor:
			case clang::Decl::CXXConstructor:
			case clang::Decl::CXXMethod:
				{
				if (llvm::cast<clang::DeclContext>(decl)->isDependentContext()) break;
				}
			case clang::Decl::Function:
				{
					static_cast<BASE*>(this)->VisitFunctionDecl(llvm::cast<clang::FunctionDecl>(decl));
					traverseDeclCtx (llvm::cast<clang::DeclContext>(decl));
					break;
				}
			case clang::Decl::Typedef:
				{
					if(llvm::isa<clang::TemplateTypeParmType>(llvm::cast<clang::TypedefDecl>(decl)->getUnderlyingType().getTypePtr())) break;
					static_cast<BASE*>(this)->VisitTypedefDecl(llvm::cast<clang::TypedefDecl>(decl));
					break;
				}
			default:
				//std::cout << "disp: " << decl->getDeclKindName() << std::endl;
				return;
		}
	}

public:

	/**
	 * entry point, we allways want to explore a declaration context.
	 */
	void traverseDeclCtx (const clang::DeclContext* declCtx){
		// iterate throw the declarations inside and dispattch 
		clang::DeclContext::decl_iterator it = declCtx->decls_begin();
		clang::DeclContext::decl_iterator end = declCtx->decls_end();
		for (; it!=end; ++it){
			dispatchDecl(*it);
		}
	}
};



} //namespace analysis
} // namespace frontend
} // namespace insime
