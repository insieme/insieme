/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

namespace insieme {
namespace frontend {
namespace analysis {


	/**
	 *		This is our own implementation of the a clang::Decl tree for the insieme compiler,
	 *		it is a prunable visitor, which means that we can prune branches but continue the
	 *		traversal in other. This functionality was not found on clang 3.2
	 *
	 * we need to do special emphasis on:
	 * 	for types definition
	 * 		- TypeDefs
	 * 		- Classes
	 *	for globals collection
	 *		- VarDecl
	 *	for function translation
	 *		- FunctionDecl
	 *	in this version only those objects will be consider
	 *
	 *	NOTES:
	 *		- non named declarations will be completely ignored
	 *		-
	 */
	template <typename BASE, bool visitTemplates = false>
	class PrunableDeclVisitor {
		/**
		 * Default implementation, override to add functionality
		 */
		void VisitFunctionDecl(const clang::FunctionDecl* funcDecl) {}
		/**
		 * Default implementation, override to add functionality
		 */
		void VisitRecordDecl(const clang::RecordDecl* typeDecl) {}
		/**
		 * Default implementation, override to add functionality
		 */
		void VisitTypedefNameDecl(const clang::TypedefNameDecl* typeDecl) {}
		/**
		 * Default implementation, override to add functionality
		 */
		void VisitVarDecl(const clang::VarDecl* var) {}

		/**
		 * class template
		 */
		void VisitClassTemplate(const clang::ClassTemplateDecl* templ) {}

		/**
		 * a function template
		 */
		void VisitFunctionTemplate(const clang::FunctionTemplateDecl* templ) {}

		/**
		 * a linkage specification:
		 * this might be needed for c++ codes to preserve the spetial linkage of
		 * c function declarations
		 *
		 *   extern "C" {
		 *       void function();
		 *   }
		 */
		void VisitLinkageSpec(const clang::LinkageSpecDecl* link) {}

		/**
		 * for debug purposes
		 */
		void echocallback(const clang::Decl* decl) {
			//		std::cout << " ==================  " << decl->getDeclKindName() << " ====================== " <<std::endl;
			//		decl->dump();
			//		std::cout << " =========================================================== " << std::endl;
		}

		/**
		 */
		void dispatchDecl(const clang::Decl* decl) {
			static_cast<BASE*>(this)->echocallback(decl);

			switch(decl->getKind()) {
			case clang::Decl::Namespace: {
				traverseDeclCtx(llvm::cast<clang::DeclContext>(decl));
				break;
			}
			case clang::Decl::NamespaceAlias: { // clang 3.4
				dispatchDecl(llvm::cast<clang::NamespaceAliasDecl>(decl)->getNamespace());
				break;
			}
			case clang::Decl::Record: {
				if(llvm::cast<clang::TagDecl>(decl)->isDependentType() && !visitTemplates) { break; }
				static_cast<BASE*>(this)->VisitRecordDecl(llvm::cast<clang::RecordDecl>(decl));
				traverseDeclCtx(llvm::cast<clang::DeclContext>(decl));
				break;
			}
			case clang::Decl::CXXRecord: {
				if(llvm::cast<clang::TagDecl>(decl)->isDependentType() && !visitTemplates) { break; }
				static_cast<BASE*>(this)->VisitRecordDecl(llvm::cast<clang::RecordDecl>(decl));
				traverseDeclCtx(llvm::cast<clang::DeclContext>(decl));
				break;
			}
			case clang::Decl::Var: {
				const clang::VarDecl* var = llvm::cast<clang::VarDecl>(decl);
				if(var->isStaticDataMember() && var->getInstantiatedFromStaticDataMember()) { var = var->getInstantiatedFromStaticDataMember(); }
				if(var->getType().getTypePtr()->isDependentType() || (var->hasInit() && var->getInit()->getType()->isDependentType())) {
					if(!visitTemplates) { break; }
				}
				static_cast<BASE*>(this)->VisitVarDecl(llvm::cast<clang::VarDecl>(decl));
				break;
			}
			case clang::Decl::CXXDestructor:
			case clang::Decl::CXXConstructor:
			case clang::Decl::CXXMethod:
			case clang::Decl::CXXConversion: {
				if(llvm::cast<clang::DeclContext>(decl)->isDependentContext()) { break; }
			}
			case clang::Decl::Function: {
				const clang::FunctionDecl* funcDecl = llvm::cast<clang::FunctionDecl>(decl);
				static_cast<BASE*>(this)->VisitFunctionDecl(funcDecl);
				traverseDeclCtx(llvm::cast<clang::DeclContext>(decl));
				break;
			}
			case clang::Decl::Typedef:
			case clang::Decl::TypeAlias: {
				if(llvm::isa<clang::TemplateTypeParmType>(llvm::cast<clang::TypedefNameDecl>(decl)->getUnderlyingType().getTypePtr())) { break; }
				static_cast<BASE*>(this)->VisitTypedefNameDecl(llvm::cast<clang::TypedefNameDecl>(decl));
				break;
			}
			case clang::Decl::LinkageSpec: {
				static_cast<BASE*>(this)->VisitLinkageSpec(llvm::cast<clang::LinkageSpecDecl>(decl));
				break;
			}
			case clang::Decl::ClassTemplate: {
				const clang::ClassTemplateDecl* classTmplDecl = llvm::cast<clang::ClassTemplateDecl>(decl);
				if(visitTemplates) { static_cast<BASE*>(this)->VisitClassTemplate(classTmplDecl); }
				auto spec_it = classTmplDecl->spec_begin();
				auto spec_end = classTmplDecl->spec_end();
				for(; spec_it != spec_end; ++spec_it) {
					if(!spec_it->isDependentType()) { dispatchDecl(*spec_it); }
				}
				break;
			}
			case clang::Decl::ClassTemplateSpecialization: {
				if(llvm::isa<clang::ClassTemplatePartialSpecializationDecl>(decl)) { break; }

				traverseDeclCtx(llvm::cast<clang::DeclContext>(decl));
				static_cast<BASE*>(this)->VisitRecordDecl(llvm::cast<clang::RecordDecl>(decl));
				break;
			}
			case clang::Decl::FunctionTemplate: {
				const clang::FunctionTemplateDecl* funcTmplDecl = llvm::cast<clang::FunctionTemplateDecl>(decl);
				if(visitTemplates) { static_cast<BASE*>(this)->VisitFunctionTemplate(funcTmplDecl); }
				auto spec_it =  funcTmplDecl->spec_begin();
				auto spec_end = funcTmplDecl->spec_end();
				for(; spec_it != spec_end; ++spec_it) {
					dispatchDecl(*spec_it);
				}
				break;
			}

			case clang::Decl::Using:
			case clang::Decl::UsingDirective:
			case clang::Decl::UsingShadow:
			case clang::Decl::AccessSpec:
			case clang::Decl::ParmVar:
			case clang::Decl::ClassTemplatePartialSpecialization:
			case clang::Decl::Enum:
			case clang::Decl::Field:
			case clang::Decl::IndirectField:
			case clang::Decl::Friend:
			case clang::Decl::Label:             // clang 3.4
			case clang::Decl::Empty:             // clang 3.4
			case clang::Decl::TypeAliasTemplate: // clang 3.4   // TODO: check if needed
			case clang::Decl::StaticAssert:      // TODO: C++11 ->this needs to be handled in a declstmt, to produce a call
				break;

			default:
				std::cout << " ==================  default: " << decl->getDeclKindName() << " ====================== " << std::endl;
				assert_not_implemented();
				return;
			}
		}

	  public:
		/**
		 * entry point, we allways want to explore a declaration context.
		 */
		void traverseDeclCtx(const clang::DeclContext* declCtx) {
			// iterate throw the declarations inside and dispattch
			clang::DeclContext::decl_iterator it = declCtx->decls_begin();
			clang::DeclContext::decl_iterator end = declCtx->decls_end();
			for(; it != end; ++it) {
				dispatchDecl(*it);
			}
		}
	};


} // namespace analysis
} // namespace frontend
} // namespace insime
