/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/extensions/frontend_extension.h"
#include "insieme/core/checks/full_check.h"
#include <map>
#include <set>
#include <fstream>
#include <string>
//#include <utility>

using namespace insieme;

namespace insieme {
namespace frontend {
namespace extensions {

	class SemanticCheckExtension : public FrontendExtension {
		// I know that this is done super nicely
		// std::map<int, const core::NodePtr> irnodelist;
		std::set<std::string> handledErrors;
		// std::unordered_map<insieme::core::checks::Message, std::pair<const clang::Expr*, insieme::core::ExpressionPtr>> exprErrors;
		// std::unordered_map<insieme::core::checks::Message, std::pair<const clang::Type*, insieme::core::TypePtr>> typeErrors;
		int current;
		int eE;
		int tE;
		insieme::core::NodePtr node;


	  public:
		SemanticCheckExtension() : current(0), eE(0), tE(0) {}

		virtual insieme::core::ExpressionPtr PostVisit(const clang::Expr* expr, const insieme::core::ExpressionPtr& irExpr,
		                                               insieme::frontend::conversion::Converter& converter) {
			if(irExpr && current < 125000) {
				auto msg = insieme::core::checks::check(irExpr);
				if(!msg.empty()) {
					for(auto m : msg.getErrors()) {
						std::stringstream s;
						s << m.getMessage() << m.getLocation() << m.getErrorCode();
						if(handledErrors.find(s.str()) == handledErrors.end()
						   && m.getMessage().find("Unresolved \"this\" literal found, check translation process") == std::string::npos) {
							std::cerr << "\n####EXPR SEM. ERROR####\n" << m << "\n\n";
							expr->dump();
							std::cerr << "\n\n";
							std::cerr << toString(*irExpr);
							std::cerr << "\n\n";
							std::cerr << toString(*m.getOrigin());
							handledErrors.insert(s.str());
							eE++;
						}
					}
				}
			}

			if(current % 5000 == 1) {
				std::cout << "Status: " << current << " clang nodes." << std::endl;
				std::cout << "type Errors: " << tE << std::endl;
				std::cout << "expr Errors: " << eE << std::endl;
			}
			current++;
			return irExpr;
		}


		virtual insieme::core::TypePtr PostVisit(const clang::QualType& type, const insieme::core::TypePtr& irType,
		                                         insieme::frontend::conversion::Converter& converter) {
			if(irType && current < 125000) {
				auto msg = insieme::core::checks::check(irType);
				if(!msg.empty()) {
					for(auto m : msg.getErrors()) {
						std::stringstream s;
						s << m.getMessage() << m.getLocation() << m.getErrorCode();
						if(handledErrors.find(s.str()) == handledErrors.end()) {
							std::cerr << "\n####TYPE SEM. ERROR####\n" << m << "\n\n";
							type.getTypePtr()->dump();
							std::cerr << "\n\n";
							std::cerr << toString(*irType);
							std::cerr << "\n\n";
							std::cerr << toString(*m.getOrigin());
							handledErrors.insert(s.str());
							tE++;
						}
					}
				}
			}

			if(current % 5000 == 1) {
				std::cout << "Status: " << current << " clang nodes." << std::endl;
				std::cout << "type Errors: " << tE << std::endl;
				std::cout << "expr Errors: " << eE << std::endl;
			}
			current++;
			return irType;
		}


		virtual stmtutils::StmtWrapper PostVisit(const clang::Stmt* stmt, const stmtutils::StmtWrapper& irStmt,
		                                         insieme::frontend::conversion::Converter& converter) {
			/*   if(irStmt.isSingleStmt() && current < 100000) {
			       if(irStmt.getSingleStmt()) {
			           auto msg = insieme::core::checks::check(irStmt.getSingleStmt());
			           if(!msg.empty()) {
			               clangstmtlist[pos] = stmt;
			               //irnodelist[pos] = p;
			               curStmt++;
			               msgs[pos] = msg;
			               pos++;
			           }
			       }
			   }
			   current++;
			   if(current%5000==1) {
			       std::cout << "Status: " << pos << " errors in "<< current << " clang nodes." << std::endl;
			       std::cout << "Type sem. errors " << curType << std::endl;
			       std::cout << "Expr sem. errors " << curExpr << std::endl;
			       std::cout << "Stmt sem. errors " << curStmt << std::endl;
			   }*/
			current++;
			return irStmt;
		}

		virtual insieme::core::ExpressionPtr FuncDeclPostVisit(const clang::FunctionDecl* decl, insieme::core::ExpressionPtr expr,
		                                                       insieme::frontend::conversion::Converter& converter, bool symbolic = false) {
			std::cout << "func decl: " << decl->getNameAsString() << std::endl;
			if(decl->getSourceRange().isValid()) {
				std::cout << decl->getSourceRange().getBegin().printToString(converter.getCompiler().getSourceManager())
				          << decl->getSourceRange().getEnd().printToString(converter.getCompiler().getSourceManager()) << std::endl;
			}
			return expr;
		}

		virtual insieme::frontend::tu::IRTranslationUnit IRVisit(insieme::frontend::tu::IRTranslationUnit& tu) {
			std::cout << "##############STRICT SEMANTIC CHECK##############\n";
			handledErrors.clear();
			std::map<core::checks::ErrorCode, int> errorCount;
			int i = 0;
			int n = 0;
			// std::ofstream file3;
			// file3.open("realErrors.txt");
			for(auto cur : tu.getFunctions()) {
				std::stringstream s;
				s << cur.first.getValue();
				std::cout << "function " << ++i << " of " << tu.getFunctions().size() << std::endl << s.str() << std::endl;

				auto msg = core::checks::check(cur.second);
				n += msg.getErrors().size();
				// dumpPretty(cur.second);
				for(auto m : msg.getErrors()) {
					std::stringstream f;
					f << m.getMessage() << m.getLocation() << m.getErrorCode();
					if(handledErrors.find(f.str()) == handledErrors.end()) {
						std::cout << "#####\n" << m << std::endl;
						std::cout << toString(*m.getOrigin());
						handledErrors.insert(f.str());
					}
					// std::cerr << "#####\n" << m << std::endl;
					// std::cerr << m.getOrigin();
					// std::cerr << "\n\n";
					errorCount[m.getErrorCode()]++;
					// std::cout << *insieme::core::annotations::getLocation(m.getOrigin()) << std::endl;
				}

				/*   visitDepthFirstOnce(cur.second, [](const core::NodePtr& currentNode) {
				       if(currentNode.isa<core::ExpressionPtr>() || currentNode.isa<core::TypePtr>()) {
				           auto msg = core::checks::check(currentNode);
				           if(!msg.empty()) {
				               if(currentNode->hasAttachedValue<const clang::Type*>()) {
				                   std::cout << "############SEMANTIC ERROR############\n";
				                   std::cout << msg << std::endl << "+++++++++++++\n";
				                   dumpPretty(currentNode);
				                   auto clangType = currentNode->getAttachedValue<const clang::Type*>();
				                   clangType->dump();
				               }
				               if(currentNode->hasAttachedValue<const clang::Expr*>()) {
				                   auto clangExpr = currentNode->getAttachedValue<const clang::Expr*>();
				                   if(clangExpr->getStmtClass() != 205) {
				                       std::cout << "############SEMANTIC ERROR############\n";
				                       std::cout << msg << std::endl << "+++++++++++++\n";
				                       dumpPretty(currentNode);
				                       clangExpr->dump();
				                   }
				               }
				           }
				       }
				   });*/
			}
			/*		    n++;
			            std::cout << "function " << n << " from " << tu.getFunctions().size() << std::endl;
			            auto msg = core::checks::check(cur.second);
			            if (!msg.empty()){
			                for(auto s : msg.getErrors()) {
			                    //file3 << s.getMessage() << "\n" << s.getAddress() << "\n";
			                    for (std::unordered_map<insieme::core::checks::Message, std::pair<const clang::Expr*, insieme::core::ExpressionPtr>>::iterator
			   it=exprErrors.begin(); it!=exprErrors.end(); ++it) {
			                        if(it->first.getMessage().compare(s.getMessage())==0) {
			                            std::cout << "#########################" << std::endl;
			                            std::cout << "found real expr error...\n";
			                            std::cout << s << std::endl;
			                            std::cout << "#####" << std::endl;
			                            try {
			                                if(it->second.first->getStmtClass() != 205)
			                                    it->second.first->dump();
			                            } catch(int n) {
			                                std::cout << "clang node is not printable \n";
			                            }
			                            std::cout << "#####" << std::endl;
			                            dumpPretty(it->second.second);
			                            std::cout << "#####" << std::endl;
			                            exprErrors.erase(it++);
			                            i++;
			                        }
			                    }
			                    for (std::unordered_map<insieme::core::checks::Message, std::pair<const clang::Type*, insieme::core::TypePtr>>::iterator
			   it=typeErrors.begin(); it!=typeErrors.end(); ++it) {
			                        if(it->first.getMessage().compare(s.getMessage())==0) {
			                            std::cout << "#########################" << std::endl;
			                            std::cout << "found real type error...\n";
			                            std::cout << s << std::endl;
			                            std::cout << "#####" << std::endl;
			                            try {
			                                it->second.first->dump();
			                            } catch (int n) {
			                                std::cout << "clang node is not printable \n";
			                            }
			                            std::cout << "#####" << std::endl;
			                            dumpPretty(it->second.second);
			                            std::cout << "#####" << std::endl;
			                            typeErrors.erase(it++);
			                            i++;
			                        }
			                    }
			                }
			            }
			        }
			        //file3.close();
			*/
			std::cout << "=================SEMANTIC CHECK RESULTS================\n";
			std::cout << "Found " << n << " matched semantic errors \n"; //<< (typeErrors.size()+exprErrors.size()) <<" errors.\n";
			std::cout << "=======================================================\n";
			std::cout << errorCount << "\n";
			return tu;
		}

		virtual insieme::core::ProgramPtr IRVisit(insieme::core::ProgramPtr& prog) {
			// std::cout << "############ SEMANTIC CHECKS AGAIN ############\n";
			// auto msg = insieme::core::checks::check(prog);
			// if(!msg.empty()) {
			//     std::cout << msg.getErrors().size() << " errors found.\n";
			// }
			return prog;
		}
	};

} // extensions
} // frontend
} // insieme
