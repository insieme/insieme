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

#include <gtest/gtest.h>

//#include "program.h"

#include "clang_compiler.h"
#include "pragma_handler.h"
//#include "utils/source_locations.h"
#include "clang_config.h"
#include "clang/AST/ASTContext.h"

//#include <iostream>
#include <clang/AST/Decl.h>
#include <clang/AST/Stmt.h>
#include <clang/AST/Attr.h>
#include <clang/Sema/AttributeList.h>

using namespace insieme::frontend;
using namespace insieme::core;

#define CHECK_LOCATION(loc, srcMgr, line, col) \
    EXPECT_EQ(util::Line(loc, srcMgr), (size_t)line); \
    EXPECT_EQ(util::Column(loc, srcMgr), (size_t)col);

enum AddressSpace {
        PRIVATE,
        LOCAL,
        GLOBAL,
        CONSTANT
};

const char* addressSpaceStr[] = {
        "private",
        "local",
        "global",
        "constant"
};

const char* kindStr[] = {
  "Alias",
  "Aligned",
  "AlwaysInline",
  "AnalyzerNoReturn", // Clang-specific.
  "Annotate",
  "AsmLabel", // Represent GCC asm label extension.
  "BaseCheck",
  "Blocks",
  "CDecl",
  "Cleanup",
  "Const",
  "Constructor",
  "Deprecated",
  "Destructor",
  "FastCall",
  "Final",
  "Format",
  "FormatArg",
  "GNUInline",
  "Hiding",
  "IBOutletKind", // Clang-specific. Use "Kind" suffix to not conflict w/ macro.
  "IBActionKind", // Clang-specific. Use "Kind" suffix to not conflict w/ macro.
  "Malloc",
  "NoDebug",
  "NoInline",
  "NonNull",
  "NoReturn",
  "NoThrow",
  "ObjCException",
  "ObjCNSObject",
  "Override",
  "CFReturnsRetained",      // Clang/Checker-specific.
  "CFReturnsNotRetained",   // Clang/Checker-specific.
  "NSReturnsRetained",      // Clang/Checker-specific.
  "NSReturnsNotRetained",   // Clang/Checker-specific.
  "Overloadable", // Clang-specific
  "Packed",
  "PragmaPack",
  "Pure",
  "Regparm",
  "ReqdWorkGroupSize",   // OpenCL-specific
  "Section",
  "Sentinel",
  "StdCall",
  "TransparentUnion",
  "Unavailable",
  "Unused",
  "Used",
  "Visibility",
  "WarnUnusedResult",
  "Weak",
  "WeakImport",
  "WeakRef"
};

//attribute specific handlers to get attribute parameters
using namespace clang;
const Attr* parseAttribute(const Attr* attr)
{
    switch(attr->getKind()){
    case attr::Kind::Annotate:
    {
        const clang::AnnotateAttr* aa = (const AnnotateAttr*)attr;
        llvm::StringRef sr = aa->getAnnotation();
        std::cout << "annotation: " << sr.str() << std::endl;
        return aa;
    }
    case attr::Kind::ReqdWorkGroupSize:
    {
        const ReqdWorkGroupSizeAttr* rwgsa = (const ReqdWorkGroupSizeAttr*)attr;
        std::cout << "ReqdWorkGroupSize: "  << rwgsa->getXDim() << rwgsa->getYDim() << rwgsa->getZDim() << std::endl;
        return rwgsa;
    }
    case attr::Kind::Packed:
    {
        std::cout << "Packed" << std::endl;
        return (const PackedAttr*)attr;
    }
    case attr::Kind::Aligned:
    {
        const AlignedAttr* aa = (const AlignedAttr*)attr;
        // std::cout << "Aligned: "  << aa->getAlignment() << std::endl;
        return aa;
    }
    default:
    {
        std::cout << "Not implemented: " << kindStr[attr->getKind()] << std::endl;
        return attr;
    }
    }
}

void checkForAttrs(clang::Decl* decl)
{
    if(decl->hasAttrs())
    {
        decl->dump();
        fprintf(stderr, " has an attribute\n");
//        parseAttribute(decl->getAttrs());
    }
    else {
        decl->dump();
        fprintf(stderr, " has no attribute\n");
    }
}

void scanStmt(clang::Stmt* stmt, clang::ASTContext& ctx) {
	attr::Kind expectedKinds[] = {attr::Kind::Packed, attr::Kind::Aligned, attr::Kind::Aligned};
    unsigned int expectedParameters[] = {16, 64};
    unsigned int matches = 0, args = 0;

    for(clang::StmtIterator si = stmt->child_begin(), se = stmt->child_end(); si != se; ++si) {
    //                clang::Stmt* body_stmt = dyn_cast<clang::Stmt> (*si);
        //check attributes of declarations
        if(isa<clang::DeclStmt> (*si)){
            clang::DeclStmt* declstmt = dyn_cast<clang::DeclStmt>(*si);

            if(declstmt->isSingleDecl()){
                clang::Decl* decl = declstmt->getSingleDecl();
                clang::Stmt* body = decl->getBody();
//                clang::CompoundStmt* cs = decl->getCompoundBody();

                if(body)
                    scanStmt(body, ctx);
                else
//                    fprintf(stderr, "bodyless ");
//                checkForAttrs(decl);
                if(!decl->hasAttrs())
                    continue;


                AttrVec attrvec = decl->getAttrs();
                EXPECT_EQ(static_cast<unsigned>(1), attrvec.size());
                const Attr* attr = attrvec[0];
                EXPECT_EQ(expectedKinds[matches++], attr->getKind());
                if(attr->getKind() == attr::Kind::Aligned) {
                    EXPECT_EQ(expectedParameters[args++], ((AlignedAttr*)attr)->getAlignment(ctx));
                }
            }
            else {
                clang::DeclGroupRef dgr = declstmt->getDeclGroup();

                clang::DeclGroup& declgroup = dgr.getDeclGroup();
                for(size_t i = 0; i < declgroup.size(); ++i){
//                    checkForAttrs(declgroup[i]);
                    if(!declgroup[i]->hasAttrs())
                        continue;

                    const AttrVec attrvec = declgroup[i]->getAttrs();
                    EXPECT_EQ(static_cast<unsigned>(1), attrvec.size());
                    const Attr* attr = attrvec[0];
                    EXPECT_EQ(expectedKinds[matches++], attr->getKind());
                    if(attr->getKind() == attr::Kind::Aligned){
                        EXPECT_EQ(expectedParameters[args++], ((AlignedAttr*)attr)->getAlignment(ctx));
                    }
                    clang::Stmt* body = declgroup[i]->getBody();
                    if(body)
                        scanStmt(body, ctx);
                }
            }
        }
        else {
            //nothing to do for now
        }
    }
}


    ProgramPtr program = Program::create();
    InsiemeTransUnitPtr TU = InsiemeTransUnit::ParseFile(std::string(SRC_DIR) + "/kernel_matcher.cl", program, false);

    clang::ASTContext& ctx = TU->getCompiler().getASTContext();

// Check if memory spaces are as expected default, __global, __constant, __local, __private
TEST(KernelMatcherTest, ReadMemorySpaces) {
    std::vector<clang::Type*> types = ctx.getTypes();
    unsigned int expected_asp[] = {GLOBAL, CONSTANT, LOCAL, PRIVATE};
    unsigned int asp = 0;
    for(size_t i = 0; i < types.size(); ++i)
    {
        clang::Type* t = types.at(i);

/* does not work as expected
        if(t->isStructureType())
        {

            const clang::RecordType* rt = t->getAsStructureType();
            const clang::RecordDecl* rd = rt->getDecl();

                printf("------------------> %d - %d\n", rt->hasUnsignedIntegerRepresentation(), rt->isAnyPointerType());
                if(!rd->isEmbeddedInDeclarator()){
                clang::Stmt* stmt = rd->getBody();
                if(stmt)
                scanStmt(stmt, ctx);
            }
        }
*/

        //read the address space attribute of all pointers
        if(t->isPointerType())
        {
            clang::QualType qt = t->getCanonicalTypeInternal();
            //skip return Type of kernel function which has to be void
            if(qt->isVoidPointerType())
                continue;

/*            const char* expected = addressSpaceStr[expected_asp[asp]];
            const char* actual = addressSpaceStr[t->getPointeeType().getAddressSpace()];
            fprintf(stderr, "%d address Spacse %s - %s\n", asp,  expected, actual); */

            EXPECT_EQ(expected_asp[asp++], t->getPointeeType().getAddressSpace());
//            std::cout << qt.getAsString() << " ADDRESS_SPACE: " << t->getPointeeType().getAddressSpace() << std::endl;
        }
    }
}

// Check the given attributes
TEST(KernelMatcherTest, ReadAttributes) {
    clang::DeclContext* declRef = clang::TranslationUnitDecl::castToDeclContext(ctx.getTranslationUnitDecl());

    for(clang::DeclContext::decl_iterator I = declRef->decls_begin(), E = declRef->decls_end(); I != E; ++I) {
            if(!isa<clang::FunctionDecl> (*I))
                continue; // skip non function declarations

            // Top-level Function declaration
            clang::FunctionDecl* func_decl = dyn_cast<clang::FunctionDecl>(*I);

            //Function declaration should have RequiredWorkGroupSize(1,2,3) attribute
            EXPECT_TRUE(func_decl->hasAttrs());
            if(func_decl->hasAttrs()) {
                const clang::AttrVec attrVec = func_decl->getAttrs();
                EXPECT_EQ(static_cast<unsigned>(1), attrVec.size());
                Attr* attr = attrVec[0];
//                std::cout << kindStr[attr->getKind()] << std::endl;
//                parseAttribute(attr);
                EXPECT_EQ(attr::Kind::ReqdWorkGroupSize, attr->getKind());
                if(attr->getKind() == attr::Kind::ReqdWorkGroupSize)
                {
                    EXPECT_EQ(static_cast<unsigned>(1), ((const ReqdWorkGroupSizeAttr*)attr)->getXDim());
                    EXPECT_EQ(static_cast<unsigned>(2), ((const ReqdWorkGroupSizeAttr*)attr)->getYDim());
                    EXPECT_EQ(static_cast<unsigned>(3), ((const ReqdWorkGroupSizeAttr*)attr)->getZDim());
                }
            }

/*
            std::cout << "is an aliasAttr: " << clang::AliasAttr::classof(attr) << std::endl;
            std::cout << "is an annotateAttr: " << clang::AnnotateAttr::classof(attr) << std::endl;
            std::cout << "is an asmLabelAttr: " << clang::AsmLabelAttr::classof(attr) << std::endl;
            std::cout << "is a formatAttr: " << clang::FormatAttr::classof(attr) << std::endl;
            std::cout << "is a sectionAttr: " << clang::SectionAttr::classof(attr) << std::endl;
*/
/*           const clang::Attr* attr2 = attr->getNext();
            std::cout << "hi" << std::endl;
            std::cout << kindStr[attr2->getKind()] << std::endl;
*/

            clang::Stmt* func_body = func_decl->getBody();
            EXPECT_TRUE(func_body);
            if(!func_body)
            {
            	printf("functions declarations with no definition\n");
                continue; // skip functions declarations with no definition
            }

            scanStmt(func_body, ctx);
    }
}
