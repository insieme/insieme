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
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/match.h"
#include "insieme/core/tu/ir_translation_unit.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/parallel.h"

#include "insieme/annotations/opencl/opencl_annotations.h"

#include "insieme/frontend/pragma/matcher.h"
#include "insieme/frontend/extensions/opencl_frontend_extension.h"

#include "insieme/utils/config.h"

using namespace insieme::frontend::pragma::tok;
using namespace insieme::frontend::pragma;
using namespace insieme::frontend;
using namespace insieme::annotations::opencl;

namespace insieme {
namespace frontend {
namespace extensions {

	static const std::string pragmaNamespace = "opencl";
	// contains all keyword which are required by this frontend extension
	namespace keywords {
		static const std::string device = "device";
		static const std::string type = "type";
		static const std::string cpu = "CPU";
		static const std::string gpu = "GPU";
		static const std::string accelerator = "ACCELERATOR";
		static const std::string defaulted = "DEFAULT";
		static const std::string loop = "loop";
		static const std::string independent = "independent";
		static const std::string yes = "yes";
		static const std::string no = "no";
		static const std::string requirement = "requirement";
		static const std::string range = "range";
		static const std::string access = "access";
		static const std::string ro = "RO";
		static const std::string wo = "WO";
		static const std::string rw = "RW";
	}

	// contains all clauses which are used to register the pragma handlers
	namespace clauses {
		auto device = kwd(keywords::type) >> l_paren >> (kwd(keywords::cpu) | kwd(keywords::gpu) | kwd(keywords::accelerator) | kwd(keywords::defaulted))[keywords::type] >> r_paren;
		auto loop = !(kwd(keywords::independent) >> l_paren >> (kwd(keywords::yes) | kwd(keywords::no))[keywords::independent] >> r_paren);
		auto range = kwd(keywords::range) >> l_paren >> tok::expr[keywords::range] >> colon >> tok::expr[keywords::range] >> colon >> tok::expr[keywords::range] >> r_paren;
		auto requirement = l_paren >> var[keywords::requirement] >> comma >> range >> comma >> (kwd(keywords::ro) | kwd(keywords::wo) | kwd(keywords::rw))[keywords::access] >> r_paren;
	}

	DevicePtr handleDeviceClause(const MatchObject& object) {
		const std::string& value = object.getString(keywords::type);
		// now try to map the value to an internal representation
		Device::Type type;
		if(value == keywords::accelerator) {
			type = Device::ACCELERATOR;
		} else if(value == keywords::cpu) {
			type = Device::CPU;
		} else if(value == keywords::defaulted) {
			type = Device::DEFAULT;
		} else if(value == keywords::gpu) {
			type = Device::GPU;
		} else {
			assert_fail() << "OpenCL: unsupported device type: " << value;
		}
		return std::make_shared<Device>(type);
	}

	bool handleIndependentClause(const MatchObject& object) {
		const std::string& value = object.getString(keywords::independent);
		return value == keywords::yes;
	}
	
	VariableRequirementPtr handleRequirementClause(const MatchObject& object) {
		auto var = object.getVars(keywords::requirement);
		assert_eq(var.size(), 1) << "OpenCL: requirement clause must contain a variable";
		// extract the supplied ranges
		auto ranges = object.getExprs(keywords::range);
		assert_eq(ranges.size(), 3) << "OpenCL: requirement clause must contain a range with three exprs";
		// check if the ranges are integer based expressions!
		#if 0
		assert_true(::all(ranges, [&](const core::ExpressionPtr& expr) { 
				return core::types::isMatchable(expr->getType(), var[0]->getNodeManager().getLangBasic().getUIntGen());
			})) << "OpenCL: ranges clause must contain uints";
		#endif
		// last but not least, grab the access mode
		const std::string& value = object.getString(keywords::access);
		VariableRequirement::AccessMode accessMode;
		if(value == keywords::ro) {
			accessMode = VariableRequirement::AccessMode::RO;
		} else if(value == keywords::wo) {
			accessMode = VariableRequirement::AccessMode::WO;
		} else if(value == keywords::rw) {
			accessMode = VariableRequirement::AccessMode::RW;
		} else {
			assert_fail() << "OpenCL: unsupported access type: " << value;
		}
		return std::make_shared<VariableRequirement>(var[0], ranges[0], ranges[1], ranges[2], accessMode);
    }

	void addAnnotations(core::NodePtr& node, BaseAnnotation::AnnotationList& annos) {
		if(annos.empty()) return;
		// get old annotation list and append our annotations
		if(node->hasAnnotation(BaseAnnotation::KEY)) {
			auto& lst = node->getAnnotation(BaseAnnotation::KEY)->getAnnotationList();
			lst.insert(lst.end(), annos.begin(), annos.end());
		} else {
			// in this case we need to create a new one
			node->addAnnotation(std::make_shared<BaseAnnotation>(annos));
		}
	}

	OpenCLFrontendExtension::OpenCLFrontendExtension(bool flagActivated) {
		// set the default value of the activation flag
		this->flagActivated = flagActivated;
		macros.insert(std::make_pair("_OPENCL", ""));
		// Add a handler for pragma opencl device <type>
		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(
		    PragmaHandler(pragmaNamespace, keywords::device, clauses::device >> tok::eod, [](const MatchObject& object, core::NodeList nodes) {
			    // check for the mandatory device type
			    DevicePtr deviceClause = handleDeviceClause(object);
			    LOG(DEBUG) << "OpenCL: found deviceClause of type: " << *deviceClause;
			    // create the annotation and put it into the BaseAnnotation list
			    BaseAnnotation::AnnotationList deviceAnnos;
			    deviceAnnos.push_back(std::make_shared<DeviceAnnotation>(deviceClause));

			    for (auto& node : nodes) {
					if (node->getNodeType() == core::NT_CompoundStmt) {
						LOG(DEBUG) << "OpenCL: annotationg compound with device";
						// now mark the compoundStmt with the annotation such that the OpenCLTransformer
						// will process i tlater on (e.g. checks and so forth)
						addAnnotations(node, deviceAnnos);
					} else {
						// in any other case, the #pragma is simply ignored (we do not want to assert here)
						LOG(WARNING) << "OpenCL: attached pragma  will be ignored";
					}
				}
			    return nodes;
			})));
		// Add a handler for pragma opencl loop [independent]
		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(
			PragmaHandler(pragmaNamespace, keywords::loop, clauses::loop >> tok::eod, [](const MatchObject& object, core::NodeList nodes) {
				// check for the optional independent keyword
				bool independent = handleIndependentClause(object);
				LOG(DEBUG) << "OpenCL: found loop clause, independent: " << (independent ? "yes" : "no");
				// create the annotation and put it into the BaseAnnotation list
			    BaseAnnotation::AnnotationList loopAnnos;
			    loopAnnos.push_back(std::make_shared<LoopAnnotation>(independent));

			    for (auto& node : nodes) {
					if (node->getNodeType() == core::NT_CompoundStmt) {
						LOG(DEBUG) << "OpenCL: annotationg compound with loop";
						// now mark the compoundStmt with the annotation such that the OpenCLTransformer
						// will process i tlater on (e.g. checks and so forth)
						addAnnotations(node, loopAnnos);
					} else {
						// in any other case, the #pragma is simply ignored (we do not want to assert here)
						LOG(WARNING) << "OpenCL: attached pragma will be ignored";
					}
				}
			    return nodes;
			})));
		// Add a handler for pragma opencl requirement
		pragmaHandlers.push_back(std::make_shared<PragmaHandler>(
			PragmaHandler(pragmaNamespace, keywords::requirement, clauses::requirement >> tok::eod, [](const MatchObject& object, core::NodeList nodes) {
				LOG(DEBUG) << "OpenCL: found requirement clause";
				// create the annotation and put it into the BaseAnnotation list
				BaseAnnotation::AnnotationList reqAnnos;
				reqAnnos.push_back(handleRequirementClause(object));
				for (auto& node : nodes) {
					if (node->getNodeType() == core::NT_CompoundStmt) {
						LOG(DEBUG) << "OpenCL: annotation compound with requirement";
						// now mark the compountStmt
						addAnnotations(node, reqAnnos);
					} else {
						// in any other case, the #pragma will simply be ignored
						LOG(WARNING) << "OpenCL: attached pragma will be ignored";
					}
				}
				return nodes;
			})));
	}

	core::tu::IRTranslationUnit OpenCLFrontendExtension::IRVisit(core::tu::IRTranslationUnit& tu) {
		return tu;
	}

	FrontendExtension::flagHandler OpenCLFrontendExtension::registerFlag(boost::program_options::options_description& options) {
		// register opencl flag
		options.add_options()("fopencl", boost::program_options::value<bool>(&flagActivated)->implicit_value(true), "OpenCL support");
		// create lambda
		auto lambda = [&](const ConversionJob& job) { return flagActivated; };
		return lambda;
	}

	core::ProgramPtr OpenCLFrontendExtension::IRVisit(core::ProgramPtr& prog) {
		return prog;
	}

} // end namespace extensions
} // end namespace frontend
} // end namespace insieme
