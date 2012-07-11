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

#include "insieme/backend/runtime/runtime_code_fragments.h"

#include <algorithm>

#include "insieme/utils/logging.h"

#include "insieme/backend/type_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/annotations/c/naming.h"

namespace insieme {
namespace backend {
namespace runtime {

	// definition of some names within the generated code
	#define INIT_CONTEXT_NAME "insieme_init_context"
	#define CLEAN_CONTEXT_NAME "insieme_cleanup_context"
	#define TYPE_TABLE_NAME "g_insieme_type_table"
	#define IMPL_TABLE_NAME "g_insieme_impl_table"

	ContextHandlingFragment::ContextHandlingFragment(const Converter& converter)
		: converter(converter), typeTable(TypeTable::get(converter)), implTable(ImplementationTable::get(converter)) {

		// add include to context definition and type and implementation table
		addInclude("irt_all_impls.h");
		addDependency(TypeTable::get(converter));
		addDependency(ImplementationTable::get(converter));
	}

	ContextHandlingFragmentPtr ContextHandlingFragment::get(const Converter& converter) {
		static string ENTRY_NAME = "ContextHandlingTable";

		// look up the entry within the fragment manager
		auto manager = converter.getFragmentManager();
		auto res = manager->getFragment(ENTRY_NAME);
		if (!res) {
			// create new instance
			ContextHandlingFragmentPtr table = manager->create<ContextHandlingFragment>(boost::ref(converter));
			manager->bindFragment(ENTRY_NAME, table);
			res = table;
		}
		return static_pointer_cast<ContextHandlingFragment>(res);
	}

	const c_ast::IdentifierPtr ContextHandlingFragment::getInitFunctionName() {
		return converter.getCNodeManager()->create(INIT_CONTEXT_NAME);
	}

	const c_ast::IdentifierPtr ContextHandlingFragment::getCleanupFunctionName() {
		return converter.getCNodeManager()->create(CLEAN_CONTEXT_NAME);
	}

	std::ostream& ContextHandlingFragment::printTo(std::ostream& out) const {
		out <<
				"void " INIT_CONTEXT_NAME "(irt_context* context) {\n"
				"    context->type_table_size = " << typeTable->size() << ";\n"
				"    context->type_table = " TYPE_TABLE_NAME ";\n"
				"    context->impl_table_size = " << implTable->size() << ";\n"
				"    context->impl_table = " IMPL_TABLE_NAME ";\n";

		for_each(initExpressions, [&](const string& cur) {
			out << format(cur.c_str(), "context");
		});

		out <<
				"}\n"
				"\n"
				"void " CLEAN_CONTEXT_NAME "(irt_context* context) {\n";

		for_each(cleanupExpressions, [&](const string& cur) {
			out << format(cur.c_str(), "context");
		});

		return out <<
				"}\n\n";
	}



	// -- Type Table ------------------------------------------------------------------------

	class TypeTableStore {
	public:

		struct Entry {
			unsigned index;
			c_ast::IdentifierPtr kind;
			c_ast::TypePtr type;
			vector<unsigned> components;
		};

	private:

		const Converter& converter;

		vector<Entry> entries;

		TypeTable& table;

	public:

		TypeTableStore(const Converter& converter, TypeTable& table) : converter(converter), table(table) {}

		const Entry& resolve(const c_ast::TypePtr& type) {

			// try looking it up within the existing entries
			auto pos = std::find_if(entries.begin(), entries.end(),
					[&](const Entry& cur) { return *(cur.type) == *type; });
			if (pos != entries.end()) {
				return *pos;
			}

			// type has not been resolved before => process now
			return addType(type);
		}

		const vector<Entry>& getEntries() const {
			return entries;
		}

	private:

		const Entry& addEntry(Entry& entry) {
			entry.index = entries.size();
			entries.push_back(entry);
			table.addDependency(converter.getTypeManager().getDefinitionOf(entry.type));
			return *entries.rbegin();
		}

		Entry unknown;

		const Entry& addType(const c_ast::TypePtr& type) {
			switch(type->getNodeType()) {
			case c_ast::NT_PrimitiveType:
				return addType(static_pointer_cast<c_ast::PrimitiveType>(type));
			case c_ast::NT_StructType:
			case c_ast::NT_UnionType:
				return addType(static_pointer_cast<c_ast::NamedCompositeType>(type));
			case c_ast::NT_PointerType:
				return addType(static_pointer_cast<c_ast::PointerType>(type));
			case c_ast::NT_NamedType:
				return addType(static_pointer_cast<c_ast::NamedType>(type));
			case c_ast::NT_FunctionType:
				return addType(c_ast::ptr(type->getManager()->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::Int64))); // TODO
			default:
				LOG(FATAL) << "Unsupported type: " << c_ast::toC(type);
				assert(false && "Unsupported type encountered!");
			}
			return unknown;
		}

		const Entry& addType(const c_ast::PrimitiveTypePtr& type) {

			char const * kind = "";
			switch(type->type) {
			case c_ast::PrimitiveType::Void:
				assert(false && "Void should not be part of the type table!"); break;
			case c_ast::PrimitiveType::Bool:   kind = "IRT_T_BOOL"; break;
			case c_ast::PrimitiveType::Int8:   kind = "IRT_T_INT8"; break;
			case c_ast::PrimitiveType::Int16:  kind = "IRT_T_INT16"; break;
			case c_ast::PrimitiveType::Int32:  kind = "IRT_T_INT32"; break;
			case c_ast::PrimitiveType::Int64:  kind = "IRT_T_INT64"; break;
			case c_ast::PrimitiveType::UInt8:  kind = "IRT_T_UINT8"; break;
			case c_ast::PrimitiveType::UInt16: kind = "IRT_T_UINT16"; break;
			case c_ast::PrimitiveType::UInt32: kind = "IRT_T_UINT32"; break;
			case c_ast::PrimitiveType::UInt64: kind = "IRT_T_UINT64"; break;
			case c_ast::PrimitiveType::Float:  kind = "IRT_T_REAL32"; break;
			case c_ast::PrimitiveType::Double: kind = "IRT_T_REAL64"; break;
			}

			// add entry
			Entry entry;
			entry.kind = converter.getCNodeManager()->create(kind);
			entry.type = type;
			return addEntry(entry);
		}

		const Entry& addType(const c_ast::NamedCompositeTypePtr& type) {

			char const* kind = (type->getNodeType() == c_ast::NT_StructType)?"IRT_T_STRUCT":"IRT_T_UNION";

			Entry entry;
			entry.kind = converter.getCNodeManager()->create(kind);
			entry.type = type;

			// add components
			for_each(type->elements, [&](const c_ast::VariablePtr& cur) {
				entry.components.push_back(resolve(cur->type).index);
			});

			return addEntry(entry);
		}

		const Entry& addType(const c_ast::NamedTypePtr& type) {

			char const* kind = "IRT_T_UINT32";

			Entry entry;
			entry.kind = converter.getCNodeManager()->create(kind);
			entry.type = type;
			return addEntry(entry);

		}

		const Entry& addType(const c_ast::PointerTypePtr& type) {

			char const* kind = "IRT_T_POINTER";

			Entry entry;
			entry.kind = converter.getCNodeManager()->create(kind);
			entry.type = type;
			entry.components.push_back(resolve(type->elementType).index);
			return addEntry(entry);
		}

	};


	TypeTable::TypeTable(const Converter& converter)
		: converter(converter), store(new TypeTableStore(converter, *this)) {}

	TypeTable::~TypeTable() {
		delete store;
	}


	TypeTablePtr TypeTable::get(const Converter& converter) {
		static string ENTRY_NAME = "TypeTable";

		// look up the entry within the fragment manager
		auto manager = converter.getFragmentManager();
		auto res = manager->getFragment(ENTRY_NAME);
		if (!res) {
			// create new instance
			TypeTablePtr table = manager->create<TypeTable>(boost::ref(converter));
			manager->bindFragment(ENTRY_NAME, table);
			res = table;
		}
		return static_pointer_cast<TypeTable>(res);
	}

	const c_ast::ExpressionPtr TypeTable::getTypeTable() {
		return c_ast::ref(converter.getCNodeManager()->create(TYPE_TABLE_NAME));
	}

	std::ostream& TypeTable::printTo(std::ostream& out) const {

		// create component arrays
		out << "// --- componenents for type table entries ---\n";
		for_each(store->getEntries(), [&](const TypeTableStore::Entry& cur) {
			if (!cur.components.empty()) {
				out << "irt_type_id g_type_" << cur.index << "_components[] = {" << join(",", cur.components) << "};\n";
			}
		});
		out << "\n";

		out << "// --- the type table ---\n"
			   "irt_type " TYPE_TABLE_NAME "[] = {\n";

		out << join(",\n",store->getEntries(), [&](std::ostream& out, const TypeTableStore::Entry& cur) {
			out << "    {" << toC(cur.kind) << ", sizeof(" << toC(cur.type) << "), " << cur.components.size() << ", ";
			if (cur.components.empty()) {
				out << "0";
			} else {
				out << "g_type_" << cur.index << "_components";
			}
			out << "}";
		});

		return out << "\n};\n\n";
	}

	unsigned TypeTable::registerType(const core::TypePtr& type) {

		// look up type information
		TypeManager& typeManager = converter.getTypeManager();
		const TypeInfo& info = typeManager.getTypeInfo(type);

		// add dependency to type definition
		addDependency(info.definition);

		// add type information to table store
		return store->resolve(info.rValueType).index;
	}

	unsigned TypeTable::size() const {
		return store->getEntries().size();
	}

	// -- Implementation Table --------------------------------------------------------------


	struct WorkItemVariantCode {
		string entryName;
		string effortName;

		WorkItemVariantFeatures features;

		/**
		 * Creates a new entry to the implementation table referencing the names
		 * of the functions describing the properties of the work item.
		 *
		 * @param name the name of the function implementing this work item variant
		 * @param effortName the name of the function implementing the effort estimation function. If the
		 * 			name is empty, no such function is present.
		 */
		WorkItemVariantCode(const string& name, const string& effortName = "", const WorkItemVariantFeatures& features = WorkItemVariantFeatures())
			: entryName(name), effortName(effortName), features(features) { }
	};

	struct WorkItemImplCode {
		vector<WorkItemVariantCode> variants;
		WorkItemImplCode(const vector<WorkItemVariantCode>& variants) : variants(variants) {}
	};

	ImplementationTable::ImplementationTable(const Converter& converter)
		: converter(converter) {}


	ImplementationTablePtr ImplementationTable::get(const Converter& converter) {
		static string ENTRY_NAME = "ImplementationTable";

		// look up the entry within the fragment manager
		auto manager = converter.getFragmentManager();
		auto res = manager->getFragment(ENTRY_NAME);
		if (!res) {
			// create new instance
			ImplementationTablePtr table = manager->create<ImplementationTable>(boost::ref(converter));
			manager->bindFragment(ENTRY_NAME, table);
			res = table;
		}
		return static_pointer_cast<ImplementationTable>(res);
	}

	unsigned ImplementationTable::registerWorkItemImpl(const core::ExpressionPtr& implementation) {

		// check whether implementation has already been resolved
		auto pos = index.find(implementation);
		if (pos != index.end()) {
			return pos->second;
		}

		// obtain reference to function manager
		FunctionManager& funManager = converter.getFunctionManager();

		// decode implementation information
		WorkItemImpl impl = WorkItemImpl::decode(implementation);

		// convert implementation and effort estimation functions
		for_each(impl.getVariants(), [&](const WorkItemVariant& cur) {

			// resolve entry point
			const FunctionInfo& entryInfo = funManager.getInfo(cur.getImplementation());

			// make this fragment depending on the entry point
			this->addDependency(entryInfo.prototype);

			// resolve effort estimation function
			if (cur.getEffortEstimator()) {

				// resolve effort function
				const FunctionInfo& effortInfo = funManager.getInfo(cur.getEffortEstimator());

				// make this fragment depending on the effort function declaration
				this->addDependency(effortInfo.prototype);
			}

		});


		// get id for this work item
		unsigned id = workItems.size();

		// create list of work item variant implementation codes + rename functions
		vector<WorkItemVariantCode> variants;
		for_each(impl.getVariants(), [&](const WorkItemVariant& cur) {

			// get id of this variation
			unsigned var_id = variants.size();

			// update implementation name
			string implName = format("insieme_wi_%d_var_%d_impl", id, var_id);
			funManager.rename(cur.getImplementation(), implName);

			// update effort estimation function name
			string effortFunName = "";
			if (cur.getEffortEstimator()) {
				effortFunName = format("insieme_wi_%d_var_%d_effort", id, var_id);
				funManager.rename(cur.getEffortEstimator(), effortFunName);
			}

			// add to variant to lists of variants
			variants.push_back(WorkItemVariantCode(implName, effortFunName, cur.getFeatures()));
		});

		// add implementation to list of implementations
		index.insert(std::make_pair(implementation, id));
		workItems.push_back(WorkItemImplCode(variants));
		return id;
	}



	const c_ast::ExpressionPtr ImplementationTable::getImplementationTable() {
		return c_ast::ref(converter.getCNodeManager()->create(IMPL_TABLE_NAME));
	}

	std::ostream& ImplementationTable::printTo(std::ostream& out) const {

		out << "// --- work item variants ---\n";

		int counter=0;
		for_each(workItems, [&](const WorkItemImplCode& cur) {
			out << "irt_wi_implementation_variant g_insieme_wi_" << counter++ << "_variants[] = {\n";
			for_each(cur.variants, [&](const WorkItemVariantCode& variant) {
				out << "    { IRT_WI_IMPL_SHARED_MEM, &" << variant.entryName << ", ";

				// add effort function ...
				if (variant.effortName.empty()) {
					out << "NULL";
				} else {
					out << "&" << variant.effortName;
				}

				out << ", 0, NULL, 0, NULL, ";
				out << "{";
					out << variant.features.effort << "ull, ";
					out << variant.features.opencl << ", ";
					out << variant.features.implicitRegionId << "ll, ";
					out << variant.features.suggestedThreadNum << "ll";
				out << "}";
				out << " },\n";
			});
			out << "};\n";
		});

		out <<
				"// --- the implementation table --- \n"
				"irt_wi_implementation " IMPL_TABLE_NAME "[] = {\n";

		counter=0;
		for_each(workItems, [&](const WorkItemImplCode& cur) {
			out << "    { " << cur.variants.size() << ", g_insieme_wi_" << counter++ << "_variants },\n";
		});

		return out << "};\n\n";
	}

	unsigned ImplementationTable::size() const {
		return workItems.size();
	}

} // end namespace runtime
} // end namespace backend
} // end namespace insieme
