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
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/annotations/meta_info/meta_infos.h"

namespace insieme {
namespace backend {
namespace runtime {

	// definition of some names within the generated code
	#define INIT_CONTEXT_NAME "insieme_init_context"
	#define CLEAN_CONTEXT_NAME "insieme_cleanup_context"
	#define TYPE_TABLE_NAME "g_insieme_type_table"
	#define IMPL_TABLE_NAME "g_insieme_impl_table"
	#define META_TABLE_NAME "g_insieme_meta_table"

	ContextHandlingFragment::ContextHandlingFragment(const Converter& converter)
		: converter(converter),
		  typeTable(TypeTable::get(converter)),
		  implTable(ImplementationTable::get(converter)),
		  infoTable(MetaInfoTable::get(converter)) {

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
				"    context->impl_table = " IMPL_TABLE_NAME ";\n"
				"    context->info_table_size = " << infoTable->size() << ";\n"
				"    context->info_table = " META_TABLE_NAME ";\n";

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
			case c_ast::NT_VectorType:
				return addType(static_pointer_cast<c_ast::VectorType>(type));
			case c_ast::NT_NamedType:
				return addType(static_pointer_cast<c_ast::NamedType>(type));
			case c_ast::NT_FunctionType:
				return addType(c_ast::ptr(type->getManager()->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::Int64))); // TODO
			case c_ast::NT_ModifiedType:
				return addType(static_pointer_cast<c_ast::ModifiedType>(type)->type);
			default:
				LOG(FATAL) << "Unsupported type: " << c_ast::toC(type);
				assert(false && "Unsupported type encountered!");
			}
			return unknown;
		}

		const Entry& addType(const c_ast::PrimitiveTypePtr& type) {

			char const * kind = "";
			switch(type->type) {
			case c_ast::PrimitiveType::Void:   kind = "IRT_T_VOID"; break;
			case c_ast::PrimitiveType::Bool:   kind = "IRT_T_BOOL"; break;
			case c_ast::PrimitiveType::Char:   kind = "IRT_T_CHAR"; break;
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
			case c_ast::PrimitiveType::LongLong: kind = "IRT_T_INT64"; break;  
			case c_ast::PrimitiveType::ULongLong: kind = "IRT_T_UINT64"; break;  
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

		const Entry& addType(const c_ast::VectorTypePtr& type) {

			char const* kind = "IRT_T_VAR_VECTOR";

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

	const c_ast::ExpressionPtr TypeTable::getTable() {
		return c_ast::ref(converter.getCNodeManager()->create(TYPE_TABLE_NAME));
	}

	std::ostream& TypeTable::printTo(std::ostream& out) const {

		// create component arrays
		out << "// --- components for type table entries ---\n";
		for_each(store->getEntries(), [&](const TypeTableStore::Entry& cur) {
			if (!cur.components.empty()) {
				out << "irt_type_id g_type_" << cur.index << "_components[] = {" << join(",", cur.components) << "};\n";
			}
		});
		out << "\n";

		out << "// --- the type table ---\n"
			   "irt_type " TYPE_TABLE_NAME "[] = {\n";

		out << join(",\n",store->getEntries(), [&](std::ostream& out, const TypeTableStore::Entry& cur) {
			out << "    {" << toC(cur.kind) << ", ";
			if(cur.type.isa<c_ast::VectorTypePtr>()) {
				out << "0";
			} else if (cur.type.isa<c_ast::PrimitiveTypePtr>() && cur.type.as<c_ast::PrimitiveTypePtr>()->type == c_ast::PrimitiveType::Void) {
				out << "1";
            } else if (cur.type.isa<c_ast::FunctionTypePtr>() || (cur.type.isa<c_ast::NamedTypePtr>() && cur.type.as<c_ast::NamedTypePtr>()->isFunctionType)) {
                out << "1"; 
			} else {
				out << "sizeof(" << toC(cur.type) << ")";
			}
			out << ", " << cur.components.size() << ", ";
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

		unsigned metaInfoEntryIndex;

		/**
		 * Creates a new entry to the implementation table referencing the names
		 * of the functions describing the properties of the work item.
		 *
		 * @param name the name of the function implementing this work item variant
		 * @param effortName the name of the function implementing the effort estimation function. If the
		 * 			name is empty, no such function is present.
		 */
		WorkItemVariantCode(const string& name, unsigned metaInfoEntryIndex = 0)
			: entryName(name), metaInfoEntryIndex(metaInfoEntryIndex) { }
	};

	struct WorkItemImplCode {
		vector<WorkItemVariantCode> variants;
		WorkItemImplCode(const vector<WorkItemVariantCode>& variants) : variants(variants) {}
	};

	ImplementationTable::ImplementationTable(const Converter& converter)
		: converter(converter) {
		this->addDependency(MetaInfoTable::get(converter));
	}


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

			// register meta info
			unsigned info_id = MetaInfoTable::get(converter)->registerMetaInfoFor(cur.getImplementation());

			// add to variant to lists of variants
			variants.push_back(WorkItemVariantCode(implName, info_id));
		});

		// add implementation to list of implementations
		index.insert(std::make_pair(implementation, id));
		workItems.push_back(WorkItemImplCode(variants));
		return id;
	}



	const c_ast::ExpressionPtr ImplementationTable::getTable() {
		return c_ast::ref(converter.getCNodeManager()->create(IMPL_TABLE_NAME));
	}

	std::ostream& ImplementationTable::printTo(std::ostream& out) const {

		out << "// --- work item variants ---\n";

		int counter=0;
		for_each(workItems, [&](const WorkItemImplCode& cur) {
			out << "irt_wi_implementation_variant g_insieme_wi_" << counter++ << "_variants[] = {\n";
			for_each(cur.variants, [&](const WorkItemVariantCode& variant) {
				out << "    { &" << variant.entryName << ", ";

				// data and channel requirements
				out << "0, NULL, 0, NULL, ";

				// meta information
				out << " &(" << META_TABLE_NAME << "[" << variant.metaInfoEntryIndex << "])";
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


	// -- Meta Info Table --------------------------------------------------------------


	struct MetaInfoTableEntry {

		// a map from an info type name to a list of values required for its initialization
		std::map<string, std::vector<c_ast::ExpressionPtr>> entries;

		void add(const string& type, const std::vector<c_ast::ExpressionPtr>& data) {
			entries[type] = data;
		}

		bool empty() const {
			return entries.empty();
		}

	};


	MetaInfoTable::MetaInfoTable(const Converter& converter)
		: converter(converter) {}


	MetaInfoTablePtr MetaInfoTable::get(const Converter& converter) {
		static string ENTRY_NAME = "MetaInfoTable";

		// look up the entry within the fragment manager
		auto manager = converter.getFragmentManager();
		auto res = manager->getFragment(ENTRY_NAME);
		if (!res) {
			// create new instance
			MetaInfoTablePtr table = manager->create<MetaInfoTable>(boost::ref(converter));
			manager->bindFragment(ENTRY_NAME, table);
			res = table;
		}
		return static_pointer_cast<MetaInfoTable>(res);
	}

	const c_ast::ExpressionPtr MetaInfoTable::getTable() {
		return c_ast::ref(converter.getCNodeManager()->create(META_TABLE_NAME));
	}

	unsigned MetaInfoTable::registerMetaInfoFor(const core::NodePtr& node) {
		auto reg = core::dump::AnnotationConverterRegister::getDefault();
		auto& mgr = node.getNodeManager();
		core::IRBuilder builder(mgr);
		auto zero = converter.getCNodeManager()->create<c_ast::Literal>("0");

		// iterate through all annotations
		MetaInfoTableEntry entry;
		for(const auto& cur : node.getAnnotations()) {

			// try obtaining a matching converter
			auto conv = reg.getConverterFor(cur.second);
			if (!conv) continue;

			// convert the information
			auto pack = conv->toIR(mgr, cur.second);

			// unpack it
			auto tuple = pack.isa<core::TupleExprPtr>();
			if (!tuple) continue;
			vector<core::ExpressionPtr> values = tuple->getExpressions();

			// isolate last
			auto typeName = values.back().isa<core::LiteralPtr>();
			if (!typeName) continue;
			values.pop_back();

			// get string name of type
			auto type = typeName->getStringValue();

			// build list of fields
			vector<c_ast::ExpressionPtr> fields;
			ConversionContext context(converter, core::LambdaPtr());
			for(const auto& cur : values) {
				auto expr = cur;

				// unpack nested expressions
				if (core::encoder::isEncodingOf<core::ExpressionPtr>(expr)) {
					expr = core::encoder::toValue<core::ExpressionPtr>(expr);
				}

				// add field if not null ...
				fields.push_back((expr) ? converter.getStmtConverter().convertExpression(context, expr) : zero);
			}

			// add dependencies
			this->addDependencies(context.getDependencies());
			this->addRequirements(context.getRequirements());
			this->addIncludes(context.getIncludes());

			// register new entry
			entry.add(type, fields);
		}

		// if there is no info => don't add empty row
		if (entry.empty()) return 0;

		// add new entry
		infos.push_back(entry);

		// return new index (+1 since 0 is utilized)
		return infos.size();
	}


	std::ostream& MetaInfoTable::printTo(std::ostream& out) const {

		// collect all meta info structs
		std::set<string> infoStructs;
		for(const auto& cur : infos) {
			for(const auto& entry : cur.entries) {
				infoStructs.insert(entry.first);
			}
		}

		// TODO: remove this:
		{
			out <<
					"\n// --- meta info table entry type declarations ---\n"
					"#include \"meta_information/struct_generator.h\"\n"
					"#include \"insieme/meta_information/meta_infos.def\"\n";

			out << "\n";

			out <<
					"// --- meta info table entry type ---\n"
					"struct _irt_meta_info_table_entry {\n"
					"	effort_estimation_info effort_estimation;\n"
					"	opencl_info opencl;\n"
					"";

			out << "};\n\n";
		}

		out <<
				"// --- the meta info table --- \n"
				"irt_meta_info_table_entry " META_TABLE_NAME "[] = {\n";
		out << "    {}, /* the no-info-entry */\n";

		out << join(",\n", infos, [&](std::ostream& out, const MetaInfoTableEntry& cur) {
			out << "    {";
			out << join(",", infoStructs, [&](std::ostream& out, const string& type){
				auto pos = cur.entries.find(type);
				out << " ." << type << "={ true, ";
				if (pos != cur.entries.end()) {
					out << join(", ", pos->second, [](std::ostream& out, const c_ast::ExpressionPtr& cur) {
						out << toC(cur);
					});
				}
				out << " }";
			});
			out << "}";
		});

		return out << "\n};\n\n";
	}

	unsigned MetaInfoTable::size() const {
		return infos.size();
	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme
