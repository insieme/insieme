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

#include "insieme/core/dump/binary_dump.h"

#include <inttypes.h>
#include <limits>
#include <map>
#include <vector>

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace core {
namespace dump {

namespace binary {

	// The binary format of the IR dump:
	//		<MAGIC_NUMBER> <NUM_CONVERTER> <CONVERTER_NAMES>+ <NUM_NODES> <NODE>+
	//
	// where the magic number is used to identify data streams,
	// the number of converters states the number of different annotation
	// converters used for the encoding, the converter name list enumerates
	// those converters, the number of nodes is recording the number of
	// contained nodes and the list of nodes is containing the actual encoding
	// of the nodes.
	// Each node encoding starts with a type_t type (= NodeType). In case
	// it is a value node, it is followed by the encoding of its value.
	// for strings, the encoding consists of the string length followed
	// by a character array (not null-terminated).
	// All other nodes are encoded using the type, the number of children
	// and the list of indices of children.
	//
	// A list of annotations is following the value fields or child list.
	//		<NUM_ANNOTATIONS> ( <ANNOTATION_TYPE> <ROOT_NODE_ENCODING> )*
	// It starts by given the number of annotations followed by pairs of
	// annotation types and head-nodes for the actual encoding. Every
	// annotation has to be encoded into an IR tree. The root of this tree
	// is referenced within the annotation list. The annotation type determines
	// which converter from the head of the encoding shell be used for
	// restoring the annotation.
	//
	// Within the binary file, every node of the DAG is only stored once
	// and referenced via its index.


	namespace {

		// -- writer --

		/**
		 * A static visitor used to store node values within a stream.
		 */
		struct ValueDumper : public boost::static_visitor<void> {
			std::ostream& out;
			ValueDumper(std::ostream& out) : out(out) {}

			void operator()(bool value) const {
				write<uint8_t>(out, (value) ? 1 : 0);
			}

			void operator()(char value) const {
				write<uint8_t>(out, value);
			}

			void operator()(int value) const {
				write<int32_t>(out, value);
			}

			void operator()(unsigned value) const {
				write<uint32_t>(out, value);
			}

			void operator()(const string& value) const {
				assert_fail() << "Should not be handled this way!";
			}
		};

		/**
		 * The binary dumper is converting the DAG representing the
		 * code fragment to be dumped into a list such that the root
		 * node is stored at position 1 (index 0). The list
		 * is then written into the output stream using a binary format.
		 */
		class BinaryDumper {
			/**
			 * The list of all involved annotation converter.
			 */
			vector<AnnotationConverterPtr> converter;

			/**
			 * The reverse index of the involved converter.
			 */
			std::map<AnnotationConverterPtr, int> converter_index;

			/**
			 * The list of all nodes to be dumped.
			 */
			vector<NodePtr> nodeList;

			/**
			 * An reverse index, assigning every node its index
			 * within the node list and a the list of annotations mapped to their index.
			 */
			utils::map::PointerMap<NodePtr, pair<index_t, std::map<NodeAnnotationPtr, index_t>>> index;

			/**
			 * The register of annotation converters to be utilized for the encoding.
			 */
			const AnnotationConverterRegister& converterRegister;

		  public:
			/**
			 * A constructor creating a new instance of this binary dumper based on
			 * the given converter register.
			 */
			BinaryDumper(const AnnotationConverterRegister& converterRegister) : converterRegister(converterRegister) {}

			/**
			 * Dumps the given ir code fragment to the given output stream.
			 */
			void dump(std::ostream& out, const NodePtr& ir) {
				// create node list and index
				createIndex(ir);

				// write magic number
				write(out, MAGIC_NUMBER);

				// write number of converters
				write<index_t>(out, converter.size());

				// write list of converters
				for(auto cur : converter) {
					dumpConverter(out, cur);
				}

				// write number of nodes
				write<index_t>(out, nodeList.size());

				// dump nodes
				for(auto cur : nodeList) {
					dumpNode(out, cur);
				}
			}

		  private:
			/**
			 * Dumps a string to the given output stream.
			 */
			void dumpString(std::ostream& out, const string& str) {
				binary::dumpString(out, str);
			}

			/**
			 * Dumps a single annotation converter instance (the name of the converter).
			 */
			void dumpConverter(std::ostream& out, const AnnotationConverterPtr& converter) {
				dumpString(out, converter->getName());
			}

			/**
			 * Dumps the annotations attached to the given node.
			 */
			void dumpAnnotations(std::ostream& out, const NodePtr& node) {
				// get all index information regarding the current nodes annotation
				const auto& info = index[node].second;

				// dump number of annotations
				write<length_t>(out, info.size());

				// write pairs of converter index / node index elements
				for(const auto& cur : info) {
					// write index of converter
					write<index_t>(out, converter_index[converterRegister.getConverterFor(cur.first)]);

					// write index of root node of converted annotation
					write<index_t>(out, cur.second);
				}
			}

			/**
			 * Dumps a single node into the output stream.
			 */
			void dumpNode(std::ostream& out, const NodePtr& node) {
				// write type (for all nodes this is the same)
				type_t type = node->getNodeType();
				write(out, type);

				// check whether it is a string value
				if(type == NT_StringValue) {
					// dump the string value
					dumpString(out, node.as<StringValuePtr>()->getValue());

					// also add annotations
					dumpAnnotations(out, node);

					return;
				}

				// check whether it is a value => special handling
				if(node->isValue()) {
					// dump content
					boost::apply_visitor(ValueDumper(out), node->getNodeValue());

					// also add annotations
					dumpAnnotations(out, node);

					return;
				}

				// all other nodes are handled the same way

				// handle standard node
				const NodeList& children = node->getChildList();

				// write length of child list
				write<length_t>(out, children.size());

				// write child list
				for_each(children, [&](const core::NodePtr& cur) {
					auto pos = index.find(cur);
					assert(pos != index.end() && "Index not correctly established!");
					write<index_t>(out, pos->second.first);
				});

				// also add annotations
				dumpAnnotations(out, node);
			}

			/**
			 * Creates the node index by enlisting all nodes within the IR DAG
			 * within the node list and the reverse lookup table (index).
			 */
			void createIndex(const NodePtr& ir) {
				// obtain the manager used for the conversion of annotations
				NodeManager& mgr = ir->getNodeManager();

				// index all nodes
				std::function<void(const NodePtr& cur)> indexer;
				auto indexer_lambda = [&](const NodePtr& cur) {
					// check whether index has been assigned before
					auto pos = index.find(cur);
					if(pos != index.end()) { return; }
					index[cur].first = (index_t)nodeList.size();
					nodeList.push_back(cur);

					// process annotations
					for(auto cur_annotation : cur->getAnnotations()) {
						auto cur_converter = converterRegister.getConverterFor(cur_annotation.second);
						if(cur_converter) {
							// 1. convert and index the annotation

							// convert annotation ...
							NodePtr converted = cur_converter->toIR(mgr, cur_annotation.second);
							assert_true(converted) << "Converted Annotation must not be NULL!";

							// .. and index converted result ..
							visitDepthFirstOnce(converted, indexer);

							// .. and add annotation to index
							assert(index.find(converted) != index.end() && "Indexed Annotation should now be present!");
							index[cur].second[cur_annotation.second] = index[converted].first;

							// 2. register the converter itself

							// register converter for this annotation
							auto pos = converter_index.find(cur_converter);
							if(pos != converter_index.end()) { continue; }

							converter_index[cur_converter] = (index_t)converter.size();
							converter.push_back(cur_converter);
						}
					}
				};
				indexer = indexer_lambda;

				// index ir node
				visitDepthFirstOnce(ir, indexer);

				// check whether index limit is sufficient
				assert(nodeList.size() < std::numeric_limits<index_t>::max() && "Number of nodes is exceeding index limit!");
			}
		};

		/**
		 * The binary loader is restoring an IR structure from a binary input stream.
		 */
		class BinaryLoader {
			/**
			 * A simple data structure summarizing the information extracted from
			 * an encoded node before restoring it.
			 */
			struct NodeInfo {
				/**
				 * The list of indices referencing the child nodes of a node.
				 */
				vector<index_t> children;

				/**
				 * The list of annotations of a node - the converter index followed
				 * by the root node index.
				 */
				vector<pair<index_t, index_t>> annotations;
			};

			/**
			 * The builder used to construct nodes.
			 */
			IRBuilder builder;

			/**
			 * A list of node skeletons extracted while passing through
			 * the the file. The first vector lists all child nodes, the second
			 * vector all annotations.
			 */
			vector<pair<NodeType, NodeInfo>> nodes;

			/**
			 * The index of all resolved nodes.
			 */
			vector<NodePtr> index;

			/**
			 * The register of annotation converters to be utilized for the decoding.
			 */
			const AnnotationConverterRegister& converterRegister;

			/**
			 * The index of the converters used to restore the annotations within the
			 * binary file as specified within the header.
			 */
			vector<AnnotationConverterPtr> converter_index;

		  public:
			BinaryLoader(NodeManager& manager, const AnnotationConverterRegister& converterRegister) : builder(manager), converterRegister(converterRegister) {}

			NodePtr load(std::istream& in) {
				// check magic number
				if(read<uint64_t>(in) != MAGIC_NUMBER) { throw InvalidEncodingException("Encoding error: wrong magic number!"); }

				// load converter list
				loadConverter(in);

				// load index
				loadIndex(in);

				// restore nodes
				NodePtr res = resolve(0); // root node has always index 0

				// restore annotations
				resolveAnnotations();

				return res;
			}

		  private:
			string loadString(std::istream& in) {
				// load string
				length_t length = read<length_t>(in);

				// load string
				char data[length + 1];
				in.read(data, length);
				data[length] = '\0';

				// register string value
				return string(data, length);
			}

			void loadConverter(std::istream& in) {
				// get number of converters
				index_t numConverter;
				in.read((char*)&numConverter, sizeof(numConverter));

				// load converters
				for(index_t i = 0; i < numConverter; i++) {
					auto converterName = loadString(in);
					converter_index.push_back(converterRegister.getConverterFor(converterName));
				}
			}

			void loadIndex(std::istream& in) {
				// load number of nodes
				index_t numNodes;
				in.read((char*)&numNodes, sizeof(numNodes));

				// allocate sufficient space
				nodes.resize(numNodes);
				index.resize(numNodes);

				// resolve nodes
				for(index_t i = 0; i < numNodes; i++) {
					loadNode(i, in);
				}
			}

			void loadAnnotations(index_t pos, std::istream& in) {
				// get list of annotations
				vector<index_t> annotations;

				// get number of annotations
				length_t numAnnotations = read<length_t>(in);

				for(length_t i = 0; i < numAnnotations; i++) {
					// read current annotation information
					index_t converterID = read<index_t>(in);
					index_t rootIndex = read<index_t>(in);

					// add information to node skeletons
					nodes[pos].second.annotations.push_back(std::make_pair(converterID, rootIndex));
				}
			}

			void loadNode(index_t pos, std::istream& in) {
				// load node type
				type_t type;
				in.read((char*)&type, sizeof(type));

				// create node-index entry
				nodes[pos].first = NodeType(type); // child list and annotations are default-initialized

				if(type == NT_StringValue) {
					// load and register string value
					index[pos] = builder.stringValue(loadString(in));

					// load annotations
					loadAnnotations(pos, in);

					return;
				}


				// handle value node
				if(type == NT_BoolValue || type == NT_CharValue || type == NT_IntValue || type == NT_UIntValue) {
					ValuePtr value;
					if(type == NT_BoolValue) {
						value = builder.boolValue(bool(read<uint8_t>(in) != 0));
					} else if(type == NT_CharValue) {
						value = builder.charValue(char(read<uint8_t>(in)));
					} else if(type == NT_UIntValue) {
						value = builder.uintValue(unsigned(read<uint32_t>(in)));
					} else if(type == NT_IntValue) {
						value = builder.intValue(int(read<int32_t>(in)));
					} else {
						assert_fail() << "Inconsistent code!";
					}

					// register value
					index[pos] = value;

					// load annotations
					loadAnnotations(pos, in);

					return;
				}

				// restore all other nodes
				length_t length = read<length_t>(in);

				vector<index_t> children;
				for(length_t i = 0; i < length; i++) {
					children.push_back(read<index_t>(in));
				}

				// add child list
				nodes[pos].second.children = children;

				// load annotations
				loadAnnotations(pos, in);
			}

			NodePtr resolve(index_t pos) {
				// check whether node has been resolved before
				if(index[pos]) { return index[pos]; }

				// resolve child list
				NodeList children = ::transform(nodes[pos].second.children, fun(*this, &BinaryLoader::resolve));
				NodePtr res = builder.get(nodes[pos].first, children);

				// remember newly resolved node
				index[pos] = res;
				return res;
			}

			void resolveAnnotations() {
				// restore all annotations
				for(index_t i = 0; i < nodes.size(); i++) {
					NodePtr node = resolve(i);
					for(auto cur : nodes[i].second.annotations) {
						// restores the encoding of the annotations
						ExpressionPtr encoded = resolve(cur.second).as<ExpressionPtr>();

						// decode the annotation
						AnnotationConverterPtr converter = converter_index[cur.first];
						if(converter) { converter->attachAnnotation(node, encoded); }
					}
				}
			}
		};

		void dumpPathIndices(std::ostream& out, const NodeAddress::Path& path) {
			// dump in order
			if(path.getLength() > 2) { dumpPathIndices(out, path.getPathToParent()); }
			write<index_t>(out, path.getIndex());
		}

		void dumpPath(std::ostream& out, const NodeAddress::Path& path) {
			// start with the length
			write<length_t>(out, path.getLength() - 1);

			// dump path
			dumpPathIndices(out, path);
		}
	}

	void dumpString(std::ostream& out, const string& str) {
		// write the string content
		write<length_t>(out, str.length());

		// write string (not including \0)
		out.write(str.c_str(), str.length());
	}

	void dumpIR(std::ostream& out, const NodePtr& ir, const AnnotationConverterRegister& converterRegister) {
		BinaryDumper(converterRegister).dump(out, ir);
	}

	void dumpAddress(std::ostream& out, const NodeAddress& address, const AnnotationConverterRegister& converterRegister) {
		dumpAddresses(out, toVector(address), converterRegister);
	}

	void dumpAddresses(std::ostream& out, const vector<NodeAddress>& addresses, const AnnotationConverterRegister& converterRegister) {
		assert_false(addresses.empty()) << "Cannot dump empty list of addresses!";

		// just dump full IR tree ...
		dumpIR(out, addresses[0].getRootNode(), converterRegister);

		// .. followed by the address paths
		for_each(addresses, [&](const NodeAddress& cur) { dumpPath(out, cur.getPath()); });
	}


	NodePtr loadIR(std::istream& in, core::NodeManager& manager, const AnnotationConverterRegister& converterRegister) {
		return BinaryLoader(manager, converterRegister).load(in);
	}

	NodeAddress loadAddress(std::istream& in, NodeManager& manager, const AnnotationConverterRegister& converterRegister) {
		vector<NodeAddress> list = loadAddresses(in, manager, converterRegister);
		assert_false(list.empty()) << "Resolved address list must not be empty!";
		return list[0];
	}

	vector<NodeAddress> loadAddresses(std::istream& in, NodeManager& manager, const AnnotationConverterRegister& converterRegister) {
		// first, load tree
		NodePtr root = loadIR(in, manager, converterRegister);

		// check whether there are any addresses.
		if(in.eof()) {
			in.clear();
			return toVector(NodeAddress(root));
		}
		// load paths
		vector<NodeAddress> res;
		while(!in.eof()) {
			length_t length = read<length_t>(in);

			if(length == 0) {
				in.clear();
				if(res.size() > 0) {
					return res;
				} else {
					return toVector(NodeAddress(root));
				}
			}

			if(!in.eof()) {
				NodeAddress cur(root);
				for(length_t i = 0; i < length; i++) {
					cur = cur.getAddressOfChild(read<index_t>(in));
				}
				res.push_back(cur);
			}
		}
		in.clear();
		return res;
	}

} // end namespace binary

} // end namespace dump
} // end namespace core
} // end namespace insieme
