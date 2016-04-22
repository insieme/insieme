#define CONCRETE(name) NT_##name,
enum NodeType {
#include "insieme/core/ir_nodes.def"
};
#undef CONCRETE
