#pragma once

#include "insieme/transform/datalayout/aos_to_soa.h"

namespace insieme {

namespace analysis {
typedef std::map<core::VariableAddress, core::CompoundStmtAddress> VariableScopeMap;
}

namespace transform {
namespace datalayout {

class AosToTaos : public AosToSoa {
public:
	AosToTaos(core::NodePtr& toTransform);

	virtual void transform();
};

} // datalayout
} // transform
} // insieme
