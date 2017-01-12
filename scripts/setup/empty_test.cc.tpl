#include <gtest/gtest.h>

#include "%PROJECT%/%MODULE%/%PART%.h"

using namespace %PROJECT%::%MODULE%::%NAMESPACES%;

TEST(%PART_NAME%Test, Basic) {
	ASSERT_EQ(2, 1 + 1);
}
