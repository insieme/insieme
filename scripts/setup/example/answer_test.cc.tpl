#include <gtest/gtest.h>

#include "%PROJECT%/%MODULE%/answer.h"

using namespace %PROJECT%::%MODULE%;

TEST(AnswerTest, Basic) {
	ASSERT_EQ(42, answer());
}
