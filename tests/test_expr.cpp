#include <gtest/gtest.h>
#include "expr.h"

TEST(BinaryOperatorJsonTest, Serialization)
{
    const auto& test_cases = BINARY_OP_MAP;
    for (const auto& [op, expected_str] : test_cases)
    {
        json j;
        j = op;
        EXPECT_EQ(j.get<std::string>(), expected_str) << "Failed for operator: " << static_cast<int>(op);
    }
}

TEST(BinaryOperatorJsonTest, Deserialization)
{
    const auto& test_cases = BINARY_OP_MAP;
    for (const auto& [expected_op, str] : test_cases)
    {
        json j = str;
        BinaryOperator op = j.get<BinaryOperator>();
        EXPECT_EQ(op, expected_op) << "Failed for string: " << str;
    }
}

TEST(BinaryOperatorJsonTest, InvalidDeserialization)
{
    std::vector<std::string> invalid_inputs = {"", "invalid", "eq", "greater", "BETWEEN"};

    for (const auto& input : invalid_inputs)
    {
        nlohmann::json j = input;
        EXPECT_THROW(j.get<BinaryOperator>(), std::invalid_argument) << "Input should throw: " << input;
    }
}
