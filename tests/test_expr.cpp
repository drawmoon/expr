#include <gtest/gtest.h>
#include "expr.hpp"

const auto& test_cases = std::array{
    std::make_pair(expr::binary_operator::equals, "equals"),
    std::make_pair(expr::binary_operator::notEquals, "notEquals"),
    std::make_pair(expr::binary_operator::lt, "lt"),
    std::make_pair(expr::binary_operator::lte, "lte"),
    std::make_pair(expr::binary_operator::gt, "gt"),
    std::make_pair(expr::binary_operator::gte, "gte"),
    std::make_pair(expr::binary_operator::contains, "contains"),
    std::make_pair(expr::binary_operator::startsWith, "startsWith"),
    std::make_pair(expr::binary_operator::between, "between"),
    std::make_pair(expr::binary_operator::every, "every"),
};

TEST(BinaryOperatorJsonTest, Serialization)
{
    for (const auto& [op, expected_str] : test_cases)
    {
        json j;
        j = op;
        EXPECT_EQ(j.get<std::string>(), expected_str) << "Failed for operator: " << static_cast<int>(op);
    }
}

TEST(BinaryOperatorJsonTest, Deserialization)
{
    for (const auto& [expected_op, str] : test_cases)
    {
        json j = str;
        expr::binary_operator op = j.get<expr::binary_operator>();
        EXPECT_EQ(op, expected_op) << "Failed for string: " << str;
    }
}

TEST(BinaryOperatorJsonTest, InvalidDeserialization)
{
    std::vector<std::string> invalid_inputs = { "", "invalid", "eq", "greater", "BETWEEN" };
    for (const auto& input : invalid_inputs)
    {
        json j = input;
        EXPECT_THROW(j.get<expr::binary_operator>(), std::invalid_argument) << "Input should throw: " << input;
    }
}
