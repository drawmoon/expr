#include "expr.h"

constexpr auto to_string(BinaryOperator op) -> std::string_view
{
    auto it = std::find_if(BINARY_OP_MAP.begin(), BINARY_OP_MAP.end(), [op](const auto& pair) { return pair.first == op; });
    if (it != BINARY_OP_MAP.end())
    {
        return it->second;
    }
    throw std::invalid_argument("Unknown BinaryOperator");
}

inline auto to_binary_operator(const std::string& op_str) -> BinaryOperator
{
    static const auto map = []
    {
        std::unordered_map<std::string_view, BinaryOperator> map;
        map.reserve(BINARY_OP_MAP.size());
        for (const auto& [op, str_view] : BINARY_OP_MAP)
        {
            map[str_view] = op;
        }
        return map;
    }();
    if (auto it = map.find(op_str); it != map.end())
    {
        return it->second;
    }
    throw std::invalid_argument("Unknown BinaryOperator=" + op_str);
}

namespace nlohmann
{
    template <>
    struct adl_serializer<BinaryOperator>
    {
        static void to_json(json& j, const BinaryOperator& op)
        {
            j = to_string(op);
        }

        static void from_json(const json& j, BinaryOperator& op)
        {
            op = to_binary_operator(j.get<std::string>());
        }
    };
}

Field::Field(std::string name, Table::ptr table, std::optional<std::string> alias)
    : name_(std::move(name))
    , table_(std::move(table))
    , alias_(std::move(alias))
{
}

auto Field::as(const std::string& alias) noexcept -> ptr
{
    alias_ = std::move(alias);
    return shared_from_this();
}

Asterisk::Asterisk(Table::ptr table)
    : table_(std::move(table))
{
}

Binary::Binary(Field::ptr left, Field::ptr right, BinaryOperator comparator)
    : left_(std::move(left))
    , right_(std::move(right))
    , comparator_(std::move(comparator))
{
}
