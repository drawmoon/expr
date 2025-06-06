#pragma once

#include <memory>
#include <unordered_map>
#include <string>
#include <string_view>
#include <stdexcept>
#include <optional>
#include <variant>
#include <functional>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

enum class BinaryOperator
{
    EQUALS,
    NOT_EQUALS,
    LT,
    LTE,
    GT,
    GTE,
    BETWEEN,
    EVERY,
};

static constexpr std::array<std::pair<BinaryOperator, std::string_view>, 8> BINARY_OP_MAP = { {
    { BinaryOperator::EQUALS, "equals" },
    { BinaryOperator::NOT_EQUALS, "not_equals" },
    { BinaryOperator::LT, "lt" },
    { BinaryOperator::LTE, "lte" },
    { BinaryOperator::GT, "gt" },
    { BinaryOperator::GTE, "gte" },
    { BinaryOperator::BETWEEN, "between" },
    { BinaryOperator::EVERY, "every" },
} };

class ExpressionVisitor;
class Expression
{
public:
    using ptr = std::shared_ptr<Expression>;

    virtual ~Expression() = default;
    virtual auto replace(const std::function<ptr(const ptr&)>& replace_fn) -> ptr = 0;
    virtual auto accept(ExpressionVisitor& visitor) -> void = 0;
    virtual auto deep_copy() const -> ptr = 0;
    virtual auto render() const -> std::string = 0;
    virtual auto render_json() const -> std::string = 0;
    virtual auto serialize() const -> std::string = 0;
    virtual auto deserialize(const std::string& serialized) -> ptr = 0;

    virtual auto begin() -> std::vector<ptr>::iterator = 0;
    virtual auto end() -> std::vector<ptr>::iterator = 0;
};

/**
 * @brief Represents a condition or predicate in SQL queries.
 *
 * Conditions are used in various SQL clauses, primarily in the WHERE clause of a SELECT
 * statement. They can be combined using logical operators to form complex conditions.
 */
class Condition : public Expression
{
public:
    virtual ~Condition() = default;
};

// ----------------- class derived from an expression -----------------

/**
 * @brief Represents a table in a database.
 *
 * This interface provides methods to interact with and manipulate tables, including retrieving
 * metadata, setting aliases, joining tables, and managing index hints.
 */
class Table : public Expression, public std::enable_shared_from_this<Table>
{
public:
    using ptr = std::shared_ptr<Table>;

    ~Table() = default;
};

/**
 * @brief Represents a field in a database table or query.
 *
 * A field can be part of a table, and it supports various operations such as comparisons and
 * aggregations. It also allows setting an alias for the field to use in queries.
 */
class Field : public Expression, public std::enable_shared_from_this<Field>
{
private:
    std::string name_;
    std::optional<std::string> alias_;
    Table::ptr table_;

public:
    using ptr = std::shared_ptr<Field>;

    ~Field() = default;
    explicit Field(std::string name, Table::ptr table, std::optional<std::string> alias = std::nullopt);

    auto table() const noexcept -> Table::ptr
    {
        return table_;
    }

    auto name() const noexcept -> const std::string&
    {
        return name_;
    }

    auto alias() const noexcept -> const std::optional<std::string>&
    {
        return alias_;
    }

    // a function for adding internal operations
    auto as(const std::string& alias) noexcept -> ptr;
};

/**
 * @brief A placeholder for all fields in a table.
 */
class Asterisk : public Expression, public std::enable_shared_from_this<Asterisk>
{
private:
    Table::ptr table_;

public:
    using ptr = std::shared_ptr<Asterisk>;

    ~Asterisk() = default;
    explicit Asterisk(Table::ptr table);

    auto table() const noexcept -> Table::ptr
    {
        return table_;
    }
};

using QueryElm = std::variant<Asterisk::ptr, Field::ptr>;

class Binary : public Condition, public std::enable_shared_from_this<Binary>
{
private:
    Field::ptr left_;
    Field::ptr right_;
    BinaryOperator comparator_;

public:
    using ptr = std::shared_ptr<Binary>;

    virtual ~Binary() = default;
    explicit Binary(Field::ptr left, Field::ptr right, BinaryOperator comparator);

    auto left() const noexcept -> Field::ptr
    {
        return left_;
    }

    auto right() const noexcept -> Field::ptr
    {
        return right_;
    }

    auto comparator() const noexcept -> BinaryOperator
    {
        return comparator_;
    }
};

// ----------------- the visitor class -----------------

class ExpressionVisitor
{
public:
    virtual ~ExpressionVisitor() = default;
    virtual auto visit_asterisk(Asterisk& expr) -> void = 0;
    virtual auto visit_field(Field& expr) -> void = 0;
    virtual auto visit_table(Table& expr) -> void = 0;
};

class ReplacingVisitor : public ExpressionVisitor
{
public:
    ~ReplacingVisitor() override = default;
    auto visit_asterisk(Asterisk& expr) -> void override;
    auto visit_field(Field& expr) -> void override;
    auto visit_table(Table& expr) -> void override;
};
