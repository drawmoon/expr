#ifndef INCLUDE_DARSH_EXPR_HPP_
#define INCLUDE_DARSH_EXPR_HPP_

#define DARSH_EXPR_HPP_VERSION_MAJOR 0
#define DARSH_EXPR_HPP_VERSION_MINOR 1
#define DARSH_EXPR_HPP_VERSION_PATCH 0

#include <stack>
#include <memory>
#include <unordered_map>
#include <string>
#include <string_view>
#include <stdexcept>
#include <optional>
#include <variant>
#include <functional>
#include <antlr4-runtime.h>
#include <nlohmann/json.hpp>
#include <magic_enum/magic_enum.hpp>
#include "ExprBaseListener.h"

using json = nlohmann::json;

namespace expr
{

    class expr_part;
    class expr_visitor;

    // query part expressions
    class asterisk_expr;
    class field_expr;

    template <typename V>
    class variable_expr;

    // condition part expressions
    class binary_expr;
    class logical_and_expr;
    class logical_or_expr;

    using query_elm = std::variant<std::shared_ptr<asterisk_expr>, std::shared_ptr<field_expr>>;
    using query_elms = std::vector<query_elm>;

    using binary_rhs_basic_type = std::variant<int, double, bool, std::string, std::shared_ptr<field_expr>>;
    using binary_rhs_type = std::vector<binary_rhs_basic_type>;

    enum class binary_operator
    {
        equals,
        notEquals,
        lt,
        lte,
        gt,
        gte,
        contains,
        startsWith,
        between,
        every,
    };

    static constexpr std::array<std::pair<std::string_view, std::string_view>, 10> SQL_OPERATOR_MAPPINGS = { {
        { "equals", "=" },
        { "notEquals", "<>" },
        { "lt", "<" },
        { "lte", "<=" },
        { "gt", ">" },
        { "gte", ">=" },
        { "contains", "LIKE" },
        { "startsWith", "LIKE" },
        { "between", "BETWEEN" },
        { "every", "EVERY" },
    } };

    class expr_part
    {
    public:
        using ptr = std::shared_ptr<expr_part>;

        virtual ~expr_part() = default;
        virtual auto replace(const std::function<ptr(const ptr&)>& replace_fn) -> ptr = 0;
        virtual auto accept(expr_visitor& visitor) -> void = 0;
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
    class condition : public expr_part
    {
    public:
        using ptr = std::shared_ptr<condition>;

        virtual ~condition() = default;
    };

    // ----------------- class derived from an expression -----------------

    /**
     * @brief Represents a table in a database.
     *
     * This interface provides methods to interact with and manipulate tables, including retrieving
     * metadata, setting aliases, joining tables, and managing index hints.
     */
    class table_expr : public expr_part, public std::enable_shared_from_this<table_expr>
    {
    public:
        using ptr = std::shared_ptr<table_expr>;

        ~table_expr() override = default;
    };

    /**
     * @brief Represents a field in a database table or query.
     *
     * A field can be part of a table, and it supports various operations such as comparisons and
     * aggregations. It also allows setting an alias for the field to use in queries.
     *
     * JSON representation example:
     * \code{.json}
     * {
     *     "name": "abc",
     *     "alias": "a",
     *     "table": "t" // the table field is nullable
     * }
     * \endcode
     */
    class field_expr : public expr_part, public std::enable_shared_from_this<field_expr>
    {
    private:
        std::string name_;
        std::optional<std::string> alias_;
        table_expr::ptr table_;

    public:
        using ptr = std::shared_ptr<field_expr>;

        ~field_expr() override = default;
        explicit field_expr(std::string name, table_expr::ptr table, std::optional<std::string> alias = std::nullopt)
            : name_(std::move(name))
            , table_(std::move(table))
            , alias_(std::move(alias))
        {
        }

        auto table() const noexcept -> table_expr::ptr
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
        auto as(const std::string& alias) noexcept -> ptr
        {
            alias_ = std::move(alias);
            return shared_from_this();
        }

        // override
        auto serialize() const -> std::string override
        {
            return "";
        }
    };

    /**
     * @brief A placeholder for all fields in a table.
     */
    class asterisk_expr : public expr_part, public std::enable_shared_from_this<asterisk_expr>
    {
    private:
        table_expr::ptr table_;

    public:
        using ptr = std::shared_ptr<asterisk_expr>;

        ~asterisk_expr() override = default;
        explicit asterisk_expr(table_expr::ptr table)
            : table_(std::move(table))
        {
        }

        auto table() const noexcept -> table_expr::ptr
        {
            return table_;
        }
    };

    /**
     * @brief Represents a variable expression that holds a value of a specific type.
     *
     * A variable_expr encapsulates a value and its associated data type. It can be used
     * within expressions and queries to represent variables or constants.
     */
    template <typename V>
    class variable_expr : public expr_part, public std::enable_shared_from_this<variable_expr<V>>
    {
    private:
        V value_;
        std::optional<std::string> alias_;

    public:
        using ptr = std::shared_ptr<variable_expr>;

        ~variable_expr() override = default;
        explicit variable_expr(std::optional<std::string> alias = std::nullopt)
            : alias_(std::move(alias))
        {
        }

        auto alias() const noexcept -> const std::optional<std::string>&
        {
            return alias_;
        }

        // a function for adding internal operations
        auto as(const std::string& alias) noexcept -> ptr
        {
            alias_ = std::move(alias);
            return this->shared_from_this();
        }
    };

    /**
     * @brief Represents a binary expression in a condition (e.g., field = value).
     *
     * This expression consists of a member (usually a field), an operator, and one or more values.
     *
     * JSON representation examples:
     * \code{.json}
     * // Example 1: Using a field expression for member and a typed value
     * {
     *     "operator": "equals",
     *     "member": { "name": "abc", "alias": "a", "table": "t" },
     *     "values": [{ "value": "2019-01-01", "type": "date" }]
     * }
     *
     * // Example 2: Simplified form with member as string and simple value
     * { "operator": "equals", "member": "abc", "values": [1] }
     * \endcode
     */
    class binary_expr : public condition, public std::enable_shared_from_this<binary_expr>
    {
    private:
        field_expr::ptr member_;
        std::unique_ptr<binary_rhs_type> values_;
        binary_operator operator_;

    public:
        using ptr = std::shared_ptr<binary_expr>;

        ~binary_expr() override = default;
        explicit binary_expr(field_expr::ptr member, std::unique_ptr<binary_rhs_type> values, binary_operator op)
            : member_(std::move(member))
            , values_(std::move(values))
            , operator_(op)
        {
        }

        auto member() const noexcept -> field_expr::ptr
        {
            return member_;
        }

        auto values() const noexcept -> const binary_rhs_type&
        {
            return *values_;
        }

        auto op() const noexcept -> binary_operator
        {
            return operator_;
        }
    };

    class logical_and_expr : public condition, public std::enable_shared_from_this<logical_and_expr>
    {
    private:
        std::vector<condition::ptr> and_;

    public:
        ~logical_and_expr() override = default;
        explicit logical_and_expr(std::vector<condition::ptr> and_)
            : and_(std::move(and_))
        {
        }

        // override
        auto serialize() const -> std::string override
        {
            return "";
        }
    };

    class logical_or_expr : public condition, public std::enable_shared_from_this<logical_or_expr>
    {
    private:
        std::vector<condition::ptr> or_;

    public:
        ~logical_or_expr() override = default;
    };

    // ----------------- the visitor class -----------------

    class expr_visitor
    {
    public:
        virtual ~expr_visitor() = default;
        virtual auto visit_asterisk(asterisk_expr& expr) -> void = 0;
        virtual auto visit_field(field_expr& expr) -> void = 0;
        virtual auto visit_table(table_expr& expr) -> void = 0;
    };

    // class replacing_visitor : public expr_visitor
    // {
    // public:
    //     ~replacing_visitor() override = default;
    //     auto visit_asterisk(asterisk_expr& expr) -> void override;
    //     auto visit_field(field& expr) -> void override;
    //     auto visit_table(table& expr) -> void override;
    // };

    // ----------------- the exception class -----------------

    namespace exception
    {

    }

    // ----------------- the detail namespace -----------------

    namespace detail
    {

        class exception_error_listener : public antlr4::BaseErrorListener
        {
            std::string err_msg;

        public:
            void syntaxError(
                antlr4::Recognizer* recognizer,
                antlr4::Token* offendingSymbol,
                size_t line,
                size_t charPositionInLine,
                const std::string& msg,
                std::exception_ptr e) override
            {
                e = nullptr;
                std::ostringstream ss;
                ss << "line " << line << ":" << charPositionInLine << " " << msg;
                err_msg = ss.str();
                throw antlr4::ParseCancellationException(err_msg);
            }
        };

        class expr_interpreter : public ExprBaseListener
        {
            std::stack<expr::expr_part> expr_stack;

        public:
            auto expr() -> expr::expr_part&;

            // a function that handles the SELECT statement
            auto exitSelect_statement(ExprParser::Select_statementContext* ctx) -> void override;
            auto exitQuery_block(ExprParser::Query_blockContext* ctx) -> void override;

            // a function that handles the SELECT clause
            auto exitSelected_element(ExprParser::Selected_elementContext* ctx) -> void override;
            auto exitSelect_list_elements(ExprParser::Select_list_elementsContext* ctx) -> void override;

            // a function that handles the FROM clause
            auto exitTable_ref(ExprParser::Table_refContext* ctx) -> void override;
            auto exitTable_ref_aux(ExprParser::Table_ref_auxContext* ctx) -> void override;
            auto exitTableview_name(ExprParser::Tableview_nameContext* ctx) -> void override;

            // a function that handles the WHERE clause
            auto exitWhere_clause(ExprParser::Where_clauseContext* ctx) -> void override;

            auto exitConcatenation(ExprParser::ConcatenationContext* ctx) -> void override;
            auto exitGeneral_element_part(ExprParser::General_element_partContext* ctx) -> void override;
            // TODO notice if it can be replaced by general_element
            auto exitTable_element(ExprParser::Table_elementContext* ctx) -> void override;

            // a function that handles the ORDER BY clause
            auto exitOrder_by_elements(ExprParser::Order_by_elementsContext* ctx) -> void override;

            // a function that handles the predicate expression
            auto exitLogical_expression(ExprParser::Logical_expressionContext* ctx) -> void override;
            auto exitRelational_expression(ExprParser::Relational_expressionContext* ctx) -> void override;
            auto exitCompound_expression(ExprParser::Compound_expressionContext* ctx) -> void override;
            auto exitIn_elements(ExprParser::In_elementsContext* ctx) -> void override;
            auto exitSimple_case_statement(ExprParser::Simple_case_statementContext* ctx) -> void override;
            auto exitSearched_case_statement(ExprParser::Searched_case_statementContext* ctx) -> void override;

            auto exitUnary_expression(ExprParser::Unary_expressionContext* ctx) -> void override;

            // a function that handles the constant expression
            auto exitConstant(ExprParser::ConstantContext* ctx) -> void override;
            auto exitNumeric(ExprParser::NumericContext* ctx) -> void override;
            auto exitQuoted_string(ExprParser::Quoted_stringContext* ctx) -> void override;

            // a function that handles the id expression
            auto exitId_expression(ExprParser::Id_expressionContext* ctx) -> void override;
            auto exitRegular_id(ExprParser::Regular_idContext* ctx) -> void override;
            auto exitSpecial_id(ExprParser::Special_idContext* ctx) -> void override;

            // a  function that handles the standard function
            auto exitNumeric_function(ExprParser::Numeric_functionContext* ctx) -> void override;
            auto exitString_function(ExprParser::String_functionContext* ctx) -> void override;
            auto exitDate_function(ExprParser::Date_functionContext* ctx) -> void override;
            auto exitDate_add_function(ExprParser::Date_add_functionContext* ctx) -> void override;
            auto exitDate_sub_function(ExprParser::Date_sub_functionContext* ctx) -> void override;
            auto exitDate_trunc_function(ExprParser::Date_trunc_functionContext* ctx) -> void override;
            auto exitExtract_function(ExprParser::Extract_functionContext* ctx) -> void override;
            auto exitMedian_function(ExprParser::Median_functionContext* ctx) -> void override;
            auto exitPercentile_function(ExprParser::Percentile_functionContext* ctx) -> void override;
            auto exitOther_function(ExprParser::Other_functionContext* ctx) -> void override;

            auto exitRaw_sql(ExprParser::Raw_sqlContext* ctx) -> void override;
            auto exitBind_variable(ExprParser::Bind_variableContext* ctx) -> void override;
        };

    }

}

// ----------------- the nlohmann -----------------

namespace nlohmann
{
    template <>
    struct adl_serializer<expr::binary_operator>
    {
        static void to_json(json& j, const expr::binary_operator& op)
        {
            j = magic_enum::enum_name(op);
        }

        static void from_json(const json& j, expr::binary_operator& op)
        {
            auto s = j.get<std::string>();
            auto op_1 = magic_enum::enum_cast<expr::binary_operator>(s);
            op = op_1 ? *op_1 : throw std::invalid_argument("Unknown binary_operator=" + s);
        }
    };

    template <>
    struct adl_serializer<expr::logical_and_expr>
    {
        static void to_json(json& j, const expr::logical_and_expr& expr)
        {
            // json cond_array = json::array();
            // for (auto it = expr.begin(); it != expr.end(); ++it)
            // {
            //     const auto& cond = *it;

            //     json cond_json;
            //     cond_json = cond;
            //     cond_array.push_back(cond_json);
            // }
        }

        static void from_json(const json& j, expr::logical_and_expr& type)
        {
        }
    };
}

#endif // INCLUDE_DARSH_EXPR_HPP_
