#pragma once

#include <stack>
#include <antlr4-runtime.h>
#include "ExprBaseListener.h"
#include "expr.h"

class ExceptionErrorListener : public antlr4::BaseErrorListener
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

class ExprInterpreter : public ExprBaseListener
{
    std::stack<Expression> expr_stack;

public:
    auto expr() -> Expression&;

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
