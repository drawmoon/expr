#include "listener.h"

auto ExprInterpreter::expr() -> Expression&
{
}

auto ExprInterpreter::exitSelect_statement(ExprParser::Select_statementContext* ctx) -> void
{
}

auto ExprInterpreter::exitQuery_block(ExprParser::Query_blockContext* ctx) -> void
{
}

auto ExprInterpreter::exitSelected_element(ExprParser::Selected_elementContext* ctx) -> void
{
}

auto ExprInterpreter::exitSelect_list_elements(ExprParser::Select_list_elementsContext* ctx) -> void
{
}

auto ExprInterpreter::exitTable_ref(ExprParser::Table_refContext* ctx) -> void
{
}

auto ExprInterpreter::exitTable_ref_aux(ExprParser::Table_ref_auxContext* ctx) -> void
{
}

auto ExprInterpreter::exitTableview_name(ExprParser::Tableview_nameContext* ctx) -> void
{
}

auto ExprInterpreter::exitWhere_clause(ExprParser::Where_clauseContext* ctx) -> void
{
}

auto ExprInterpreter::exitConcatenation(ExprParser::ConcatenationContext* ctx) -> void
{
}

auto ExprInterpreter::exitGeneral_element_part(ExprParser::General_element_partContext* ctx) -> void
{
}

auto ExprInterpreter::exitTable_element(ExprParser::Table_elementContext* ctx) -> void
{
}

auto ExprInterpreter::exitOrder_by_elements(ExprParser::Order_by_elementsContext* ctx) -> void
{
}

auto ExprInterpreter::exitLogical_expression(ExprParser::Logical_expressionContext* ctx) -> void
{
}

auto ExprInterpreter::exitRelational_expression(ExprParser::Relational_expressionContext* ctx) -> void
{
}

auto ExprInterpreter::exitCompound_expression(ExprParser::Compound_expressionContext* ctx) -> void
{
}

auto ExprInterpreter::exitIn_elements(ExprParser::In_elementsContext* ctx) -> void
{
}

auto ExprInterpreter::exitSimple_case_statement(ExprParser::Simple_case_statementContext* ctx) -> void
{
}

auto ExprInterpreter::exitSearched_case_statement(ExprParser::Searched_case_statementContext* ctx) -> void
{
}

auto ExprInterpreter::exitUnary_expression(ExprParser::Unary_expressionContext* ctx) -> void
{
}

auto ExprInterpreter::exitConstant(ExprParser::ConstantContext* ctx) -> void
{
}

auto ExprInterpreter::exitNumeric(ExprParser::NumericContext* ctx) -> void
{
}

auto ExprInterpreter::exitQuoted_string(ExprParser::Quoted_stringContext* ctx) -> void
{
}

auto ExprInterpreter::exitId_expression(ExprParser::Id_expressionContext* ctx) -> void
{
}

auto ExprInterpreter::exitRegular_id(ExprParser::Regular_idContext* ctx) -> void
{
}

auto ExprInterpreter::exitSpecial_id(ExprParser::Special_idContext* ctx) -> void
{
}

auto ExprInterpreter::exitNumeric_function(ExprParser::Numeric_functionContext* ctx) -> void
{
}

auto ExprInterpreter::exitString_function(ExprParser::String_functionContext* ctx) -> void
{
}

auto ExprInterpreter::exitDate_function(ExprParser::Date_functionContext* ctx) -> void
{
}

auto ExprInterpreter::exitDate_add_function(ExprParser::Date_add_functionContext* ctx) -> void
{
}

auto ExprInterpreter::exitDate_sub_function(ExprParser::Date_sub_functionContext* ctx) -> void
{
}

auto ExprInterpreter::exitDate_trunc_function(ExprParser::Date_trunc_functionContext* ctx) -> void
{
}

auto ExprInterpreter::exitExtract_function(ExprParser::Extract_functionContext* ctx) -> void
{
}

auto ExprInterpreter::exitMedian_function(ExprParser::Median_functionContext* ctx) -> void
{
}

auto ExprInterpreter::exitPercentile_function(ExprParser::Percentile_functionContext* ctx) -> void
{
}

auto ExprInterpreter::exitOther_function(ExprParser::Other_functionContext* ctx) -> void
{
}

auto ExprInterpreter::exitRaw_sql(ExprParser::Raw_sqlContext* ctx) -> void
{
}

auto ExprInterpreter::exitBind_variable(ExprParser::Bind_variableContext* ctx) -> void
{
}
