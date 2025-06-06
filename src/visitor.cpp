#include "expr.h"

ReplacingVisitor::~ReplacingVisitor()
{
}

auto ReplacingVisitor::visit_asterisk(Asterisk& expr) -> void
{
}

auto ReplacingVisitor::visit_field(Field& expr) -> void
{
}

auto ReplacingVisitor::visit_table(Table& expr) -> void
{
}
