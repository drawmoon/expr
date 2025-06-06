find_package(GTest CONFIG REQUIRED)

# include_directories(${GTEST_INCLUDE_DIRS})

add_executable(exprtest test_expr.cpp)
target_link_libraries(exprtest PUBLIC
    expr
    GTest::gtest
    GTest::gtest_main
    GTest::gmock
    GTest::gmock_main)

add_test(AllTestsInExpr exprtest)
