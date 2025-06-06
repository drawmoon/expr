# minimum required CMAKE version
cmake_minimum_required(VERSION 3.16...3.31)

# add custom CMake modules directory to search path
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

# compiler must be 20
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# strongly encouraged to enable this globally to avoid conflicts between
# -Wpedantic being enabled and -std=c++20 and -std=gnu++20 for example
# when compiling with PCH enabled
set(CMAKE_CXX_EXTENSIONS OFF)

# Note: by default ENABLE_DEVELOPER_MODE is True
# this means that all analysis (sanitizers, static analysis)
# is enabled and all warnings are treated as errors
# if you want to switch this behavior, change TRUE to FALSE
set(ENABLE_DEVELOPER_MODE
    TRUE
    CACHE BOOL "Enable 'developer mode'")
# change this to false if you want to disable warnings_as_errors in developer mode
set(OPT_WARNINGS_AS_ERRORS_DEVELOPER_DEFAULT TRUE)

# setup vcpkg - should be called before defining project()
# clone the repository to the project root: https://gitee.com/mirrors/vcpkg.git
# implementation ./vcpkg/bootstrap-vcpkg.sh initialization vcpkg
# run_vcpkg()
# adding vcpkg toolchain support
if (NOT DEFINED CMAKE_TOOLCHAIN_FILE)
    set(CMAKE_TOOLCHAIN_FILE
        ${CMAKE_SOURCE_DIR}/vcpkg/scripts/buildsystems/vcpkg.cmake
        CACHE STRING "vcpkg toolchain file")
endif()

project(expr
    VERSION 0.0.1
    DESCRIPTION ""
    LANGUAGES CXX C)

# required if linking to static library
add_definitions(-DANTLR4CPP_STATIC)

# using /MD flag for antlr4_runtime (for Visual C++ compilers only)
# set(ANTLR4_WITH_STATIC_CRT OFF)

# add external build for antlrcpp
include(ExternalAntlr4Cpp)
# add antrl4cpp artifacts to project environment
include_directories(${ANTLR4_INCLUDE_DIRS})

# set variable pointing to the antlr tool that supports C++
# this is not required if the jar file can be found under PATH environment
set(ANTLR_EXECUTABLE ${CMAKE_SOURCE_DIR}/parser/antlr-4.13.2-complete.jar)

# add macros to generate ANTLR Cpp code from grammar
find_package(ANTLR REQUIRED)
antlr_target(Expr ${CMAKE_SOURCE_DIR}/parser/Expr.g4
    LISTENER
    OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR}/src/parser)

# include generated parser code directory
include_directories(${ANTLR_Expr_OUTPUT_DIR})

add_library(expr STATIC
    ${CMAKE_SOURCE_DIR}/src/expr.hpp
    ${ANTLR_Expr_CXX_OUTPUTS}        # ANTLR-generated source files
    )

# exposes the path to the header file
target_include_directories(expr PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>)

# disable Clang-Tidy for ANTLR-generated source files
set_source_files_properties(
    ${ANTLR_Expr_CXX_OUTPUTS}
    PROPERTIES CXX_CLANG_TIDY "")

# find_package(fmt CONFIG REQUIRED)
find_package(nlohmann_json CONFIG REQUIRED)
find_package(magic_enum CONFIG REQUIRED)

# not provide implicit conversions via a custom triplet file
# set(nlohmann-json_IMPLICIT_CONVERSIONS OFF)

# enable the test function
enable_testing()

# add_subdirectory(${CMAKE_SOURCE_DIR}/src)
add_subdirectory(${CMAKE_SOURCE_DIR}/tests)

# link dependencies
target_link_libraries(expr PUBLIC
    antlr4_static
    # fmt::fmt
    nlohmann_json::nlohmann_json
    magic_enum::magic_enum
    )
