alias fmt := format

[private]
help:
    @just --list

# init vcpkg
# ./vcpkg/bootstrap-vcpkg.sh

# install dependencies
setup:
    @./vcpkg/vcpkg install --feature-flags=manifests

# format the code
format:
    @find . -name "*.cpp" -o -name "*.h" | xargs clang-format -i

# check the code cleanliness
# check:
#     @run-clang-tidy -p build -j 4
