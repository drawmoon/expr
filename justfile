alias fmt := format

[private]
help:
    @just --list

# init vcpkg
# ./vcpkg/bootstrap-vcpkg.sh

# format the code
format:
    @python format.py

# check the code cleanliness
# check:
#     @run-clang-tidy -p build -j 4

# run tests
test:
    @cd build && make test

# build the project
build:
    @cmake -B build -DCMAKE_TOOLCHAIN_FILE=./vcpkg/scripts/buildsystems/vcpkg.cmake
    @cmake --build build -j$(nproc)

# clean the project
clean:
    @rm -rf build/*
    @rm -rf src/parser/*
