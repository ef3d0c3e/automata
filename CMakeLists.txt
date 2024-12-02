cmake_minimum_required(VERSION 3.2)
project("automata")

set(CMAKE_CXX_FLAGS "-std=c++23 -Wall -Wpedantic -Wextra -fconcepts-diagnostics-depth=2 -ggdb")
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# source
file(GLOB_RECURSE SOURCE "src/*.cpp")


# Executable
add_executable(automata ${SOURCE})
