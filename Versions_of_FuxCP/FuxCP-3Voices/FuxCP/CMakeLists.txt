cmake_minimum_required(VERSION 3.26)
project(FuxCP)

set(CMAKE_CXX_STANDARD 14)

include_directories(c++/headers)

add_executable(FuxCP
        c++/headers/Counterpoint.hpp
        c++/headers/problem_wrapper.hpp
        c++/headers/Solver.hpp
        c++/headers/Utilities.hpp
        c++/Counterpoint.cpp
        c++/problem_wrapper.cpp
        c++/Solver.cpp
        c++/Utilities.cpp
        c++/new_problem_wrapper.cpp)
