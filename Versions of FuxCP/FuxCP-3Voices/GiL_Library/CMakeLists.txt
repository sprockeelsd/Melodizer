cmake_minimum_required(VERSION 3.26)
project(GiL_Library)

set(CMAKE_CXX_STANDARD 14)

include_directories(c++/headers)

add_executable(GiL_Library
        c++/headers/Counterpoint.hpp
        c++/headers/gecode_wrapper.hpp
        c++/headers/space_wrapper.hpp
        c++/Counterpoint.cpp
        c++/gecode_wrapper.cpp
        c++/space_wrapper.cpp)
