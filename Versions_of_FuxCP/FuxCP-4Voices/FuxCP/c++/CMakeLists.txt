cmake_minimum_required(VERSION 3.10)
project(MyProject)
set(CMAKE_OSX_ARCHITECTURES "arm64" CACHE STRING "Build architectures" FORCE)
set(CMAKE_CXX_STANDARD 11)
# Set path to the Gecode library and headers
# Tell CMake where to find the Gecode library
find_library(GECODE_LIBRARY
        NAMES Gecode
        PATHS /Library/Frameworks
        NO_DEFAULT_PATH)
message(STATUS "Gecode library: ${GECODE_LIBRARY}")
# Add the include directory to the include path
include_directories(/Library/Frameworks/Gecode.framework/Headers)

# Add the include directories to the include path
include_directories(${GECODE_INCLUDE_DIR})
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/headers)
# Create the dynamic library and link to the Gecode library
set(MY_HEADERS headers/problem_wrapper.hpp headers/gecode_problem.hpp src/gecode_problem.cpp main.cpp
        headers/Utilities.hpp
        src/Parts/FirstSpeciesCounterpoint.cpp
        src/Parts/Part.cpp
        src/Utilities.cpp
        src/Parts/SecondSpeciesCounterpoint.cpp
        headers/Parts/SecondSpeciesCounterpoint.hpp
        src/Parts/CantusFirmus.cpp
        headers/Parts/CantusFirmus.hpp
        src/CounterpointProblems/TwoVoiceCounterpoint.cpp
        headers/CounterpointProblems/TwoVoiceCounterpoint.hpp
)
add_library(damien ${MY_HEADERS} src/problem_wrapper.cpp main.cpp
        headers/Utilities.hpp
        src/Parts/FirstSpeciesCounterpoint.cpp
        src/Parts/Part.cpp
        src/Utilities.cpp
        src/Parts/SecondSpeciesCounterpoint.cpp
        headers/Parts/SecondSpeciesCounterpoint.hpp
        src/Parts/CantusFirmus.cpp
        headers/Parts/CantusFirmus.hpp
        src/CounterpointProblems/TwoVoiceCounterpoint.cpp
        headers/CounterpointProblems/TwoVoiceCounterpoint.hpp
)
target_link_libraries(damien ${GECODE_LIBRARY})