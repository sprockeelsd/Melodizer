# all: so dylib

# # # #compile the dynamic library for linux
# # so:
# # 	g++ -fPIC src/problem_wrapper.cpp -std=c++11 -shared -lgecodedriver -lgecodeflatzinc -lgecodefloat -lgecodeint -lgecodekernel -lgecodeminimodel -lgecodesearch -lgecodeset -lgecodesupport -o ../sources/libgecode.so -O3 -ffast-math

# #compile the dynamic library for macOS
dylib: 
	g++  main.cpp src/Utilities.cpp src/Voice.cpp src/Stratum.cpp src/CounterpointUtils.cpp src/CounterpointProblems/CounterpointProblem.cpp src/Parts/Part.cpp src/Parts/FirstSpeciesCounterpoint.cpp \
		src/Parts/SecondSpeciesCounterpoint.cpp src/Parts/ThirdSpeciesCounterpoint.cpp src/Parts/FourthSpeciesCounterpoint.cpp src/Parts/FifthSpeciesCounterpoint.cpp src/Parts/CantusFirmus.cpp src/CounterpointProblems/TwoVoiceCounterpoint.cpp src/CounterpointProblems/ThreeVoiceCounterpoint.cpp src/CounterpointProblems/FourVoiceCounterpoint.cpp src/fuxTest.cpp src/constraints.cpp -std=c++11 -dynamiclib -fPIC -F/Library/Frameworks -framework gecode -o ../sources/libgecode.dylib  -ffast-math -O3
	install_name_tool -change gecode.framework/Versions/49/gecode /Library/Frameworks/gecode.framework/Versions/49/gecode ../sources/libgecode.dylib

# #run the main.cpp file on macOS
run_mac_os:
	g++ -std=c++11 -F/Library/Frameworks -c main.cpp src/Utilities.cpp src/Voice.cpp src/Stratum.cpp src/CounterpointUtils.cpp src/CounterpointProblems/CounterpointProblem.cpp src/Parts/Part.cpp src/Parts/FirstSpeciesCounterpoint.cpp \
		src/Parts/SecondSpeciesCounterpoint.cpp src/Parts/ThirdSpeciesCounterpoint.cpp src/Parts/FourthSpeciesCounterpoint.cpp src/Parts/FifthSpeciesCounterpoint.cpp src/Parts/CantusFirmus.cpp src/CounterpointProblems/TwoVoiceCounterpoint.cpp src/CounterpointProblems/ThreeVoiceCounterpoint.cpp src/CounterpointProblems/FourVoiceCounterpoint.cpp src/fuxTest.cpp src/constraints.cpp
# 	g++ -std=c++11 -F/Library/Frameworks -framework gecode -o Main src/Utilities.cpp src/Voice.cpp src/Stratum.cpp src/CounterpointUtils.cpp src/CounterpointProblems/CounterpointProblem.cpp src/Parts/Part.cpp src/Parts/FirstSpeciesCounterpoint.cpp \
# 		src/Parts/SecondSpeciesCounterpoint.cpp src/Parts/ThirdSpeciesCounterpoint.cpp src/Parts/CantusFirmus.cpp src/CounterpointProblems/TwoVoiceCounterpoint.cpp src/CounterpointProblems/ThreeVoiceCounterpoint.cpp src/CounterpointProblems/FourVoiceCounterpoint.cpp src/fuxTest.cpp src/constraints.cpp main.cpp
# 	./Main

# # #remove temporary files and dynamic library files
# # clean:
# # 	rm -f *.o *.so *.dylib ../sources/*.so ../sources/*.dylib Main

run_linux:
	g++ -std=c++11 main.cpp -o Main src/Utilities.cpp src/Voice.cpp src/Stratum.cpp src/CounterpointUtils.cpp src/CounterpointProblems/CounterpointProblem.cpp src/Parts/Part.cpp src/Parts/FirstSpeciesCounterpoint.cpp \
		src/Parts/SecondSpeciesCounterpoint.cpp src/Parts/ThirdSpeciesCounterpoint.cpp src/Parts/FourthSpeciesCounterpoint.cpp src/Parts/FifthSpeciesCounterpoint.cpp src/Parts/CantusFirmus.cpp src/CounterpointProblems/TwoVoiceCounterpoint.cpp src/CounterpointProblems/ThreeVoiceCounterpoint.cpp src/CounterpointProblems/FourVoiceCounterpoint.cpp src/fuxTest.cpp src/constraints.cpp -lgecodekernel -lgecodesearch -lgecodeint -lgecodedriver -lgecodeminimodel -lgecodesupport
	./Main

compile_for_debug:
	g++ -g -std=c++11 main.cpp -o Main src/Utilities.cpp src/Voice.cpp src/Stratum.cpp src/CounterpointUtils.cpp src/CounterpointProblems/CounterpointProblem.cpp src/Parts/Part.cpp src/Parts/FirstSpeciesCounterpoint.cpp \
		src/Parts/SecondSpeciesCounterpoint.cpp src/Parts/ThirdSpeciesCounterpoint.cpp src/Parts/FourthSpeciesCounterpoint.cpp src/Parts/FifthSpeciesCounterpoint.cpp src/Parts/CantusFirmus.cpp src/CounterpointProblems/TwoVoiceCounterpoint.cpp src/CounterpointProblems/ThreeVoiceCounterpoint.cpp src/CounterpointProblems/FourVoiceCounterpoint.cpp src/fuxTest.cpp src/constraints.cpp -lgecodekernel -lgecodesearch -lgecodeint -lgecodedriver -lgecodeminimodel -lgecodesupport





# Source directory
SRC_DIR = src

# Output directory
OUT_DIR = ../sources

# Find all .cpp files recursively in the SRC_DIR
SRCS = $(shell find $(SRC_DIR) -name '*.cpp')

# Object files
OBJS = $(patsubst %.cpp, %.o, $(SRCS))

# Compiler flags
CXXFLAGS = -std=c++11 -O3 -ffast-math -fPIC
LDFLAGS = -shared
LIBS = -lgecodedriver -lgecodeflatzinc -lgecodefloat -lgecodeint -lgecodekernel -lgecodeminimodel -lgecodesearch -lgecodeset -lgecodesupport

# Target
TARGET = $(OUT_DIR)/libgecode.so

# Default target
all: so dylib

# Compile the dynamic library for linux
so: $(TARGET)

# Link object files into a shared library
$(TARGET): $(OBJS)
	$(CXX) $(CXXFLAGS) -o $(TARGET) $(OBJS) $(LDFLAGS) $(LIBS)

# Compile .cpp files to .o files
$(SRC_DIR)/%.o: $(SRC_DIR)/%.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

# Clean
clean:
	find $(SRC_DIR) -name '*.o' -delete
	rm -f $(TARGET)
