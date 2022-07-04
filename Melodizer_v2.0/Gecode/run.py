import os
import sys

def main(argv):
    name = argv[0]
    str1 = "g++ -std=c++11 -F/Library/Frameworks -O3 -c " + name + ".cpp"
    str2 = "g++ -std=c++11 -F/Library/Frameworks -framework gecode -o " + name + " " + name + ".cpp"
    str3 = "./" + name

    os.system(str1)
    os.system(str2)
    os.system(str3)

if __name__ == '__main__':
    main(sys.argv[1:])

os.system("ls -l")
