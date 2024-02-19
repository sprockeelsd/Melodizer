#!/bin/sh

    #This program makes sure that the dynamic library can find gecode
    #adapt this to your gecode location

    #go to the appropriate file
    cd ..
    cd sources

    #update the path
    #in MacOS 10.15.7, there was a change in the security and the relative path to gecode didn't allow the dynamic library to
    #correctly locate gecode. This script allows to change the rpath to the full path so gecode can be found.
    install_name_tool -change gecode.framework/Versions/49/gecode /Library/Frameworks/gecode.framework/Versions/49/gecode libgecode.dylib
