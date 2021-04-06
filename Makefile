all:macOS

macOS:
	g++ -std=c++11 -F/Library/Frameworks -c openMusicTool.cpp

	g++ -std=c++11 -F/Library/Frameworks -framework gecode -o openMusicTool openMusicTool.cpp

	./OpenMusicTool

wvsc:
	cl /DNDEBUG /EHsc /MD /0x /wd4355 -I "C:\Program Files\Gecode\include" -c FoOpenMusicTool.obj -TpOpenMusicTool.cpp

	cl /DNDEBUG /EHsc /MD /0x /wd4355 -I "C:\Program Files\Gecode\include" -FeOpenMusicTool.exe OpenMusicTool.obj /link /LIBPATH:"C:\Program Files\Gecode\lib"