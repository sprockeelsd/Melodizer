#ifndef OMT_BAR
#define OMT_BAR

#include<iostream>
#include<vector>
#include<string>

#include "Event.h"

class Bar{
    public:
        Bar(int new_startPosition, std::vector<Event> newNotes);

        int getStartPosition();
        
        std::vector<Event> getNotes();
        
        void print();
    /* 
        start position : position from the start of the music piece in beats(0 for the first bar, 4 for the second one,...)
        notes : the list of musical events in the bar
    */
    private:
        int startPosition;
        std::vector<Event> notes;

};

#endif