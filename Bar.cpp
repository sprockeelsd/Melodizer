#include "Bar.h"
#include "Event.cpp"

using namespace std;

Bar::Bar(int new_startPosition, vector<Event> newNotes){
    startPosition = new_startPosition;
    notes = newNotes;
}

int Bar::getStartPosition(){
    return startPosition;
}
vector<Event> Bar::getNotes(){
    return notes;
}

void Bar::print(){
    std::cout << "starting position : " << startPosition << "\n" << "notes : \n";
    for(int i = 0; i < 4; i++){
        notes[i].print();
    }
    
}