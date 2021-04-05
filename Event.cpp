#include "Event.h"

using namespace std;

Event::Event(){
    pitch = 0;
    duration = 0;
    position = 0;
}

Event::Event(int new_pitch, int new_duration, int new_position){
    pitch = new_pitch;
    duration = new_duration;
    position = new_position;
}

Event::Event(const Event &event){
    pitch = event.pitch;
    duration = event.duration;
    position = event.position;
}

Event& Event::operator= (const Event &event)
{
    // do the copy
    pitch = event.pitch;
    duration = event.duration;
    position = event.position;
 
    // return the existing object so we can chain this operator
    return *this;
}

int Event::getPitch() {
    return pitch;
}

void Event::setPitch(int newPitch) {
    pitch = newPitch;
}

int Event::getDuration(){
    return duration;
}

void Event::setDuration(int newDuration) {
    pitch = newDuration;
}

int Event::getPosition(){
    return position;
}

void Event::setPosition(int newPosition) {
    pitch = newPosition;
}


void Event::print(){
    std::cout << "pitch : " << getPitch() << "\n duration : " << getDuration() << "\n position : " << getPosition() <<"\n";
}


