#ifndef OMT_EVENT
#define OMT_EVENT

#include<iostream>
#include<vector>
#include<string>

class Event{
public:
    Event();//nécéssaire pour faire un vector de Event
    Event(int new_pitch, int new_duration, int new_position);
    Event(const Event &event); //nécéssaire pour faire un vector de Event
    //overload the assignment operator
    Event& operator= (const Event &event);

    int getPitch();
    void setPitch(int newPitch);

    int getDuration();
    void setDuration(int newDuration);

    int getPosition();
    void setPosition(int setPosition);

    void print();

private:
    /* 
        pitch : value between 21 (A0) and 108 (C8)
        duration : rythmic proportion in beats( in 4/4, 1 is a black note, 2 a white note,... )
        position : the position from the beginning of the bar(0 = first beat, 3 = last beat)
    */
    int pitch;
    int duration;
    int position;
};

#endif