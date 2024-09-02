// 
// Created by Luc Cleenewerk and Diego de Patoul. 
//

#include "../headers/Stratum.hpp"

Stratum::Stratum(Home home, int nMes, int lb, int ub) : Voice(home, nMes, lb, ub){
    //notes = IntVarArray(home, nMes*4-3, 0, 127);
    //h_intervals = IntVarArray(home, notes.size(), -PERFECT_OCTAVE, PERFECT_OCTAVE);
    //m_intervals_brut = IntVarArray(home, notes.size()-1, -PERFECT_OCTAVE, PERFECT_OCTAVE);
}

//if the stratum is in the upper strata, this constructor will create the h_intervals to the lowest stratum
Stratum::Stratum(Home home, int nMes, int lb, int ub, IntVarArray lowestNotes) : Stratum(home, nMes, lb, ub){
    for(int i = 0; i < h_intervals.size(); i++){
        rel(home, (h_intervals[i])==((notes[i]-lowestNotes[i])%12));
    }
}

//this constructor applies 3rd voice specific constraints which apply to the upper strata
Stratum::Stratum(Home home, int nMes, int lb, int ub, IntVarArray lowestNotes, int nV) : Stratum(home, nMes, lb, ub, lowestNotes){

    //1.H3 (formerly G8) Last chord can only consist of notes of the harmonic triad
    dom(home, expr(home, abs(h_intervals[h_intervals.size()-1])), IntSet(IntArgs(TRIAD)));

    //H10 No tenths in last chord
    rel(home, ((notes[notes.size()-4]-lowestNotes[lowestNotes.size()-1])>12) >> (expr(home, abs(h_intervals[h_intervals.size()-1]))!=MINOR_THIRD && 
        expr(home, abs(h_intervals[h_intervals.size()-4]))!=MAJOR_THIRD));

    //H12 Last chord cannot include a minor third
    rel(home, expr(home, abs(h_intervals[h_intervals.size()-1])), IRT_NQ, 3);

}

Stratum::Stratum(Home home, int nMes, int lb, int ub, IntVarArray lowestNotes, int nV1, int nV2) : Stratum(home, nMes, lb, ub, lowestNotes){
    //1.H3 (formerly G8) Last chord can only consist of notes of the harmonic triad
    dom(home, expr(home, abs(h_intervals[h_intervals.size()-1])), IntSet(IntArgs(TRIAD)));

    //H12 Last chord cannot include a minor third
    rel(home, expr(home, abs(h_intervals[h_intervals.size()-1])), IRT_NQ, 3);
}


string Stratum::to_string() const {
    string text = "Notes : ";
    text += intVarArray_to_string(notes);
    text += "\n";
    text += "H intervals : ";
    text += intVarArray_to_string(h_intervals);
    text += "\n";
    text += "M intervals : ";
    text += intVarArray_to_string(m_intervals_brut);
    text += "\n";
    return text;
}


// clone constructor
Stratum::Stratum(Home home,  Stratum &s) : Voice(home, s){
    // update/clone here all variables that are not in voice
}

Stratum* Stratum::clone(Home home){
    return new Stratum(home, *this);
}

void Stratum::setNote(Home home, int index, IntVar note){
    rel(home, notes[index], IRT_EQ, note);
}