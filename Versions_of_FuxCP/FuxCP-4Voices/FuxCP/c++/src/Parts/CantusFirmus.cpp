//
// Created by Damien Sprockeels on 12/06/2024. 
// Extended and developed by Luc Cleenewerk and Diego de Patoul up to August 2024. 
//

#include "../../headers/Parts/CantusFirmus.hpp"

CantusFirmus::CantusFirmus(Home home, int size, vector<int> cf, Stratum* low, int v_type, vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int nV) :
    Part(home, size, CANTUS_FIRMUS, cf, 0, 127, v_type, m_costs, g_costs, s_costs, nV, -1){
    cf_vector = cf;
    notes = IntVarArray(home, size, lowerBound, upperBound);
    /// Melodic intervals for the first species notes
    m_intervals_brut = IntVarArray(home, notes.size()-1, -PERFECT_OCTAVE, PERFECT_OCTAVE);
    h_intervals = IntVarArray(home, size, -PERFECT_OCTAVE, PERFECT_OCTAVE);
    ///link melodic intervals
    for(int i = 0; i < m_intervals_brut.size(); i++)
        rel(home, m_intervals_brut[i], IRT_EQ, expr(home, notes[i+1] - notes[i]));
    for(int i = 0; i < size; i++){
        rel(home, h_intervals[i], IRT_EQ, expr(home, (notes[i]-low->getFirstNotes()[i])%12));
    }

    for(int i = 0; i < size; i++)
        rel(home, notes[i], IRT_EQ, cf_vector[i]);

    motions = IntVarArray(home, notes.size()-1, IntSet{-1, CONTRARY_MOTION, OBLIQUE_MOTION, PARALLEL_MOTION});
    //create motions
    
    for(int i = 0; i < motions.size(); i++){
        //direct motions help creation
        
        BoolVar both_up = expr(home, (m_intervals_brut[i]>0)&&(low->getMelodicIntervals()[i]>0)); //if both parts are going in the same direction
        BoolVar both_stay = expr(home, (m_intervals_brut[i]==0)&&(low->getMelodicIntervals()[i]==0)); //if both parts are staying
        BoolVar both_down = expr(home, (m_intervals_brut[i]<0)&&(low->getMelodicIntervals()[i]<0)); //if both parts are going down
        //oblique motions help creation
        BoolVar cf_stays_1 = expr(home, (m_intervals_brut[i]>0)&&(low->getMelodicIntervals()[i]==0)); //if the lowest part stays and one goes up
        BoolVar cf_stays_2 = expr(home, (m_intervals_brut[i]<0)&&(low->getMelodicIntervals()[i]==0)); //if the lowest part stays and one goes down
        BoolVar cp_stays_1 = expr(home, (m_intervals_brut[i]==0)&&(low->getMelodicIntervals()[i]>0)); //if the lowest part goes up and one stays
        BoolVar cp_stays_2 = expr(home, (m_intervals_brut[i]==0)&&(low->getMelodicIntervals()[i]<0)); //if the lowest part goes down and one stays
        //contrary motions help creation
        BoolVar cpd_cfu = expr(home, (m_intervals_brut[i]<0)&&(low->getMelodicIntervals()[i]>0)); //if the cf goes up and the cp down
        BoolVar cpu_cfd = expr(home, (m_intervals_brut[i]>0)&&(low->getMelodicIntervals()[i]<0)); //if the cf goes down and the cp up

        //direct constraints
        rel(home, ((both_up || both_stay || both_down) && (isNotLowest[i]==1)) >> (motions[i]==PARALLEL_MOTION));
        //oblique constraints
        rel(home, ((cf_stays_1 || cf_stays_2 || cp_stays_1 || cp_stays_2) && (isNotLowest[i]==1)) >> (motions[i]==OBLIQUE_MOTION));
        //contrary constraints
        rel(home, ((cpd_cfu || cpu_cfd) && (isNotLowest[i]==1)) >> (motions[i]==CONTRARY_MOTION));
        //bass constraints
        rel(home, (isNotLowest[i]==0) >> (motions[i]==-1));
    }

    dom(home, h_intervals, IntSet({UNISSON, MINOR_THIRD, MAJOR_THIRD, PERFECT_FIFTH, MINOR_SIXTH, MAJOR_SIXTH, PERFECT_OCTAVE, 
        -MINOR_THIRD, -MAJOR_THIRD, -PERFECT_FIFTH, -MINOR_SIXTH, -MAJOR_SIXTH, -PERFECT_OCTAVE}));

    if(nV==TWO_VOICES){
        /// H2 from Thibault: The first harmonic interval must be a perfect consonance
        dom(home, h_intervals[0], IntSet(IntArgs(PERFECT_CONSONANCES)));

        /// H3 from Thibault: The last harmonic interval must be a perfect consonance
        dom(home, h_intervals[h_intervals.size()-1], IntSet(IntArgs(PERFECT_CONSONANCES)));
        
        //H7,H8 cf version
        rel(home, h_intervals[h_intervals.size()-2], IRT_EQ, MINOR_THIRD, Reify(isNotLowest[isNotLowest.size()-2], RM_IMP));

        //P1 from Thibault : Perfect consonances cannot be reached by direct motion
        for(int j = 0; j < motions.size(); j++){

            rel(home, ((h_intervals[j+1]==UNISSON || h_intervals[j+1]==PERFECT_FIFTH)&&isNotLowest[j+1]==1) >>
                (motions[j]!=PARALLEL_MOTION));

        }
    } else {
        //H7,H8 cf version, 3v adapted
        rel(home, expr(home, abs(h_intervals[h_intervals.size()-2]))==UNISSON||expr(home, abs(h_intervals[h_intervals.size()-2]))==MINOR_THIRD
            ||expr(home, abs(h_intervals[h_intervals.size()-2]))==PERFECT_FIFTH||expr(home, abs(h_intervals[h_intervals.size()-2]))==MAJOR_SIXTH);
    }

    //No battuta
    P3_0_noBattuta(home, this);
}

string CantusFirmus::to_string() const {
    string text = "Cantus Firmus notes : ";
    text += intVarArray_to_string(notes);
    text += "\n";
    text += "H intervals brut : ";
    text += intVarArray_to_string(h_intervals);
    text += "\n";
    text += "Motions : ";
    text += intVarArray_to_string(motions);
    text += "\n";
    text += "is Lowest : ";
    text += boolVarArray_to_string(isNotLowest);
    text += "\n";
    text += "is Highest : ";
    text += boolVarArray_to_string(isHighest);
    text += "\n";
    return text;
}


// clone constructor
CantusFirmus::CantusFirmus(Home home, CantusFirmus &s) : Part(home, s){
    
}

CantusFirmus* CantusFirmus::clone(Home home){
    return new CantusFirmus(home, *this);
}

IntVarArray CantusFirmus::getFirstHInterval(){
    return h_intervals;
}

IntVarArray CantusFirmus::getMotions(){
    return motions;
}

IntVarArray CantusFirmus::getFirstMInterval(){
    return m_intervals_brut;
}

IntVarArgs CantusFirmus::getFirstNotes(){
    return notes.slice(0, 4/4, notes.size());
}

int CantusFirmus::getHIntervalSize(){
    return h_intervals.size();
}