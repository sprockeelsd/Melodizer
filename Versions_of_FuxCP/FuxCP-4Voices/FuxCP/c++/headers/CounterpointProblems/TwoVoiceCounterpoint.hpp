//
// Created by Damien Sprockeels on 13/06/2024.
// Extended and developed by Luc Cleenewerk and Diego de Patoul up to August 2024. 
//

#ifndef MYPROJECT_TWOVOICECOUNTERPOINT_HPP
#define MYPROJECT_TWOVOICECOUNTERPOINT_HPP

#include "../Utilities.hpp"
#include "CounterpointProblem.hpp"
#include "../Parts/FirstSpeciesCounterpoint.hpp"
#include "../Parts/SecondSpeciesCounterpoint.hpp"
#include "../Parts/CantusFirmus.hpp"
#include "../constraints.hpp"

/**
 * This class models a counterpoint problem with 2 voices.
 */
class TwoVoiceCounterpoint : public CounterpointProblem{
protected:
    Species species;        /// the species of the counterpoint to generate

public:
    /**
     * Constructor of the class.
     * @param cf a vector<int> representing the cantus firmus.
     * @param sp the species of the counterpoint. it takes values from the enum "species" in headers/Utilities.hpp
     * @param k the key of the score. it takes values from the notes in headers/Utilities.hpp
     * @param lb the lowest note possible for the counterpoint in MIDI
     * @param ub the highest note possible for the counterpoint in MIDI
     */
    TwoVoiceCounterpoint(vector<int> cf, Species sp, int v_type, vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, vector<int> imp, int bm);

    TwoVoiceCounterpoint(TwoVoiceCounterpoint& s);
    IntLexMinimizeSpace* copy() override; 

    string to_string() const override; 

    

    ///destructor : mother class destructor is sufficient, nothing to add

};

#endif //MYPROJECT_TWOVOICECOUNTERPOINT_HPP