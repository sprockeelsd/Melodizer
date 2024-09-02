// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// 

#ifndef MYPROJECT_FOURVOICECOUNTERPOINT_HPP
#define MYPROJECT_FOURVOICECOUNTERPOINT_HPP

#include "../Utilities.hpp"
#include "CounterpointProblem.hpp"
#include "../Parts/FirstSpeciesCounterpoint.hpp"
#include "../Parts/SecondSpeciesCounterpoint.hpp"
#include "../Parts/ThirdSpeciesCounterpoint.hpp"
#include "../Parts/CantusFirmus.hpp"

/**
 * This class models a counterpoint problem with 4 voices.
 */
class FourVoiceCounterpoint : public CounterpointProblem{
protected:

    vector<Species> species;        /// the species of the counterpoints to generate

public:
    /**
     * Constructor of the class.
     * @param cf a vector<int> representing the cantus firmus.
     * @param sp the species of the counterpoint. it takes values from the enum "species" in headers/Utilities.hpp
     * @param k the key of the score. it takes values from the notes in headers/Utilities.hpp
     * @param lb the lowest note possible for the counterpoint in MIDI
     * @param ub the highest note possible for the counterpoint in MIDI
     */
    FourVoiceCounterpoint(vector<int> cf, vector<Species> sp, vector<int> v_type, vector<int> m_costs, vector<int> g_costs, 
        vector<int> s_costs, vector<int> imp, int bm);

    FourVoiceCounterpoint(FourVoiceCounterpoint& s);
    IntLexMinimizeSpace* copy() override; 

    string to_string() const override;

    /// Getters
    // Part* getCounterpoint(){ return counterpoint; }

    /// destructor : mother class destructor is sufficient, nothing to add

    void uniteCounterpoints();

    void uniteCosts();
};

#endif