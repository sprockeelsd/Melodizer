// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// This file contains the declarations of the main functions to create problems and counterpoints.  
// 

#ifndef COUNTERPOINTUTILS_HPP
#define COUNTERPOINTUTILS_HPP

#include "Utilities.hpp"
#include "CounterpointProblems/TwoVoiceCounterpoint.hpp"
#include "CounterpointProblems/ThreeVoiceCounterpoint.hpp"
#include "CounterpointProblems/FourVoiceCounterpoint.hpp"
#include "Parts/FirstSpeciesCounterpoint.hpp"
#include "Parts/SecondSpeciesCounterpoint.hpp"
#include "Parts/ThirdSpeciesCounterpoint.hpp"
#include "Parts/FourthSpeciesCounterpoint.hpp"
#include "Parts/FifthSpeciesCounterpoint.hpp"

using namespace std;
using namespace Gecode;


/**
 * This function creates the appropriate Part given the species of the counterpoint requested. 
 * @return a pointer to a Part object
*/
Part* create_counterpoint(Home home, int species, int nMeasures, vector<int> cantusFirmus, int lowerBound, int upperBound, Stratum* low,
    CantusFirmus* c, int v_type, vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV);

/**
 * This function creates the appropriate counterpoint problem given the number of counterpoints (size of the species list) requested. 
 * @return a pointer to a counterpoint problem object
*/
CounterpointProblem* create_problem(vector<int> cf, vector<Species> sp, vector<int> v_type, vector<int> m_costs, vector<int> g_costs,
    vector<int> s_costs, vector<int> imp, int bm);

bool notInt(char* argv);



#endif