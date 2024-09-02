// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// This file contains the implementations of the main functions to create problems and counterpoints.  
//

#include "../headers/CounterpointUtils.hpp"


Part* create_counterpoint(Home home, int species, int nMeasures, vector<int> cantusFirmus, int lowerBound, int upperBound, Stratum* low,
    CantusFirmus* c, int v_type, vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV){
    switch(nV) {
        case TWO_VOICES :
            switch (species) { /// call the appropriate constructor for the counterpoint
            case FIRST_SPECIES:
                return new FirstSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm, TWO_VOICES);
                break;
            case SECOND_SPECIES:
                return new SecondSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm, TWO_VOICES);
                break;
            case THIRD_SPECIES:
                return new ThirdSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm, TWO_VOICES);
                break;
            case FOURTH_SPECIES:
                return new FourthSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm, TWO_VOICES);
                break;
            case FIFTH_SPECIES:
                return new FifthSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm, TWO_VOICES);
                break;
            default:
                throw std::invalid_argument("Species not implemented");
            }
            break;
        case THREE_VOICES :
            switch (species) { /// call the appropriate constructor for the counterpoint
            case FIRST_SPECIES:
                return new FirstSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm,
                    TWO_VOICES, THREE_VOICES);
                break;
            case SECOND_SPECIES:
                return new SecondSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm, 
                    TWO_VOICES, THREE_VOICES);
                break;
            case THIRD_SPECIES:
                return new ThirdSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm,
                    TWO_VOICES, THREE_VOICES);
                break;
            case FOURTH_SPECIES:
                return new FourthSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm,
                    TWO_VOICES, THREE_VOICES);
                break;
            case FIFTH_SPECIES:
                return new FifthSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm,
                    TWO_VOICES, THREE_VOICES);
                break;
            default:
                throw std::invalid_argument("Species not implemented");
            }
            break;
        case FOUR_VOICES :
            switch (species) { /// call the appropriate constructor for the counterpoint
            case FIRST_SPECIES:
                return new FirstSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm,
                    TWO_VOICES, THREE_VOICES, FOUR_VOICES);
                break;
            case SECOND_SPECIES:
                return new SecondSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm, 
                    TWO_VOICES, THREE_VOICES, FOUR_VOICES);
                break;
            case THIRD_SPECIES:
                return new ThirdSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm,
                    TWO_VOICES, THREE_VOICES, FOUR_VOICES);
                break;
            case FOURTH_SPECIES:
                return new FourthSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm,
                    TWO_VOICES, THREE_VOICES, FOUR_VOICES);
                break;
            case FIFTH_SPECIES:
                return new FifthSpeciesCounterpoint(home, nMeasures, cantusFirmus, lowerBound, upperBound, low, c, v_type, m_costs, g_costs, s_costs, bm,
                    TWO_VOICES, THREE_VOICES, FOUR_VOICES);
                break;
            default:
                throw std::invalid_argument("Species not implemented");
            }
            break;
        default :
            throw std::invalid_argument("Number of voices not implemented");
    }
};


CounterpointProblem* create_problem(vector<int> cf, vector<Species> spList, vector<int> v_type, vector<int> m_costs, vector<int> g_costs,
    vector<int> s_costs, vector<int> imp, int bm){

    switch (spList.size())
    {
    case 1:
        return new TwoVoiceCounterpoint(cf, spList[0], v_type[0], m_costs, g_costs, s_costs, imp, bm);
        break;
    case 2: 
        return new ThreeVoiceCounterpoint(cf, spList, v_type, m_costs, g_costs, s_costs, imp, bm); 
        break;
    case 3:
        return new FourVoiceCounterpoint(cf, spList, v_type, m_costs, g_costs, s_costs, imp, bm); 
        break;
    default:
        throw std::invalid_argument("The number of voices you asked for is not implemented (yet).");
        break;
    }
}

bool notInt(char* argv){
    bool noInt = false;
    for(int i = 0; i < strlen(argv); i++){
        if(!isdigit(argv[i])){
            noInt = true;
            break;
        }
    }
    return noInt;
}
