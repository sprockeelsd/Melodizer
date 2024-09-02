//
// Created by Damien Sprockeels on 11/06/2024.
// Extended and developed by Luc Cleenewerk and Diego de Patoul up to August 2024. 
//

#include "../../headers/Parts/Part.hpp"

/// This class represents a part, so it creates all the variables associated to that part and posts the constraints that are species independent
Part::Part(Home home, int nMes, Species sp, vector<int> cf, int lb, int ub, int v_type, vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, 
    int nV, int bm) : 
    Voice(home, nMes, lb, ub){
    nVoices         = nV;
    species = sp;
    borrowMode      = bm;
    voice_type      = v_type;
    //lowest          = low;
    isNotLowest        = BoolVarArray(home, nMeasures, 0, 1);
    isHighest          = BoolVarArray(home, nMeasures, 0, 1);

    borrowed_scale = get_all_notes_from_scale(cf[0]%12, BORROWED_SCALE);
    scale = get_all_notes_from_scale(cf[0]%12, MAJOR_SCALE);
    chromatic_scale = get_all_notes_from_scale(cf[0]%12, CHROMATIC_SCALE);
    cp_range = {};

    secondCost = m_costs[0];
    thirdCost = m_costs[1];
    fourthCost = m_costs[2];
    tritoneCost = m_costs[3];
    fifthCost = m_costs[4];
    sixthCost = m_costs[5];
    seventhCost = m_costs[6];
    octaveCost = m_costs[7];

    borrowCost = g_costs[0];
    h_fifthCost = g_costs[1];
    h_octaveCost = g_costs[2];
    succCost = g_costs[3];
    varietyCost = g_costs[4];
    triadCost = g_costs[5];
    directMoveCost = g_costs[6];
    penultCost = g_costs[7];

    penultSixthCost = s_costs[0];
    cambiataCost = s_costs[1];
    mSkipCost = s_costs[2];
    triad3rdCost = s_costs[3];
    m2ZeroCost = s_costs[4];
    syncopationCost = s_costs[5];
    prefSlider = s_costs[6];

    directCost = 0;
    obliqueCost = 1;
    contraryCost = 2;

}

string Part::to_string() const{
    string part = "Part characteristics :\n";
    part += "Part costs : \n";
    part += intVarArray_to_string(costs);
    part += "\n";
    return part;
}




Part::Part(Home home, Part& s) : Voice(home, s) {
    nVoices = s.nVoices;
    borrowMode = s.borrowMode;
    
    borrowed_scale = s.borrowed_scale;
    scale = s.scale;
    chromatic_scale = s.chromatic_scale;
    species = s.species;
    voice_type = s.voice_type;
    
    cp_range = s.cp_range;

    extended_domain = s.extended_domain;
    off_domain = s.off_domain;

    secondCost = s.secondCost;
    thirdCost = s.thirdCost;
    fourthCost = s.fourthCost;
    tritoneCost = s.tritoneCost;
    fifthCost = s.fifthCost;
    sixthCost = s.sixthCost;
    seventhCost = s.seventhCost;
    octaveCost = s.octaveCost;

    borrowCost = s.borrowCost;
    h_fifthCost = s.h_fifthCost;
    h_octaveCost = s.h_octaveCost;
    succCost = s.succCost;
    varietyCost = s.varietyCost;
    triadCost = s.triadCost;
    directMoveCost = s.directMoveCost;
    penultCost = s.penultCost;

    penultSixthCost = s.penultSixthCost;
    cambiataCost = s.cambiataCost;
    mSkipCost = s.mSkipCost;
    triad3rdCost = s.triad3rdCost;
    m2ZeroCost = s.m2ZeroCost;
    syncopationCost = s.syncopationCost;
    prefSlider = s.prefSlider;

    directCost = s.directCost;
    obliqueCost = s.obliqueCost;
    contraryCost = s.contraryCost;

    cost_names = s.cost_names;

    melodicDegreeCost.update(home, s.melodicDegreeCost);
    fifthCostArray.update(home, s.fifthCostArray);
    octaveCostArray.update(home, s.octaveCostArray);
    is_off.update(home, s.is_off);
    offCostArray.update(home, s.offCostArray);
    costs.update(home, s.costs);
    varietyCostArray.update(home, s.varietyCostArray);
    directCostArray.update(home, s.directCostArray);
    isConsonance.update(home, s.isConsonance);
    isNotLowest.update(home, s.isNotLowest);
    isHighest.update(home, s.isHighest);

    firstSpeciesHarmonicIntervals.update(home, s.firstSpeciesHarmonicIntervals);
    firstSpeciesNotesCp.update(home, s.firstSpeciesNotesCp);
    firstSpeciesMelodicIntervals.update(home, s.firstSpeciesMelodicIntervals);
    firstSpeciesMotions.update(home, s.firstSpeciesMotions);
    firstSpeciesMotionCosts.update(home, s.firstSpeciesMotionCosts);

    isDiminution.update(home, s.isDiminution);
    penultCostArray.update(home, s.penultCostArray);
    secondSpeciesMotions.update(home, s.secondSpeciesMotions);
    secondSpeciesMelodicIntervals.update(home, s.secondSpeciesMelodicIntervals);
    secondSpeciesRealMotions.update(home, s.secondSpeciesRealMotions);

    is5QNArray.update(home, s.is5QNArray);
    thirdSpeciesHarmonicIntervals.update(home, s.thirdSpeciesHarmonicIntervals);
    thirdSpeciesMelodicIntervals.update(home, s.thirdSpeciesMelodicIntervals);
    cambiataCostArray.update(home, s.cambiataCostArray);

    isNoSyncopeArray.update(home, s.isNoSyncopeArray);
    snycopeCostArray.update(home, s.snycopeCostArray);

    speciesArray.update(home, s.speciesArray);

}

// Virtual clone function
Part* Part::clone(Home home) {
    return new Part(home, *this);
}

IntVarArray Part::getBranchingNotes(){
    return notes;
}

IntVarArray Part::getPartNotes(){
    return notes;
}

int Part::getSuccCost(){
    return succCost;
}

IntVarArray Part::getFirstHInterval(){
    return h_intervals;
}

IntVarArray Part::getMotions(){
    return motions;
}

IntVarArray Part::getFirstMInterval(){
    return m_intervals_brut;
}

IntVarArray Part::getCosts(){
    return costs;
}

int Part::getVarietyCost(){
    return varietyCost;
}

IntVar Part::getVarietyArray(int idx){
    return varietyCostArray[idx];
}

int Part::getHIntervalSize(){
    return h_intervals.size();
}

int Part::getTriadCost(){
    return triadCost;
}

int Part::getSecondCost(){
    return secondCost;
}
int Part::getThirdCost(){
    return thirdCost;
}
int Part::getFourthCost(){
    return fourthCost;
}
int Part::getTritoneCost(){
    return tritoneCost;
}
int Part::getFifthCost(){
    return fifthCost;
}
int Part::getSixthCost(){
    return sixthCost;
}
int Part::getSeventhCost(){
    return seventhCost;
}
int Part::getOctaveCost(){
    return octaveCost;
}

IntVarArray Part::getMelodicDegreeCost(){
    return melodicDegreeCost;
}

vector<string> Part::getCostNames(){
    return cost_names;
}

BoolVarArray Part::getConsonance(){
    return isConsonance;
}

void Part::add_cost(Home home, int idx, IntVarArray to_be_added, IntVarArray costs){
    int sz = to_be_added.size();
    IntVarArgs args(sz);
    for(int i = 0; i < sz; i++){
        args[i] = to_be_added[i];
    }
    rel(home, costs[idx], IRT_EQ, expr(home, sum(args)));
}

BoolVarArray Part::getIsNotLowest(){
    return isNotLowest;
}

IntVarArray Part::getFirstSpeciesHIntervals(){
    return firstSpeciesHarmonicIntervals;
}

IntVarArray Part::getFirstSpeciesNotes(){
    return firstSpeciesNotesCp;
}

IntVarArray Part::getFirstSpeciesMIntervals(){
    return firstSpeciesMelodicIntervals;
}

IntVarArray Part::getFifthCostArray(){
    return fifthCostArray;
}

IntVarArray Part::getOctaveCostArray(){
    return octaveCostArray;
}

int Part::getHFifthCost(){
    return h_fifthCost;
}

int Part::getHOctaveCost(){
    return h_octaveCost;
}

IntVarArray Part::getFirstSpeciesMotions(){
    return firstSpeciesMotions;
}

IntVarArray Part::getDirectCostArray(){
    return directCostArray;
}

int Part::getDirectCost(){
    return directCost;
}

BoolVarArray Part::getIsOffArray(){
    return is_off;
}

vector<int> Part::getOffDomain(){
    return off_domain;
}

vector<int> Part::getExtendedDomain(){
    return extended_domain;
}

IntVarArray Part::getOffCostArray(){
    return offCostArray;
}

int Part::getBorrowCost(){
    return borrowCost;
}

BoolVarArray Part::getIsDiminution(){
    return isDiminution;
}

int Part::getPenultCost(){
    return penultCost;
}

IntVarArray Part::getPenultCostArray(){
    return penultCostArray;
}

IntVarArray Part::getSecondSpeciesMotions(){
    return secondSpeciesMotions;
}

IntVarArray Part::getSecondSpeciesMIntervals(){
    return secondSpeciesMelodicIntervals;
}

IntVarArray Part::getSecondSpeciesRealMotions(){
    return secondSpeciesRealMotions;
}

int Part::getDirectMoveCost(){
    return directMoveCost;
}

BoolVarArray Part::getIs5QNArray(){
    return is5QNArray;
}

IntVarArray Part::getThirdSpeciesHIntervals(){
    return thirdSpeciesHarmonicIntervals;
}

IntVarArray Part::getThirdSpeciesMIntervals(){
    return thirdSpeciesMelodicIntervals;
}

int Part::getCambiataCost(){
    return cambiataCost;
}

IntVarArray Part::getCambiataCostArray(){
    return cambiataCostArray;
}

BoolVarArray Part::getNoSyncope(){
    return isNoSyncopeArray;
}

IntVarArray Part::getSyncopeCostArray(){
    return snycopeCostArray;
}

IntVarArray Part::getSpeciesArray(){
    return speciesArray;
}

BoolVarArray Part::getIsHighest(){
    return isHighest;
}