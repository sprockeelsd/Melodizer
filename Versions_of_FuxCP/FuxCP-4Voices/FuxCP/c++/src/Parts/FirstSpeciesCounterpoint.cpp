//
// Created by Damien Sprockeels on 11/06/2024.
// Extended and developed by Luc Cleenewerk and Diego de Patoul up to August 2024. 
//

#include "../../headers/Parts/FirstSpeciesCounterpoint.hpp"

/**
 * GENERAL CONSTRUCTOR
 */

FirstSpeciesCounterpoint::FirstSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub, Species mSpecies, Stratum* low, CantusFirmus* c,
     int v_type, vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV):
        Part(home, nMes, mSpecies, cf, lb, ub, v_type, m_costs, g_costs, s_costs, nV, bm) { /// super constructor

    motherSpecies =         mSpecies;
    for(int i = lowerBound; i <= upperBound; i++){
        cp_range.push_back(i);
    }
    /*
    if borrowMode is enabled, the extended domain is extended to make the inclusion of borrowed notes possible. We can see from Fux's examples
    that he does like to borrow notes, so the borrow cost should just do the job and we should still allow borrowed notes, not outright forbid them
    */
    if(borrowMode==1){
        extended_domain = vector_union(cp_range, vector_union(scale, borrowed_scale));
    } else {
        extended_domain = vector_intersection(cp_range, vector_union(scale, borrowed_scale));
    }
    off_domain = vector_difference(vector_intersection(cp_range, scale), lowerBound, upperBound);

    /// First species notes in the counterpoint
    firstSpeciesNotesCp = IntVarArray(home, nMeasures * notesPerMeasure.at(FIRST_SPECIES), IntSet(IntArgs(vector_intersection(cp_range, extended_domain))));

    if(borrowMode==1 && motherSpecies==FIRST_SPECIES){
        firstSpeciesNotesCp[firstSpeciesNotesCp.size()-2] = IntVar(home, IntSet(IntArgs(vector_intersection(cp_range, chromatic_scale))));
    } else {
        firstSpeciesNotesCp[firstSpeciesNotesCp.size()-2] = IntVar(home, IntSet(IntArgs(vector_intersection(cp_range, extended_domain))));
    }

    rel(home, firstSpeciesNotesCp, IRT_EQ, notes.slice(0,4/notesPerMeasure.at(FIRST_SPECIES),notes.size()));
    
    /// Harmonic intervals for the first species notes
    firstSpeciesHarmonicIntervals = IntVarArray(home, nMeasures* notesPerMeasure.at(FIRST_SPECIES), -PERFECT_OCTAVE, PERFECT_OCTAVE);
    
    for(int i = 0; i < firstSpeciesHarmonicIntervals.size(); i++){
        rel(home, (firstSpeciesHarmonicIntervals[i])==((firstSpeciesNotesCp[i]-low->getNotes()[i*4])%12));
    }

    rel(home, firstSpeciesHarmonicIntervals, IRT_EQ, h_intervals.slice(0,4/notesPerMeasure.at(FIRST_SPECIES),h_intervals.size()));
    
    /// Melodic intervals for the first species notes
    firstSpeciesMelodicIntervals = IntVarArray(home, nMeasures* notesPerMeasure.at(FIRST_SPECIES) -1, -PERFECT_OCTAVE, PERFECT_OCTAVE);
    for(int i = 0; i < firstSpeciesMelodicIntervals.size(); i++)
      rel(home, firstSpeciesMelodicIntervals[i], IRT_EQ, expr(home, firstSpeciesNotesCp[i+1] - firstSpeciesNotesCp[i]));
    
    //create the is off array which determines if a notes is borrowed or not
    is_off = BoolVarArray(home, notes.size(), 0, 1);
    initializeIsOffArray(home, this);

    //create off_cost array
    offCostArray = IntVarArray(home, is_off.size(), IntSet({0, borrowCost}));
    
    //create the melodic degree cost array
    melodicDegreeCost = IntVarArray(home, m_intervals_brut.size(), IntSet({secondCost, thirdCost, fourthCost, tritoneCost, fifthCost, 
        sixthCost, seventhCost, octaveCost}));

    //create motions arrays
    firstSpeciesMotions = IntVarArray(home, nMeasures* notesPerMeasure.at(FIRST_SPECIES) -1, IntSet{-1, CONTRARY_MOTION, OBLIQUE_MOTION, PARALLEL_MOTION});
    firstSpeciesMotionCosts = IntVarArray(home, firstSpeciesMotions.size(), IntSet{0, directCost, obliqueCost, contraryCost});

    //create motions

    for(int i = 0; i < firstSpeciesMotions.size(); i++){

        //direct motions help creation
        BoolVar both_up = expr(home, (firstSpeciesMelodicIntervals[i]>0)&&(low->getMelodicIntervals()[i]>0)); //if both parts are going in the same direction
        BoolVar both_stay = expr(home, (firstSpeciesMelodicIntervals[i]==0)&&(low->getMelodicIntervals()[i]==0)); //if both parts are staying
        BoolVar both_down = expr(home, (firstSpeciesMelodicIntervals[i]<0)&&(low->getMelodicIntervals()[i]<0)); //if both parts are going down
        //oblique motions help creation
        BoolVar cf_stays_1 = expr(home, (firstSpeciesMelodicIntervals[i]>0)&&(low->getMelodicIntervals()[i]==0)); //if the lowest part stays and one goes up
        BoolVar cf_stays_2 = expr(home, (firstSpeciesMelodicIntervals[i]<0)&&(low->getMelodicIntervals()[i]==0)); //if the lowest part stays and one goes down
        BoolVar cp_stays_1 = expr(home, (firstSpeciesMelodicIntervals[i]==0)&&(low->getMelodicIntervals()[i]>0)); //if the lowest part goes up and one stays
        BoolVar cp_stays_2 = expr(home, (firstSpeciesMelodicIntervals[i]==0)&&(low->getMelodicIntervals()[i]<0)); //if the lowest part goes down and one stays
        //contrary motions help creation
        BoolVar cpd_cfu = expr(home, (firstSpeciesMelodicIntervals[i]<0)&&(low->getMelodicIntervals()[i]>0)); //if the cf goes up and the cp down
        BoolVar cpu_cfd = expr(home, (firstSpeciesMelodicIntervals[i]>0)&&(low->getMelodicIntervals()[i]<0)); //if the cf goes down and the cp up

        //direct constraints
        rel(home, ((both_up || both_stay || both_down) && (this->isNotLowest[i]==1)) >> (firstSpeciesMotions[i]==PARALLEL_MOTION));
        rel(home, ((both_up || both_stay || both_down) && (this->isNotLowest[i]==1)) >> (firstSpeciesMotionCosts[i]==directCost));
        //oblique constraints
        rel(home, ((cf_stays_1 || cf_stays_2 || cp_stays_1 || cp_stays_2) && (this->isNotLowest[i]==1)) >> (firstSpeciesMotions[i]==OBLIQUE_MOTION));
        rel(home, ((cf_stays_1 || cf_stays_2 || cp_stays_1 || cp_stays_2) && (this->isNotLowest[i]==1)) >> (firstSpeciesMotionCosts[i]==obliqueCost));
        //contrary constraints
        rel(home, ((cpd_cfu || cpu_cfd) && (this->isNotLowest[i]==1)) >> (firstSpeciesMotions[i]==CONTRARY_MOTION));
        rel(home, ((cpd_cfu || cpu_cfd) && (this->isNotLowest[i]==1)) >> (firstSpeciesMotionCosts[i]==contraryCost));
        //bass constraints
        rel(home, (this->isNotLowest[i]==0) >> (firstSpeciesMotions[i]==-1));
        rel(home, (this->isNotLowest[i]==0) >> (firstSpeciesMotionCosts[i]==0));

    }
    
    // create pefectConsArray
    fifthCostArray = IntVarArray(home, h_intervals.size(), IntSet({0, h_fifthCost}));
    octaveCostArray = IntVarArray(home, h_intervals.size(), IntSet({0, h_octaveCost}));

    isConsonance = BoolVarArray(home, h_intervals.size(), 0, 1);
    //this loop checks that every harmonic interval is a consonance or not
    for(int i = 0; i < isConsonance.size(); i++){
        rel(home, expr(home, (h_intervals[i]==UNISSON)||(h_intervals[i]==MINOR_THIRD)||(h_intervals[i]==MAJOR_THIRD)||(h_intervals[i]==PERFECT_FIFTH)||
            (h_intervals[i]==MINOR_SIXTH)||(h_intervals[i]==MAJOR_SIXTH)||(h_intervals[i]==PERFECT_OCTAVE)), IRT_EQ, isConsonance[i]);
    }
    /// General rules

    //G4 : 
    G4_counterpointMustBeInTheSameKey(home, this);

    // G7 : melodic intervals should be small (works for 1st, 2nd and 3rd species)
    G7_melodicIntervalsShouldBeSmall(home, this, motherSpecies);
    
    /// Harmonic rules
    /// H1 from Thibault: All harmonic intervals must be consonances
    H1_1_harmonicIntervalsAreConsonances(home, this);
    
    //H2 and H3 are found in the TwoVoiceCounterpoint class, since these rules are 2 voice specific

    // H4 : applied as rule G9 in the Two, Three and FourVoice Counterpointproblem

    // H6 from Thibault : Imperfect consonances are preferred
    H6_1_preferImperfectConsonances(home, this);
}

/**
 * 2 VOICES CONSTRUCTOR
 */

FirstSpeciesCounterpoint::FirstSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub, Stratum* low, CantusFirmus* c, int v_type
    , vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV) :
        FirstSpeciesCounterpoint(home, nMes, cf, lb, ub, FIRST_SPECIES, low, c, v_type, m_costs, g_costs, s_costs, bm, nV) ///call the general constructor
{
    rel(home, firstSpeciesMelodicIntervals, IRT_EQ, m_intervals_brut.slice(0,4/notesPerMeasure.at(FIRST_SPECIES),m_intervals_brut.size()));

    //H7,H8 from Thibault : penultimate note major sixth or minor third
    H7_1_2v_penultimateSixthOrThird(home, this);

    //1.M2 from Thibault: Melodic intervals cannot exceed a minor sixth
    M1_1_2v_melodicIntervalsNotExceedMinorSixth(home, this);

    // Motion rules
    //1.P1 from Thibault : Perfect consonances cannot be reached by direct motion
    P1_1_2v_noDirectMotionFromPerfectConsonance(home, this);

    //P2 from Thibault : already done when creating motions array
    
    //P3 from Thibault : no battuta
    P3_1_noBattuta(home, this);

    costs = IntVarArray(home, 5, 0, 1000000);
    cost_names = {"fifth", "octave", "motion", "melodic", "borrow"};

    //set cost[0] to be fifth cost
    add_cost(home, 0, IntVarArray(home, fifthCostArray.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), fifthCostArray.size())), costs);
    //set cost[1] to be octave cost
    add_cost(home, 1, IntVarArray(home, octaveCostArray.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), octaveCostArray.size())), costs);
    //set cost[2] to be motion cost
    add_cost(home, 2, firstSpeciesMotionCosts, costs);
    //set cost[3] to be melodic cost
    add_cost(home, 3, IntVarArray(home, melodicDegreeCost.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), melodicDegreeCost.size())), costs);
    //set cost[4] to be off cost
    add_cost(home, 4, IntVarArray(home, offCostArray.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), offCostArray.size())), costs);
}

/**
 * 3 VOICES CONSTRUCTOR
 */

FirstSpeciesCounterpoint::FirstSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub,  Stratum* low, CantusFirmus* c,  int v_type, 
    vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV1, int nV2) : 
    FirstSpeciesCounterpoint(home, nMes, cf, lb, ub, FIRST_SPECIES, low, c, v_type, m_costs, g_costs, s_costs, bm, nV2) ///call the general constructor
{
    rel(home, firstSpeciesMelodicIntervals, IRT_EQ, m_intervals_brut.slice(0,4/notesPerMeasure.at(FIRST_SPECIES),m_intervals_brut.size()));

    varietyCostArray = IntVarArray(home, 3*(firstSpeciesHarmonicIntervals.size()-2), IntSet({0, varietyCost}));
    directCostArray = IntVarArray(home, firstSpeciesMotions.size()-1,IntSet({0, directMoveCost}));

    //1.H7 -- after careful testing, this constraint does not work Fux's examples
    //H7_1_3v_penultimateSixthOrThird(home, this);
   
    //1.M2 from Thibault: Melodic intervals cannot exceed a minor sixth (also include octave?)
    M1_1_3v_melodicIntervalsNotExceedMinorSixth(home, this);

    //1.P1
    P1_1_3v_noDirectMotionFromPerfectConsonance(home, this);

    //1.P3 from Thibault : no battuta
    P3_1_noBattuta(home, this);

    costs = IntVarArray(home, 7, 0, 1000000);
    cost_names = {"borrow", "fifth", "octave", "variety", "motion", "melodic", "direct"};
    //need to set cost[0] to be off cost
    add_cost(home, 0, IntVarArray(home, offCostArray.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), offCostArray.size())), costs);
    //set cost[1] to be fifth cost
    add_cost(home, 1, IntVarArray(home, fifthCostArray.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), fifthCostArray.size())), costs);
    //set cost[2] to be octave cost
    add_cost(home, 2, IntVarArray(home, octaveCostArray.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), octaveCostArray.size())), costs);
    //need to set cost[3] to be variety cost
    add_cost(home, 3, varietyCostArray, costs);
    //set cost[4] to be motion cost
    add_cost(home, 4, firstSpeciesMotionCosts, costs);
    //set cost[5] to be melodic cost
    add_cost(home, 5, IntVarArray(home, melodicDegreeCost.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), melodicDegreeCost.size())), costs);
    //need to set cost[6] to be direct cost
    add_cost(home, 6, directCostArray, costs);
}

/**
 * 4 VOICES CONSTRUCTOR
 */

FirstSpeciesCounterpoint::FirstSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub, Stratum* low, CantusFirmus* c,  int v_type, 
    vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV1, int nV2, int nV3):
    FirstSpeciesCounterpoint(home, nMes, cf, lb, ub, FIRST_SPECIES, low, c, v_type, m_costs, g_costs, s_costs, bm, nV3)
{
    rel(home, firstSpeciesMelodicIntervals, IRT_EQ, m_intervals_brut.slice(0,4/notesPerMeasure.at(FIRST_SPECIES),m_intervals_brut.size()));

    varietyCostArray = IntVarArray(home, 3*(firstSpeciesHarmonicIntervals.size()-2), IntSet({0, varietyCost}));
    directCostArray = IntVarArray(home, firstSpeciesMotions.size()-1,IntSet({0, 2, directMoveCost}));
    
    /// M2 from Thibault: Melodic intervals cannot exceed a minor sixth (also include octave?)
    M1_1_3v_melodicIntervalsNotExceedMinorSixth(home, this);

    //P1 4 voices version
    P1_1_4v_noDirectMotionFromPerfectConsonance(home, this);

    //P3 from Thibault : no battuta
    P3_1_noBattuta(home, this);

    costs = IntVarArray(home, 7, 0, 1000000);
    cost_names = {"borrow", "fifth", "octave", "variety", "motion", "melodic", "direct"};
    //need to set cost[0] to be off cost
    add_cost(home, 0, IntVarArray(home, offCostArray.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), offCostArray.size())), costs);
    //set cost[1] to be fifth cost
    add_cost(home, 1, IntVarArray(home, fifthCostArray.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), fifthCostArray.size())), costs);
    //set cost[2] to be octave cost
    add_cost(home, 2, IntVarArray(home, octaveCostArray.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), octaveCostArray.size())), costs);
    //need to set cost[3] to be variety cost
    add_cost(home, 3, varietyCostArray, costs);
    //set cost[4] to be motion cost
    add_cost(home, 4, firstSpeciesMotionCosts, costs);
    //set cost[5] to be melodic cost
    add_cost(home, 5, IntVarArray(home, melodicDegreeCost.slice(0, 4/notesPerMeasure.at(FIRST_SPECIES), melodicDegreeCost.size())), costs);
    //need to set cost[6] to be direct cost
    add_cost(home, 6, directCostArray, costs);
}

/**
 * This function returns a string with the characteristics of the counterpoint. It calls the to_string() method from
 * the Part class and adds 1st species specific characteristics.
 * @return a string representation of the current instance of the FirstSpeciesCounterpoint class.
 */
string FirstSpeciesCounterpoint::to_string() const {
    string text = Part::to_string() + "\nFirst species :\n";
    text += "First species first notes: " + intVarArray_to_string(firstSpeciesNotesCp) + "\n";
    text += "First species isLowest array : " + boolVarArray_to_string(isNotLowest) + "\n";
    text += "First species motions : " + intVarArray_to_string(firstSpeciesMotions) + "\n";
    text += "First species h intervals : " + intVarArray_to_string(firstSpeciesHarmonicIntervals) + "\n";
    return text;
}

// clone constructor
FirstSpeciesCounterpoint::FirstSpeciesCounterpoint(Home home, FirstSpeciesCounterpoint &s) : Part(home, s){
    motherSpecies = s.motherSpecies;
    //cantus = s.cantus;
}

FirstSpeciesCounterpoint* FirstSpeciesCounterpoint::clone(Home home){
    return new FirstSpeciesCounterpoint(home, *this);
}

IntVarArray FirstSpeciesCounterpoint::getBranchingNotes(){
    return firstSpeciesNotesCp;
}

IntVarArray FirstSpeciesCounterpoint::getFirstHInterval(){
    return firstSpeciesHarmonicIntervals;
}

IntVarArray FirstSpeciesCounterpoint::getMotions(){
    return firstSpeciesMotions;
}

IntVarArray FirstSpeciesCounterpoint::getFirstMInterval(){
    return firstSpeciesMelodicIntervals;
}

int FirstSpeciesCounterpoint::getHIntervalSize(){
    return firstSpeciesHarmonicIntervals.size();
}
