// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// 

#include "../../headers/Parts/ThirdSpeciesCounterpoint.hpp"

/**
 * Third species general constructor
 */
ThirdSpeciesCounterpoint::ThirdSpeciesCounterpoint(Home home, int size, vector<int> cf,int lb, int ub, int mSpec, Stratum* low, CantusFirmus* c, 
    int v_type, vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV):
    FirstSpeciesCounterpoint(home, size, cf, lb, ub, THIRD_SPECIES, low, c, v_type, m_costs, g_costs, s_costs, bm, nV)
{
    thirdSpeciesNotesCp = IntVarArray(home, notes.size(), IntSet(IntArgs(vector_intersection(cp_range, extended_domain))));
    if(borrowMode==1){
        thirdSpeciesNotesCp[thirdSpeciesNotesCp.size()-2] = IntVar(home, IntSet(IntArgs(vector_intersection(cp_range, chromatic_scale))));
    }
    rel(home, thirdSpeciesNotesCp, IRT_EQ, notes.slice(0,4/notesPerMeasure.at(THIRD_SPECIES),(notes.size())));

    thirdSpeciesHarmonicIntervals = IntVarArray(home, h_intervals.size(), -PERFECT_OCTAVE, PERFECT_OCTAVE);
    rel(home, thirdSpeciesHarmonicIntervals, IRT_EQ, h_intervals);
    for(int i = 0; i < thirdSpeciesHarmonicIntervals.size(); i++){
        rel(home, (thirdSpeciesHarmonicIntervals[i])==((thirdSpeciesNotesCp[i]-low->getNotes()[floor(i/4)*4])%12));
    }
    
    
    thirdSpeciesMelodicIntervals = IntVarArray(home, m_intervals_brut.size(), -PERFECT_OCTAVE, PERFECT_OCTAVE);
    
    for(int i = 0; i < thirdSpeciesMelodicIntervals.size(); i++)
        rel(home, thirdSpeciesMelodicIntervals[i], IRT_EQ, expr(home, thirdSpeciesNotesCp[i+1] - thirdSpeciesNotesCp[i]));
    
    rel(home, thirdSpeciesMelodicIntervals, IRT_EQ, m_intervals_brut.slice(0,4/notesPerMeasure.at(THIRD_SPECIES),m_intervals_brut.size()));

    for(int i = 0; i < thirdSpeciesMelodicIntervals.size(); i++){
        rel(home, expr(home, abs(thirdSpeciesMelodicIntervals[i])), IRT_GR, 0);
    }

    thirdSpeciesMotions = IntVarArray(home, notes.size()/4, IntSet({-1, CONTRARY_MOTION, OBLIQUE_MOTION, PARALLEL_MOTION}));
    thirdSpeciesMotionCosts = IntVarArray(home, notes.size()/4, IntSet{0, directCost, obliqueCost, contraryCost});

    for(int i = 0; i < thirdSpeciesMotions.size(); i++){

        //direct motions help creation
        BoolVar both_up = expr(home, (thirdSpeciesMelodicIntervals[(i*4)+3]>0)&&(low->getMelodicIntervals()[i]>0)); //if both parts are going in the same direction
        BoolVar both_stay = expr(home, (thirdSpeciesMelodicIntervals[(i*4)+3]==0)&&(low->getMelodicIntervals()[i]==0)); //if both parts are staying
        BoolVar both_down = expr(home, (thirdSpeciesMelodicIntervals[(i*4)+3]<0)&&(low->getMelodicIntervals()[i]<0)); //if both parts are going down
        //oblique motions help creation
        BoolVar cf_stays_1 = expr(home, (thirdSpeciesMelodicIntervals[(i*4)+3]>0)&&(low->getMelodicIntervals()[i]==0)); //if the lowest part stays and one goes up
        BoolVar cf_stays_2 = expr(home, (thirdSpeciesMelodicIntervals[(i*4)+3]<0)&&(low->getMelodicIntervals()[i]==0)); //if the lowest part stays and one goes down
        BoolVar cp_stays_1 = expr(home, (thirdSpeciesMelodicIntervals[(i*4)+3]==0)&&(low->getMelodicIntervals()[i]>0)); //if the lowest part goes up and one stays
        BoolVar cp_stays_2 = expr(home, (thirdSpeciesMelodicIntervals[(i*4)+3]==0)&&(low->getMelodicIntervals()[i]<0)); //if the lowest part goes down and one stays
        //contrary motions help creation
        BoolVar cpd_cfu = expr(home, (thirdSpeciesMelodicIntervals[(i*4)+3]<0)&&(low->getMelodicIntervals()[i]>0)); //if the cf goes up and the cp down
        BoolVar cpu_cfd = expr(home, (thirdSpeciesMelodicIntervals[(i*4)+3]>0)&&(low->getMelodicIntervals()[i]<0)); //if the cf goes down and the cp up
 
        //direct constraints
        rel(home, ((both_up || both_stay || both_down) && (isNotLowest[i])) >> (thirdSpeciesMotions[i]==PARALLEL_MOTION));
        rel(home, ((both_up || both_stay || both_down) && (isNotLowest[i])) >> (thirdSpeciesMotionCosts[i]==directCost));
        //oblique constraints
        rel(home, ((cf_stays_1 || cf_stays_2 || cp_stays_1 || cp_stays_2) && (isNotLowest[i])) >> (thirdSpeciesMotions[i]==OBLIQUE_MOTION));
        rel(home, ((cf_stays_1 || cf_stays_2 || cp_stays_1 || cp_stays_2) && (isNotLowest[i])) >> (thirdSpeciesMotionCosts[i]==obliqueCost));
        //contrary constraints
        rel(home, ((cpd_cfu || cpu_cfd) && (isNotLowest[i])) >> (thirdSpeciesMotions[i]==CONTRARY_MOTION));
        rel(home, ((cpd_cfu || cpu_cfd) && (isNotLowest[i])) >> (thirdSpeciesMotionCosts[i]==contraryCost));
        //bass constraints
        rel(home, (!isNotLowest[i]) >> (thirdSpeciesMotions[i]==-1));
        rel(home, (!isNotLowest[i]) >> (thirdSpeciesMotionCosts[i]==0));
    }
    
    thirdSpeciesAbsMelodic = IntVarArray(home, thirdSpeciesMelodicIntervals.size(), UNISSON, PERFECT_OCTAVE);
    for(int i = 0; i < thirdSpeciesAbsMelodic.size(); i++){
        rel(home, thirdSpeciesAbsMelodic[i] == abs(thirdSpeciesMelodicIntervals[i]));
    }

    m2IntervalsArray = IntVarArray(home, thirdSpeciesNotesCp.size()-2, -PERFECT_OCTAVE, PERFECT_OCTAVE);
    for(int i = 0; i < thirdSpeciesNotesCp.size()-2; i++){
        //size-2 of notes (butlast butlast) ; from 2 onwards in notes (rest rest)
        rel(home, m2IntervalsArray[i], IRT_EQ, expr(home, thirdSpeciesNotesCp[i+2]-thirdSpeciesNotesCp[i]));
    }

    is5QNArray = BoolVarArray(home, nMeasures-1, 0, 1);
    for(int i = 0; i < is5QNArray.size()*4; i+=4){

        BoolVar b1 = BoolVar(home, 0, 1);
        BoolVar b2 = BoolVar(home, 0, 1);
        BoolVar b3 = BoolVar(home, 0, 1);
        BoolVar b4 = BoolVar(home, 0, 1);
        BoolVar bb1 = BoolVar(home, 0, 1);
        BoolVar bb2 = BoolVar(home, 0, 1);
        BoolVar bb3 = BoolVar(home, 0, 1);
        BoolVar bb4 = BoolVar(home, 0, 1);
        BoolVar band1 = BoolVar(home, 0, 1);
        BoolVar band2 = BoolVar(home, 0, 1);
        BoolVar band3 = BoolVar(home, 0, 1);
        BoolVar beq1 = BoolVar(home, 0, 1);
        BoolVar beq2 = BoolVar(home, 0, 1);
        BoolVar beq3 = BoolVar(home, 0, 1);

        rel(home, thirdSpeciesAbsMelodic[i], IRT_LQ, 2, Reify(b1));
        rel(home, thirdSpeciesAbsMelodic[i+1], IRT_LQ, 2, Reify(b2));
        rel(home, thirdSpeciesAbsMelodic[i+2], IRT_LQ, 2, Reify(b3));
        rel(home, thirdSpeciesAbsMelodic[i+3], IRT_LQ, 2, Reify(b4));

        rel(home, thirdSpeciesMelodicIntervals[i], IRT_GQ, 0, Reify(bb1));
        rel(home, thirdSpeciesMelodicIntervals[i+1], IRT_GQ, 0, Reify(bb2));
        rel(home, thirdSpeciesMelodicIntervals[i+2], IRT_GQ, 0, Reify(bb3));
        rel(home, thirdSpeciesMelodicIntervals[i+3], IRT_GQ, 0, Reify(bb4));

        rel(home, b1, BOT_AND, b2, band1);
        rel(home, b3, BOT_AND, b4, band2);
        rel(home, band1, BOT_AND, band2, band3);

        rel(home, bb1, BOT_EQV, bb2, beq1);
        rel(home, bb3, BOT_EQV, bb4, beq2);
        rel(home, beq1, BOT_EQV, beq2, beq3);
        rel(home, band3, BOT_AND, beq3, is5QNArray[i/4]);
    }

    isDiminution = BoolVarArray(home, (nMeasures* notesPerMeasure.at(FIRST_SPECIES) -1), 0, 1);
    for(int i = 0; i < isDiminution.size()*4; i+=4){

        BoolVar btt3 = BoolVar(home, 0, 1);
        BoolVar btt4 = BoolVar(home, 0, 1);
        BoolVar bta2nd = BoolVar(home, 0, 1);
        BoolVar btt3rd = BoolVar(home, 0, 1);
        BoolVar bat2nd = BoolVar(home, 0, 1);
        BoolVar band = BoolVar(home, 0, 1);
        IntVar addition = expr(home, abs(m2IntervalsArray[i+1]));
        rel(home, addition, IRT_EQ, 3, Reify(btt3));
        rel(home, addition, IRT_EQ, 4, Reify(btt4));
        rel(home, expr(home, abs(thirdSpeciesMelodicIntervals[i+1])), IRT_LQ, 2, Reify(bta2nd));
        rel(home, expr(home, abs(thirdSpeciesMelodicIntervals[i+2])), IRT_LQ, 2, Reify(bat2nd));
        rel(home, btt3, BOT_OR, btt4, btt3rd);
        rel(home, bta2nd, BOT_AND, btt3rd, band);
        rel(home, band, BOT_AND, bat2nd, isDiminution[i/4]);

    }

    cambiataCostArray = IntVarArray(home, nMeasures-1, IntSet({0, cambiataCost}));

    m2ZeroArray = IntVarArray(home, thirdSpeciesMelodicIntervals.size()-2, IntSet({0, m2ZeroCost}));

    //third note of the penultimate measure must be below the fourth one
    rel(home, thirdSpeciesMelodicIntervals[thirdSpeciesMelodicIntervals.size()-2], IRT_GR, 1);
    //second one must also be more distant than a semi tone from the last note of the penultimate measure
    rel(home, expr(home, thirdSpeciesMelodicIntervals[thirdSpeciesMelodicIntervals.size()-3]+thirdSpeciesMelodicIntervals[thirdSpeciesMelodicIntervals.size()-2])
        , IRT_NQ, 1);
    
    //3.H1 : five consecutive notes by joint degree implies that the first and the third note are consonants
    H1_3_fiveConsecutiveNotesByJointDegree(home, this);

    //3.H2 : any dissonant note implies that it is surrounded by consonant notes
    H2_3_disonanceImpliesDiminution(home, this);

    //3.H3 : cambiata cost
    H3_3_cambiataCost(home, this);
    
    //no melodic interval between 9 and 11
    for(int i = 0; i < nMeasures-1; i++){
        rel(home, abs(thirdSpeciesMelodicIntervals[(i*4)+3])!=MAJOR_SIXTH && abs(thirdSpeciesMelodicIntervals[(i*4)+3])!=MINOR_SEVENTH && abs(thirdSpeciesMelodicIntervals[(i*4)+3])!=MAJOR_SEVENTH);
    }

    //3.M1 : each note and its two beats further peer are preferred to be different
    //i + i+1 + i+2
    for(int i = 0; i < m2ZeroArray.size(); i++){
        rel(home, ((thirdSpeciesMelodicIntervals[i]+thirdSpeciesMelodicIntervals[i+1]+thirdSpeciesMelodicIntervals[i+2])==0) >> (m2ZeroArray[i]==m2ZeroCost));
        rel(home, ((thirdSpeciesMelodicIntervals[i]+thirdSpeciesMelodicIntervals[i+1]+thirdSpeciesMelodicIntervals[i+2])!=0) >> (m2ZeroArray[i]==0));
    }

    //marcel's rule
    /*for(int i = 0; i < thirdSpeciesMelodicIntervals.size()-1; i++){
        BoolVar bSkip = BoolVar(home, 0, 1);
        BoolVar bMbUp = BoolVar(home, 0, 1);
        BoolVar bMbDown = BoolVar(home, 0, 1);
        BoolVar bContrary = BoolVar(home, 0, 1);

        rel(home, expr(home, abs(thirdSpeciesMelodicIntervals[i])), IRT_GR, 2, Reify(bSkip));
        rel(home, thirdSpeciesMelodicIntervals[i], IRT_GR, 0, Reify(bMbUp));
        rel(home, thirdSpeciesMelodicIntervals[i+1], IRT_LE, 1, Reify(bMbDown));
        rel(home, bMbUp, BOT_EQV, bMbDown, bContrary);
        rel(home, thirdSpeciesMelodicIntervals[i+1], IRT_LQ, 2, Reify(bSkip, RM_IMP));
        rel(home, bSkip, BOT_IMP, bContrary, 1);
    }*/
    
    //no battuta adapted for third species
    for(int j = 0; j < thirdSpeciesMotions.size(); j++){
        rel(home, expr(home, thirdSpeciesMotions[j]==CONTRARY_MOTION && firstSpeciesHarmonicIntervals[j+1]==0 && thirdSpeciesMelodicIntervals[(j*4)+3]<-4),
            BOT_AND, isNotLowest[j], 0);
    }
}

ThirdSpeciesCounterpoint::ThirdSpeciesCounterpoint(Home home, int size, vector<int> cf,int lb, int ub, Stratum* low, CantusFirmus* c, int v_type,
    vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV):
    ThirdSpeciesCounterpoint(home, size, cf, lb, ub, THIRD_SPECIES, low, c, v_type, m_costs,g_costs, s_costs, bm, nV)
{
    //3.H4 : in the penultimate measure, if the cantusFirmus is in the upper part, then the h_interval of the first note should be a minor third
    rel(home, (getIsNotLowest()[getIsNotLowest().size()-2]==0) >> 
        (expr(home, abs(thirdSpeciesHarmonicIntervals[thirdSpeciesHarmonicIntervals.size()-5]))==MINOR_THIRD));

    costs = IntVarArray(home, 7, 0, 10000);
    cost_names = {"fifth", "octave", "motion", "melodic", "borrow", "cambiata", "m2"};

    //set cost[0] to be fifth cost
    add_cost(home, 0, IntVarArray(home, fifthCostArray.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), fifthCostArray.size())), costs);
    //set cost[1] to be octave cost
    add_cost(home, 1, IntVarArray(home, octaveCostArray.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), octaveCostArray.size())), costs);
    //set cost[2] to be motion cost
    add_cost(home, 2, thirdSpeciesMotionCosts, costs);
    //set cost[3] to be melodic cost
    add_cost(home, 3, IntVarArray(home, melodicDegreeCost.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), melodicDegreeCost.size())), costs);
    //need to set cost[4] to be off cost
    add_cost(home, 4, IntVarArray(home, offCostArray.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), offCostArray.size())), costs);
    //need to set cost[5] to be cambiata cost
    add_cost(home, 5, cambiataCostArray, costs);
    //need to set cost[5] to be cambiata cost
    add_cost(home, 6, m2ZeroArray, costs);
}

ThirdSpeciesCounterpoint::ThirdSpeciesCounterpoint(Home home, int size, vector<int> cf,int lb, int ub, Stratum* low, CantusFirmus* c, int v_type,
    vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV1, int nV2):
    ThirdSpeciesCounterpoint(home, size, cf, lb, ub, THIRD_SPECIES, low, c, v_type, m_costs,g_costs, s_costs, bm, nV2)
{
    varietyCostArray = IntVarArray(home, 3*(thirdSpeciesHarmonicIntervals.size()-2), IntSet({0, varietyCost}));
    directCostArray = IntVarArray(home, thirdSpeciesMotions.size()-1,IntSet({0, directMoveCost}));

    //1.H7,H8 adapted
    rel(home, thirdSpeciesHarmonicIntervals[thirdSpeciesHarmonicIntervals.size()-1]==UNISSON||thirdSpeciesHarmonicIntervals[thirdSpeciesHarmonicIntervals.size()-1]==MINOR_THIRD||thirdSpeciesHarmonicIntervals[thirdSpeciesHarmonicIntervals.size()-1]==PERFECT_FIFTH||
        thirdSpeciesHarmonicIntervals[thirdSpeciesHarmonicIntervals.size()-1]==MAJOR_SIXTH);

    //3.H6 : harmonic triad should be used on the second or third beat
    thirdHTriadArray = IntVarArray(home, nMeasures-1, IntSet({0, triad3rdCost}));
    for(int i = 0; i < thirdHTriadArray.size(); i++){
        rel(home, ((thirdSpeciesHarmonicIntervals[(i*4)+1]!=UNISSON&&thirdSpeciesHarmonicIntervals[(i*4)+1]!=MINOR_THIRD&&thirdSpeciesHarmonicIntervals[(i*4)+1]!=MAJOR_THIRD&&thirdSpeciesHarmonicIntervals[(i*4)+1]!=PERFECT_FIFTH)&&
            (thirdSpeciesHarmonicIntervals[(i*4)+2]!=UNISSON&&thirdSpeciesHarmonicIntervals[(i*4)+2]!=MINOR_THIRD&&thirdSpeciesHarmonicIntervals[(i*4)+2]!=MAJOR_THIRD&&thirdSpeciesHarmonicIntervals[(i*4)+2]!=PERFECT_FIFTH)) >> 
            (thirdHTriadArray[i]==triad3rdCost));
        rel(home, ((thirdSpeciesHarmonicIntervals[(i*4)+1]==UNISSON||thirdSpeciesHarmonicIntervals[(i*4)+1]==MINOR_THIRD||thirdSpeciesHarmonicIntervals[(i*4)+1]==MAJOR_THIRD||thirdSpeciesHarmonicIntervals[(i*4)+1]==PERFECT_FIFTH)||
            (thirdSpeciesHarmonicIntervals[(i*4)+2]==UNISSON||thirdSpeciesHarmonicIntervals[(i*4)+2]==MINOR_THIRD||thirdSpeciesHarmonicIntervals[(i*4)+2]==MAJOR_THIRD||thirdSpeciesHarmonicIntervals[(i*4)+2]==PERFECT_FIFTH)) >> 
            (thirdHTriadArray[i]==0));
    }

    //1.P1 3 voices version
    for(int j = 0; j < firstSpeciesMotions.size()-1; j++){
        //set a cost when it is reached through direct motion, it is 0 when not
        rel(home, (thirdSpeciesMotions[j]==2&&(firstSpeciesHarmonicIntervals[j+1]==0||firstSpeciesHarmonicIntervals[j+1]==7))>>
            (directCostArray[j]==directMoveCost));
        rel(home, (thirdSpeciesMotions[j]!=2||(firstSpeciesHarmonicIntervals[j+1]!=0&&firstSpeciesHarmonicIntervals[j+1]!=7))>>
            (directCostArray[j]==0));
    }

    costs = IntVarArray(home, 10, 0, 10000);
    cost_names = {"borrow", "fifth", "octave", "variety", "motion", "melodic", "direct", "cambiata", "m2", "triad3"};
    //need to set cost[0] to be off cost
    add_cost(home, 0, IntVarArray(home, offCostArray.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), offCostArray.size())), costs);
    //set cost[1] to be fifth cost
    add_cost(home, 1, IntVarArray(home, fifthCostArray.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), fifthCostArray.size())), costs);
    //set cost[2] to be octave cost
    add_cost(home, 2, IntVarArray(home, octaveCostArray.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), octaveCostArray.size())), costs);
    //need to set cost[3] to be variety cost
    add_cost(home, 3, varietyCostArray, costs);
    //set cost[4] to be motion cost
    add_cost(home, 4, thirdSpeciesMotionCosts, costs);
    //set cost[5] to be melodic cost
    add_cost(home, 5, IntVarArray(home, melodicDegreeCost.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), melodicDegreeCost.size())), costs);
    //need to set cost[6] to be direct cost
    add_cost(home, 6, directCostArray, costs);
    //set cost[7] to be cambiata sixth cost
    add_cost(home, 7, cambiataCostArray, costs);
    //set cost[8] to be m2 zero cost
    add_cost(home, 8, m2ZeroArray, costs);
    //set cost[9] to be triad h third cost
    add_cost(home, 9, thirdHTriadArray, costs);
    
}

ThirdSpeciesCounterpoint::ThirdSpeciesCounterpoint(Home home, int size, vector<int> cf,int lb, int ub, Stratum* low, CantusFirmus* c, int v_type,
    vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV1, int nV2, int nV3):
    ThirdSpeciesCounterpoint(home, size, cf, lb, ub, THIRD_SPECIES, low, c, v_type, m_costs,g_costs, s_costs, bm, nV3)
{
    varietyCostArray = IntVarArray(home, 3*(thirdSpeciesHarmonicIntervals.size()-2), IntSet({0, varietyCost}));
    directCostArray = IntVarArray(home, thirdSpeciesMotions.size()-1,IntSet({0, 2, directMoveCost}));

    //1.H7,H8 adapted
    rel(home, thirdSpeciesHarmonicIntervals[thirdSpeciesHarmonicIntervals.size()-1]==UNISSON||thirdSpeciesHarmonicIntervals[thirdSpeciesHarmonicIntervals.size()-1]==MINOR_THIRD||thirdSpeciesHarmonicIntervals[thirdSpeciesHarmonicIntervals.size()-1]==PERFECT_FIFTH||
        thirdSpeciesHarmonicIntervals[thirdSpeciesHarmonicIntervals.size()-1]==MAJOR_SIXTH);

    //3.H6 : harmonic triad should be used on the second or third beat
    thirdHTriadArray = IntVarArray(home, nMeasures-1, IntSet({0, triad3rdCost}));
    for(int i = 0; i < thirdHTriadArray.size(); i++){
        rel(home, ((thirdSpeciesHarmonicIntervals[(i*4)+1]!=UNISSON&&thirdSpeciesHarmonicIntervals[(i*4)+1]!=MINOR_THIRD&&thirdSpeciesHarmonicIntervals[(i*4)+1]!=MAJOR_THIRD&&thirdSpeciesHarmonicIntervals[(i*4)+1]!=PERFECT_FIFTH)&&
            (thirdSpeciesHarmonicIntervals[(i*4)+2]!=UNISSON&&thirdSpeciesHarmonicIntervals[(i*4)+2]!=MINOR_THIRD&&thirdSpeciesHarmonicIntervals[(i*4)+2]!=MAJOR_THIRD&&thirdSpeciesHarmonicIntervals[(i*4)+2]!=PERFECT_FIFTH)) >> 
            (thirdHTriadArray[i]==triad3rdCost));
        rel(home, ((thirdSpeciesHarmonicIntervals[(i*4)+1]==UNISSON||thirdSpeciesHarmonicIntervals[(i*4)+1]==MINOR_THIRD||thirdSpeciesHarmonicIntervals[(i*4)+1]==MAJOR_THIRD||thirdSpeciesHarmonicIntervals[(i*4)+1]==PERFECT_FIFTH)||
            (thirdSpeciesHarmonicIntervals[(i*4)+2]==UNISSON||thirdSpeciesHarmonicIntervals[(i*4)+2]==MINOR_THIRD||thirdSpeciesHarmonicIntervals[(i*4)+2]==MAJOR_THIRD||thirdSpeciesHarmonicIntervals[(i*4)+2]==PERFECT_FIFTH)) >> 
            (thirdHTriadArray[i]==0));
    }

    //1.P1 4 voices version
    for(int j = 0; j < firstSpeciesMotions.size()-1; j++){
        //set a cost when it is reached through direct motion, it is 0 when not
        rel(home, (thirdSpeciesMotions[j]==2&&(firstSpeciesHarmonicIntervals[j+1]==0||firstSpeciesHarmonicIntervals[j+1]==7))>>
            (directCostArray[j]==directMoveCost));
        rel(home, (thirdSpeciesMotions[j]!=2||(firstSpeciesHarmonicIntervals[j+1]!=0&&firstSpeciesHarmonicIntervals[j+1]!=7))>>
            (directCostArray[j]==0));
    }

    costs = IntVarArray(home, 10, 0, 10000);
    cost_names = {"borrow", "fifth", "octave", "variety", "motion", "melodic", "direct", "cambiata", "m2", "triad3"};
    //need to set cost[0] to be off cost
    add_cost(home, 0, IntVarArray(home, offCostArray.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), offCostArray.size())), costs);
    //set cost[1] to be fifth cost
    add_cost(home, 1, IntVarArray(home, fifthCostArray.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), fifthCostArray.size())), costs);
    //set cost[2] to be octave cost
    add_cost(home, 2, IntVarArray(home, octaveCostArray.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), octaveCostArray.size())), costs);
    //need to set cost[3] to be variety cost
    add_cost(home, 3, varietyCostArray, costs);
    //set cost[4] to be motion cost
    add_cost(home, 4, thirdSpeciesMotionCosts, costs);
    //set cost[5] to be melodic cost
    add_cost(home, 5, IntVarArray(home, melodicDegreeCost.slice(0, 4/notesPerMeasure.at(THIRD_SPECIES), melodicDegreeCost.size())), costs);
    //need to set cost[6] to be direct cost
    add_cost(home, 6, directCostArray, costs);
    //set cost[7] to be cambiata sixth cost
    add_cost(home, 7, cambiataCostArray, costs);
    //set cost[8] to be m2 zero cost
    add_cost(home, 8, m2ZeroArray, costs);
    //set cost[9] to be triad h third cost
    add_cost(home, 9, thirdHTriadArray, costs);
}

string ThirdSpeciesCounterpoint::to_string() const {
    string text = Part::to_string() + "\nThird species isConsonance intervals : \n";
    text += boolVarArray_to_string(isConsonance) += "\n";
    text += "Third species is Lowest : \n";
    text += boolVarArray_to_string(isNotLowest) += "\n";
    text += "Third species motions : \n";
    text += intVarArray_to_string(thirdSpeciesMotions) += "\n";
    text += "Third species m intervals : \n";
    text += intVarArray_to_string(thirdSpeciesMelodicIntervals) += "\n";
    text += "m intervals : \n";
    text += intVarArray_to_string(m_intervals_brut) += "\n";
    text += "Third species is5QN array : \n";
    text += boolVarArray_to_string(is5QNArray) += "\n";
    return text;
}

ThirdSpeciesCounterpoint::ThirdSpeciesCounterpoint(Home home, ThirdSpeciesCounterpoint &s): FirstSpeciesCounterpoint(home, s){  
    thirdSpeciesNotesCp.update(home, s.thirdSpeciesNotesCp);
    thirdSpeciesMotions.update(home, s.thirdSpeciesMotions);
    thirdSpeciesMotionCosts.update(home, s.thirdSpeciesMotionCosts);
    m2IntervalsArray.update(home, s.m2IntervalsArray);
    m2ZeroArray.update(home, s.m2ZeroArray);
    thirdHTriadArray.update(home, s.thirdHTriadArray);
    thirdSpeciesAbsMelodic.update(home, s.thirdSpeciesAbsMelodic);
}

ThirdSpeciesCounterpoint* ThirdSpeciesCounterpoint::clone(Home home){
    return new ThirdSpeciesCounterpoint(home, *this);
}

IntVarArray ThirdSpeciesCounterpoint::getFirstHInterval(){
    return firstSpeciesHarmonicIntervals;
}

IntVarArray ThirdSpeciesCounterpoint::getMotions(){
    return thirdSpeciesMotions;
}

IntVarArray ThirdSpeciesCounterpoint::getFirstMInterval(){
    return firstSpeciesMelodicIntervals;
}

IntVarArray ThirdSpeciesCounterpoint::getBranchingNotes(){
    return thirdSpeciesNotesCp;
}

int ThirdSpeciesCounterpoint::getHIntervalSize(){
    return thirdSpeciesHarmonicIntervals.size();
}