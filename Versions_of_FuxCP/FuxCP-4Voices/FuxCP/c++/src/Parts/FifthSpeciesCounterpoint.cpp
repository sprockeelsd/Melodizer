// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// 

#include "../../headers/Parts/FifthSpeciesCounterpoint.hpp"

/**
 * GENERAL CONSTRUCTOR
 */
FifthSpeciesCounterpoint::FifthSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub, Species mSpecies, Stratum* low, CantusFirmus* c,
    int v_type, vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV):
    Part(home, nMes, mSpecies, cf, lb, ub, v_type, m_costs, g_costs, s_costs, nV, bm)
{

    for(int i = lowerBound; i <= upperBound; i++){
        cp_range.push_back(i);
    }
    cout << lowerBound << endl;
    cout << upperBound << endl;
    /*
    if borrowMode is enabled, the extended domain is extended to make the inclusion of borrowed notes possible. We can see from Fux's examples
    that he does like to borrow notes, so the borrow cost should just do the job and still allow borrowed notes, not outright forbid them
    */
    if(borrowMode==1){
        extended_domain = vector_union(cp_range, vector_union(scale, borrowed_scale));
    } else {
        extended_domain = vector_intersection(cp_range, vector_union(scale, borrowed_scale));
    }

    off_domain = vector_difference(vector_intersection(cp_range, scale), lowerBound, upperBound);

    solutionLength = notes.size();
    speciesArray = IntVarArray(home, solutionLength, IntSet({-1, THIRD_SPECIES, FOURTH_SPECIES}));
    isNthSpeciesArray = BoolVarArray(home, notes.size()*5, 0, 1);
    isConstrainedArray = BoolVarArray(home, solutionLength, 0, 1);

    /**
     * CREATE THE SPECIES ARRAY
     */
    createSpeciesArrays(home);

    fifthSpeciesNotesCp = IntVarArray(home, notes.size(), IntSet(IntArgs(vector_intersection(cp_range, extended_domain))));
    if(borrowMode==1){
        fifthSpeciesNotesCp[fifthSpeciesNotesCp.size()-2] = IntVar(home, IntSet(IntArgs(vector_intersection(cp_range, chromatic_scale))));
    }
    

    fifthSpeciesHIntervals = IntVarArray(home, h_intervals.size(), -PERFECT_OCTAVE, PERFECT_OCTAVE);

    firstSpeciesHarmonicIntervals = IntVarArray(home, fifthSpeciesHIntervals.slice(0, 4, fifthSpeciesHIntervals.size()));

    for(int i = 0; i < fifthSpeciesHIntervals.size(); i++){
        rel(home, (fifthSpeciesHIntervals[i])==((fifthSpeciesNotesCp[i]-low->getNotes()[floor(i/4)*4])%12));
    }
    //rel(home, (fifthSpeciesHIntervals[0])==((fifthSpeciesNotesCp[2]-low->getNotes()[0])%12));

    fifthSpeciesSuccMIntervals = IntVarArray(home, m_intervals_brut.size(), -PERFECT_OCTAVE, PERFECT_OCTAVE);

    firstSpeciesMelodicIntervals = IntVarArray(home, nMeasures-1, -PERFECT_OCTAVE, PERFECT_OCTAVE);

    for(int i = 0; i < fifthSpeciesSuccMIntervals.size(); i++){
        rel(home, fifthSpeciesSuccMIntervals[i] == (fifthSpeciesNotesCp[i+1]-fifthSpeciesNotesCp[i]));
    }

    for(int i = 0; i < firstSpeciesMelodicIntervals.size(); i++){
        rel(home, firstSpeciesMelodicIntervals[i] == (fifthSpeciesNotesCp[(i+1)*4]-fifthSpeciesNotesCp[i*4]));
    }

    fifthSpeciesMIntervals = IntVarArray(home, m_intervals_brut.size(), -16, 16);
    for(int i = 0; i < fifthSpeciesMIntervals.size(); i+=4){
        rel(home, fifthSpeciesMIntervals[i+2] == (fifthSpeciesNotesCp[i+4]-fifthSpeciesNotesCp[i+2]));
        rel(home, fifthSpeciesMIntervals[i+3] == (fifthSpeciesNotesCp[i+4]-fifthSpeciesNotesCp[i+3]));
    }

    fifthSpeciesMTAIntervals = IntVarArray(home, nMeasures-1, -16, 16);
    for(int i = 0; i < fifthSpeciesMTAIntervals.size(); i++){
        rel(home, fifthSpeciesMTAIntervals[i] == (fifthSpeciesNotesCp[(i*4)+2]-fifthSpeciesNotesCp[(i*4)]));
    }

    m2Len = ((nMeasures-1)*4)-1;
    fifthSpeciesM2Intervals = IntVarArray(home, m2Len, -16, 16);
    for(int i = 0; i < m2Len; i++){
        rel(home, fifthSpeciesM2Intervals[i] == (fifthSpeciesNotesCp[i+2]-fifthSpeciesNotesCp[i]));
    }

    fifthSpeciesMAllIntervals = IntVarArray(home, m_intervals_brut.size(), -PERFECT_OCTAVE, PERFECT_OCTAVE);
    for(int i = 0; i < fifthSpeciesMAllIntervals.size(); i++){
        rel(home, fifthSpeciesMAllIntervals[i] == (fifthSpeciesNotesCp[i+1]-fifthSpeciesNotesCp[i]));
    }
    
    fifthSpeciesMotions = IntVarArray(home, nMeasures-1, IntSet({-1, CONTRARY_MOTION, OBLIQUE_MOTION, PARALLEL_MOTION}));
    fifthSpeciesMotionsCosts = IntVarArray(home, nMeasures-1, IntSet({0, contraryCost, obliqueCost, directCost}));

    //create motions
    for(int i = 0; i < fifthSpeciesMotions.size(); i++){
        //direct motions help creation
        
        BoolVar both_up = expr(home, (fifthSpeciesMIntervals[(i*4)+3]>0)&&(low->getMelodicIntervals()[i]>0)); //if both parts are going in the same direction
        BoolVar both_stay = expr(home, (fifthSpeciesMIntervals[(i*4)+3]==0)&&(low->getMelodicIntervals()[i]==0)); //if both parts are staying
        BoolVar both_down = expr(home, (fifthSpeciesMIntervals[(i*4)+3]<0)&&(low->getMelodicIntervals()[i]<0)); //if both parts are going down
        //oblique motions help creation
        BoolVar cf_stays_1 = expr(home, (fifthSpeciesMIntervals[(i*4)+3]>0)&&(low->getMelodicIntervals()[i]==0)); //if the lowest part stays and one goes up
        BoolVar cf_stays_2 = expr(home, (fifthSpeciesMIntervals[(i*4)+3]<0)&&(low->getMelodicIntervals()[i]==0)); //if the lowest part stays and one goes down
        BoolVar cp_stays_1 = expr(home, (fifthSpeciesMIntervals[(i*4)+3]==0)&&(low->getMelodicIntervals()[i]>0)); //if the lowest part goes up and one stays
        BoolVar cp_stays_2 = expr(home, (fifthSpeciesMIntervals[(i*4)+3]==0)&&(low->getMelodicIntervals()[i]<0)); //if the lowest part goes down and one stays
        //contrary motions help creation
        BoolVar cpd_cfu = expr(home, (fifthSpeciesMIntervals[(i*4)+3]<0)&&(low->getMelodicIntervals()[i]>0)); //if the cf goes up and the cp down
        BoolVar cpu_cfd = expr(home, (fifthSpeciesMIntervals[(i*4)+3]>0)&&(low->getMelodicIntervals()[i]<0)); //if the cf goes down and the cp up

        //direct constraints
        rel(home, ((both_up || both_stay || both_down) && (this->isNotLowest[i]==1)) >> (fifthSpeciesMotions[i]==PARALLEL_MOTION));
        rel(home, ((both_up || both_stay || both_down) && (this->isNotLowest[i]==1)) >> (fifthSpeciesMotionsCosts[i]==directCost));
        //oblique constraints
        rel(home, ((cf_stays_1 || cf_stays_2 || cp_stays_1 || cp_stays_2) && (this->isNotLowest[i]==1)) >> (fifthSpeciesMotions[i]==OBLIQUE_MOTION));
        rel(home, ((cf_stays_1 || cf_stays_2 || cp_stays_1 || cp_stays_2) && (this->isNotLowest[i]==1)) >> (fifthSpeciesMotionsCosts[i]==obliqueCost));
        //contrary constraints
        rel(home, ((cpd_cfu || cpu_cfd) && (this->isNotLowest[i]==1)) >> (fifthSpeciesMotions[i]==CONTRARY_MOTION));
        rel(home, ((cpd_cfu || cpu_cfd) && (this->isNotLowest[i]==1)) >> (fifthSpeciesMotionsCosts[i]==contraryCost));
        //bass constraints
        rel(home, (this->isNotLowest[i]==0) >> (fifthSpeciesMotions[i]==-1));
        rel(home, (this->isNotLowest[i]==0) >> (fifthSpeciesMotionsCosts[i]==0));

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

        rel(home, expr(home, abs(fifthSpeciesSuccMIntervals[i])), IRT_LQ, 2, Reify(b1));
        rel(home, expr(home, abs(fifthSpeciesSuccMIntervals[i+1])), IRT_LQ, 2, Reify(b2));
        rel(home, expr(home, abs(fifthSpeciesSuccMIntervals[i+2])), IRT_LQ, 2, Reify(b3));
        rel(home, expr(home, abs(fifthSpeciesSuccMIntervals[i+3])), IRT_LQ, 2, Reify(b4));

        rel(home, fifthSpeciesSuccMIntervals[i], IRT_GQ, 0, Reify(bb1));
        rel(home, fifthSpeciesSuccMIntervals[i+1], IRT_GQ, 0, Reify(bb2));
        rel(home, fifthSpeciesSuccMIntervals[i+2], IRT_GQ, 0, Reify(bb3));
        rel(home, fifthSpeciesSuccMIntervals[i+3], IRT_GQ, 0, Reify(bb4));

        rel(home, b1, BOT_AND, b2, band1);
        rel(home, b3, BOT_AND, b4, band2);
        rel(home, band1, BOT_AND, band2, band3);

        rel(home, bb1, BOT_EQV, bb2, beq1);
        rel(home, bb3, BOT_EQV, bb4, beq2);
        rel(home, beq1, BOT_EQV, beq2, beq3);
        rel(home, band3, BOT_AND, beq3, is5QNArray[i/4]);
    }

    isMostlyThirdArray = BoolVarArray(home, nMeasures-1, 0, 1);
    for(int i = 0; i < isMostlyThirdArray.size(); i++){
        BoolVar b23 = BoolVar(home, 0, 1);
        /**
         * EXPLANATION : 
         * -for each measure there are 20 entries in the isNthSpeciesArray (5 possibilities for each note, and a measure has 4 notes)
         * -therefor, we multiply i by 20 to always land at the start of a measure (0 is the first note of the first measure, 20 the first of the second 
         *  measure etc etc)
         * -since each note has 5 entries to check for which type of species it is, to land on the first entry of the second note, we do 20+5
         * -now to check if the second note is of the third species, we do 20+5+3 (second measure, second note, check for third species) 
         */
        rel(home, isNthSpeciesArray[(i*20)+8], BOT_AND, isNthSpeciesArray[(i*20)+13], b23); 
        rel(home, b23, BOT_AND, isNthSpeciesArray[(i*20)+18], isMostlyThirdArray[i]);

    }

    isConsonance = BoolVarArray(home, notes.size(), 0, 1);
    isThirdSpeciesArray = BoolVarArray(home, notes.size(), 0, 1);
    isFourthSpeciesArray = BoolVarArray(home, notes.size(), 0, 1);

    //create isConsonance array
    for(int i = 0; i < isConsonance.size(); i++){
        rel(home, expr(home, (h_intervals[i]==UNISSON)||(h_intervals[i]==MINOR_THIRD)||(h_intervals[i]==MAJOR_THIRD)||(h_intervals[i]==PERFECT_FIFTH)||
            (h_intervals[i]==MINOR_SIXTH)||(h_intervals[i]==MAJOR_SIXTH)||(h_intervals[i]==PERFECT_OCTAVE)||(h_intervals[i]==-MINOR_THIRD)||
            (h_intervals[i]==-MAJOR_THIRD)||(h_intervals[i]==-PERFECT_FIFTH)||(h_intervals[i]==-MINOR_SIXTH)||(h_intervals[i]==-MAJOR_SIXTH)||
            (h_intervals[i]==-PERFECT_OCTAVE)), IRT_EQ, isConsonance[i]);
    }

    //create isThird and isFourth arrays
    for(int i = 0; i < notes.size(); i++){
        rel(home, isThirdSpeciesArray[i], IRT_EQ, isNthSpeciesArray[(i*5)+3]);
        rel(home, isFourthSpeciesArray[i], IRT_EQ, isNthSpeciesArray[(i*5)+4]);
    }

    isDiminution = BoolVarArray(home, nMeasures-1, 0, 1);
    for(int i = 0; i < isDiminution.size()*4; i+=4){

        BoolVar btt3 = BoolVar(home, 0, 1);
        BoolVar btt4 = BoolVar(home, 0, 1);
        BoolVar bta2nd = BoolVar(home, 0, 1);
        BoolVar btt3rd = BoolVar(home, 0, 1);
        BoolVar bat2nd = BoolVar(home, 0, 1);
        BoolVar band = BoolVar(home, 0, 1);
        IntVar addition = expr(home, abs(fifthSpeciesM2Intervals[i+1]));
        rel(home, addition, IRT_EQ, 3, Reify(btt3));
        rel(home, addition, IRT_EQ, 4, Reify(btt4));
        rel(home, fifthSpeciesSuccMIntervals[i+1], IRT_LQ, 2, Reify(bta2nd));
        rel(home, fifthSpeciesSuccMIntervals[i+2], IRT_LQ, 2, Reify(bat2nd));
        rel(home, btt3, BOT_OR, btt4, btt3rd);
        rel(home, bta2nd, BOT_AND, btt3rd, band);
        rel(home, band, BOT_AND, bat2nd, isDiminution[i/4]);

    }

    //set cambiata array
    isNotCambiata = BoolVarArray(home, nMeasures-1, 0, 1);
    for(int i = 0; i < isNotCambiata.size(); i++){
        rel(home, ((fifthSpeciesHIntervals[(i*4)+1]==UNISSON || fifthSpeciesHIntervals[(i*4)+1]==PERFECT_FIFTH)&&(fifthSpeciesHIntervals[(i*4)+2]==UNISSON || fifthSpeciesHIntervals[(i*4)+2]==PERFECT_FIFTH)
            &&(abs(fifthSpeciesSuccMIntervals[(i*4)+1])<=2)) >> (isNotCambiata[i]==1));
        rel(home, ((fifthSpeciesHIntervals[(i*4)+1]!=UNISSON && fifthSpeciesHIntervals[(i*4)+1]!=PERFECT_FIFTH)||(fifthSpeciesHIntervals[(i*4)+2]!=UNISSON && fifthSpeciesHIntervals[(i*4)+2]!=PERFECT_FIFTH)
            ||(abs(fifthSpeciesSuccMIntervals[(i*4)+1])>2)) >> (isNotCambiata[i]==0));
    }

    is_off = BoolVarArray(home, notes.size(), 0, 1);
    initializeIsOffArray(home, this);

    isNoSyncopeArray = BoolVarArray(home, nMeasures-1, 0, 1);
    for(int i = 0; i < isNoSyncopeArray.size(); i++){
        rel(home, fifthSpeciesMIntervals[(i*2)+2], IRT_NQ, 0, Reify(isNoSyncopeArray[i]));
    }
    rel(home, fifthSpeciesNotesCp, IRT_EQ, notes.slice(0,4/notesPerMeasure.at(FIFTH_SPECIES),(notes.size())));
    rel(home, fifthSpeciesHIntervals, IRT_EQ, h_intervals.slice(0,4/notesPerMeasure.at(FIFTH_SPECIES),h_intervals.size()));
    rel(home, fifthSpeciesSuccMIntervals, IRT_EQ, m_intervals_brut.slice(0,4/notesPerMeasure.at(FIFTH_SPECIES),m_intervals_brut.size()));

    /** ===========================================================================
     *                            HARMONIC CONSTRAINS
     *  ===========================================================================
     */ 

    //Only one possible value for non constrained variables
    for(int i = 0; i < fifthSpeciesNotesCp.size()-1; i++){
        //rel(home, fifthSpeciesNotesCp[i], IRT_EQ, fifthSpeciesNotesCp[i+1], Reify(isNthSpeciesArray[(i*5)], RM_IMP));
    }
    
    //Third note of penult measure must be below the fourth one
    rel(home, fifthSpeciesSuccMIntervals[fifthSpeciesSuccMIntervals.size()-3], IRT_GR, MINOR_SECOND, 
        Reify(isThirdSpeciesArray[isThirdSpeciesArray.size()-2], RM_IMP));
    //Second and third note distant by more than one semi tone from fourth note
    rel(home, expr(home, abs(fifthSpeciesM2Intervals[fifthSpeciesM2Intervals.size()-2])), IRT_NQ, 1, 
        Reify(isThirdSpeciesArray[isThirdSpeciesArray.size()-2], RM_IMP));
    
    //is penult cons to cf
    BoolVar isPenultConsToCf = BoolVar(home, 0, 1);
    vector<int> consonances = {0,3,4,7,8,9,-3,-4,-7,-8,-9};
    IntVarArray res = IntVarArray(home, consonances.size(), 0, 1);
    IntVar sm = IntVar(home, 0, consonances.size());
    for(int l = 0; l < consonances.size(); l++){                          
        BoolVar b1 = BoolVar(home, 0, 1);
        rel(home, fifthSpeciesNotesCp[fifthSpeciesNotesCp.size()-5], IRT_EQ, consonances[l], Reify(b1)); 
        ite(home, b1, IntVar(home, 1, 1), IntVar(home, 0, 0), res[l]);           
    }
    IntVarArgs x(res.size());
    for(int t = 0; t < consonances.size(); t++){
        x[t] = res[t];                                                          
    }
    rel(home, sm, IRT_EQ, expr(home, sum(x)));                                     
    rel(home, sm, IRT_GR, 0, Reify(isPenultConsToCf));                     

    rel(home, isFourthSpeciesArray[isFourthSpeciesArray.size()-5], BOT_AND, isPenultConsToCf, 0);
    
    //every thesis note should be consonant
    for(int i = 0; i < isConsonance.size(); i+=4){ //checks every thesis note
        rel(home, isNthSpeciesArray[(i*5)+1], BOT_IMP, isConsonance[i], 1); //first species check
        rel(home, isNthSpeciesArray[(i*5)+2], BOT_IMP, isConsonance[i], 1); //second species check
        rel(home, isThirdSpeciesArray[i], BOT_IMP, isConsonance[i], 1);     //third species check
        if(i!=isConsonance.size()-1){
            //rel(home, isFourthSpeciesArray[i+2], BOT_IMP, isConsonance[i+2], 1);  //fourth species check
        }
        if(i!=0 && i!=isConsonance.size()-1){
            //rel(home, expr(home, isFourthSpeciesArray[i]==1 && isNoSyncopeArray[i]==0), BOT_IMP, isConsonance[i],1);
        }
    }
    
    for(int i = 4; i < isConsonance.size()-1; i+=4){ //start at the first note of the second measure
        rel(home, expr(home, isFourthSpeciesArray[i]==1 && isNoSyncopeArray[i/4]==1), BOT_IMP, isConsonance[i], 1);
    }
    
    //five consecutive notes by joint degree (3rd species)
    for(int i = 0; i < is5QNArray.size(); i++){
        BoolVar b = BoolVar(home, 0, 1);
        rel(home, is5QNArray[i], BOT_AND, isMostlyThirdArray[i], b);
        rel(home, b, BOT_IMP, isConsonance[i*4+2], 1);
    }
    
    //any dissonant note implies it is surrounded by consonant notes (3rd species)
    for(int i = 0; i < isDiminution.size(); i++){
        BoolVar band1 = BoolVar(home, 0, 1);
        rel(home, expr(home, isConsonance[(i*4)+2] || isThirdSpeciesArray[(i*4)+2]), BOT_OR, isDiminution[i], 1);
    }

    /** ===========================================================================
     *                             MELODIC CONSTRAINS
     *  ===========================================================================
     */ 

    //no melodic interval between MAJOR_SIXTH and MAJOR_SEVENTH
    for(int i = 0; i < fifthSpeciesMAllIntervals.size(); i++){
        rel(home, expr(home, abs(fifthSpeciesMAllIntervals[i])), IRT_NQ, MAJOR_SIXTH, Reify(expr(home, isConstrainedArray[i]==1 && isConstrainedArray[i+1]==1), RM_IMP));
        rel(home, expr(home, abs(fifthSpeciesMAllIntervals[i])), IRT_NQ, MINOR_SEVENTH, Reify(expr(home, isConstrainedArray[i]==1 && isConstrainedArray[i+1]==1), RM_IMP));
        rel(home, expr(home, abs(fifthSpeciesMAllIntervals[i])), IRT_NQ, MAJOR_SEVENTH, Reify(expr(home, isConstrainedArray[i]==1 && isConstrainedArray[i+1]==1), RM_IMP));
    }
    
    //no unisson between two consecutive notes
    for(int i = 0; i < nMeasures-1; i++){
        rel(home, fifthSpeciesNotesCp[(i*4)], IRT_NQ, fifthSpeciesNotesCp[(i*4)+1], Reify(expr(home, (isConstrainedArray[(i*4)]==1) && (isConstrainedArray[(i*4)+1]==1)), RM_IMP));
        rel(home, fifthSpeciesNotesCp[(i*4)+2], IRT_NQ, fifthSpeciesNotesCp[(i*4)+3], Reify(expr(home, (isConstrainedArray[(i*4)+2]==1) && (isConstrainedArray[(i*4)+3]==1)), RM_IMP));
    }
    
    //no more than minor sixth interval between arsis and thesis notes
    for(int i = 0; i < nMeasures-1; i++){
        rel(home, expr(home, abs(fifthSpeciesMTAIntervals[i])), IRT_NQ, MAJOR_SIXTH, Reify(expr(home, isConstrainedArray[i+1]==1), RM_IMP));
        rel(home, expr(home, abs(fifthSpeciesMTAIntervals[i])), IRT_NQ, MINOR_SEVENTH, Reify(expr(home, isConstrainedArray[i+1]==1), RM_IMP));
        rel(home, expr(home, abs(fifthSpeciesMTAIntervals[i])), IRT_NQ, MAJOR_SEVENTH, Reify(expr(home, isConstrainedArray[i+1]==1), RM_IMP));
    }
    
    //no same syncopation
    for(int i = 1; i < nMeasures-1; i++){
        rel(home, fifthSpeciesNotesCp[(i*4)], IRT_NQ, fifthSpeciesNotesCp[(i*4)+2], 
            Reify(expr(home, isFourthSpeciesArray[(i*4)]==1 && isConstrainedArray[(i*4)+2]), RM_IMP));

        rel(home, (isFourthSpeciesArray[(i*4)]==1 && isConstrainedArray[(i*4)+2]==1) >> (fifthSpeciesNotesCp[(i*4)]!=fifthSpeciesNotesCp[(i*4)+2]));
    }

    /** ===========================================================================
     *                             MOTION CONSTRAINS
     *  ===========================================================================
     */ 

    //no battuta kind of motion
    for(int i = 0; i < fifthSpeciesMotions.size(); i++){
        BoolVar isCM = BoolVar(home, 0, 1);
        BoolVar isOct = BoolVar(home, 0, 1);
        BoolVar isCPDown = BoolVar(home, 0, 1);
        BoolVar band1 = BoolVar(home, 0, 1);
        BoolVar band2 = BoolVar(home, 0, 1);
        BoolVar band3 = BoolVar(home, 0, 1);

        rel(home, fifthSpeciesMotions[i], IRT_EQ, CONTRARY_MOTION, Reify(isCM));
        rel(home, expr(home, (fifthSpeciesHIntervals[((i+1)*4)]==UNISSON || fifthSpeciesHIntervals[((i+1)*4)]==PERFECT_OCTAVE || 
            fifthSpeciesHIntervals[((i+1)*4)]==-PERFECT_OCTAVE)) >> isOct);
        rel(home, fifthSpeciesMIntervals[(i*4)+3], IRT_LE, -4, Reify(isCPDown));

        rel(home, isCM, BOT_AND, isOct, band1);
        rel(home, band1, BOT_AND, isCPDown, band2);
        rel(home, band2, BOT_AND, expr(home, c->getNotes()[i] <= fifthSpeciesNotesCp[(i*4)+3]), band3);
        rel(home, isThirdSpeciesArray[(i*4)+3], BOT_AND, band3, 0);
    }
    
    //dissonant notes must be followed by the consonant note below
    for(int i = 0; i < fifthSpeciesMTAIntervals.size(); i++){
        //isFourthArray first note
        //is Consonance array first note
        //mTa intervals, general
        BoolVar bNot = BoolVar(home, 0, 1);
        BoolVar isCst = BoolVar(home, 0, 1);
        BoolVar bAnd = BoolVar(home, 0, 1);

        rel(home, isConsonance[i*4], BOT_EQV, BoolVar(home, 0, 0), bNot);
        rel(home, bNot, BOT_AND, isFourthSpeciesArray[i*4], bAnd);
        rel(home, fifthSpeciesMTAIntervals[i], IRT_LE, 0, Reify(bAnd, RM_IMP));
        rel(home, fifthSpeciesMTAIntervals[i], IRT_GQ, -2, Reify(bAnd, RM_IMP));
    }
    
    //no second dissonant note if the cantusFirmus is at the bass
    for(int i = 0; i < nMeasures-1; i++){
        BoolVar bUni = BoolVar(home, 0, 1);
        BoolVar bAnd = BoolVar(home, 0, 1);
        BoolVar isCst = BoolVar(home, 0, 1);
        BoolVar bAndCst = BoolVar(home, 0, 1);

        rel(home, isFourthSpeciesArray[((i+1)*4)], BOT_EQV, isCst, 1);
        rel(home, fifthSpeciesHIntervals[(i*4)+2], IRT_EQ, 0, Reify(bUni));
        rel(home, isNotLowest[i], BOT_AND, bUni, bAnd);
        rel(home, bAnd, BOT_AND, isCst, bAndCst);
        rel(home, fifthSpeciesHIntervals[((i+1)*4)], IRT_NQ, 1, Reify(bAndCst, RM_IMP));
        rel(home, fifthSpeciesHIntervals[((i+1)*4)], IRT_NQ, 2, Reify(bAndCst, RM_IMP));
    }

    //marcel's rule
    for(int i = 0; i < fifthSpeciesSuccMIntervals.size()-1; i++){
        BoolVar bSkip = BoolVar(home, 0, 1);
        BoolVar bMbUp = BoolVar(home, 0, 1);
        BoolVar bMbDown = BoolVar(home, 0, 1);
        BoolVar bContrary = BoolVar(home, 0, 1);

        rel(home, expr(home, abs(fifthSpeciesSuccMIntervals[i])), IRT_GR, 2, Reify(bSkip));
        rel(home, fifthSpeciesSuccMIntervals[i], IRT_GR, 0, Reify(bMbUp));
        rel(home, fifthSpeciesSuccMIntervals[i+1], IRT_LE, 1, Reify(bMbDown));
        rel(home, bMbUp, BOT_EQV, bMbDown, bContrary);
        rel(home, fifthSpeciesSuccMIntervals[i+1], IRT_LQ, 2, Reify(bSkip, RM_IMP));
        rel(home, bSkip, BOT_IMP, bContrary, 1);
    }

    /** ===========================================================================
     *                              COST CONSTRAINS
     *  ===========================================================================
     */

    //Imperfect consonances are preferred
    fifthCostArray = IntVarArray(home, notes.size(), IntSet({0, fifthCost}));
    octaveCostArray = IntVarArray(home, notes.size(), IntSet({0, octaveCost}));

    //set fifth cost
    for(int i = 0; i < notes.size(); i++){
        BoolVar b = BoolVar(home, 0, 1);
        BoolVar band = BoolVar(home, 0, 1);

        rel(home, fifthSpeciesHIntervals[i], IRT_EQ, 7, Reify(b));
        rel(home, b, BOT_AND, isConstrainedArray[i], band);
        ite(home, band, IntVar(home, fifthCost, fifthCost), IntVar(home, 0, 0), fifthCostArray[i]);
    }

    //set octave cost
    for(int i = 0; i < notes.size(); i++){
        BoolVar b = BoolVar(home, 0, 1);
        BoolVar band = BoolVar(home, 0, 1);
        BoolVar band2 = BoolVar(home, 0, 1);

        rel(home, fifthSpeciesHIntervals[i], IRT_EQ, 7, Reify(b));
        rel(home, isNotLowest[floor(i/4)], BOT_AND, isConstrainedArray[i], band2);
        rel(home, b, BOT_AND, band2, band);
        ite(home, band, IntVar(home, octaveCost, octaveCost), IntVar(home, 0, 0), octaveCostArray[i]);
    }

    //create off_cost array
    offCostArray = IntVarArray(home, is_off.size(), IntSet({0, borrowCost}));
    //set the cost for borrowing this note (G4 constraint, modified)
    for(int i = 0; i < is_off.size(); i++){
        rel(home, (is_off[i]==0) >> (offCostArray[i]==0));
        rel(home, (is_off[i]==1 && isConstrainedArray[i]==1) >> (offCostArray[i]==borrowCost));
        rel(home, (is_off[i]==1 && isConstrainedArray[i]==0) >> (offCostArray[i]==0));
    }

    melodicDegreeCost = IntVarArray(home, m_intervals_brut.size(), IntSet({0, secondCost, thirdCost, fourthCost, tritoneCost, fifthCost, 
        sixthCost, seventhCost, octaveCost}));

    //G7 (modified)
    for(int i = 0; i < m_intervals_brut.size(); i+=4/notesPerMeasure.at(FIFTH_SPECIES)){
        rel(home, (isConstrainedArray[i]==0 || isConstrainedArray[i+1]==0) >> (melodicDegreeCost[i]==0));
        rel(home, (abs(m_intervals_brut[i])<MINOR_THIRD && isConstrainedArray[i]==1 && isConstrainedArray[i+1]==1) >> (melodicDegreeCost[i]==secondCost));
        rel(home, ((abs(m_intervals_brut[i])==MINOR_THIRD || abs(m_intervals_brut[i])==MAJOR_THIRD)&& isConstrainedArray[i]==1&& isConstrainedArray[i+1]==1) >> (melodicDegreeCost[i]==thirdCost));
        rel(home, (abs(m_intervals_brut[i])==PERFECT_FOURTH && isConstrainedArray[i]==1&& isConstrainedArray[i+1]==1) >> (melodicDegreeCost[i]==fourthCost));
        rel(home, (abs(m_intervals_brut[i])==TRITONE && isConstrainedArray[i]==1&& isConstrainedArray[i+1]==1) >> (melodicDegreeCost[i]==tritoneCost));
        rel(home, (abs(m_intervals_brut[i])==PERFECT_FIFTH && isConstrainedArray[i]==1&& isConstrainedArray[i+1]==1) >> (melodicDegreeCost[i]==fifthCost));
        rel(home, ((abs(m_intervals_brut[i])==MINOR_SIXTH || abs(m_intervals_brut[i])==MAJOR_SIXTH)&& isConstrainedArray[i]==1&& isConstrainedArray[i+1]==1) >> (melodicDegreeCost[i]==sixthCost));
        rel(home, ((abs(m_intervals_brut[i])==MINOR_SEVENTH || abs(m_intervals_brut[i])==MAJOR_SEVENTH)&& isConstrainedArray[i]==1&& isConstrainedArray[i+1]==1) >> (melodicDegreeCost[i]==seventhCost));
        rel(home, (abs(m_intervals_brut[i])==PERFECT_OCTAVE && isConstrainedArray[i]==1&& isConstrainedArray[i+1]==1) >> (melodicDegreeCost[i]==octaveCost));
    }

    cambiataCostArray = IntVarArray(home, nMeasures-1, IntSet({0, cambiataCost}));

    for(int i = 0; i < cambiataCostArray.size(); i++){
        BoolVar band = BoolVar(home, 0, 1);
        rel(home, isNotCambiata[i], BOT_AND, isMostlyThirdArray[i], band);
        ite(home, band, IntVar(home, cambiataCost, cambiataCost), IntVar(home, 0, 0), cambiataCostArray[i]);
    }

    m2ZeroCostArray = IntVarArray(home, m2Len, IntSet({0, m2ZeroCost}));

    for(int i = 0; i < fifthSpeciesM2Intervals.size(); i++){
        BoolVar b = BoolVar(home, 0, 1);
        BoolVar band = BoolVar(home, 0, 1);
        BoolVar band2 = BoolVar(home, 0, 1);

        rel(home, fifthSpeciesM2Intervals[i], IRT_EQ, 0, Reify(b));
        rel(home, isConstrainedArray[i], BOT_AND, isConstrainedArray[i+2], band2);
        rel(home, b, BOT_AND, band2, band);
        ite(home, band, IntVar(home, m2ZeroCost, m2ZeroCost), IntVar(home, 0, 0), m2ZeroCostArray[i]);
    }

    snycopeCostArray = IntVarArray(home, (fifthSpeciesMIntervals.size())/4, IntSet({0, syncopationCost}));
    for(int i = 0; i < snycopeCostArray.size(); i++){
        BoolVar b = BoolVar(home, 0, 1);
        BoolVar band = BoolVar(home, 0, 1);

        rel(home, fifthSpeciesMIntervals[(i*4)+2], IRT_NQ, 0, Reify(b));
        rel(home, b, BOT_AND, isFourthSpeciesArray[(i*4)+2], band);
        ite(home, band, IntVar(home, syncopationCost, syncopationCost), IntVar(home, 0, 0), snycopeCostArray[i]);
    }
}

FifthSpeciesCounterpoint::FifthSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub, Stratum* low, CantusFirmus* c,  int v_type, 
    vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV):
FifthSpeciesCounterpoint(home, nMes, cf, lb, ub, FIFTH_SPECIES, low, c, v_type, m_costs, g_costs, s_costs, bm, nV)
{
    //following two rel are H2_1 but modified, so we check in TwoVoiceProblem that that constrained isn't activated if fifth species
    //if the first note is constrained, then it must be a perfect consonance (works)
    rel(home, (isConstrainedArray[0]==1) >> (fifthSpeciesHIntervals[0]==UNISSON || fifthSpeciesHIntervals[0]==PERFECT_FIFTH));
    //else if the first note is not constrained, then the third note must be a perfect consonance (works)
    rel(home, (isConstrainedArray[0]==0) >> (fifthSpeciesHIntervals[2]==UNISSON || fifthSpeciesHIntervals[2]==PERFECT_FIFTH));

    //If the cantusFirmus is in the upper part, then no hamonic seventh
    for(int j = 1; j < c->getIsNotLowest().size(); j++){
        rel(home, (getIsNotLowest()[j]==0 && isFourthSpeciesArray[j*4]) >> (firstSpeciesHarmonicIntervals[j]!=MINOR_SEVENTH && firstSpeciesHarmonicIntervals[j]!=-MINOR_SEVENTH));
        rel(home, (getIsNotLowest()[j]==0 && isFourthSpeciesArray[j*4]) >> (firstSpeciesHarmonicIntervals[j]!=MAJOR_SEVENTH && firstSpeciesHarmonicIntervals[j]!=-MAJOR_SEVENTH));
    }

    //H3_1 can be activated as-is

    //penultimate note (only applies if it is 3rd species)
    BoolVar bandThen_1 = BoolVar(home, 0, 1);
    BoolVar testNot_1 = BoolVar(home, 0, 1);
    BoolVar bandElse_1 = BoolVar(home, 0, 1);
    BoolVar test_1 = expr(home, c->getNotes()[c->getNotes().size()-2] <= fifthSpeciesNotesCp[fifthSpeciesNotesCp.size()-2]);

    rel(home, test_1, BOT_AND, isThirdSpeciesArray[isThirdSpeciesArray.size()-2], bandThen_1);
    rel(home, test_1, BOT_EQV, BoolVar(home, 0, 0), testNot_1);
    rel(home, testNot_1, BOT_AND, isThirdSpeciesArray[isThirdSpeciesArray.size()-2], bandElse_1);

    rel(home, expr(home, fifthSpeciesNotesCp[fifthSpeciesNotesCp.size()-2]-c->getNotes()[c->getNotes().size()-2]), IRT_EQ, MAJOR_SIXTH, Reify(bandThen_1, RM_IMP));
    rel(home, expr(home, fifthSpeciesNotesCp[fifthSpeciesNotesCp.size()-2]-c->getNotes()[c->getNotes().size()-2]), IRT_EQ, MINOR_THIRD, Reify(bandElse_1, RM_IMP));

    //penultimate note (only applies if it is 4th species)
    BoolVar bandThen = BoolVar(home, 0, 1);
    BoolVar testNot = BoolVar(home, 0, 1);
    BoolVar bandElse = BoolVar(home, 0, 1);
    BoolVar test = expr(home, c->getNotes()[c->getNotes().size()-2] <= fifthSpeciesNotesCp[fifthSpeciesNotesCp.size()-3]);

    rel(home, test, BOT_AND, isThirdSpeciesArray[isThirdSpeciesArray.size()-2], bandThen);
    rel(home, test, BOT_EQV, BoolVar(home, 0, 0), testNot);
    rel(home, testNot, BOT_AND, isThirdSpeciesArray[isThirdSpeciesArray.size()-2], bandElse);
    rel(home, expr(home, fifthSpeciesNotesCp[fifthSpeciesNotesCp.size()-3]-c->getNotes()[c->getNotes().size()-2]), IRT_EQ, MAJOR_SIXTH, Reify(bandThen, RM_IMP));
    rel(home, expr(home, fifthSpeciesNotesCp[fifthSpeciesNotesCp.size()-3]-c->getNotes()[c->getNotes().size()-2]), IRT_EQ, MINOR_THIRD, Reify(bandElse, RM_IMP));

    //no direct motion to reach a perfect consonance
    for(int i = 0; i < fifthSpeciesMotions.size(); i++){
        BoolVar isThirdSpeciesAndPCons = BoolVar(home, 0, 1);
        BoolVar isPConsAndNotLowest = BoolVar(home, 0, 1);

        rel(home, isThirdSpeciesArray[i*4], BOT_AND, expr(home, fifthSpeciesHIntervals[(i*4)+3]==UNISSON || fifthSpeciesHIntervals[(i*4)+3]==PERFECT_FIFTH ||
            fifthSpeciesHIntervals[(i*4)+3]==-PERFECT_FIFTH), isThirdSpeciesAndPCons);
        rel(home, isNotLowest[i], BOT_AND, isThirdSpeciesAndPCons, isPConsAndNotLowest);

        rel(home, fifthSpeciesMotions[i], IRT_NQ, PARALLEL_MOTION, Reify(isPConsAndNotLowest, RM_IMP));
    
    }

    costs = IntVarArray(home, 8, 0, 1000000);
    cost_names = {"fifth", "octave", "borrow", "melodic", "motion", "cambiata", "m2", "syncopation"};

    //set cost[0] to be fifth cost
    add_cost(home, 0, IntVarArray(home, fifthCostArray.slice(0, 4/notesPerMeasure.at(FIFTH_SPECIES), fifthCostArray.size())), costs);
    //set cost[1] to be octave cost
    add_cost(home, 1, IntVarArray(home, octaveCostArray.slice(0, 4/notesPerMeasure.at(FIFTH_SPECIES), octaveCostArray.size())), costs);
    //set cost[2] to be off cost
    add_cost(home, 2, IntVarArray(home, offCostArray.slice(0, 4/notesPerMeasure.at(FIFTH_SPECIES), offCostArray.size())), costs);
    //set cost[3] to be melodic cost
    add_cost(home, 3, IntVarArray(home, melodicDegreeCost.slice(0, 4/notesPerMeasure.at(FIFTH_SPECIES), melodicDegreeCost.size())), costs);
    //set cost[4] to be motion cost
    add_cost(home, 4, fifthSpeciesMotionsCosts, costs);
    //need to set cost[5] to be cambiata cost
    add_cost(home, 5, cambiataCostArray, costs);
    //need to set cost[6] to be m2Zero cost
    add_cost(home, 6, m2ZeroCostArray, costs);
    //need to set cost[7] to be syncopation cost
    add_cost(home, 7, snycopeCostArray, costs);
}

FifthSpeciesCounterpoint::FifthSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub, Stratum* low, CantusFirmus* c,  int v_type, 
    vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV1, int nV2):
FifthSpeciesCounterpoint(home, nMes, cf, lb, ub, FIFTH_SPECIES, low, c, v_type, m_costs, g_costs, s_costs, bm, nV2)
{
    directCostArray = IntVarArray(home, fifthSpeciesMotions.size()-1,IntSet({0, directMoveCost}));
    varietyCostArray = IntVarArray(home, 3*(fifthSpeciesHIntervals.size()-2), IntSet({0, varietyCost}));

    //3.H6 : harmonic triad should be used on the second or third beat
    thirdHTriadArray = IntVarArray(home, nMeasures-1, IntSet({0, triad3rdCost}));
    for(int i = 0; i < thirdHTriadArray.size(); i++){
        rel(home, ((fifthSpeciesHIntervals[(i*4)+1]!=UNISSON&&fifthSpeciesHIntervals[(i*4)+1]!=MINOR_THIRD&&fifthSpeciesHIntervals[(i*4)+1]!=MAJOR_THIRD&&fifthSpeciesHIntervals[(i*4)+1]!=PERFECT_FIFTH)&&
            (fifthSpeciesHIntervals[(i*4)+2]!=UNISSON&&fifthSpeciesHIntervals[(i*4)+2]!=MINOR_THIRD&&fifthSpeciesHIntervals[(i*4)+2]!=MAJOR_THIRD&&fifthSpeciesHIntervals[(i*4)+2]!=PERFECT_FIFTH)) >> 
            (thirdHTriadArray[i]==triad3rdCost));
        rel(home, ((fifthSpeciesHIntervals[(i*4)+1]==UNISSON||fifthSpeciesHIntervals[(i*4)+1]==MINOR_THIRD||fifthSpeciesHIntervals[(i*4)+1]==MAJOR_THIRD||fifthSpeciesHIntervals[(i*4)+1]==PERFECT_FIFTH)||
            (fifthSpeciesHIntervals[(i*4)+2]==UNISSON||fifthSpeciesHIntervals[(i*4)+2]==MINOR_THIRD||fifthSpeciesHIntervals[(i*4)+2]==MAJOR_THIRD||fifthSpeciesHIntervals[(i*4)+2]==PERFECT_FIFTH)) >> 
            (thirdHTriadArray[i]==0));
    }

    //1.P1 3 voices version
    for(int j = 0; j < fifthSpeciesMotions.size()-1; j++){
        //set a cost when it is reached through direct motion, it is 0 when not
        rel(home, (fifthSpeciesMotions[j]==2&&(firstSpeciesHarmonicIntervals[j+1]==0||firstSpeciesHarmonicIntervals[j+1]==7))>>
            (directCostArray[j]==directMoveCost));
        rel(home, (fifthSpeciesMotions[j]!=2||(firstSpeciesHarmonicIntervals[j+1]!=0&&firstSpeciesHarmonicIntervals[j+1]!=7))>>
            (directCostArray[j]==0));
    }

    costs = IntVarArray(home, 11, 0, 1000000);
    cost_names = {"fifth", "octave", "borrow", "melodic", "motion", "cambiata", "m2", "syncopation", "direct", "variety", "triad3"};

    //set cost[0] to be fifth cost
    add_cost(home, 0, IntVarArray(home, fifthCostArray.slice(1, 4/notesPerMeasure.at(FIFTH_SPECIES), fifthCostArray.size())), costs);
    //set cost[1] to be octave cost
    add_cost(home, 1, IntVarArray(home, octaveCostArray.slice(1, 4/notesPerMeasure.at(FIFTH_SPECIES), octaveCostArray.size())), costs);
    //set cost[2] to be off cost
    add_cost(home, 2, IntVarArray(home, offCostArray.slice(0, 4/notesPerMeasure.at(FIFTH_SPECIES), offCostArray.size())), costs);
    //set cost[3] to be melodic cost
    add_cost(home, 3, IntVarArray(home, melodicDegreeCost.slice(0, 4/notesPerMeasure.at(FIFTH_SPECIES), melodicDegreeCost.size())), costs);
    //set cost[4] to be motion cost
    add_cost(home, 4, fifthSpeciesMotionsCosts, costs);
    //need to set cost[5] to be cambiata cost
    add_cost(home, 5, cambiataCostArray, costs);
    //need to set cost[6] to be m2Zero cost
    add_cost(home, 6, m2ZeroCostArray, costs);
    //need to set cost[7] to be syncopation cost
    add_cost(home, 7, snycopeCostArray, costs);
    //need to set cost[8] to be syncopation cost
    add_cost(home, 8, directCostArray, costs);
    //need to set cost[8] to be syncopation cost
    add_cost(home, 9, varietyCostArray, costs);
    //set cost[10] to be triad h third cost
    add_cost(home, 10, thirdHTriadArray, costs);
}

FifthSpeciesCounterpoint::FifthSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub, Stratum* low, CantusFirmus* c,  int v_type, 
    vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV1, int nV2, int nV3):
FifthSpeciesCounterpoint(home, nMes, cf, lb, ub, FIFTH_SPECIES, low, c, v_type, m_costs, g_costs, s_costs, bm, nV3)
{
    directCostArray = IntVarArray(home, fifthSpeciesMotions.size()-1,IntSet({0, directMoveCost}));
    varietyCostArray = IntVarArray(home, 3*(fifthSpeciesHIntervals.size()-2), IntSet({0, varietyCost}));

    //3.H6 : harmonic triad should be used on the second or third beat
    thirdHTriadArray = IntVarArray(home, nMeasures-1, IntSet({0, triad3rdCost}));
    for(int i = 0; i < thirdHTriadArray.size(); i++){
        rel(home, ((fifthSpeciesHIntervals[(i*4)+1]!=UNISSON&&fifthSpeciesHIntervals[(i*4)+1]!=MINOR_THIRD&&fifthSpeciesHIntervals[(i*4)+1]!=MAJOR_THIRD&&fifthSpeciesHIntervals[(i*4)+1]!=PERFECT_FIFTH)&&
            (fifthSpeciesHIntervals[(i*4)+2]!=UNISSON&&fifthSpeciesHIntervals[(i*4)+2]!=MINOR_THIRD&&fifthSpeciesHIntervals[(i*4)+2]!=MAJOR_THIRD&&fifthSpeciesHIntervals[(i*4)+2]!=PERFECT_FIFTH)) >> 
            (thirdHTriadArray[i]==triad3rdCost));
        rel(home, ((fifthSpeciesHIntervals[(i*4)+1]==UNISSON||fifthSpeciesHIntervals[(i*4)+1]==MINOR_THIRD||fifthSpeciesHIntervals[(i*4)+1]==MAJOR_THIRD||fifthSpeciesHIntervals[(i*4)+1]==PERFECT_FIFTH)||
            (fifthSpeciesHIntervals[(i*4)+2]==UNISSON||fifthSpeciesHIntervals[(i*4)+2]==MINOR_THIRD||fifthSpeciesHIntervals[(i*4)+2]==MAJOR_THIRD||fifthSpeciesHIntervals[(i*4)+2]==PERFECT_FIFTH)) >> 
            (thirdHTriadArray[i]==0));
    }

    //1.P1 4 voices version
    for(int j = 0; j < fifthSpeciesMotions.size()-1; j++){
        //set a cost when it is reached through direct motion, it is 0 when not
        rel(home, (fifthSpeciesMotions[j]==2&&(firstSpeciesHarmonicIntervals[j+1]==0||firstSpeciesHarmonicIntervals[j+1]==7))>>
            (directCostArray[j]==directMoveCost));
        rel(home, (fifthSpeciesMotions[j]!=2||(firstSpeciesHarmonicIntervals[j+1]!=0&&firstSpeciesHarmonicIntervals[j+1]!=7))>>
            (directCostArray[j]==0));
    }

    costs = IntVarArray(home, 11, 0, 1000000);
    cost_names = {"fifth", "octave", "borrow", "melodic", "motion", "cambiata", "m2", "syncopation", "direct", "variety", "triad3"};

    //set cost[0] to be fifth cost
    add_cost(home, 0, IntVarArray(home, fifthCostArray.slice(1, 4/notesPerMeasure.at(FIFTH_SPECIES), fifthCostArray.size())), costs);
    //set cost[1] to be octave cost
    add_cost(home, 1, IntVarArray(home, octaveCostArray.slice(1, 4/notesPerMeasure.at(FIFTH_SPECIES), octaveCostArray.size())), costs);
    //set cost[2] to be off cost
    add_cost(home, 2, IntVarArray(home, offCostArray.slice(0, 4/notesPerMeasure.at(FIFTH_SPECIES), offCostArray.size())), costs);
    //set cost[3] to be melodic cost
    add_cost(home, 3, IntVarArray(home, melodicDegreeCost.slice(0, 4/notesPerMeasure.at(FIFTH_SPECIES), melodicDegreeCost.size())), costs);
    //set cost[4] to be motion cost
    add_cost(home, 4, fifthSpeciesMotionsCosts, costs);
    //need to set cost[5] to be cambiata cost
    add_cost(home, 5, cambiataCostArray, costs);
    //need to set cost[6] to be m2Zero cost
    add_cost(home, 6, m2ZeroCostArray, costs);
    //need to set cost[7] to be syncopation cost
    add_cost(home, 7, snycopeCostArray, costs);
    //need to set cost[8] to be syncopation cost
    add_cost(home, 8, directCostArray, costs);
    //need to set cost[8] to be syncopation cost
    add_cost(home, 9, varietyCostArray, costs);
    //set cost[10] to be triad h third cost
    add_cost(home, 10, thirdHTriadArray, costs);
}

/**
 * This function returns a string with the characteristics of the counterpoint. It calls the to_string() method from
 * the Part class and adds 1st species specific characteristics.
 * @return a string representation of the current instance of the FirstSpeciesCounterpoint class.
 */
string FifthSpeciesCounterpoint::to_string() const {
    string text = Part::to_string() + "\n Fifth species :\n";
    text += "Fifth species notes : " + intVarArray_to_string(fifthSpeciesNotesCp) + "\n";
    text += "M Intervals array : " + intVarArray_to_string(fifthSpeciesSuccMIntervals) + "\n";
    text += "isMostlyThird : " + boolVarArray_to_string(isMostlyThirdArray) + "\n";
    text += "not Lowest array : " + boolVarArray_to_string(isNotLowest) + "\n";
    text += "Species array : " + intVarArray_to_string(speciesArray) + "\n";
    return text;
}

// clone constructor
FifthSpeciesCounterpoint::FifthSpeciesCounterpoint(Home home, FifthSpeciesCounterpoint &s) : Part(home, s){
    solutionLength = s.solutionLength;
    m2Len = s.m2Len;
    fifthSpeciesNotesCp.update(home, s.fifthSpeciesNotesCp);
    fifthSpeciesHIntervals.update(home, s.fifthSpeciesHIntervals);
    firstHInterval.update(home, s.firstHInterval);
    isNthSpeciesArray.update(home, s.isNthSpeciesArray);
    isConstrainedArray.update(home, s.isConstrainedArray);
    fifthSpeciesSuccMIntervals.update(home, s.fifthSpeciesSuccMIntervals);
    fifthSpeciesMIntervals.update(home, s.fifthSpeciesMIntervals);
    fifthSpeciesMTAIntervals.update(home, s.fifthSpeciesMTAIntervals);
    fifthSpeciesM2Intervals.update(home, s.fifthSpeciesM2Intervals);
    fifthSpeciesMAllIntervals.update(home, s.fifthSpeciesMAllIntervals);
    fifthSpeciesMotions.update(home, s.fifthSpeciesMotions);
    fifthSpeciesMotionsCosts.update(home, s.fifthSpeciesMotionsCosts);
    isMostlyThirdArray.update(home, s.isMostlyThirdArray);
    isFourthSpeciesArray.update(home, s.isFourthSpeciesArray);
    isThirdSpeciesArray.update(home, s.isThirdSpeciesArray);
    isNotCambiata.update(home, s.isNotCambiata);
    m2ZeroCostArray.update(home, s.m2ZeroCostArray);
    thirdHTriadArray.update(home, s.thirdHTriadArray);
}

FifthSpeciesCounterpoint* FifthSpeciesCounterpoint::clone(Home home){
    return new FifthSpeciesCounterpoint(home, *this);
}

IntVarArray FifthSpeciesCounterpoint::getBranchingNotes(){
    return fifthSpeciesNotesCp;
}

IntVarArray FifthSpeciesCounterpoint::getFirstHInterval(){
    return firstSpeciesHarmonicIntervals;
}

IntVarArray FifthSpeciesCounterpoint::getMotions(){
    return fifthSpeciesMotions;
}

IntVarArray FifthSpeciesCounterpoint::getFirstMInterval(){
    return firstSpeciesMelodicIntervals;
}

int FifthSpeciesCounterpoint::getHIntervalSize(){
    return fifthSpeciesHIntervals.size();
}

void FifthSpeciesCounterpoint::createSpeciesArrays(Home home){
    IntVarArray countThird = IntVarArray(home, solutionLength, 0, 1);
    IntVarArray countFourth = IntVarArray(home, solutionLength, 0, 1);
    int nThirdInt = floor((prefSlider-1)*0.66);
    int nFourthInt = floor(prefSlider*0.5);
    IntVar sumThird = IntVar(home, nThirdInt, solutionLength);
    IntVar sumFourth = IntVar(home, nFourthInt, solutionLength);
    
    //Count 3rd and 4th species
    for(int i = 0; i < solutionLength; i++){
        BoolVar b = BoolVar(home, 0, 1);
        rel(home, speciesArray[i], IRT_EQ, THIRD_SPECIES, Reify(b));
        ite(home, b, IntVar(home, 1, 1), IntVar(home, 0, 0), countThird[i]);
    }

    for(int i = 0; i < solutionLength; i++){
        BoolVar b = BoolVar(home, 0, 1);
        rel(home, speciesArray[i], IRT_EQ, FOURTH_SPECIES, Reify(b));
        ite(home, b, IntVar(home, 1, 1), IntVar(home, 0, 0), countFourth[i]);
    }
    
    //sum the counts
    //rel(home, sumThird, IRT_EQ, expr(home, sum(countThird)));
    //rel(home, sumFourth, IRT_EQ, expr(home, sum(countFourth)));

    //no 4th species in second and fourth positions in the species array
    for(int i = 1; i < solutionLength; i+=2){
        rel(home, speciesArray[i], IRT_NQ, FOURTH_SPECIES);
    }

    //4th species in the third position is followed by no species (no note / constraint) and then a 4th species
    for(int i = 0; i < solutionLength-1; i+=4){
        BoolVar b34 = BoolVar(home, 0, 1);
        BoolVar b14 = BoolVar(home, 0, 1);

        rel(home, speciesArray[i+2], IRT_EQ, FOURTH_SPECIES, Reify(b34));
        //the following line works to allow quarter notes after fourth species, but behaves weird
        //rel(home, expr(home, speciesArray[i+4]==FOURTH_SPECIES), BOT_OR, expr(home, speciesArray[i+4]==THIRD_SPECIES), b14);
        rel(home, speciesArray[i+4], IRT_EQ, FOURTH_SPECIES, Reify(b14));
        rel(home, speciesArray[i+3], IRT_EQ, -1, Reify(b34, RM_IMP));
        rel(home, b34, BOT_EQV, b14, 1);
    }
   
    //only 3rd and 4th species
    for(int i = 0; i < speciesArray.size(); i++){
        rel(home, speciesArray[i], IRT_NQ, FIRST_SPECIES);
        rel(home, speciesArray[i], IRT_NQ, SECOND_SPECIES);
    }
 
    //first and penultimate measures are 4th species
    rel(home, speciesArray[0], IRT_EQ, -1);
    rel(home, speciesArray[1], IRT_EQ, -1);
    rel(home, speciesArray[2], IRT_EQ, FOURTH_SPECIES);

    rel(home, speciesArray[solutionLength-5], IRT_EQ, FOURTH_SPECIES);
    rel(home, speciesArray[solutionLength-4], IRT_EQ, -1);
    rel(home, speciesArray[solutionLength-3], IRT_EQ, FOURTH_SPECIES);
    
    //no silence after third species notes
    for(int i = 0; i < solutionLength-1; i++){
        BoolVar b = BoolVar(home, 0, 1);
        rel(home, speciesArray[i], IRT_EQ, THIRD_SPECIES, Reify(b));
        rel(home, speciesArray[i+1], IRT_NQ, -1, Reify(b, RM_IMP));
    }
    
    //no silence after fourth species notes
    for(int i = 0; i < solutionLength-5; i++){
        BoolVar b = BoolVar(home, 0, 1);
        rel(home, speciesArray[i], IRT_EQ, FOURTH_SPECIES, Reify(b));
        rel(home, speciesArray[i+4], IRT_NQ, -1, Reify(b, RM_IMP));
    }
    
    //maximum two consecutive measures without a fourth species
    for(int i = 2; i < solutionLength-13; i+=4){
        BoolVar b1n4 = BoolVar(home, 0, 1);
        BoolVar b2n4 = BoolVar(home, 0, 1);
        BoolVar band = BoolVar(home, 0, 1);

        rel(home, speciesArray[i+4], IRT_NQ, FOURTH_SPECIES, Reify(b1n4));
        rel(home, speciesArray[i+8], IRT_NQ, FOURTH_SPECIES, Reify(b2n4));
        rel(home, b1n4, BOT_AND, b2n4, band);
        rel(home, speciesArray[i+12], IRT_EQ, FOURTH_SPECIES, Reify(band, RM_IMP));
    }
    
    /**
     * CREATE NTH SPECIES ARRAY
     */
    cout << speciesArray.size() << endl;
    cout << isNthSpeciesArray.size() << endl;
    for(int i = 0; i < isNthSpeciesArray.size(); i+=5){
        rel(home, speciesArray[floor(i/5)], IRT_EQ, -1, Reify(isNthSpeciesArray[i]));
        rel(home, speciesArray[floor(i/5)], IRT_EQ, FIRST_SPECIES, Reify(isNthSpeciesArray[i+1]));
        rel(home, speciesArray[floor(i/5)], IRT_EQ, SECOND_SPECIES, Reify(isNthSpeciesArray[i+2]));
        rel(home, speciesArray[floor(i/5)], IRT_EQ, THIRD_SPECIES, Reify(isNthSpeciesArray[i+3]));
        rel(home, speciesArray[floor(i/5)], IRT_EQ, FOURTH_SPECIES, Reify(isNthSpeciesArray[i+4]));
    }
    
    /**
     * CREATE IS CONSTRAINED ARRAY
     */
    for(int i = 0; i < isConstrainedArray.size(); i++){
        rel(home, isNthSpeciesArray[i*5], IRT_EQ, 0, Reify(isConstrainedArray[i]));
    }
}
