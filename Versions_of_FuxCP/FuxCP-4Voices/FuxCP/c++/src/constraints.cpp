// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// This file contains the implementations of the functions that post the constraints.  
//

#include "../headers/constraints.hpp"

void initializeIsOffArray(Home home, Part* part){
    for(int i = 0; i < part->getIsOffArray().size(); i++){                              //loop goes through every note of the counterpoint
        IntVarArray res = IntVarArray(home, part->getOffDomain().size(), 0, 1);
        IntVar sm = IntVar(home, 0, part->getOffDomain().size());
        for(int l = 0; l < part->getOffDomain().size(); l++){                           //loop goes through the domain of borrowed notes
            BoolVar b1 = BoolVar(home, 0, 1);
            rel(home, part->getNotes()[i], IRT_EQ, part->getOffDomain()[l], Reify(b1)); //checks if a note is borrowed
            ite(home, b1, IntVar(home, 1, 1), IntVar(home, 0, 0), res[l]);              //if it is borrowed, set res[l] to one
        }
        IntVarArgs x(res.size());
        for(int t = 0; t < part->getOffDomain().size(); t++){
            x[t] = res[t];                                                              //create an IntVarArgs array of the contents of res[l] (for sum)
        }
        rel(home, sm, IRT_EQ, expr(home, sum(x)));                                      //set the sm variable to be the sum of the IntVarArgs
        rel(home, sm, IRT_GR, 0, Reify(part->getIsOffArray()[i]));                      //if the sum is greater than 0, then the note is borrowed (off key)
    }
}

void twoFifthSpeciesDiversity_3v(Home home, Part* cp1, Part* cp2){
    if(cp1->getSpecies()==FIFTH_SPECIES && cp2->getSpecies()==FIFTH_SPECIES){
        BoolVarArray isSameSpecies = BoolVarArray(home, cp1->getNotes().size(), 0, 1);
        IntVarArray isSameSpeciesInt = IntVarArray(home, cp1->getNotes().size(), 0, 1);
        IntVar percentageSame = IntVar(home, 0, cp1->getNotes().size());
        for(int i = 0; i < cp1->getNotes().size(); i++){
            rel(home, cp1->getSpeciesArray()[i], IRT_EQ, cp2->getSpeciesArray()[i], Reify(isSameSpecies[i]));
            rel(home, isSameSpeciesInt[i], IRT_EQ, 1, Reify(isSameSpecies[i]));
        }
        rel(home, percentageSame, IRT_EQ, expr(home, sum(isSameSpeciesInt)));
        rel(home, percentageSame, IRT_LE, floor(cp1->getNotes().size()/2));
    }
}

void noMinorSecondBetweenUpper(Home home, vector<Part*> parts){
    for(int v1 = 0; v1 < parts.size(); v1++){
        for(int v2 = v1+1; v2 < parts.size(); v2++){
            for(int i = 0; i < parts[1]->getNMeasures(); i++){
                BoolVar noneLowest = BoolVar(home, 0, 1);
                rel(home, parts[v1]->getIsNotLowest()[i], IRT_EQ, parts[v2]->getIsNotLowest()[i], Reify(noneLowest, RM_PMI));

                IntVar interval = IntVar(home, -PERFECT_OCTAVE, PERFECT_OCTAVE);
                if(i==parts[1]->getNMeasures()-1){
                    rel(home, interval == ((parts[v1]->getFirstNotes()[i]-parts[v2]->getFirstNotes()[i])%12));
                }
                else if(parts[v1]->getSpecies()!=FOURTH_SPECIES&&parts[v2]->getSpecies()!=FOURTH_SPECIES){
                    rel(home, interval == ((parts[v1]->getFirstNotes()[i]-parts[v2]->getFirstNotes()[i])%12));
                } else if(parts[v1]->getSpecies()==FOURTH_SPECIES&&parts[v2]->getSpecies()!=FOURTH_SPECIES){
                    rel(home, interval == ((parts[v1]->getNotes()[(i*4)+2]-parts[v2]->getFirstNotes()[i])%12));
                } else if(parts[v1]->getSpecies()!=FOURTH_SPECIES&&parts[v2]->getSpecies()==FOURTH_SPECIES){
                    rel(home, interval == ((parts[v1]->getFirstNotes()[i]-parts[v2]->getNotes()[(i*4)+2])%12));
                } else {
                    rel(home, interval == ((parts[v1]->getNotes()[(i*4)+2]-parts[v2]->getNotes()[(i*4)+2])%12));
                }
                rel(home, (noneLowest==1) >> (expr(home, abs(interval)!=1)));
            }
        }
    }
}

void G4_counterpointMustBeInTheSameKey(Home home, Part* part){
    for(int i = 0; i < part->getIsOffArray().size(); i++){
        rel(home, (part->getIsOffArray()[i]==0) >> (part->getOffCostArray()[i]==0));                     //sets no cost if not borrowed
        rel(home, (part->getIsOffArray()[i]==1) >> (part->getOffCostArray()[i]==part->getBorrowCost())); //sets a cost if borrowed
    }
}

void G6_noChromaticMelodies(Home home, Part* part, int mSpec){

    //forbids that three notes one after the other increase by a step, making the melody chromatic

    for(int i = 0; i < part->getMelodicIntervals().size()-1; i+=4/notesPerMeasure.at(mSpec)){
        rel(home, expr(home, part->getMelodicIntervals()[i]==1), BOT_AND, expr(home, part->getMelodicIntervals()[i+1]==1), 0);
        rel(home, expr(home, part->getMelodicIntervals()[i]==-1), BOT_AND, expr(home, part->getMelodicIntervals()[i+1]==-1), 0);
    }
}

void G7_melodicIntervalsShouldBeSmall(Home home, Part* part, int mSpec){

    //loop goes through every note of the melodic interval and sets the cost

    for(int i = 0; i < part->getMelodicIntervals().size(); i+=4/notesPerMeasure.at(mSpec)){
        rel(home, (abs(part->getMelodicIntervals()[i])<MINOR_THIRD) >> (part->getMelodicDegreeCost()[i]==part->getSecondCost()));
        rel(home, (abs(part->getMelodicIntervals()[i])==MINOR_THIRD || abs(part->getMelodicIntervals()[i])==MAJOR_THIRD) >> (part->getMelodicDegreeCost()[i]==part->getThirdCost()));
        rel(home, (abs(part->getMelodicIntervals()[i])==PERFECT_FOURTH) >> (part->getMelodicDegreeCost()[i]==part->getFourthCost()));
        rel(home, (abs(part->getMelodicIntervals()[i])==TRITONE) >> (part->getMelodicDegreeCost()[i]==part->getTritoneCost()));
        rel(home, (abs(part->getMelodicIntervals()[i])==PERFECT_FIFTH) >> (part->getMelodicDegreeCost()[i]==part->getFifthCost()));
        rel(home, (abs(part->getMelodicIntervals()[i])==MINOR_SIXTH || abs(part->getMelodicIntervals()[i])==MAJOR_SIXTH) >> (part->getMelodicDegreeCost()[i]==part->getSixthCost()));
        rel(home, (abs(part->getMelodicIntervals()[i])==MINOR_SEVENTH || abs(part->getMelodicIntervals()[i])==MAJOR_SEVENTH) >> (part->getMelodicDegreeCost()[i]==part->getSeventhCost()));
        rel(home, (abs(part->getMelodicIntervals()[i])==PERFECT_OCTAVE) >> (part->getMelodicDegreeCost()[i]==part->getOctaveCost()));
    }
}

void G9_lastChordSameAsFundamental(Home home, Stratum* lowest, Part* cantusFirmus){

    //we check the last note of the cantusFirmus to set the last note of the counterpoint to be in the same key

    rel(home, expr(home, lowest->getNotes()[lowest->getNotes().size()-1]%12), IRT_EQ, expr(home, cantusFirmus->getNotes()[cantusFirmus->getNotes().size()-1]%12));
}   

void H1_1_harmonicIntervalsAreConsonances(Home home, Part* part){

    //we also include negative values, since the fourth species could render the harmonic intervals to be negative because of its deplacement
    //mostly the harmonic intervals are going to be positive

    dom(home, part->getFirstSpeciesHIntervals(), IntSet({UNISSON, MINOR_THIRD, MAJOR_THIRD, PERFECT_FIFTH, MINOR_SIXTH, MAJOR_SIXTH, PERFECT_OCTAVE, 
        -MINOR_THIRD, -MAJOR_THIRD, -PERFECT_FIFTH, -MINOR_SIXTH, -MAJOR_SIXTH, -PERFECT_OCTAVE}));
}

void H1_3_fiveConsecutiveNotesByJointDegree(Home home, Part* part){
    for(int i = 0; i < part->getIs5QNArray().size(); i++){
        rel(home, (part->getIs5QNArray()[i]==1) >> (part->getConsonance()[(i*4)+2]==1));
    }
}

void H2_1_startWithPerfectConsonance(Home home, Part* part){
    dom(home, part->getHInterval()[0], IntSet(IntArgs(PERFECT_CONSONANCES)));
}

void H2_2_arsisHarmoniesCannotBeDisonnant(Home home, Part* part){
    
    for(int i = 0; i < part->getNMeasures()-1; i++){
        if(i != part->getNMeasures()-2){ //if it is the penultimate measure
            for(int d : DISONANCE){
                rel(home, part->getHInterval()[(i*4)+2], IRT_EQ, d, Reify(part->getIsDiminution()[i], RM_PMI));
            }
        }
    }
}

void H2_3_disonanceImpliesDiminution(Home home, Part* part){
    for(int i = 0; i < part->getIsDiminution().size(); i++){
        BoolVar band1 = BoolVar(home, 0, 1);
        BoolVar band2 = BoolVar(home, 0, 1);

        rel(home, part->getConsonance()[(i*4)+2], BOT_OR, part->getIsDiminution()[i], 1);
    }
}

void H3_1_endWithPerfectConsonance(Home home, Part* part){
    dom(home, part->getHInterval()[part->getHInterval().size()-1], IntSet(IntArgs(PERFECT_CONSONANCES)));
}

void H3_2_penultimateNoteDomain(Home home, Part* part){ 
    dom(home, expr(home, abs(part->getHInterval()[part->getHIntervalSize()-3])), IntSet({UNISSON, PERFECT_FIFTH, MINOR_SIXTH, MAJOR_SIXTH}));

    rel(home, (part->getHInterval()[part->getHIntervalSize()-3]!=PERFECT_FIFTH) >> (part->getPenultCostArray()[0]==part->getPenultCost()));
    rel(home, (part->getHInterval()[part->getHIntervalSize()-3]==PERFECT_FIFTH) >> (part->getPenultCostArray()[0]==0));
}

void H3_3_cambiataCost(Home home, Part* part){
    for(int i = 0; i < part->getCambiataCostArray().size(); i++){
        rel(home, ((part->getThirdSpeciesHIntervals()[(i*4)+1]==UNISSON || part->getThirdSpeciesHIntervals()[(i*4)+1]==PERFECT_FIFTH)&&(part->getThirdSpeciesHIntervals()[(i*4)+2]==UNISSON || part->getThirdSpeciesHIntervals()[(i*4)+2]==PERFECT_FIFTH)
            &&(abs(part->getThirdSpeciesMIntervals()[(i*4)+1])<=2)) >> (part->getCambiataCostArray()[i]==part->getCambiataCost()));
        rel(home, ((part->getThirdSpeciesHIntervals()[(i*4)+1]!=UNISSON && part->getThirdSpeciesHIntervals()[(i*4)+1]!=PERFECT_FIFTH)||(part->getThirdSpeciesHIntervals()[(i*4)+2]!=UNISSON && part->getThirdSpeciesHIntervals()[(i*4)+2]!=PERFECT_FIFTH)
            ||(abs(part->getThirdSpeciesMIntervals()[(i*4)+1])>2)) >> (part->getCambiataCostArray()[i]==0));
    }
}

void H5_1_cpAndCfDifferentNotes(Home home, Part* part, Part* cf){
    for(int i = 1; i < part->getFirstSpeciesNotes().size()-1; i++){
        rel(home, part->getFirstSpeciesNotes()[i], IRT_NQ, cf->getNotes()[i]);
    }
}

void H5_1_differentNotes(Home home, vector<Part*> parts){
    for(int v1 = 0; v1 < parts.size(); v1++){
        for(int v2 = v1+1; v2 < parts.size(); v2++){
            for(int i = 1; i < parts[v1]->getNotes().size()-1; i++){
                if(parts[v1]->getSpecies()==CANTUS_FIRMUS){
                    rel(home, parts[v1]->getNotes()[floor(i/4)], IRT_NQ, parts[v2]->getNotes()[i]);
                } else {
                    rel(home, parts[v1]->getNotes()[i], IRT_NQ, parts[v2]->getNotes()[i]);
                }
            }
        }
    }
}

void H6_1_preferImperfectConsonances(Home home, Part* part){
    for(int i = 0; i < part->getHInterval().size(); i++){
        //set the octave cost
        rel(home, part->getOctaveCostArray()[i], IRT_EQ, part->getHOctaveCost(), Reify(expr(home, part->getHInterval()[i]==UNISSON), RM_PMI));
        rel(home, part->getOctaveCostArray()[i], IRT_EQ, 0, Reify(expr(home, part->getHInterval()[i]!=UNISSON), RM_PMI));

        //set the fifth cost
        rel(home, part->getFifthCostArray()[i], IRT_EQ, part->getHFifthCost(), Reify(expr(home, part->getHInterval()[i]==PERFECT_FIFTH), RM_PMI));
        rel(home, part->getFifthCostArray()[i], IRT_EQ, 0, Reify(expr(home, part->getHInterval()[i]!=PERFECT_FIFTH), RM_PMI));
    }
}

void H7_1_2v_penultimateSixthOrThird(Home home, Part* part){
    int p = part->getFirstSpeciesNotes().size()-2; //index of the penultimate note

    //if it is the lowest, then it is a major sixth (cf version in the cantusFirmus class)
    rel(home, part->getFirstSpeciesHIntervals()[part->getFirstSpeciesHIntervals().size()-2], IRT_EQ, MAJOR_SIXTH, 
        Reify(part->getIsNotLowest()[part->getIsNotLowest().size()-2], RM_IMP));
}

void H7_1_3v_penultimateSixthOrThird(Home home, Part* part){
    dom(home, part->getFirstSpeciesHIntervals()[part->getFirstSpeciesHIntervals().size()-2], IntSet({UNISSON, MINOR_THIRD, PERFECT_FIFTH, MAJOR_SIXTH}));
}

void H8_3v_preferHarmonicTriad(Home home, Part* part, IntVarArray triadCostArray, Stratum* upper1, Stratum* upper2){
    for(int i = 0; i < triadCostArray.size(); i++){
        BoolVar h1_3 = BoolVar(home, 0 ,1);         //check if the first interval is a minor third
        BoolVar h1_4 = BoolVar(home, 0 ,1);         //check if the first interval is a major third
        BoolVar h1_third = BoolVar(home, 0 ,1);     //check is the first interval is a third
        BoolVar h1_7 = BoolVar(home, 0 ,1);         //check if the first interval is a perfect fifth

        BoolVar h2_3 = BoolVar(home, 0 ,1);         //check if the second interval is a minor third
        BoolVar h2_4 = BoolVar(home, 0 ,1);         //check if the second interval is a major third
        BoolVar h2_third = BoolVar(home, 0 ,1);     //check if the second interval is a third
        BoolVar h2_7 = BoolVar(home, 0 ,1);         //check if the second interval is a perfect fifth

        BoolVar h_firstPoss = BoolVar(home, 0, 1);  //check if the first interval is a third and the second a perfect fifth
        BoolVar h_secondPoss = BoolVar(home, 0, 1); //check if the second interval is a third and the first a perfect fifth
        BoolVar triad = BoolVar(home, 0, 1);        //check if it is a triad
        BoolVar not_triad = BoolVar(home, 0, 1);    //check if it is not a triad

        //check for the first possibility
        rel(home, expr(home, abs(upper1->getHInterval()[i*4])), IRT_EQ, 3, Reify(h1_3));
        rel(home, expr(home, abs(upper1->getHInterval()[i*4])), IRT_EQ, 4, Reify(h1_4));
        rel(home, expr(home, abs(upper2->getHInterval()[i*4])), IRT_EQ, 7, Reify(h2_7));
        rel(home, h1_3, BOT_OR, h1_4, h1_third);
        rel(home, h1_third, BOT_AND, h2_7, h_firstPoss);

        //check for the second possibility
        rel(home, expr(home, abs(upper2->getHInterval()[i*4])), IRT_EQ, 3, Reify(h2_3));
        rel(home, expr(home, abs(upper2->getHInterval()[i*4])), IRT_EQ, 4, Reify(h2_4));
        rel(home, expr(home, abs(upper1->getHInterval()[i*4])), IRT_EQ, 7, Reify(h1_7));
        rel(home, h2_3, BOT_OR, h2_4, h2_third);
        rel(home, h2_third, BOT_AND, h1_7, h_secondPoss);

        rel(home, h_firstPoss, BOT_OR, h_secondPoss, triad);   //is a triad if it is either the first or the second possibility
        rel(home, triad, BOT_XOR, not_triad, 1);               //set not triad
        rel(home, triadCostArray[i], IRT_EQ, 0, Reify(triad, RM_IMP));
        rel(home, triadCostArray[i], IRT_EQ, part->getTriadCost(), Reify(not_triad, RM_IMP));
    }
}

void H8_4v_preferHarmonicTriad(Home home, IntVarArray triadCostArray, Stratum* upper1, Stratum* upper2, Stratum* upper3){
    for(int i = 0; i < triadCostArray.size(); i++){

        IntVar H_b = upper1->getHInterval()[i*4];
        IntVar H_c = upper2->getHInterval()[i*4];
        IntVar H_d = upper3->getHInterval()[i*4];


        BoolVar H_b_is_third    = expr(home, H_b==MINOR_THIRD || H_b==MAJOR_THIRD);
        BoolVar H_b_is_fifth    = expr(home, H_b==PERFECT_FIFTH);
        BoolVar H_b_is_octave   = expr(home, H_b==UNISSON);

        BoolVar H_c_is_third    = expr(home, H_c==MINOR_THIRD || H_c==MAJOR_THIRD);
        BoolVar H_c_is_fifth    = expr(home, H_c==PERFECT_FIFTH);
        BoolVar H_c_is_octave   = expr(home, H_c==UNISSON);

        BoolVar H_d_is_third    = expr(home, H_d==MINOR_THIRD || H_d==MAJOR_THIRD);
        BoolVar H_d_is_fifth    = expr(home, H_d==PERFECT_FIFTH);
        BoolVar H_d_is_octave   = expr(home, H_d==UNISSON);


        // worst case : not a harmonic triad with at least a third and a fifth
        BoolVar no_fifth_or_no_third = expr(home, H_b_is_third + H_c_is_third + H_d_is_third == 0 || H_b_is_fifth + H_c_is_fifth + H_d_is_fifth == 0);
        BoolVar note_outside_harmonic_triad = expr(home, H_b_is_third + H_b_is_fifth + H_b_is_octave == 0 || H_c_is_third + H_c_is_fifth + H_c_is_octave == 0 || H_d_is_third + H_d_is_fifth + H_d_is_octave == 0);

        rel(home, (note_outside_harmonic_triad || no_fifth_or_no_third) >> (triadCostArray[i] == not_harmonic_triad_cost));

        // now we are left with only combinations with at a third, a fifth, and another note of the harmonic triad. 
        // Doubling the fifth
        rel(home, expr(home, H_b_is_fifth + H_c_is_fifth + H_d_is_fifth == 2) >> (triadCostArray[i] == double_fifths_cost));

        // Doubling the third, and ensure it is the same type of third (not a major and a minor)
        rel(home, expr(home, H_b_is_third + H_c_is_third + H_d_is_third == 2) >> (triadCostArray[i] == double_thirds_cost));
        rel(home, (H_b_is_third && H_c_is_third) >> (H_b == H_c));
        rel(home, (H_b_is_third && H_d_is_third) >> (H_b == H_d));
        rel(home, (H_c_is_third && H_d_is_third) >> (H_c == H_d));

        // triad with octave
        rel(home, (H_b_is_fifth && H_c_is_third && H_d_is_octave) >> (triadCostArray[i] == triad_with_octave_cost)); // 5 3 8
        rel(home, (H_b_is_third && H_c_is_octave && H_d_is_fifth) >> (triadCostArray[i] == triad_with_octave_cost)); // 3 8 5
        rel(home, (H_b_is_third && H_c_is_fifth && H_d_is_octave) >> (triadCostArray[i] == triad_with_octave_cost)); // 3 5 8
        rel(home, (H_b_is_octave && H_c_is_third && H_d_is_fifth) >> (triadCostArray[i] == triad_with_octave_cost)); // 8 3 5
        rel(home, (H_b_is_octave && H_c_is_fifth && H_d_is_third) >> (triadCostArray[i] == triad_with_octave_cost)); // 8 5 3

        // Best case : 5 8 3
        rel(home, (H_b_is_fifth && H_c_is_octave && H_d_is_third) >> (triadCostArray[i] == 0)); 
    
    }
}

void M1_1_2v_melodicIntervalsNotExceedMinorSixth(Home home, Part* part){
    rel(home, part->getFirstSpeciesMIntervals(), IRT_LQ, MINOR_SIXTH);
    rel(home, part->getFirstSpeciesMIntervals(), IRT_GQ, -MINOR_SIXTH);
}

void M1_1_3v_melodicIntervalsNotExceedMinorSixth(Home home, Part* part){
    dom(home, part->getMelodicIntervals(), IntSet({-UNISSON, -MINOR_SECOND, -MAJOR_SECOND, -MINOR_THIRD, -MAJOR_THIRD, -PERFECT_FOURTH, -TRITONE,
            -PERFECT_FIFTH, -MINOR_SIXTH, -PERFECT_OCTAVE, UNISSON, MINOR_SECOND, MAJOR_SECOND, MINOR_THIRD, MAJOR_THIRD, PERFECT_FOURTH, TRITONE,
            PERFECT_FIFTH, MINOR_SIXTH, PERFECT_OCTAVE}));
}

void M1_2_octaveLeap(Home home, Part* part, Stratum* low){
    for(int j = 0; j < part->getFirstSpeciesMIntervals().size(); j++){
        rel(home, part->getFirstSpeciesMIntervals()[j], IRT_EQ, 12, Reify(expr(home,(abs(part->getFirstSpeciesHIntervals()[j])>=4)&&
            (expr(home, expr(home, abs(low->getMelodicIntervals()[j])>0) == part->getIsNotLowest()[j]))), RM_PMI));
    }
}

void M2_2_2v_twoConsecutiveNotesAreNotTheSame(Home home, Part* part){
    rel(home, part->getSecondSpeciesMIntervals(), IRT_NQ, 0);
}

void M2_2_3v_melodicIntervalsNotExceedMinorSixth(Home home, vector<Part*> parts, bool containsThirdSpecies){
    for(int i = 1; i < parts.size(); i++){
        if(parts[i]->getSpecies()==THIRD_SPECIES){
            containsThirdSpecies=1;
            break;
        }
    }
    for(int p1 = 1; p1 < parts.size(); p1++){
        for(int p2 = 1; p2 < parts.size(); p2++){
            if(p1!=p2 && parts[p1]->getSpecies()==SECOND_SPECIES){
                if(!containsThirdSpecies){
                    for(int i = 0; i < parts[p1]->getBranchingNotes().size()-4; i++){
                        rel(home, parts[p1]->getMelodicIntervals().slice(0, notesPerMeasure.at(SECOND_SPECIES), parts[p1]->getMelodicIntervals().size())[i], 
                            IRT_NQ, 0);
                    }
                } else {
                    for(int i = 0; i < parts[p1]->getBranchingNotes().size()-4; i++){
                        rel(home, parts[p1]->getMelodicIntervals().slice(0, notesPerMeasure.at(SECOND_SPECIES), parts[p1]->getMelodicIntervals().size())[i], 
                            IRT_NQ, 0);
                    }
                    //no unison in the two last notes
                    rel(home, parts[p1]->getBranchingNotes()[parts[p1]->getBranchingNotes().size()-2], IRT_NQ, parts[p1]->getBranchingNotes()[parts[p1]->getBranchingNotes().size()-1]);
                }
            }
        }
    }
}

void M2_1_varietyCost(Home home, vector<Part*> parts){
    for(int i = 1; i < parts.size(); i++){
        Part* p = parts[i];
        cout << "Branching notes size : " << endl;
        cout << p->getBranchingNotes().size() << endl;
        int temp = 0;
        IntVarArray notes = p->getBranchingNotes();
        for(int j = 0; j < p->getHIntervalSize()-1; j++){
            cout << j << endl;
            int upbnd = 0;
            if(j+3<p->getHIntervalSize()){
                upbnd = j+4;
            } else {upbnd = p->getHIntervalSize();}
            for(int k = j+1; k < upbnd;k++){
                //setting a cost if notes inside a window are the same in a part
                rel(home, (notes[j]==notes[k])>>(p->getVarietyArray(temp)==p->getVarietyCost()));
                rel(home, (notes[j]!=notes[k])>>(p->getVarietyArray(temp)==0));
                temp++;
            }
            
        }
    }
}

void P1_1_2v_noDirectMotionFromPerfectConsonance(Home home, Part* part){
    for(int j = 0; j < part->getFirstSpeciesMotions().size(); j++){

        rel(home, ((part->getFirstSpeciesHIntervals()[j+1]==UNISSON || part->getFirstSpeciesHIntervals()[j+1]==PERFECT_FIFTH)&&part->getIsNotLowest()[j+1]==1) >>
            (part->getFirstSpeciesMotions()[j]!=PARALLEL_MOTION));

    }
}

void P1_1_4v_noDirectMotionFromPerfectConsonance(Home home, Part* part){
    for(int j = 0; j < part->getFirstSpeciesMotions().size()-1; j++){

        //if the counterpoint is the lowest part -> the cost is 0
        rel(home, (!part->getIsNotLowest()[j]) >> (part->getDirectCostArray()[j]==0));

        //if the counterpoint is neither the lowest nor the highest part, and the condition is true, then add a cost of 2
        rel(home, (part->getIsNotLowest()[j] && !part->getIsHighest()[j] && (part->getFirstSpeciesMotions()[j]==2&&(part->getFirstSpeciesHIntervals()[j+1]==0||
            expr(home, abs(part->getFirstSpeciesHIntervals()[j+1]))==7))) >> (part->getDirectCostArray()[j]==2));
        //else the cost is 0
        rel(home, (part->getIsNotLowest()[j] && !part->getIsHighest()[j] && (part->getFirstSpeciesMotions()[j]!=2||(part->getFirstSpeciesHIntervals()[j+1]!=0 
            && expr(home, abs(part->getFirstSpeciesHIntervals()[j+1]))!=7))) >> (part->getDirectCostArray()[j]==0));

        //if the counterpoint is the highest part and the condition is true, then add a direct move cost (8 in this case)
        rel(home, (part->getIsHighest()[j] && (part->getFirstSpeciesMotions()[j]==2&&(part->getFirstSpeciesHIntervals()[j+1]==0||
            expr(home, abs(part->getFirstSpeciesHIntervals()[j+1]))==7))) >> (part->getDirectCostArray()[j]==part->getDirectMoveCost()));
        //else the cost is 0
        rel(home, (part->getIsHighest()[j] && (part->getFirstSpeciesMotions()[j]!=2||(part->getFirstSpeciesHIntervals()[j+1]!=0 &&
            expr(home, abs(part->getFirstSpeciesHIntervals()[j+1]))!=7))) >> (part->getDirectCostArray()[j]==0));

    }
}

void P1_2_2v_noDirectMotionFromPerfectConsonance(Home home, Part* part){
    for(int j = 0; j < part->getSecondSpeciesRealMotions().size(); j++){
        rel(home, ((part->getFirstSpeciesHIntervals()[j+1]==UNISSON || part->getFirstSpeciesHIntervals()[j+1]==PERFECT_FIFTH)&&part->getIsNotLowest()[j+1]==1) >>
            (part->getSecondSpeciesRealMotions()[j]!=PARALLEL_MOTION));
    }
}

void P1_2_3v_noDirectMotionFromPerfectConsonance(Home home, Part* part){

    for(int j = 0; j < part->getFirstSpeciesMotions().size()-1; j++){
        rel(home, (part->getSecondSpeciesRealMotions()[j]==2&&(part->getFirstSpeciesHIntervals()[j+1]==0||part->getFirstSpeciesHIntervals()[j+1]==7))>>
            (part->getDirectCostArray()[j]==part->getDirectMoveCost()));
        rel(home, (part->getSecondSpeciesRealMotions()[j]!=2||(part->getFirstSpeciesHIntervals()[j+1]!=0&&part->getFirstSpeciesHIntervals()[j+1]!=7))>>
            (part->getDirectCostArray()[j]==0));
    }

}

void P1_2_4v_noDirectMotionFromPerfectConsonance(Home home, Part* part){
    for(int j = 0; j < part->getFirstSpeciesMotions().size()-1; j++){

        rel(home, (!part->getIsNotLowest()[j]) >> (part->getDirectCostArray()[j]==0));

        rel(home, (part->getIsNotLowest()[j] && !part->getIsHighest()[j] && (part->getSecondSpeciesRealMotions()[j]==2&&(part->getFirstSpeciesHIntervals()[j+1]==0||
            part->getFirstSpeciesHIntervals()[j+1]==7))) >> (part->getDirectCostArray()[j]==2));
        rel(home, (part->getIsNotLowest()[j] && !part->getIsHighest()[j] && (part->getSecondSpeciesRealMotions()[j]!=2||(part->getFirstSpeciesHIntervals()[j+1]!=0 &&
            part->getFirstSpeciesHIntervals()[j+1]!=7))) >> (part->getDirectCostArray()[j]==0));

        rel(home, (part->getIsHighest()[j] && (part->getSecondSpeciesRealMotions()[j]==2&&(part->getFirstSpeciesHIntervals()[j+1]==0||
            part->getFirstSpeciesHIntervals()[j+1]==7))) >> (part->getDirectCostArray()[j]==part->getDirectMoveCost()));
        rel(home, (part->getIsHighest()[j] && (part->getSecondSpeciesRealMotions()[j]!=2||(part->getFirstSpeciesHIntervals()[j+1]!=0 &&
            part->getFirstSpeciesHIntervals()[j+1]!=7))) >> (part->getDirectCostArray()[j]==0));

    }
}

void P3_0_noBattuta(Home home, Part* part){
    for(int j = 0; j < part->getMotions().size(); j++){
        rel(home, expr(home, part->getMotions()[j]==CONTRARY_MOTION && part->getHInterval()[j+1]==0 && 
            part->getMelodicIntervals()[j]<-4), BOT_AND, part->getIsNotLowest()[j], 0);
    }
}

void P3_1_noBattuta(Home home, Part* part){
    for(int j = 0; j < part->getFirstSpeciesMotions().size(); j++){
        rel(home, expr(home, part->getFirstSpeciesMotions()[j]==CONTRARY_MOTION && part->getFirstSpeciesHIntervals()[j+1]==0 && 
            part->getFirstSpeciesMIntervals()[j]<-4), BOT_AND, part->getIsNotLowest()[j], 0);
    }
}

void P3_2_noBattuta(Home home, Part* part){
    for(int j = 0; j < part->getSecondSpeciesMotions().size(); j++){
        rel(home, expr(home, part->getSecondSpeciesMotions()[j]==CONTRARY_MOTION && part->getFirstSpeciesHIntervals()[j+1]==0 && 
            part->getFirstSpeciesMIntervals()[j]<-4), BOT_AND, part->getIsNotLowest()[j], 0);
    }
}

void P4_successiveCost(Home home, vector<Part*> parts, int scc_cz, IntVarArray successiveCostArray, vector<Species> species){
    int idx = 0;
    for(int v1 = 1; v1 < parts.size(); v1++){
        for(int v2 = v1+1; v2 < parts.size(); v2++){
            IntVarArray hIntervals12 = IntVarArray(home, parts[v1]->getNMeasures(), 0, MAJOR_SEVENTH);
            BoolVarArray isPCons12 = BoolVarArray(home, parts[v1]->getNMeasures(), 0, 1);

            if(parts[v1]->getSpecies()==FOURTH_SPECIES || parts[v2]->getSpecies()==FOURTH_SPECIES){
                if(parts[v1]->getSpecies()==FOURTH_SPECIES && parts[v2]->getSpecies()==FOURTH_SPECIES){
                    for(int i = 0; i < parts[v1]->getNMeasures()-1; i++){
                        rel(home, hIntervals12[i]==(abs(parts[v1]->getNotes()[i*4+2]-parts[v2]->getNotes()[i*4+2])%12));
                    }
                } else if(parts[v1]->getSpecies()==FOURTH_SPECIES && parts[v2]->getSpecies()!=FOURTH_SPECIES){
                    for(int i = 0; i < parts[v1]->getNMeasures()-1; i++){
                        rel(home, hIntervals12[i]==(abs(parts[v1]->getNotes()[i*4+2]-parts[v2]->getNotes()[i*4])%12));
                    }
                } else{
                    for(int i = 0; i < parts[v1]->getNMeasures()-1; i++){
                        rel(home, hIntervals12[i]==(abs(parts[v1]->getNotes()[i*4]-parts[v2]->getNotes()[i*4+2])%12));
                    }
                }
                rel(home, hIntervals12[hIntervals12.size()-1]==(abs(parts[v1]->getNotes()[parts[v1]->getNotes().size()-1]-
                    parts[v2]->getNotes()[parts[v2]->getNotes().size()-1])%12));
            } else {
                for(int i = 0; i < parts[v1]->getNMeasures(); i++){
                    rel(home, hIntervals12[i]==(abs(parts[v1]->getNotes()[i*4]-parts[v2]->getNotes()[i*4])%12));
                }
            }

            for(int i = 0; i < hIntervals12.size(); i++){
                rel(home, expr(home, hIntervals12[i]==UNISSON), BOT_OR, expr(home, hIntervals12[i]==PERFECT_FIFTH), isPCons12[i]);
            }

            if(parts[v1]->getSpecies()!=SECOND_SPECIES && parts[v2]->getSpecies()!=SECOND_SPECIES && parts[v1]->getSpecies()!=FOURTH_SPECIES &&
                parts[v2]->getSpecies()!=FOURTH_SPECIES)
            {
                for(int i = 0; i < isPCons12.size()-1; i++){
                    BoolVar succPCons = BoolVar(home, 0, 1);
                    rel(home, isPCons12[i], BOT_AND, isPCons12[i+1], succPCons);
                    rel(home, (succPCons==1) >> (successiveCostArray[idx]==parts[v1]->getSuccCost()));
                    rel(home, (succPCons==0) >> (successiveCostArray[idx]==0));
                    idx++;
                }
            }
            else if(parts[v1]->getSpecies()==SECOND_SPECIES){
                for(int i = 0; i < isPCons12.size()-1; i++){
                    BoolVar firstNotFifth = BoolVar(home, 0, 1);
                    BoolVar secondNotFifth = BoolVar(home, 0, 1);
                    BoolVar notSuccessiveFifths = BoolVar(home, 0, 1);
                    BoolVar succPCons = BoolVar(home, 0, 1);
                    BoolVar succPConsAndNotSuccFifths = BoolVar(home, 0, 1);

                    BoolVar mNotThird1 = BoolVar(home, 0, 1);
                    BoolVar mNotThird2 = BoolVar(home, 0, 1);
                    BoolVar mNotThird = BoolVar(home, 0, 1);
                    BoolVar firstFifth = BoolVar(home, 0, 1);
                    BoolVar secondFifth = BoolVar(home, 0, 1);
                    BoolVar succFifth = BoolVar(home, 0, 1);
                    BoolVar succFifthNotThird = BoolVar(home, 0, 1);

                    BoolVar applyCost = BoolVar(home, 0, 1);

                    rel(home, hIntervals12[i], IRT_NQ, 7, Reify(firstNotFifth));
                    rel(home, hIntervals12[i+1], IRT_NQ, 7, Reify(secondNotFifth));
                    rel(home, firstNotFifth, BOT_OR, secondNotFifth, notSuccessiveFifths);

                    rel(home, isPCons12[i], BOT_AND, isPCons12[i+1], succPCons);
                    rel(home, succPCons, BOT_AND, notSuccessiveFifths, succPConsAndNotSuccFifths);

                    rel(home, parts[v1]->getMelodicIntervals()[i*2], IRT_NQ, 3, mNotThird1);
                    rel(home, parts[v1]->getMelodicIntervals()[i*2], IRT_NQ, 4, mNotThird2);
                    rel(home, mNotThird1, BOT_AND, mNotThird2, mNotThird);

                    rel(home, hIntervals12[i], IRT_EQ, 7, Reify(firstFifth));
                    rel(home, hIntervals12[i+1], IRT_EQ, 7, Reify(secondFifth));
                    rel(home, firstFifth, BOT_AND, secondFifth, succFifth);
                    rel(home, mNotThird, BOT_AND, succFifth, succFifthNotThird);

                    rel(home, succPConsAndNotSuccFifths, BOT_OR, succFifthNotThird, applyCost);
                    rel(home, (applyCost==1) >> (successiveCostArray[idx]==parts[v1]->getSuccCost()));
                    rel(home, (applyCost==0) >> (successiveCostArray[idx]==0));

                    idx++;
                }
            }
            else if(parts[v2]->getSpecies()==SECOND_SPECIES){
                for(int i = 0; i < isPCons12.size()-1; i++){
                    BoolVar firstNotFifth = BoolVar(home, 0, 1);
                    BoolVar secondNotFifth = BoolVar(home, 0, 1);
                    BoolVar notSuccessiveFifths = BoolVar(home, 0, 1);
                    BoolVar succPCons = BoolVar(home, 0, 1);
                    BoolVar succPConsAndNotSuccFifths = BoolVar(home, 0, 1);

                    BoolVar mNotThird1 = BoolVar(home, 0, 1);
                    BoolVar mNotThird2 = BoolVar(home, 0, 1);
                    BoolVar mNotThird = BoolVar(home, 0, 1);
                    BoolVar firstFifth = BoolVar(home, 0, 1);
                    BoolVar secondFifth = BoolVar(home, 0, 1);
                    BoolVar succFifth = BoolVar(home, 0, 1);
                    BoolVar succFifthNotThird = BoolVar(home, 0, 1);

                    BoolVar applyCost = BoolVar(home, 0, 1);

                    rel(home, hIntervals12[i], IRT_NQ, 7, Reify(firstNotFifth));
                    rel(home, hIntervals12[i+1], IRT_NQ, 7, Reify(secondNotFifth));
                    rel(home, firstNotFifth, BOT_OR, secondNotFifth, notSuccessiveFifths);

                    rel(home, isPCons12[i], BOT_AND, isPCons12[i+1], succPCons);
                    rel(home, succPCons, BOT_AND, notSuccessiveFifths, succPConsAndNotSuccFifths);

                    rel(home, parts[v2]->getMelodicIntervals()[i*2], IRT_NQ, 3, mNotThird1);
                    rel(home, parts[v2]->getMelodicIntervals()[i*2], IRT_NQ, 4, mNotThird2);
                    rel(home, mNotThird1, BOT_AND, mNotThird2, mNotThird);

                    rel(home, hIntervals12[i], IRT_EQ, 7, Reify(firstFifth));
                    rel(home, hIntervals12[i+1], IRT_EQ, 7, Reify(secondFifth));
                    rel(home, firstFifth, BOT_AND, secondFifth, succFifth);
                    rel(home, mNotThird, BOT_AND, succFifth, succFifthNotThird);

                    rel(home, succPConsAndNotSuccFifths, BOT_OR, succFifthNotThird, applyCost);
                    rel(home, (applyCost==1) >> (successiveCostArray[idx]==parts[v1]->getSuccCost()));
                    rel(home, (applyCost==0) >> (successiveCostArray[idx]==0));

                    idx++;
                }
            } else if(parts[v1]->getSpecies()==FOURTH_SPECIES || parts[v2]->getSpecies()==FOURTH_SPECIES){
                for(int i = 0; i < hIntervals12.size()-1; i++){
                    BoolVar firstNotFifth = BoolVar(home, 0, 1);
                    BoolVar secondNotFifth = BoolVar(home, 0, 1);
                    BoolVar notSuccessiveFifths = BoolVar(home, 0, 1);

                    BoolVar succPCons = BoolVar(home, 0, 1);
                    BoolVar succPConsNotFifths = BoolVar(home, 0, 1);

                    rel(home, hIntervals12[i], IRT_NQ, 7, Reify(firstNotFifth));
                    rel(home, hIntervals12[i+1], IRT_NQ, 7, Reify(secondNotFifth));
                    rel(home, firstNotFifth, BOT_OR, secondNotFifth, notSuccessiveFifths);

                    rel(home, isPCons12[i], BOT_AND, isPCons12[i+1], succPCons);
                    rel(home, succPCons, BOT_AND, notSuccessiveFifths, succPConsNotFifths);
                    rel(home, (succPConsNotFifths==1) >> (successiveCostArray[idx]==parts[v1]->getSuccCost()));
                    rel(home, (succPConsNotFifths==0) >> (successiveCostArray[idx]==0));
                    idx++;
                }
            }
        }
    }
}

void P6_noMoveInSameDirection(Home home, vector<Part*> parts){
    for(int i = 0; i < parts[0]->getMotions().size(); i++){
        rel(home, expr(home, parts[0]->getMotions()[i]==2 && parts[1]->getMotions()[i]==2), BOT_AND, expr(home, parts[2]->getMotions()[i]==2), 0);
    }
}

void P7_noSuccessiveAscendingSixths(Home home, vector<Part*> parts){
    for(int p1 = 0; p1 < parts.size(); p1++){
        for(int p2 = p1+1; p2 < parts.size(); p2++){
            for(int j = 1; j < parts[p1]->getFirstHInterval().size()-1; j++){
                rel(home, ((parts[p1]->getFirstHInterval()[j-1]!=MINOR_SIXTH && parts[p1]->getFirstHInterval()[j-1]!=MAJOR_SIXTH) && 
                    (parts[p2]->getFirstHInterval()[j-1]!=MINOR_SIXTH && parts[p2]->getFirstHInterval()[j-1]!=MAJOR_SIXTH)) || (
                        (parts[p1]->getFirstHInterval()[j]!=-MINOR_SIXTH && parts[p1]->getFirstHInterval()[j]!=-MAJOR_SIXTH) && 
                    (parts[p2]->getFirstHInterval()[j]!=- MINOR_SIXTH && parts[p2]->getFirstHInterval()[j]!=-MAJOR_SIXTH)) || (
                        parts[p1]->getFirstMInterval()[j]>0 || parts[p2]->getFirstMInterval()[j] > 0));
            }
        }
    }
}

void P1_1_3v_noDirectMotionFromPerfectConsonance(Home home, Part* part){
    for(int j = 0; j < part->getFirstSpeciesMotions().size()-1; j++){
        //set a cost when it is reached through direct motion, it is 0 when not
        rel(home, (part->getFirstSpeciesMotions()[j]==2&&(part->getFirstSpeciesHIntervals()[j+1]==0||part->getFirstSpeciesHIntervals()[j+1]==7))>>
            (part->getDirectCostArray()[j]==part->getDirectCost()));
        rel(home, (part->getFirstSpeciesMotions()[j]!=2||(part->getFirstSpeciesHIntervals()[j+1]!=0&&part->getFirstSpeciesHIntervals()[j+1]!=7))>>
            (part->getDirectCostArray()[j]==0));
    }
}