//
// Created by Damien Sprockeels on 11/06/2024.
// Extended and developed by Luc Cleenewerk and Diego de Patoul up to August 2024. 
//

#ifndef FUXCP_BASE_COUNTERPOINT_HPP
#define FUXCP_BASE_COUNTERPOINT_HPP

#include "../Utilities.hpp"
#include "../Voice.hpp"
#include "../Stratum.hpp"
#include "../constraints.hpp"

#include "gecode/kernel.hh"
#include "gecode/int.hh"
#include "gecode/search.hh"
#include "gecode/minimodel.hh"
#include "gecode/set.hh"

using namespace Gecode;
using namespace Gecode::Search;
using namespace std;

/// abstract class!!! Should not be instanciated
/// This class represents a part, so it creates all the variables associated to that part and posts the constraints that are species independent
class Part : public Voice {
    protected:
        Species species;
        int nVoices;
        int borrowMode;
        int voice_type;
        //Stratum* lowest;
        IntVarArray melodicDegreeCost;
        IntVarArray fifthCostArray;
        IntVarArray octaveCostArray;
        BoolVarArray is_off;
        IntVarArray offCostArray;
        IntVarArray costs;
        IntVarArray varietyCostArray;
        IntVarArray directCostArray;
        BoolVarArray isConsonance;
        IntVarArray speciesArray;
        BoolVarArray isNotLowest;
        BoolVarArray isHighest;

        vector<int> borrowed_scale;
        vector<int> scale;
        vector<int> chromatic_scale;

        vector<int> cp_range;

        vector<int> extended_domain;
        vector<int> off_domain;

        vector<string> cost_names;

        int secondCost;
        int thirdCost;
        int fourthCost;
        int tritoneCost;
        int fifthCost;
        int sixthCost;
        int seventhCost;
        int octaveCost;

        int borrowCost;
        int h_fifthCost;
        int h_octaveCost;
        int succCost;
        int varietyCost;
        int triadCost;
        int directMoveCost;
        int penultCost;

        int penultSixthCost;
        int cambiataCost;
        int mSkipCost;
        int triad3rdCost;
        int m2ZeroCost;
        int syncopationCost;
        int prefSlider;

        int directCost;
        int obliqueCost;
        int contraryCost;

        //First Species specific variables
        IntVarArray firstSpeciesNotesCp;
        IntVarArray firstSpeciesHarmonicIntervals;
        IntVarArray firstSpeciesMelodicIntervals;
        IntVarArray firstSpeciesMotions;
        IntVarArray firstSpeciesMotionCosts;

        //Second species specific variables
        BoolVarArray isDiminution;
        IntVarArray penultCostArray;
        IntVarArray secondSpeciesMotions;
        IntVarArray secondSpeciesMelodicIntervals;
        IntVarArray secondSpeciesRealMotions;

        //Third species specific variables
        IntVarArray thirdSpeciesHarmonicIntervals;
        IntVarArray thirdSpeciesMelodicIntervals;
        BoolVarArray is5QNArray;
        IntVarArray cambiataCostArray;

        //Fourth species specific variables
        BoolVarArray isNoSyncopeArray;
        IntVarArray snycopeCostArray;
    public:
        Part(Home home, int nMes, Species sp, vector<int> cf, int lb, int ub, int v_type, vector<int> m_costs, vector<int> g_costs,
            vector<int> s_costs, int nV, int bm);

        // Part(Part& s); (no longer copy constructor since not a space anymore. Now just a clone constructor to deep copy the object (called by the Space's copy constructor))
        Part(Home home, Part& s);  // clone constructor

        /// must be implemented in the child classes, returns the variables to branch on
        // virtual IntVarArray getBranchingNotes();

        virtual Part* clone(Home home) override;

        virtual string to_string() const;

        virtual IntVarArray getBranchingNotes();

        virtual IntVarArray getFirstHInterval();

        virtual IntVarArray getFirstMInterval();

        virtual IntVarArray getMotions();
        
        int getSpecies() { return species; }

        IntVarArray getPartNotes();

        IntVarArray getCosts();

        BoolVarArray getIsHighest();

        int getSuccCost();

        int getTriadCost();

        int getVarietyCost();

        IntVar getVarietyArray(int idx);

        virtual int getHIntervalSize();

        BoolVarArray getConsonance();

        BoolVarArray getIsNotLowest();

        int getSecondCost();
        int getThirdCost();
        int getFourthCost();
        int getTritoneCost();
        int getFifthCost();
        int getSixthCost();
        int getSeventhCost();
        int getOctaveCost();
        int getHFifthCost();
        int getHOctaveCost();
        int getDirectCost();
        int getBorrowCost();
        int getPenultCost();
        int getDirectMoveCost();
        int getCambiataCost();

        IntVarArray getMelodicDegreeCost();

        IntVarArray getFirstSpeciesHIntervals();

        IntVarArray getFirstSpeciesNotes();

        IntVarArray getFifthCostArray();

        IntVarArray getOctaveCostArray();

        IntVarArray getFirstSpeciesMIntervals();

        IntVarArray getFirstSpeciesMotions();

        IntVarArray getDirectCostArray();

        BoolVarArray getIsOffArray();

        vector<int> getOffDomain();

        vector<int> getExtendedDomain();

        IntVarArray getOffCostArray();

        BoolVarArray getIsDiminution();

        IntVarArray getPenultCostArray();

        IntVarArray getSecondSpeciesMotions();

        IntVarArray getSecondSpeciesMIntervals();

        IntVarArray getSecondSpeciesRealMotions();

        BoolVarArray getIs5QNArray();

        IntVarArray getThirdSpeciesHIntervals();

        IntVarArray getThirdSpeciesMIntervals();

        IntVarArray getCambiataCostArray();

        BoolVarArray getNoSyncope();

        IntVarArray getSyncopeCostArray();

        IntVarArray getSpeciesArray();

        void add_cost(Home home, int idx, IntVarArray to_be_added, IntVarArray costs);

        vector<string> getCostNames();
};


#endif //FUXCP_BASE_COUNTERPOINT_HPP
