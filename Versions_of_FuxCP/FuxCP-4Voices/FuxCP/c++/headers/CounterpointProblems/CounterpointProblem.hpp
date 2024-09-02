// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// 

#ifndef MYPROJECT_COUNTERPOINTPROBLEM_HPP
#define MYPROJECT_COUNTERPOINTPROBLEM_HPP

#include "../Utilities.hpp"
#include "../Parts/FirstSpeciesCounterpoint.hpp"
#include "../Parts/SecondSpeciesCounterpoint.hpp"
#include "../Parts/CantusFirmus.hpp"

/** Types of search engines */
enum {
    dfs_solver, //0
    bab_solver, //1
};


/**
 * This (abstract) class gives a general model for a counterpoint problem. 
 */
class CounterpointProblem : public IntLexMinimizeSpace{
protected:
    CantusFirmus* cantusFirmus;

    int nMeasures;      /// the number of measures in the score to generate
    int n_unique_costs;
    Stratum* lowest;
    IntVarArray successiveCostArray;
    IntVarArray triadCostArray;
    Part* counterpoint_1;
    Part* counterpoint_2;
    Part* counterpoint_3;
    Stratum* upper_1;
    Stratum* upper_2;
    Stratum* upper_3;
    IntVarArray unitedCosts;
    IntVarArray sortedCosts;
    IntVarArray solutionArray;
    vector<int> importance;
    vector<string> importanceNames;
    vector<string> unitedCostNames;
    IntVarArray orderedFactors;
    IntVarArray finalCosts;
    vector<IntVarArray> sorted_voices;
    unordered_map<string, int> prefs;
    vector<vector<string>> costLevels;

    IntVar globalCost;
    // vector<int> species;        /// the species of the counterpoint to generate

public:
    /**
     * Constructor of the class.
     * @param cf a vector<int> representing the cantus firmus.
     * @param k the key of the score. it takes values from the notes in headers/Utilities.hpp
     * @param lb the lowest note possible for the counterpoints in MIDI
     * @param ub the highest note possible for the counterpoints in MIDI
     */
    CounterpointProblem(vector<int> cf, int v_type, vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, vector<int> imp, int nV);

    CounterpointProblem(CounterpointProblem& s);
    virtual IntLexMinimizeSpace* copy(); 

    virtual string to_string() const;  

    /**
     * Constrain method for bab search
     * @todo modify this function if you want to use branch and bound
     * @param _b a space to constrain the current instance of the Problem class with upon finding a solution
     */
    void constrain(const IntLexMinimizeSpace& _b);

    IntVarArgs cost() const;

    /// Getters
    //Part* getCounterpoint(){ return counterpoint_1; }
    ///destructor
    virtual ~CounterpointProblem(){ delete cantusFirmus; delete lowest; delete counterpoint_1; delete counterpoint_2; delete counterpoint_3; delete upper_1;
        delete upper_2, delete upper_3;}

    Home getHome();

    void setPreferenceMap(vector<string> importance_names);

    void orderCosts();

    int getSize();

    int* return_solution();

    IntVarArray getSolutionArray();

    Stratum* getLowest();

    int* get_species_array_5sp(int ctp_index);
    int* get_extended_cp_domain(int ctp_index);
    int  get_ext_cp_domain_size(int ctp_index);

    void setStrata();
};


/**
 * Creates a search engine for the given problem
 * Should only be used when using OM, otherwise you can create the solver etc in the main file
 * @todo Modify this function to add search options etc
 * @param pb an instance of the Problem class representing a given problem
 * @param type the type of search engine to create (see enumeration in headers/gecode_problem.hpp)
 * @return a search engine for the given problem
 */
Search::Base<CounterpointProblem>* make_solver(CounterpointProblem* pb, int type);


/**
 * Returns the next solution space for the problem
 * Should only be used when using OM
 * @param solver a solver for the problem
 * @return an instance of the Problem class representing the next solution to the problem
 */
CounterpointProblem* get_next_solution_space(Search::Base<CounterpointProblem>* solver);

#endif //MYPROJECT_COUNTERPOINTPROBLEM_HPP