//
// Created by Damien Sprockeels on 11/06/2024.
// Extended and developed by Luc Cleenewerk and Diego de Patoul up to August 2024. 
//

#ifndef FUXCP_BASE_FIRSTSPECIESCOUNTERPOINT_HPP
#define FUXCP_BASE_FIRSTSPECIESCOUNTERPOINT_HPP

#include "Part.hpp"
#include "CantusFirmus.hpp"
#include "../constraints.hpp"

/**
 * This class represents a counterpoint of the first species. It inherits from the Part class.
 * todo modify it so it also works for 3 and 4 voices. Add the appropriate constraints by making a constructor that takes the number of voices as a parameter
 * todo maybe it should take a Stratum (object or just IntVarArray) for the lowest voice or something like that depending on the formalization
 */
class FirstSpeciesCounterpoint : public Part{
protected:
    Species motherSpecies;                          /// The species from which this is called.
    CantusFirmus* cantus;
public:
    /**
     * General constructor. It takes the mother species as an argument and calls the super constructor from the part class.
     * Additionally, it sets all the 1st species specific variables as well as the general rules that have to be applied
     * regardless of the species. It does not apply 1st species specific rules and does not post branching.
     * todo maybe add a Stratum object or IntVarArray for the lowest voice
     * @param nMes the number of measures in the composition
     * @param cf the cantus firmus todo maybe it should be a CantusFirmusObject
     * @param lb the lower bound for the counterpoint
     * @param ub the upper bound for the counterpoint
     * @param k the key of the composition
     * @param mSpecies the species from which this is called.
     * @param low the lowest stratum
     * @param c the cantus firmus
     * @param v_type the voice type of the counterpoint
     * @param m_costs the user-defined melodic costs 
     * @param g_costs the user-defined general costs 
     * @param s_costs the user-defined specific costs 
     * @param bm parameter specifying if borrow Mode is enabled or not
     * @param nV the number of voices - as it is a 2 voice constructor, this parameter contains the number of voices
     */
    FirstSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub, Species mSpecies, Stratum* low, CantusFirmus* c,  int v_type
    , vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, int bm, int nV);

    /**
     * 2 VOICES CONSTRUCTOR. This constructor is only used when creating a counterpoint of the first species. It calls the other constructor with
     * FIRST_SPECIES as the mother species. Additionally, it posts 1st species specific constraints.
     * @param nMes the number of measures in the composition
     * @param cf the cantus firmus todo maybe it should be a CantusFirmusObject
     * @param lb the lower bound for the counterpoint
     * @param ub the upper bound for the counterpoint
     * @param k the key of the composition
     * @param mSpecies the species from which this is called.
     * @param low the lowest stratum
     * @param c the cantus firmus
     * @param v_type the voice type of the counterpoint
     * @param m_costs the user-defined melodic costs 
     * @param g_costs the user-defined general costs 
     * @param s_costs the user-defined specific costs 
     * @param bm parameter specifying if borrow Mode is enabled or not
     * @param nV the number of voices - as it is a 2 voice constructor, this parameter is passed to the general constructor
     */
    FirstSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub, Stratum* low, CantusFirmus* c,  int v_type, vector<int> m_costs
    , vector<int> g_costs, vector<int> s_costs, int bm, int nV);

    /**
     * 3 VOICES CONSTRUCTOR. This constructor is only used when creating a counterpoint of the first species. It calls the other constructor with
     * FIRST_SPECIES as the mother species. Additionally, it posts 1st species specific constraints.
     * @param nMes the number of measures in the composition
     * @param cf the cantus firmus todo maybe it should be a CantusFirmusObject
     * @param lb the lower bound for the counterpoint
     * @param ub the upper bound for the counterpoint
     * @param k the key of the composition
     * @param mSpecies the species from which this is called.
     * @param low the lowest stratum
     * @param c the cantus firmus
     * @param v_type the voice type of the counterpoint
     * @param m_costs the user-defined melodic costs 
     * @param g_costs the user-defined general costs 
     * @param s_costs the user-defined specific costs 
     * @param bm parameter specifying if borrow Mode is enabled or not
     * @param nV1 the number of voices - this parameter is there to differenciate it from other number of voices
     * @param nV2 the number of voices - as it is a 3 voice constructor, this parameter is passed to the general constructor
     */
    FirstSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub, Stratum* low, CantusFirmus* c,  int v_type, vector<int> m_costs
    , vector<int> g_costs, vector<int> s_costs, int bm, int nV1, int nV2);

    /**
     * 4 VOICES CONSTRUCTOR. This constructor is only used when creating a counterpoint of the first species. It calls the other constructor with
     * FIRST_SPECIES as the mother species. Additionally, it posts 1st species specific constraints.
     * @param nMes the number of measures in the composition
     * @param cf the cantus firmus todo maybe it should be a CantusFirmusObject
     * @param lb the lower bound for the counterpoint
     * @param ub the upper bound for the counterpoint
     * @param k the key of the composition
     * @param mSpecies the species from which this is called.
     * @param low the lowest stratum
     * @param c the cantus firmus
     * @param v_type the voice type of the counterpoint
     * @param m_costs the user-defined melodic costs 
     * @param g_costs the user-defined general costs 
     * @param s_costs the user-defined specific costs 
     * @param bm parameter specifying if borrow Mode is enabled or not
     * @param nV1 the number of voices - this parameter is there to differenciate it from other number of voices
     * @param nV2 the number of voices - this parameter is there to differenciate it from other number of voices
     * @param nV3 the number of voices - as it is a 4 voice constructor, this parameter is passed to the general constructor
     */
    FirstSpeciesCounterpoint(Home home, int nMes, vector<int> cf, int lb, int ub, Stratum* low, CantusFirmus* c,  int v_type, vector<int> m_costs
    , vector<int> g_costs, vector<int> s_costs, int bm, int nV1, int nV2, int nV3);

    /**
     * This function returns a string with the characteristics of the counterpoint. It calls the to_string() method from
     * the Part class and adds 1st species specific characteristics.
     * @return a string representation of the current instance of the FirstSpeciesCounterpoint class.
     */
    string to_string() const override;

    /// Copy constructor. This needs to copy all useful attributes and update variables. Must call the super copy constructor       NO LONGER NEEDED
    // FirstSpeciesCounterpoint(FirstSpeciesCounterpoint &s);
    /// Copy function
    // virtual Space *copy() override;

    FirstSpeciesCounterpoint(Home home, FirstSpeciesCounterpoint& s); // clone constructor
    FirstSpeciesCounterpoint* clone(Home home) override;

    virtual IntVarArray getBranchingNotes() override;

    IntVarArray getFirstHInterval() override;

    IntVarArray getMotions() override;

    IntVarArray getFirstMInterval() override;

    int getHIntervalSize() override;

};

#endif //FUXCP_BASE_FIRSTSPECIESCOUNTERPOINT_HPP