//
// Created by Damien Sprockeels on 12/06/2024.
// Extended and developed by Luc Cleenewerk and Diego de Patoul up to August 2024. 
//

#ifndef MYPROJECT_CANTUSFIRMUS_HPP
#define MYPROJECT_CANTUSFIRMUS_HPP

#include "../Utilities.hpp"
#include "Part.hpp"
class CantusFirmus : public Part {
    protected:
        vector<int> cf_vector;
    public:
        CantusFirmus(Home home, int size, vector<int> cf, Stratum* low, int v_type, vector<int> m_costs, vector<int> g_costs, vector<int> s_costs, 
            int nV);

        string to_string() const override;

        CantusFirmus(Home home, CantusFirmus& s); // clone constructor
        CantusFirmus* clone(Home home) override;

        IntVarArray getFirstHInterval() override;

        IntVarArray getFirstMInterval() override;

        IntVarArray getMotions() override;

        IntVarArgs getFirstNotes() override;

        int getHIntervalSize() override;

};


#endif //MYPROJECT_CANTUSFIRMUS_HPP
