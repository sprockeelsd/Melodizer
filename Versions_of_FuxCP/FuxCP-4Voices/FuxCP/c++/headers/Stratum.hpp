// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// 

#ifndef STRATUM_HPP
#define STRATUM_HPP

#include "Utilities.hpp"
#include "Voice.hpp"

#include "gecode/kernel.hh"
#include "gecode/int.hh"
#include "gecode/search.hh"
#include "gecode/minimodel.hh"
#include "gecode/set.hh"

using namespace Gecode;
using namespace Gecode::Search;
using namespace std;

/// This class represents a Stratum
class Stratum : public Voice{
    protected:
        // intvararray with species of each measure?

    public:
        Stratum(Home home, int nMes, int lb, int ub);

        Stratum(Home home, int nMes, int lb, int ub, IntVarArray lowestNotes);

        Stratum(Home home, int nMes, int lb, int ub, IntVarArray lowestNotes, int nV);

        Stratum(Home home, int nMes, int lb, int ub, IntVarArray lowestNotes, int nV1, int nV2);

        Stratum(Home home, Stratum& s);  // clone constructor

        Stratum* clone(Home home) override;

        string to_string() const override;

        void setNote(Home home, int index, IntVar note);
        
};


#endif //STRATUM_HPP
