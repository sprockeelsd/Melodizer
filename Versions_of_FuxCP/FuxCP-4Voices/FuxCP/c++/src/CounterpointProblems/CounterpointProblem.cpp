// 
// Created by Luc Cleenewerk and Diego de Patoul. 
// 

#include "../../headers/CounterpointProblems/CounterpointProblem.hpp"
#include "../../headers/CounterpointUtils.hpp"

/**
 * Constructor of the class.
 * @param cf a vector<int> representing the cantus firmus.
 * @param k the key of the score. it takes values from the notes in headers/Utilities.hpp
 * @param lb the lowest note possible for the counterpoint in MIDI
 * @param ub the highest note possible for the counterpoint in MIDI
 */
CounterpointProblem::CounterpointProblem(vector<int> cf, int v_type, vector<int> m_costs, vector<int> g_costs, vector<int> s_costs,
    vector<int> imp, int nV){
    nMeasures = cf.size();
    lowest = new Stratum(*this, nMeasures, 0, 127);
    cantusFirmus = new CantusFirmus(*this, nMeasures, cf, lowest, v_type, m_costs, g_costs, s_costs, nV);
    importance = imp;
    n_unique_costs = 0;
    importanceNames = {"borrow", "fifth", "octave", "succ", "variety", "triad", "direct", "motion", "penult", "cambiata", "triad3" ,"m2", "syncopation", "melodic"};
    
    //creating the map with the names of the costs and their importance
    
    prefs = {};

    setPreferenceMap(importanceNames);

    orderedFactors = IntVarArray(*this, 14, 0, 1000);   //orderedFactors will contain the costs in order of their importance
    for(int i = 0; i < 14; i++){
        vector<string> tmp = {};
        costLevels.push_back(tmp);                      //costLevels will contain all the costs at a specific level of importance (1 to 14)
    }

    for(const auto& entry : prefs){ 
        int val = entry.second-1;
        costLevels[val].push_back(entry.first);         //initialize the costLevels
    }

    globalCost = IntVar(*this, 0, 2000000);             //contains the global cost

    writeToLogFile("counterpointproblem constructor"); 

}

// COPY CONSTRUCTOR
CounterpointProblem::CounterpointProblem(CounterpointProblem& s) : IntLexMinimizeSpace(s){
    if (s.cantusFirmus) {
        cantusFirmus = s.cantusFirmus->clone(*this);
    } else {
        cantusFirmus = nullptr;
    }
    if (s.lowest) {
        lowest = s.lowest->clone(*this);
    } else {
        lowest = nullptr;
    }
    if (s.counterpoint_1) {
        counterpoint_1 = s.counterpoint_1->clone(*this);
    } else {
        counterpoint_1 = nullptr;
    }
    if (s.counterpoint_2) {
        counterpoint_2 = s.counterpoint_2->clone(*this);
    } else {
        counterpoint_2 = nullptr;
    }
    if (s.counterpoint_3) {
        counterpoint_3 = s.counterpoint_3->clone(*this);
    } else {
        counterpoint_3 = nullptr;
    }
    if(s.upper_1){
        upper_1 = s.upper_1->clone(*this);
    } else {
        upper_1 = nullptr;
    }
    if(s.upper_2){
        upper_2 = s.upper_2->clone(*this);
    } else {
        upper_2 = nullptr;
    }
    if(s.upper_3){
        upper_3 = s.upper_3->clone(*this);
    } else {
        upper_3 = nullptr;
    }
    nMeasures = s.nMeasures; 
    importance = s.importance;
    prefs = s.prefs;
    sorted_voices = s.sorted_voices;
    unitedCostNames = s.unitedCostNames;
    costLevels = s.costLevels;
    n_unique_costs = s.n_unique_costs;
    importanceNames = s.importanceNames;
    successiveCostArray.update(*this, s.successiveCostArray);
    triadCostArray.update(*this, s.triadCostArray);
    unitedCosts.update(*this, s.unitedCosts);
    solutionArray.update(*this, s.solutionArray);
    sortedCosts.update(*this, s.sortedCosts);
    orderedFactors.update(*this, s.orderedFactors);
    finalCosts.update(*this, s.finalCosts);
    for(int i = 0; i < sorted_voices.size(); i++){
        sorted_voices[i].update(*this, s.sorted_voices[i]);
    }
    globalCost.update(*this, s.globalCost);
}

IntLexMinimizeSpace* CounterpointProblem::copy(){   
    return new CounterpointProblem(*this);
}

void CounterpointProblem::constrain(const IntLexMinimizeSpace& _b){

    const CounterpointProblem &b = dynamic_cast<const CounterpointProblem &>(_b);
    
}

IntVarArgs CounterpointProblem::cost() const{

    return IntVarArgs(finalCosts);

}

string CounterpointProblem::to_string() const {
    string text = "Counterpoint problem : \n";
    text += cantusFirmus->to_string(); 
    text += "\n";
    text += "All costs : \n";
    text += intVarArray_to_string(unitedCosts); 
    text += "\nLowest : \n";
    text += lowest->to_string();
    text += "\n";
    text += "Upper : \n";
    text += upper_1->to_string();
    text += "\n";
    text += "Final Costs : \n";
    text += intVarArray_to_string(finalCosts);
    text += "\n";
    return text;
}

Home CounterpointProblem::getHome(){
    return *this;
}

void CounterpointProblem::setPreferenceMap(vector<string> importance_names){
    for(int i = 0; i < importance.size(); i++){
        prefs.insert({importance_names[i], importance[i]});
    }
}

void CounterpointProblem::orderCosts(){
    for(int i = 0; i < 14; i++){
        if(!costLevels[i].empty()){
            int sm_size = 0;
            for(int k = 0; k < costLevels[i].size(); k++){
                //goes through every cost at the cost level
                for(int t = 0; t < unitedCostNames.size(); t++){
                    if(unitedCostNames[t]==costLevels[i][k]){
                        //adjusts the amount of costs at this level
                        sm_size++;
                    }
                }
            }
            //now cst_sm_size contains the size of all the cost array on this level combined
            IntVarArgs sm(sm_size); //sum of all the costs on this level
            int idx = 0;
            for(int k = 0; k < costLevels[i].size(); k++){
                //goes through every cost at the cost level
                for(int t = 0; t < unitedCostNames.size(); t++){
                    if(unitedCostNames[t]==costLevels[i][k]){
                        //add the cost to the intvarargs
                        sm[idx] = unitedCosts[t];
                        idx++;
                    }
                }
            }
            //if there is at least one cost at this level
            if(idx>0){
                //then sum all the intvarargs of this level together and add them to the orderedFactors
                rel(*this, orderedFactors[n_unique_costs], IRT_EQ, expr(*this, sum(sm)));
                //increase the amount of unique costs
                n_unique_costs++;
            }
        }
    }
    
    finalCosts = IntVarArray(*this, n_unique_costs, 0, 1000000);
    for(int i = 0; i < n_unique_costs; i++){
        //adds the cost/costs to the finalCosts list, which will be the one that will be minimized
        //(this eliminates all the <not assigned> values of the intvararray for costs which are not set)
        rel(*this, finalCosts[i], IRT_EQ, orderedFactors[i]);
    }
    //globalCost is the sum of all the finalCosts
    rel(*this, globalCost, IRT_EQ, expr(*this, sum(finalCosts)));
}

void CounterpointProblem::setStrata(){
    //decided how many voices we have
    int nVoices = 2;
    if(upper_2!=nullptr){
        nVoices ++;
    }
    if(upper_3!=nullptr){
        nVoices++;
    }
    
    int size = counterpoint_1->getNMeasures();
    //sortedVoices : will contain in order at index 0 the lowest note and then the next highest at index 1 etc.
    sorted_voices = {};
    for(int i = 0; i < size; i++){
        //initialize the voices array which will contain the current note of a part
        IntVarArray voices = IntVarArray(*this, nVoices, 0, 127);
        //at index 0, we have the cantusFirmus
        rel(*this, voices[0], IRT_EQ, cantusFirmus->getNotes()[i]);
        //adds the correct note to the voices array at the index. special rules for fourth and fifth species apply
        if(counterpoint_1->getSpecies()==FOURTH_SPECIES && i!=size-1){
            rel(*this, voices[1], IRT_EQ, counterpoint_1->getNotes()[(i*4)+2]);
        } else if(counterpoint_1->getSpecies()==FIFTH_SPECIES && i==0) {
            rel(*this, voices[1], IRT_EQ, counterpoint_1->getNotes()[(i*4)+2]); 
        } else{
            rel(*this, voices[1], IRT_EQ, counterpoint_1->getFirstNotes()[i]);
        }
        //same for 3 voices
        if(nVoices>=3){
            if(counterpoint_2->getSpecies()==FOURTH_SPECIES && i!=size-1){
                rel(*this, voices[2], IRT_EQ, counterpoint_2->getNotes()[(i*4)+2]);
            } else if(counterpoint_2->getSpecies()==FIFTH_SPECIES && i==0) {
                rel(*this, voices[2], IRT_EQ, counterpoint_2->getNotes()[(i*4)+2]);
            }else{
                rel(*this, voices[2], IRT_EQ, counterpoint_2->getFirstNotes()[i]);
            }
        }
        //same for 4 voices
        if(nVoices>=4){
            if(counterpoint_3->getSpecies()==FOURTH_SPECIES && i!=size-1){
                rel(*this, voices[3], IRT_EQ, counterpoint_3->getNotes()[(i*4)+2]);
            } else if(counterpoint_3->getSpecies()==FIFTH_SPECIES && i==0) {
                rel(*this, voices[3], IRT_EQ, counterpoint_3->getNotes()[(i*4)+2]);
            }else{
                rel(*this, voices[3], IRT_EQ, counterpoint_3->getFirstNotes()[i]);
            }
        }

        //sorting the voices. Order is necessary to get the correct index of the voices array to put into the sorted_voices array
        IntVarArray order = IntVarArray(*this, nVoices, 0, nVoices-1);
        sorted_voices.push_back(IntVarArray(*this, nVoices, 0, 127));
        sorted(*this, voices, sorted_voices[i], order);

        //set lowest and upper strata notes
        lowest->setNote(*this, i*4, sorted_voices[i][0]);

        upper_1->setNote(*this, i*4, sorted_voices[i][1]);
        if(nVoices>=3){
            upper_2->setNote(*this, i*4, sorted_voices[i][2]);
        }
        if(nVoices>=4){
            upper_3->setNote(*this, i*4, sorted_voices[i][3]);
        }

        //if the lowest note is the same as the cantusFirmus note, then the cantusFirmus is the lowest stratum
        rel(*this, lowest->getFirstNotes()[i], IRT_NQ, cantusFirmus->getNotes()[i], Reify(cantusFirmus->getIsNotLowest()[i]));
        
        if(nVoices>=2){
            //sets the isNotLowest boolean for the first counterpoint
            if(counterpoint_1->getSpecies()==FOURTH_SPECIES && i!=size-1){
                rel(*this, expr(*this, (cantusFirmus->getIsNotLowest()[i]==1)&&(lowest->getFirstNotes()[i]==counterpoint_1->getNotes()[(i*4)+2])), IRT_NQ, 1, Reify(counterpoint_1->getIsNotLowest()[i]));
            } else if(counterpoint_1->getSpecies()==FIFTH_SPECIES && i==0){
                rel(*this, expr(*this, (cantusFirmus->getIsNotLowest()[i]==1)&&(lowest->getFirstNotes()[i]==counterpoint_1->getNotes()[(i*4)+2])), IRT_NQ, 1, Reify(counterpoint_1->getIsNotLowest()[i])); 
            } else{
                rel(*this, expr(*this, (cantusFirmus->getIsNotLowest()[i]==1)&&(lowest->getFirstNotes()[i]==counterpoint_1->getFirstNotes()[i])), IRT_NQ, 1, Reify(counterpoint_1->getIsNotLowest()[i]));
            }

        } 
        if(nVoices==3){
            
            //if we have 3 voices and both the counterpoint and the cantusFirmus are the same (both must be 1), then the counterpoint2 is the lowest
            rel(*this, expr(*this, counterpoint_1->getIsNotLowest()[i]!=cantusFirmus->getIsNotLowest()[i]), IRT_EQ, counterpoint_2->getIsNotLowest()[i]);
            
        }  
        if(nVoices==4){
            
            //set the counterpoint2 lowest boolean
            if(counterpoint_2->getSpecies()==FOURTH_SPECIES && i!=size-1){
                rel(*this, expr(*this, (cantusFirmus->getIsNotLowest()[i]==1)&&(counterpoint_1->getIsNotLowest()[i]==1)&&(lowest->getFirstNotes()[i]==counterpoint_2->getNotes()[(i*4)+2])), IRT_NQ, 1, Reify(counterpoint_2->getIsNotLowest()[i])); 
            } else if(counterpoint_2->getSpecies()==FIFTH_SPECIES && i==0){
                rel(*this, expr(*this, (cantusFirmus->getIsNotLowest()[i]==1)&&(counterpoint_1->getIsNotLowest()[i]==1)&&(lowest->getFirstNotes()[i]==counterpoint_2->getNotes()[(i*4)+2])), IRT_NQ, 1, Reify(counterpoint_2->getIsNotLowest()[i])); 
            } else {
                rel(*this, expr(*this, (cantusFirmus->getIsNotLowest()[i]==1)&&(counterpoint_1->getIsNotLowest()[i]==1)&&(lowest->getFirstNotes()[i]==counterpoint_2->getFirstNotes()[i])), IRT_NQ, 1, Reify(counterpoint_2->getIsNotLowest()[i]));
            }
            //if we have 4 voices and the counterpoint1, counterpoint2 and the cantusFirmus are the same (all must be 1), then the counterpoint3 is the lowest
            rel(*this, expr(*this, (cantusFirmus->getIsNotLowest()[i]==1) && (counterpoint_1->getIsNotLowest()[i]==1) && (counterpoint_2->getIsNotLowest()[i]==1)), IRT_NQ, counterpoint_3->getIsNotLowest()[i]);

            //the following are the same but for the isHighest array to define the highest stratum (needed for a 4 voice specific constraint)
            rel(*this, upper_3->getFirstNotes()[i], IRT_EQ, cantusFirmus->getNotes()[i], Reify(cantusFirmus->getIsHighest()[i]));

            if(counterpoint_1->getSpecies()==FOURTH_SPECIES && i!=size-1){
                rel(*this, expr(*this, (cantusFirmus->getIsHighest()[i]==0)&&(upper_3->getFirstNotes()[i]==counterpoint_1->getNotes()[i*4+2])), IRT_NQ, 0, Reify(counterpoint_1->getIsHighest()[i]));    
            } else if(counterpoint_1->getSpecies()==FIFTH_SPECIES && i==0){
                rel(*this, expr(*this, (cantusFirmus->getIsHighest()[i]==0)&&(upper_3->getFirstNotes()[i]==counterpoint_1->getNotes()[i*4+2])), IRT_NQ, 0, Reify(counterpoint_1->getIsHighest()[i])); 
            } else {
                rel(*this, expr(*this, (cantusFirmus->getIsHighest()[i]==0)&&(upper_3->getFirstNotes()[i]==counterpoint_1->getFirstNotes()[i])), IRT_NQ, 0, Reify(counterpoint_1->getIsHighest()[i]));
            }

            if(counterpoint_2->getSpecies()==FOURTH_SPECIES && i!=size-1){
                rel(*this, expr(*this, (cantusFirmus->getIsHighest()[i]==0)&&(counterpoint_1->getIsHighest()[i]==0)&&(upper_3->getFirstNotes()[i]==counterpoint_2->getNotes()[i*4+2])), IRT_NQ, 0, Reify(counterpoint_2->getIsHighest()[i]));
            } else if(counterpoint_2->getSpecies()==FIFTH_SPECIES && i==0){
                rel(*this, expr(*this, (cantusFirmus->getIsHighest()[i]==0)&&(counterpoint_1->getIsHighest()[i]==0)&&(upper_3->getFirstNotes()[i]==counterpoint_2->getNotes()[i*4+2])), IRT_NQ, 0, Reify(counterpoint_2->getIsHighest()[i]));
            } else {
                rel(*this, expr(*this, (cantusFirmus->getIsHighest()[i]==0)&&(counterpoint_1->getIsHighest()[i]==0)&&(upper_3->getFirstNotes()[i]==counterpoint_2->getFirstNotes()[i])), IRT_NQ, 0, Reify(counterpoint_2->getIsHighest()[i]));
            }

            rel(*this, expr(*this, (cantusFirmus->getIsHighest()[i]==0) && (counterpoint_1->getIsHighest()[i]==0) && (counterpoint_2->getIsHighest()[i]==0)), IRT_EQ, counterpoint_3->getIsHighest()[i]);
        }

        if(i > 0){

            //the following lines define the melodic interval of the lowest stratum

            vector<IntVarArray> corresponding_m_intervals;

            corresponding_m_intervals.push_back(cantusFirmus->getMelodicIntervals());
            for(int j = 0; j < nVoices-1; j++){
                Part* curr_cp;

                if(j==0) curr_cp=counterpoint_1;
                else if(j==1) curr_cp=counterpoint_2;
                else curr_cp = counterpoint_3;

                if(curr_cp->getSpecies()==FIRST_SPECIES){
                    corresponding_m_intervals.push_back(IntVarArray(*this, curr_cp->getMelodicIntervals().slice(0, 4, curr_cp->getMelodicIntervals().size())));
                } else if(curr_cp->getSpecies()==SECOND_SPECIES){
                    corresponding_m_intervals.push_back(IntVarArray(*this, curr_cp->getMelodicIntervals().slice(2, 4, curr_cp->getMelodicIntervals().size())));
                } else if(curr_cp->getSpecies()==THIRD_SPECIES){
                    corresponding_m_intervals.push_back(IntVarArray(*this, curr_cp->getMelodicIntervals().slice(3, 4, curr_cp->getMelodicIntervals().size())));
                } else if(curr_cp->getSpecies()==FOURTH_SPECIES){
                    corresponding_m_intervals.push_back(IntVarArray(*this, curr_cp->getMelodicIntervals().slice(2, 4, curr_cp->getMelodicIntervals().size())));
                } else if(curr_cp->getSpecies()==FIFTH_SPECIES){
                    corresponding_m_intervals.push_back(IntVarArray(*this, curr_cp->getMelodicIntervals().slice(2, 4, curr_cp->getMelodicIntervals().size())));
                }
            }

            rel(*this, (cantusFirmus->getIsNotLowest()[i]==0) >> (lowest->getMelodicIntervals()[i-1]==corresponding_m_intervals[0][i-1]));
            rel(*this, (counterpoint_1->getIsNotLowest()[i]==0) >> (lowest->getMelodicIntervals()[i-1]==corresponding_m_intervals[1][i-1]));
            if(nVoices>=3){
                rel(*this, (counterpoint_2->getIsNotLowest()[i]==0) >> (lowest->getMelodicIntervals()[i-1]==corresponding_m_intervals[2][i-1]));
            }
            if(nVoices>=4){
                rel(*this, (counterpoint_3->getIsNotLowest()[i]==0) >> (lowest->getMelodicIntervals()[i-1]==corresponding_m_intervals[3][i-1]));
            }

        }
    }
}

IntVarArray CounterpointProblem::getSolutionArray(){
    return solutionArray;
}

/**
 * Returns the size of the problem
 * @return an integer representing the size of the vars array           RETURNS THE SIZE OF THE SOLUTION_ARRAY
 */
int CounterpointProblem::getSize(){
    string message = "getSize function called. size = " + std::to_string(solutionArray.size()) + "\n";
    writeToLogFile(message.c_str());
    return this->solutionArray.size();  /// have to use this-> ?
}

/**
 * Returns the values taken by the variables vars in a solution
 * @todo Modify this to return the solution for your problem. This function uses @param size to generate an array of integers
 * @return an array of integers representing the values of the variables in a solution
 */
int* CounterpointProblem::return_solution(){
    string message = "return_solution method. Solution : [";
    int* solution = new int[solutionArray.size()];
    for(int i = 0; i < solutionArray.size(); i++){
        solution[i] = solutionArray[i].val();       
        message += std::to_string(solution[i]) + " ";
    }
    message += "]\n";
    writeToLogFile(message.c_str());
    return solution;
}


int* CounterpointProblem::get_species_array_5sp(int ctp_index){  

    FifthSpeciesCounterpoint* fifth_sp_ctp; 
    if (ctp_index == 0) fifth_sp_ctp = dynamic_cast<FifthSpeciesCounterpoint*>(counterpoint_1); 
    else if (ctp_index == 1) fifth_sp_ctp = dynamic_cast<FifthSpeciesCounterpoint*>(counterpoint_2); 
    else if (ctp_index == 2) fifth_sp_ctp = dynamic_cast<FifthSpeciesCounterpoint*>(counterpoint_3); 
    else{
        writeToLogFile("invalid value of ctp_index given as argument to get_species_array_5sp");
        return nullptr;
    }

    if (!fifth_sp_ctp){
        cout << "type cast of part to fifth species failed " << endl;
        writeToLogFile("type cast of part to fifth species failed");
        return nullptr;
    }

    IntVarArray speciesArray = fifth_sp_ctp->getSpeciesArray();

    string message = "return_species_array method. Species array : [";
    int* solution = new int[speciesArray.size()];
    for(int i = 0; i < speciesArray.size(); i++){
        
        switch (speciesArray[i].val())
        {
        case FIRST_SPECIES:
            solution[i] = 1;
            break;

        case SECOND_SPECIES:
            solution[i] = 2;
            break;

        case THIRD_SPECIES:
            solution[i] = 3;
            break;

        case FOURTH_SPECIES:
            solution[i] = 4;
            break;

        case -1:
            solution[i] = 0;
            break;

        default:
            cout << "invalid value in species array" << endl;
            writeToLogFile("invalid value in species array");
            return nullptr;
        }
        message += std::to_string(solution[i]) + " ";
    }
    message += "]\n";
    writeToLogFile(message.c_str());
    return solution;
}


int* CounterpointProblem::get_extended_cp_domain(int ctp_index){
    vector<int> ext_cp_dom;
    if(ctp_index == 0) ext_cp_dom = counterpoint_1->getExtendedDomain();
    else if(ctp_index == 1) ext_cp_dom = counterpoint_2->getExtendedDomain();
    else if(ctp_index == 2) ext_cp_dom = counterpoint_3->getExtendedDomain();
    else{
        writeToLogFile("invalid value of ctp_index given as argument to get_extended_cp_domain");
        return nullptr;
    }
    writeToLogFile(int_vector_to_string(ext_cp_dom).c_str());
    
    int* ext_cp_dom_int_ptr = new int[ext_cp_dom.size()];
    for(int i = 0; i < ext_cp_dom.size(); i++){
        ext_cp_dom_int_ptr[i] = ext_cp_dom[i];       
        // message += std::to_string(solution[i]) + " ";
    }

    return ext_cp_dom_int_ptr;
}

int CounterpointProblem::get_ext_cp_domain_size(int ctp_index){
    int cpDomSize;
    if(ctp_index == 0) cpDomSize = counterpoint_1->getExtendedDomain().size();
    else if(ctp_index == 1) cpDomSize = counterpoint_2->getExtendedDomain().size();
    else if(ctp_index == 2) cpDomSize = counterpoint_3->getExtendedDomain().size();
    else{
        writeToLogFile("invalid value of ctp_index given as argument to get_extended_cp_domain");
        return -1;
    }

    string message = "getextcpdomainsize function called. size = " + std::to_string(cpDomSize) + "\n";
    writeToLogFile(message.c_str());
    return cpDomSize;
}

Stratum* CounterpointProblem::getLowest(){
    return lowest;
}

/*************************
 * Search engine methods *
 *************************/

//Gecode::Search::TimeStop global_timeout(50000);

/**
 * Creates a search engine for the given problem
 * Should only be used when using OM, otherwise you can create the solver etc in the main file
 * @todo Modify this function to add search options etc
 * @param pb an instance of the Problem class representing a given problem
 * @param type the type of search engine to create (see enumeration in headers/gecode_problem.hpp)
 * @return a search engine for the given problem
 */
Search::Base<CounterpointProblem>* make_solver(CounterpointProblem* pb, int type){
    string message = "make_solver function called. type of solver :\n" + to_string(type) + "\n";
    writeToLogFile(message.c_str());

    Gecode::Search::Options opts;   
    /**@todo add here any options you want*/
    //opts.stop = &global_timeout;
    opts.threads = 1;

    if (type == bab_solver)
        return new BAB<CounterpointProblem>(pb, opts);
    else // default case
        return new DFS<CounterpointProblem>(pb, opts);
}

/**
 * Returns the next solution space for the problem
 * Should only be used when using OM
 * @param solver a solver for the problem
 * @return an instance of the Problem class representing the next solution to the problem
 */
CounterpointProblem* get_next_solution_space(Search::Base<CounterpointProblem>* solver){
    string message = "get_next_solution_space function called.\n";
    // RESET TIMEOUT OBJECT HERE
    //global_timeout.reset();
    CounterpointProblem* sol_space = solver->next();
    if (sol_space == nullptr){
        message += "solution_space was null. \n";
        writeToLogFile(message.c_str());
        return NULL;
    }
    message += sol_space->to_string();
    writeToLogFile(message.c_str());
    return sol_space;
}

