// 
// This file contains the implementations of the C++ interface functions. 
//

#include "../headers/problem_wrapper.hpp"
#include "../headers/CounterpointProblems/CounterpointProblem.hpp"

#include "../headers/Utilities.hpp"
#include "../headers/CounterpointUtils.hpp"

std::vector<Species> convertToSpeciesVector(const std::vector<int>& intVec); 


/**
 * Wraps the Problem constructor.
 * @return A pointer to a Problem object casted as a void*
 */
void* create_new_problem(int* cantusFirmus, int size, int n_cp, int* splist, int* v_types, int b_mode, int min_skips, int* general_params, 
        int* motion_params, int* melodic, int* specific, int* importance, int t_off, int* scle, int scale_size, 
        int* chromatic, int chrom_size, int* borrow, int borrow_size){

    writeToLogFile("Entered problem_wrapper.cpp . ");

    vector<int> cf(int_pointer_to_vector(cantusFirmus, size));
    vector<int> sp(int_pointer_to_vector(splist, n_cp));
    vector<int> mot(int_pointer_to_vector(motion_params, 3));
    vector<int> vt(int_pointer_to_vector(v_types, n_cp));
    vector<int> sc(int_pointer_to_vector(scle, scale_size));
    vector<int> chr(int_pointer_to_vector(chromatic, chrom_size));
    vector<int> brw(int_pointer_to_vector(borrow, borrow_size));
    vector<int> mel(int_pointer_to_vector(melodic, 8));
    vector<int> gen(int_pointer_to_vector(general_params, 8));
    vector<int> spec(int_pointer_to_vector(specific, 7));
    vector<int> imp(int_pointer_to_vector(importance, 14));

    vector<Species> speciesList = convertToSpeciesVector(sp);
    return create_problem(cf, speciesList, vt, mel, gen, spec, imp, b_mode);
}

/**
 * returns the size of the problem
 * @param sp a void* pointer to a CounterpointProblem object
 * @return an integer representing the size of the problem
 */
int get_size(void* sp){
    return static_cast<CounterpointProblem*>(sp)->getSize();
}


void delete_pointer(void* p){
    writeToLogFile("deleting counterpointproblem pointer...");
    delete static_cast<CounterpointProblem*>(p);
    writeToLogFile("deleted CounterpointProblem pointer . ");
}

void delete_solver_pointer(void* p){
    delete static_cast<DFS<CounterpointProblem>*>(p);
    writeToLogFile("deleted solver pointer");
}


/**
 * returns the values of the variables for a solution
 * @param sp a void* pointer to a Problem object
 * @return an int* pointer representing the values of the variables
 */
int* return_solution(void* sp){
    return static_cast<CounterpointProblem*>(sp)->return_solution();
}


// =========== NEEDED BY 5SP PARSER =================

int* return_species_array_5sp(void* sp, int ctp_index){
    return static_cast<CounterpointProblem*>(sp)->get_species_array_5sp(ctp_index);
}

int* return_extended_cp_domain(void* sp, int ctp_index){
    return static_cast<CounterpointProblem*>(sp)->get_extended_cp_domain(ctp_index);
}

int get_extended_cp_domain_size(void* sp, int ctp_index){
    return static_cast<CounterpointProblem*>(sp)->get_ext_cp_domain_size(ctp_index);
}

// ====================================================


/**
 * creates a search engine for Problem objects
 * @param sp a void* pointer to a Problem object
 * @return a void* cast of a Base<Problem>* pointer
 */
void* create_solver(void* sp, int type){
    return (void*) make_solver(static_cast<CounterpointProblem*>(sp), type);  // TODO 
}

/**
 * returns the next solution space, it should be bound. If not, it will return NULL.
 * @param solver a void* pointer to a Base<CounterpointProblem>* pointer for the search engine of the problem
 * @return a void* cast of a CounterpointProblem* pointer
 */
void* return_next_solution_space(void* solver){
    writeToLogFile("return_next_solution_space function called");
    return (void*) get_next_solution_space(static_cast<BAB<CounterpointProblem>*>(solver));
}



int search_stopped(void* solver){
    return static_cast<int>(static_cast<Base<CounterpointProblem>*>(solver)->stopped());
}





// UTILITY FUNCTION JUST FOR PROBLEMWRAPPER
// Conversion function from std::vector<int> to std::vector<species>, DOES NOT HANDLE CANTUS FIRMUS because it is not supposed to be in spList
std::vector<Species> convertToSpeciesVector(const std::vector<int>& intVec) {
    std::vector<Species> speciesVec;
    speciesVec.reserve(intVec.size()); // Reserve space to avoid multiple allocations
    
    for (int value : intVec) {
        switch (value) {
            case 1:
                speciesVec.push_back(FIRST_SPECIES);
                break;
            case 2:
                speciesVec.push_back(SECOND_SPECIES);
                break;
            case 3:
                speciesVec.push_back(THIRD_SPECIES);
                break;
            case 4:
                speciesVec.push_back(FOURTH_SPECIES);
                break;
            case 5:
                speciesVec.push_back(FIFTH_SPECIES);
                break;
            default:
                std::cerr << "Warning: Value " << value << " is out of range for species enum\n";
                // Handle out of range values if needed
                // throw std::out_of_range("Value out of range for species enum");
                // speciesVec.push_back(static_cast<species>(0)); // Or handle it appropriately
                break;
        }
    }
    return speciesVec;
}