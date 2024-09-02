// 
// This file contains the declarations of the C++ interface functions. 
//

#ifndef gecode_wrapper_hpp
#define gecode_wrapper_hpp

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * Wraps the Problem constructor.
 * @todo modify this to include any parameters your Problem constructor requires
 * @param size an integer representing the size of the problem
 * @param lower_bound_domain an integer representing the lower bound of the domain of the variables
 * @param upper_bound_domain an integer representing the upper bound of the domain of the variables
 * @return A pointer to a Problem object casted as a void*
 */
void* create_new_problem(int* cantusFirmus, int size, int n_cp, int* splist, int* v_types, int b_mode, int min_skips, int* general_params, 
        int* motion_params, int* melodic, int* specific, int* importance, int t_off, int* scle, int scale_size, 
        int* chromatic, int chrom_size, int* borrow, int borrow_size);

/**
 * returns the size of the problem
 * @param sp a void* pointer to a Problem object
 * @return an integer representing the size of the problem
 */
int get_size(void* sp);

void delete_pointer(void* p);

void delete_solver_pointer(void* p);


/**
 * returns the values of the variables for a solution
 * @param sp a void* pointer to a Problem object
 * @return an int* pointer representing the values of the variables
 */
int* return_solution(void* sp);

/**
 * creates a search engine for Problem objects
 * @param sp a void* pointer to a Problem object
 * @return a void* cast of a Base<Problem>* pointer
 */
void* create_solver(void* sp, int type);

/**
 * returns the next solution space, it should be bound. If not, it will return NULL.
 * @param solver a void* pointer to a Base<Problem>* pointer for the search engine of the problem
 * @return a void* cast of a Problem* pointer
 */
void* return_next_solution_space(void* solver);


int search_stopped(void* solver);


int* return_species_array_5sp(void* sp, int ctp_index); 
int* return_extended_cp_domain(void* sp, int ctp_index);
int get_extended_cp_domain_size(void* sp, int ctp_index);


#ifdef __cplusplus
};
#endif

#endif