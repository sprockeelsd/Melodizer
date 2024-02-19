#include "headers/space_wrapper.hpp"
#include <iostream>
#include <fstream>

using namespace Gecode;
using namespace Gecode::Int;
using namespace Gecode::Set;
using namespace std;

/*
To Print value to a file :
    ofstream myfile;
    myfile.open ("/home/amdiels/Bureau/example.txt", ios::app);
    myfile << value << endl;
    myfile.close();
*/

/**
 Default constructor
 */
WSpace::WSpace() {
    i_size = 0;
    b_size = 0;
    s_size = 0;
}

//======================
//= Variables from idx =
//======================

/**
 Return the IntVar contained in int_vars at index vid
 */
IntVar WSpace::get_int_var(int vid) {
    return int_vars.at(vid);
}

/**
 Return the BoolVar contained in bool_vars at index vid
 */
BoolVar WSpace::get_bool_var(int vid) {
    return bool_vars.at(vid);
}

/**
Return the SetVar contained in set_vars at index vid.
*/
SetVar WSpace::get_set_var(int vid){
    return set_vars.at(vid);
}

//====================
//= Args for methods =
//====================

/**
 Return an IntVarArgs of size n, containing the n IntVars contained in
 int_vars at indices vids.
 */
IntVarArgs WSpace::int_var_args(int n, int* vids) {
    IntVarArgs x(n);
    for(int i = 0; i < n; i++)
        x[i] = get_int_var(vids[i]);
    return x;
}

/**
 Return an BoolVarArgs of size n, containing the n BoolVars contained in
 bool_vars at indices vids.
 */
BoolVarArgs WSpace::bool_var_args(int n, int* vids) {
    BoolVarArgs x(n);
    for(int i = 0; i < n; i++)
        x[i] = get_bool_var(vids[i]);
    return x;
}

/**
 Return a SetVarArgs of size n, containing the n SetVars contained in
 set_vars at indices vids.
 */
SetVarArgs WSpace::set_var_args(int n, int* vids) {
    SetVarArgs x(n);
    for(int i = 0; i < n; i++)
        x[i] = get_set_var(vids[i]);
    return x;
}

/**
 Return an IntArgs of size n, containing the n values in vals
*/
IntArgs WSpace::int_args(int n, int* vals) {
    IntArgs c(n);
    for(int i = 0; i < n; i++)
        c[i] = vals[i];
    return c;
}

/**
 Return the expression int_rel(vid, val)
*/
BoolVar WSpace::bool_expr_val(int vid, int int_rel, int val) {
    switch(int_rel) {
        case IRT_EQ: return expr(*this, get_int_var(vid) == val);
        case IRT_NQ: return expr(*this, get_int_var(vid) != val);
        case IRT_LE: return expr(*this, get_int_var(vid) < val);
        case IRT_LQ: return expr(*this, get_int_var(vid) <= val);
        case IRT_GQ: return expr(*this, get_int_var(vid) >= val);
        case IRT_GR: return expr(*this, get_int_var(vid) > val);
        default:
            cout << "Wrong expression type in BoolVar creation." << endl;
            return BoolVar();
    }
}

/**
 Return the expression int_rel(vid1, vid2)
*/
BoolVar WSpace::bool_expr_var(int vid1, int int_rel, int vid2) {
    switch(int_rel) {
        case IRT_EQ: return expr(*this, get_int_var(vid1) == get_int_var(vid2));
        case IRT_NQ: return expr(*this, get_int_var(vid1) != get_int_var(vid2));
        case IRT_LE: return expr(*this, get_int_var(vid1) < get_int_var(vid2));
        case IRT_LQ: return expr(*this, get_int_var(vid1) <= get_int_var(vid2));
        case IRT_GQ: return expr(*this, get_int_var(vid1) >= get_int_var(vid2));
        case IRT_GR: return expr(*this, get_int_var(vid1) > get_int_var(vid2));
        default:
            cout << "Wrong expression type in BoolVar creation." << endl;
            return BoolVar();
    }
}


/**
 Return the expression int_rel(vid int_op val)
*/
IntVar WSpace::int_expr_val(int vid, int int_op, int val) {
    switch(int_op) {
        case IOP_ADD: return expr(*this, get_int_var(vid) + val);
        case IOP_SUB: return expr(*this, get_int_var(vid) - val);
        case IOP_MUL: return expr(*this, get_int_var(vid) * val);
        case IOP_DIV: return expr(*this, get_int_var(vid) / val);
        case IOP_MOD: return expr(*this, get_int_var(vid) % val);
        default:
            cout << "Wrong expression type in IntVar creation." << endl;
            return IntVar();
    }
}

/**
 Return the expression vid1 int_op vid2
*/
IntVar WSpace::int_expr_var(int vid1, int int_op, int vid2) {
    switch(int_op) {
        case IOP_ADD: return expr(*this, get_int_var(vid1) + get_int_var(vid2));
        case IOP_SUB: return expr(*this, get_int_var(vid1) - get_int_var(vid2));
        case IOP_MUL: return expr(*this, get_int_var(vid1) * get_int_var(vid2));
        case IOP_DIV: return expr(*this, get_int_var(vid1) / get_int_var(vid2));
        case IOP_MOD: return expr(*this, get_int_var(vid1) % get_int_var(vid2));
        default:
            cout << "Wrong expression type in IntVar creation." << endl;
            return IntVar();
    }
}


//=========================
//= Variables and domains =
//=========================

/**
 Add an IntVar to the WSpace ranging from min to max.
 In practice, push a new IntVar at the end of the vector int_vars.
 Return the index of the IntVar in int_vars
 */
int WSpace::add_intVar(int min, int max) {
    int_vars.push_back(IntVar(*this, min, max));
    return i_size++;
}

/**
 Add an IntVar to the WSpace with domain dom of size s.
 In practice, push a new IntVar at the end of the vector int_vars.
 Return the index of the IntVar in int_vars
 */
int WSpace::add_intVarWithDom(int s, int* dom) {
    int_vars.push_back(IntVar(*this, IntSet(dom, s)));
    return i_size++;
}

/**
 Add n IntVars to the WSpace ranging from min to max.
 In practice, push n new IntVars at the end of the vector int_vars.
 Return the indices of the IntVars in int_vars.
 */
int* WSpace::add_intVarArray(int n, int min, int max) {
    int* vids = new int[n];
    for(int i = 0; i < n; i++)
        vids[i] = this->add_intVar(min, max);
    return vids;
}

/**
 Add n IntVars to the WSpace with domain dom of size s.
 In practice, push n new IntVars at the end of the vector int_vars.
 Return the indices of the IntVars in int_vars.
 */
int* WSpace::add_intVarArrayWithDom(int n, int s, int* dom) {
    int* vids = new int[n];
    for(int i = 0; i < n; i++)
        vids[i] = this->add_intVarWithDom(s, dom);
    return vids;
}

/**
 Add a IntVar to the WSpace corresponding to the evaluation of int_rel(vid, val).
 In practice, push a new IntVar at the end of the vector int_vars.
 Return the index of the IntVar in int_vars
 */
int WSpace::add_intVar_expr_val(int vid, int int_rel, int val) {
    int_vars.push_back(int_expr_val(vid, int_rel, val));
    return i_size++;
}

/**
 Add a IntVar to the WSpace corresponding to the evaluation of int_rel(vid1, vid2).
 In practice, push a new IntVar at the end of the vector int_vars.
 Return the index of the IntVar in int_vars
 */
int WSpace::add_intVar_expr_var(int vid1, int int_rel, int vid2) {
    int_vars.push_back(int_expr_var(vid1, int_rel, vid2));
    return i_size++;
}

/**
 Define which variables are to be the solution so they can be accessed to add a constraint with bab
 */
void WSpace::set_as_solution_variables(int n, int* vids){
    solution_variable_indexes = new int[n];
    for (int i=0; i<n; i++) {
        solution_variable_indexes[i]=vids[i];
    }
    var_sol_size = n;
}

/**
 Define the percentage of the solution that should change when searching for the next solution with BAB
 */
void WSpace::set_percent_diff(int diff){
    percent_diff = diff;
}

/**
 Return the number of IntVars in the space.
 */
int WSpace::nvars() {
    return i_size;
}

/**
 Add a BoolVar to the WSpace ranging from min to max.
 In practice, push a new BoolVar at the end of the vector bool_vars.
 Return the index of the BoolVar in bool_vars
 */
int WSpace::add_boolVar(int min, int max) {
    bool_vars.push_back(BoolVar(*this, min, max));
    return b_size++;
}

/**
 Add n BoolVars to the WSpace ranging from min to max.
 In practice, push n new BoolVars at the end of the vector bool_vars.
 Return the indices of the BoolVars in bool_vars.
 */
int* WSpace::add_boolVarArray(int n, int min, int max) {
    int* vids = new int[n];
    for(int i = 0; i < n; i++)
        vids[i] = this->add_boolVar(min, max);
    return vids;
}

/**
 Add a BoolVar to the WSpace corresponding to the evaluation of int_rel(vid, val).
 In practice, push a new BoolVar at the end of the vector bool_vars.
 Return the index of the BoolVar in bool_vars
 */
int WSpace::add_boolVar_expr_val(int vid, int int_rel, int val) {
    bool_vars.push_back(bool_expr_val(vid, int_rel, val));
    return b_size++;
}

/**
 Add a BoolVar to the WSpace corresponding to the evaluation of int_rel(vid1, vid2).
 In practice, push a new BoolVar at the end of the vector bool_vars.
 Return the index of the BoolVar in bool_vars
 */
int WSpace::add_boolVar_expr_var(int vid1, int int_rel, int vid2) {
    bool_vars.push_back(bool_expr_var(vid1, int_rel, vid2));
    return b_size++;
}

/**
Add a SetVar to the WSpace initialized with n integer from array r.
In practice, push a new SetVar at the end of the vector set_vars.
Return the index of the SetVar in set_vars.
*/
int WSpace::add_setVar(int lub_min, int lub_max, int card_min, int card_max) {
    set_vars.push_back(SetVar(*this,IntSet::empty, lub_min, lub_max, card_min, card_max));
    return s_size++;
}

/**
 Add n SetVars to the WSpace ranging with cardinality card_min to card_max.
 In practice, push n new SetVars at the end of the vector set_vars.
 Return the indices of the SetVars in set_vars.
 */
int* WSpace::add_setVarArray(int n, int lub_min, int lub_max, int card_min, int card_max) {
    int* vids = new int[n];
    for(int i = 0; i < n; i++)
        vids[i] = this->add_setVar(lub_min, lub_max, card_min, card_max);
    return vids;
}


//=======================
//= Posting constraints =
//=======================

//=== INTVAR ===

/**
 Post a relation constraint between the IntVar denoted by vid and the val.
 */
void WSpace::cst_val_rel(int vid, int rel_type, int val) {
    rel(*this, get_int_var(vid), (IntRelType) rel_type, val);
}

/**
 Post a relation constraint between the IntVars denoted by vid1 and vid2.
*/
void WSpace::cst_var_rel(int vid1, int rel_type, int vid2) {
    rel(*this, get_int_var(vid1), (IntRelType) rel_type, get_int_var(vid2));
}

/**
 Post a relation constraint between the IntVars denoted by vid1 and vid2 with reification.
*/
void WSpace::cst_var_rel_reify(int vid1, int rel_type, int vid2, int vid3, int mode) {
    rel(*this, get_int_var(vid1), (IntRelType) rel_type, get_int_var(vid2), Reify(get_bool_var(vid3), (ReifyMode) mode));
}

/**
 Post a relation constraint between the IntVars denoted by vid1 and vid2 with reification.
*/
void WSpace::cst_val_rel_reify(int vid1, int rel_type, int val, int vid2, int mode) {
    rel(*this, get_int_var(vid1), (IntRelType) rel_type, val, Reify(get_bool_var(vid2), (ReifyMode) mode));
}

/**
 Post a relation constraint between the n IntVars denoted by vids and the val.
*/
void WSpace::cst_arr_val_rel(int n, int* vids, int rel_type, int val) {
    rel(*this, int_var_args(n, vids), (IntRelType) rel_type, val);
}

/**
 Post a relation constraint between the n IntVars denoted by vids and the the IntVar vid.
*/
void WSpace::cst_arr_var_rel(int n, int* vids, int rel_type, int vid) {
    rel(*this, int_var_args(n, vids), (IntRelType) rel_type, get_int_var(vid));
}

/**
 Post a relation constraint between the n IntVars denoted by vids.
*/
void WSpace::cst_arr_rel(int n, int* vids, int rel_type) {
    rel(*this, int_var_args(n, vids), (IntRelType) rel_type);
}

/**
 Post a lexicographic relation constraint between the n1 IntVars denoted by vids1 and
 the n2 IntVars denoted by vids2.
*/
void WSpace::cst_arr_arr_rel(int n1, int* vids1, int rel_type, int n2, int* vids2) {
    rel(*this, int_var_args(n1, vids1), (IntRelType) rel_type, int_var_args(n2, vids2));
}

/**
 Post a if-then-else relation constraint between the IntVars denoted by the vids.
 */
void WSpace::cst_ite_rel(int vid1, int vid2, int vid3, int vid4) {
    ite(*this, get_bool_var(vid1), get_int_var(vid2), get_int_var(vid3), get_int_var(vid4));
}

/**
 Post the constraint that all IntVars denoted by vids are distinct
 */
void WSpace::cst_distinct(int n, int* vids) {
    distinct(*this, int_var_args(n, vids));
}

/**
 Post the linear constraint [c]*[vids] rel_type val.
 */
void WSpace::cst_val_linear(int n, int* c, int* vids, int rel_type, int val) {
    linear(*this, int_args(n, c), int_var_args(n, vids), (IntRelType) rel_type, val);
}

/**
 Post the linear constraint [c]*[vids] rel_type vid.
*/
void WSpace::cst_var_linear(int n, int* c, int* vids, int rel_type, int vid) {
    linear(*this, int_args(n, c), int_var_args(n, vids), (IntRelType) rel_type, get_int_var(vid));
}

/**
 Post the constraint that |vid1| = vid2.
 */
void WSpace::cst_abs(int vid1, int vid2) {
    abs(*this, get_int_var(vid1), get_int_var(vid2));
}

/**
 Post the constaraint that dom(vid) = d.
 */
void WSpace::cst_dom(int vid, int n, int* d) {
    dom(*this, get_int_var(vid), IntSet(d, n));
}

/**
 Post the constraint that vid is included in {vids[0], ..., vids[n-1]}
*/
void WSpace::cst_member(int n, int* vids, int vid) {
    member(*this, int_var_args(n, vids), get_int_var(vid));
}

/**
 Post the constraint that vid1 / vid2 = vid3.
 */
void WSpace::cst_div(int vid1, int vid2, int vid3) {
    div(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3));
}

/**
 Post the constraint that vid1 % vid2 = vid3.
 */
void WSpace::cst_mod(int vid1, int vid2, int vid3) {
    mod(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3));
}

/**
 Post the constraint that vid1 / vid2 = vid3
 and vid1 % vid2 = div4
 */
void WSpace::cst_divmod(int vid1, int vid2, int vid3, int vid4) {
    divmod(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3), get_int_var(vid4));
}

/**
 Post the constraint that min(vid1, vid2) = vid3.
 */
void WSpace::cst_min(int vid1, int vid2, int vid3) {
    Gecode::min(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3));
}

/**
 Post the constraint that vid = min(vids).
 */
void WSpace::cst_arr_min(int n, int* vids, int vid) {
    Gecode::min(*this, int_var_args(n, vids), get_int_var(vid));
}

/**
 Post the constraint that vid = argmin(vids).
 */
void WSpace::cst_argmin(int n, int* vids, int vid) {
    Gecode::argmin(*this, int_var_args(n, vids), get_int_var(vid));
}

/**
 Post the constraint that max(vid1, vid2) = vid3.
 */
void WSpace::cst_max(int vid1, int vid2, int vid3) {
    Gecode::max(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3));
}

/**
 Post the constraint that vid = max(vids).
 */
void WSpace::cst_arr_max(int n, int* vids, int vid) {// corrigé
    Gecode::max(*this, int_var_args(n, vids), get_int_var(vid));
}

/**
 Post the constraint that vid = argmax(vids).
 */
void WSpace::cst_argmax(int n, int* vids, int vid) {
    Gecode::argmax(*this, int_var_args(n, vids), get_int_var(vid));
}

/**
 Post the constraint that vid1 * vid2 = vid3.
 */
void WSpace::cst_mult(int vid1, int vid2, int vid3) {
    mult(*this, get_int_var(vid1), get_int_var(vid2), get_int_var(vid3));
}

/**
 Post the constraint that sqr(vid1) = vid2.
 */
void WSpace::cst_sqr(int vid1, int vid2) {
    sqr(*this, get_int_var(vid1), get_int_var(vid2));
}

/**
 Post the constraint that sqrt(vid1) = vid2.
 */
void WSpace::cst_sqrt(int vid1, int vid2) {
    Gecode::sqrt(*this, get_int_var(vid1), get_int_var(vid2));
}

/**
 Post the constraint that pow(vid1, n) = vid2.
 */
void WSpace::cst_pow(int vid1, int n, int vid2) {
    Gecode::pow(*this, get_int_var(vid1), n, get_int_var(vid2));
}

/**
 Post the constraint that nroot(vid1, n) = vid2.
 */
void WSpace::cst_nroot(int vid1, int n, int vid2) {
    nroot(*this, get_int_var(vid1), n, get_int_var(vid2));
}

/**
 Post the constraint that vid = sum(vids).
 */
void WSpace::cst_sum(int vid, int n, int* vids) {
    rel(*this, get_int_var(vid), IRT_EQ, expr(*this, sum(int_var_args(n, vids))));
}

/**
 Post the constraint that vids1 = sorted(vids2).
 */
void WSpace::cst_sorted(int n1, int* vids1, int n2, int* vids2, int n3, int* vids3) {
    std::cout << "cst_sorted in space_wrapper.cpp" << std::endl;
    sorted(*this, int_var_args(n1, vids1), int_var_args(n2, vids2), int_var_args(n3, vids3));
}

/**
 Post the constraint that the number of variables in vids equal to val1 has relation rel_type
 with val2.
 */
void WSpace::cst_count_val_val(int n, int* vids, int val1, int rel_type, int val2) {
    count(*this, int_var_args(n, vids), val1, (IntRelType) rel_type, val2);
}

/**
 Post the constraint that the number of variables in vids equal to val has relation rel_type
 with vid.
 */
void WSpace::cst_count_val_var(int n, int* vids, int val, int rel_type, int vid) {
    count(*this, int_var_args(n, vids), val, (IntRelType) rel_type, get_int_var(vid));
}

/**
 Post the constraint that the number of variables in vids equal to vid has relation rel_type
 with val.
 */
void WSpace::cst_count_var_val(int n, int* vids, int vid, int rel_type, int val) {// corrigé
    count(*this, int_var_args(n, vids), get_int_var(vid), (IntRelType) rel_type, val);
}

/**
 Post the constraint that the number of variables in vids equal to vid1 has relation rel_type
 with vid2.
 */
void WSpace::cst_count_var_var(int n, int* vids, int vid1, int rel_type, int vid2) {
    count(*this, int_var_args(n, vids), vid1, (IntRelType) rel_type, get_int_var(vid2));
}

/**
 Post the constraint that the number of variables in vids in the set set has relation rel_type with val
 */
void WSpace::cst_count_var_set_val(int n, int*vids, int s, int* set, int rel_type, int val){// ajouté
    count(*this, int_var_args(n, vids), IntSet(set, s), (IntRelType) rel_type, val);
}

/**
 Post the constraint that the number of variables in vids where vars[i] = c[i] and c is an array of integers has rel_type to val
 */
void WSpace::cst_count_array_val(int n, int*vids, int* c, int rel_type, int val){
    count(*this, int_var_args(n, vids), int_args(n, c), (IntRelType) rel_type, val);
}

/**
 Post the constraint that vids1[i] rel_type vids2[i] val% of the time
 */
void WSpace::cst_count_setvararray_val(int n, int* vids1, int* vids2, int rel_type, int val){

    SetVarArgs v1 = set_var_args(n, vids1);
    SetVarArgs v2 = set_var_args(n, vids2);

    for (int i = 0; i < n; i++){
        if ((rand()%100) < val){
            rel(*this, v1[i], (SetRelType) rel_type, v2[i]);
        }
    }
}

/**
  Post the constraint that the number of occurences of s-set in every subsequence of length
  val1 in vids must be higher than val2 and lower than val3
 */
void WSpace::cst_sequence_var(int n, int*vids, int s, int* set, int val1, int val2, int val3){// ajouté
    sequence(*this, int_var_args(n, vids), IntSet(set, s), val1, val2, val3);
}


/**
 Post the constraint that the number of distinct values in the n variables denoted by vids
 has the given rel_type relation with the variable vid.
 */
void WSpace::cst_nvalues(int n, int* vids, int rel_type, int vid) {
    nvalues(*this, int_var_args(n, vids), (IntRelType) rel_type, get_int_var(vid));
}

/**
 Post the constraint that values of vids1 are the edges of an hamiltonian circuit in
 the graph formed by the n variables in vids1, vids2 are the costs of these edges described
 by c, and vid is the total cost of the circuit, i.e. sum(vids2).
 */
void WSpace::cst_circuit(int n, int* c, int* vids1, int* vids2, int vid) {
    circuit(*this, int_args(n*n, c), int_var_args(n, vids1), int_var_args(n, vids2), get_int_var(vid));
}

/**
 Post the constraint that if there exists j (0 ≤ j < |x|) such that x[j] = u,
 then there must exist i with i < j such that x[i] = s
*/
void WSpace::cst_precede(int n, int* vids, int s, int u){
    precede(*this, int_var_args(n, vids), s, u);
}


//=== BOOLVAR ===

/**
 Post the constraint that vid1 bool_op vid2 = val.
 */
void WSpace::cst_boolop_val(int vid1, int bool_op, int vid2, int val) {
    rel(*this, get_bool_var(vid1), (BoolOpType) bool_op, get_bool_var(vid2), val);
}

/**
 Post the constraint that elements of vids bool_op val.
 */
void WSpace::cst_boolop_arr_val(int bool_op, int s, int* vids, int val) {
    rel(*this, (BoolOpType) bool_op, bool_var_args(s, vids), val);
}

/**
 Post the constraint that y is the result of bool_op between all element of vids.
 */
void WSpace::cst_boolop_arr_var(int bool_op, int s, int* vids, int vid1) {
    rel(*this, (BoolOpType) bool_op, bool_var_args(s, vids), get_bool_var(vid1));
}

/**
 Post the constraint that vid1 bool_op vid2 = vid3.
 */
void WSpace::cst_boolop_var(int vid1, int bool_op, int vid2, int vid3) {
    rel(*this, get_bool_var(vid1), (BoolOpType) bool_op, get_bool_var(vid2), get_bool_var(vid3));
}

/**
 Post a relation constraint between vid and val.
 */
void WSpace::cst_boolrel_val(int vid, int rel_type, int val) {
    rel(*this, get_bool_var(vid), (IntRelType) rel_type, val);
}

/**
 Post a relation constraint between vid1 and vid2.
 */
void WSpace::cst_boolrel_var(int vid1, int rel_type, int vid2) {
    rel(*this, get_bool_var(vid1), (IntRelType) rel_type, get_bool_var(vid2));
}

//=== SETVAR ===

/**
 Post the constraint that vid1 set_op vid2 = vid3.
 */
void WSpace::cst_setop_var(int vid1, int set_op, int vid2, int set_rel, int vid3) {
    rel(*this, get_set_var(vid1), (SetOpType) set_op, get_set_var(vid2), (SetRelType) set_rel, get_set_var(vid3));
}

/**
 Post the constraint that y set_op x.
 */
void WSpace::cst_setop_arr(int set_op, int s, int* vid1, int vid2) {
    rel(*this, (SetOpType) set_op, set_var_args(s, vid1), get_set_var(vid2));
}

/**
 Post a relation constraint between vid1 and vid2.
 */
void WSpace::cst_setrel_var(int vid1, int rel_type, int vid2) {
    rel(*this, get_set_var(vid1), (SetRelType) rel_type, get_set_var(vid2));
}

/**
 Post a relation constraint between vid1 and domain dom.
 */
void WSpace::cst_setrel_val(int vid1, int rel_type, int* domain, int s) {
    dom(*this, get_set_var(vid1), (SetRelType) rel_type, IntSet(domain, s));
}

/**
 Post a relation constraint between vid1 and domain dom with a reify variable
 */
void WSpace::cst_setrel_val_reify(int vid1, int rel_type, int* domain, int s, int r, int mode) {
    dom(*this, get_set_var(vid1), (SetRelType) rel_type, IntSet(domain, s), Reify(get_bool_var(r), (ReifyMode) mode));
}

/**
 Post a relation constraint between vid1 and vid2 with a reify variable
 */
void WSpace::cst_setrel_var_reify(int vid1, int rel_type, int vid2, int r, int mode) {
    rel(*this, get_set_var(vid1), (SetRelType) rel_type, get_set_var(vid2), Reify(get_bool_var(r), (ReifyMode) mode));
}

/**
 Post a constraint that SetVar vid1 has to be empty
 */
void WSpace::cst_set_empty(int vid1) {
    dom(*this, get_set_var(vid1), (SetRelType) 0, IntSet::empty);
}

/**
 Post a dom constraint between vid1 and dom {i,..., j}.
 */
void WSpace::cst_setdom_ints(int vid1, int rel_type, int i, int j) {
    dom(*this, get_set_var(vid1), (SetRelType) rel_type, i, j);
}

/**
 Post a dom constraint between vid1 and dom vid2.
 */
void WSpace::cst_setdom_set(int vid1, int vid2) {
    dom(*this, get_set_var(vid1), get_set_var(vid2));
}

/**
 Post a cardinality constraint on vids with 2 bounds min_card max_card
 */
void WSpace::cst_card_val(int n, int* vids, int min_card, int max_card) {
    cardinality(*this, set_var_args(n, vids), min_card, max_card);
}

/**
 Post a cardinality constraint on vid1 with intvar vid2
 */
void WSpace::cst_card_var(int vid1, int vid2) {
    cardinality(*this, get_set_var(vid1), get_int_var(vid2));
}

/**
Post a channeling constraint between vid1 and vid2
*/
void WSpace::cst_channel(int n1, int* vids1, int n2, int* vids2){
    channel(*this, set_var_args(n1, vids1), set_var_args(n2, vids2));
}

/**
Post a channeling constraint between boolVarArray vid1 and SetVar vid2
*/
void WSpace::cst_channel_sb(int n1, int* vids1, int vid2){
    channel(*this, bool_var_args(n1, vids1), get_set_var(vid2));
}

/**
Return an intvar constrained to the minimum of the setvar vid1
*/
int WSpace::cst_setmin(int vid1){
    int_vars.push_back(expr(*this, min(get_set_var(vid1))));
    return i_size++ ;
}

/**
Return an intvar constrained to the maximum of the setvar vid1
*/
int WSpace::cst_setmax(int vid1){
    int_vars.push_back(expr(*this, max(get_set_var(vid1))));
    return i_size++ ;
}

/**
Post a constraint between vid2 and the minimum of the setvar vid1 with reification
*/
void WSpace::cst_setmin_reify(int vid1, int vid2, int r, int mode){
    min(*this, get_set_var(vid1), get_int_var(vid2), Reify(get_bool_var(r), (ReifyMode) mode));
}

/**
Post a constraint between vid2 and the maximum of the setvar vid1 with reification
*/
void WSpace::cst_setmax_reify(int vid1, int vid2, int r, int mode){
    max(*this, get_set_var(vid1), get_int_var(vid2), Reify(get_bool_var(r), (ReifyMode) mode));
}

/**
Post a relation constraint beween setvar vid1 and the union of the set in vids
*/
void WSpace::cst_setunion(int vid1, int n, int* vids){
    rel(*this, SOT_UNION, set_var_args(n, vids), get_set_var(vid1));
}

/**
Post an element constraints
*/
void WSpace::cst_element(int set_op, int n, int* vids, int vid1, int vid2){
    element(*this, (SetOpType) set_op, set_var_args(n, vids), get_set_var(vid1), get_set_var(vid2));
}

//======================================
//Branch and bound constraint function =
//======================================

/*
 Constrain method for BAB search
 This is called everytime the solver finds a solution
 This is a virtual method as declared in space_wrapper.h
*/
/*
void WSpace::constrain(const Space& _best) {
    std::cout << "Calling 'constrain':" << std::endl;
    const auto* best = dynamic_cast<const WSpace*>(&_best);
    if (best == nullptr) throw DynamicCastFailed("IntLexMinimizeSpace::constrain");
    int sum_of_costs = 0;
    IntVarArgs best_costs = best->cost();
    std::cout << "Number of costs = " << number_of_costs << std::endl;
    std::cout << "Previous best costs = " << best_costs << std::endl;
    for (int i = 0; i < number_of_costs; i++)
    {
        sum_of_costs += best_costs[i].val();
    }
    std::cout << "Previous best cost = " << sum_of_costs << std::endl;
    sum_of_costs *= 0.9; // this line has to be deleted
    std::cout << "Trying to find better than = " << sum_of_costs << std::endl;
    linear(*this, cost(), IRT_LE, sum_of_costs);
} 
*/
//==========================
//= Exploration strategies =
//==========================

/**
 Post a branching strategy on the variables in vids, with strategies denoted by var_strategy and
 val_strategy.
 var_strategy:
    - 0 : INT_VAR_SIZE_MIN()
    - 1 : INT_VAR_RND(r)
    - 2 : INT_VAR_DEGREE_MAX()
    - 3 : INT_VAR_NONE()
 val_strategy:
    - 0 : INT_VAL_MIN()
    - 1 : INT_VAL_RND(r)
    - 2 : INT_VAL_SPLIT_MIN()
    - 3 : INT_VAL_SPLIT_MAX()
    - 4 : INT_VAL_MED()
 */
void WSpace::branch(int n, int* vids, int var_strategy, int val_strategy) {
    IntVarBranch var_strat;
    IntValBranch val_strat;

    Rnd r1(1U);
    Rnd r2(3U);

    //determine the variable strategy
    if(var_strategy == 0){//INT_VAR_SIZE_MIN()
        var_strat = INT_VAR_SIZE_MIN();
    }
    else if(var_strategy == 1){//INT_VAR_RND(r1)
        var_strat = INT_VAR_RND(r1);
    }
    else if(var_strategy == 2){//INT_VAR_DEGREE_MAX()
        var_strat = INT_VAR_DEGREE_MAX();
    }
    else if(var_strategy == 3){//INT_VAR_NONE()
        var_strat = INT_VAR_NONE();
    }

    //determine the value strategy
    if(val_strategy == 0){//INT_VAL_MIN()
        val_strat = INT_VAL_MIN();
    }
    else if(val_strategy == 1){//INT_VAL_RND(r2)
        val_strat = INT_VAL_RND(r2);
    }
    else if(val_strategy == 2){//INT_VAL_SPLIT_MIN()
        val_strat = INT_VAL_SPLIT_MIN();
    }
    else if(val_strategy == 3){//INT_VAL_SPLIT_MAX()
        val_strat = INT_VAL_SPLIT_MAX();
    }
    else if(val_strategy == 4){//INT_VAL_MED()
        val_strat = INT_VAL_MED();
    }

    Gecode::branch(*this, int_var_args(n, vids), var_strat, val_strat);
}


/**
 Post a branching strategy on the n BoolVars in vids, with strategies denoted by var_strategy and
 val_strategy.
 */
void WSpace::branch_b(int n, int* vids, int var_strategy, int val_strategy) {
    Gecode::branch(*this, bool_var_args(n, vids), BOOL_VAR_NONE(), BOOL_VAL_MIN()); //default for now
}

/**
 Post a branching strategy on the n SetVars in vids.
 */
void WSpace::branch_set(int n, int* vids, int var_strategy, int val_strategy) {
    SetVarBranch var_strat;
    SetValBranch val_strat;

    Rnd r1(1U);
    Rnd r2(3U);

    //determine the variable strategy
    if(var_strategy == 0){//SET_VAR_SIZE_MIN()
        var_strat = SET_VAR_SIZE_MIN();
    }
    else if(var_strategy == 1){//SET_VAR_RND(r1)
        var_strat = SET_VAR_RND(r1);
    }
    else if(var_strategy == 2){//SET_VAR_DEGREE_MAX()
        var_strat = SET_VAR_DEGREE_MAX();
    }
    else if(var_strategy == 3){//SET_VAR_NONE()
        var_strat = SET_VAR_NONE();
    }

    //determine the value strategy
    if(val_strategy == 0){//SET_VAL_MIN()
        val_strat = SET_VAL_MIN_INC();
    }
    else if(val_strategy == 1){//SET_VAL_RND(r2)
        val_strat = SET_VAL_RND_INC(r2);
    }
    else if(val_strategy == 2){//SET_VAL_SPLIT_MIN()
        val_strat = SET_VAL_MIN_EXC();
    }
    else if(val_strategy == 3){//SET_VAL_SPLIT_MAX()
        val_strat = SET_VAL_RND_EXC(r2);
    }
    else if(val_strategy == 4){//SET_VAL_MED()
        val_strat = SET_VAL_MED_INC();
    }

    Gecode::branch(*this, set_var_args(n, vids), var_strat, val_strat);
}

//==================
//= Search support =
//==================

/**
 Define which variable, denoted by vid, will be considered as the cost.
 */
void WSpace::cost(const int* vid, int nb_costs) {
    number_of_costs = nb_costs;
    std::cout << "setting costs: Cost vars = ";
    for (int i = 0; i < nb_costs; ++i) {
        int var_id = vid[i];
        if (var_id >= 0 && var_id < int_vars.size()) {
            cost_vars.push_back(int_vars[var_id]);
            std::cout << cost_vars[i] << ", ";
        }
    }
    std::cout << std::endl;
}

IntVarArgs WSpace::cost() const {
    IntVarArgs cost_vars_args;
    for (const auto& var : cost_vars) {
        cost_vars_args << var;  // Add each variable to IntVarArgs
    }
    return cost_vars_args;
    // return cost_vars;
}

WSpace::WSpace(WSpace& s): IntLexMinimizeSpace(s), int_vars(s.i_size), bool_vars(s.b_size), set_vars(s.s_size), cost_vars(s.number_of_costs), i_size(s.i_size), b_size(s.b_size), s_size(s.s_size), number_of_costs(s.number_of_costs),
                           var_sol_size(s.var_sol_size), solution_variable_indexes(s.solution_variable_indexes), percent_diff(s.percent_diff) {
    //IntVars update
    vector<IntVar>::iterator itd, its;
    for(itd = int_vars.begin(), its = s.int_vars.begin(); itd != int_vars.end(); ++itd, ++its)
        itd->update(*this, *its);

    //CostVars update
    vector<IntVar>::iterator ctd, cts;
    for(ctd = cost_vars.begin(), cts = s.cost_vars.begin(); ctd != cost_vars.end(); ++ctd, ++cts)
        ctd->update(*this, *cts);
        
    //BoolVars update
    vector<BoolVar>::iterator btd, bts;
    for(btd = bool_vars.begin(), bts = s.bool_vars.begin(); btd != bool_vars.end(); ++btd, ++bts)
        btd->update(*this, *bts);

    //SetVars update
    vector<SetVar>::iterator std, sts;
    for(std = set_vars.begin(), sts = s.set_vars.begin(); std != set_vars.end(); ++std, ++sts)
        std->update(*this, *sts);

    //Solutions for BAB
    for(int i=0; i<var_sol_size; i++)
        s.solution_variable_indexes[i]=solution_variable_indexes[i];


}

Space* WSpace::copy() {
    return new WSpace(*this);
}

//=====================
//= Getting solutions =
//=====================

/**
 Return the current values of the variable denoted by vid.
 */
int WSpace::value(int vid) {
    return get_int_var(vid).val();
}

/**
 Return the current values of the variable denoted by vid.
 */
int WSpace::value_bool(int vid) {
    return get_bool_var(vid).val();
}


/**
 Return the current values of the variable denoted by vid.
 */
int* WSpace::value_set(int vid, int n) {
    SetVar sv = get_set_var(vid);
    int* vals = new int[n] ;
    int i = 0 ;
    for (SetVarGlbValues d(sv);d();++d){
        vals[i] = d.val() ;
        i++ ;
    }
    return vals;
}

int WSpace::value_size(int vid) {
    return get_set_var(vid).glbSize() ;
}

/**
 Return the current values of the n variables denoted by vids.
 */
int* WSpace::values(int n, int* vids) {
    int* vals = new int[n];
    for(int i = 0; i < n; i++)
        vals[i] = value(vids[i]);
    return vals;
}

/**
 * WARNING: Does not work and seems to returns the wrong values because of wrong ids
 * Try to return the current boolean values of the n variables denoted by vids.
 */
int* WSpace::values_bool(int n, int* vids) {
    int* vals = new int[n];
    for(int i = 0; i < 2; i++){
        try
        {
            vals[i] = value_bool(vids[i]);
        }
        catch(const std::exception& e)
        {
            vals[i] = 666;
        }
    }
    return vals;
}

//======================
//= Printing solutions =
//======================

void WSpace::print(int n, int* vids) {
    std::cout << "{";
    for(int i = 0; i < n; i++) {
        std::cout << get_int_var(vids[i]);
        if(i < n - 1) std::cout << ", ";
    }
    std::cout << "}" << std::endl;
}

//============================
//= Search options managment =
//============================

// === TIME STOP OBJECT ===

/**
 Default constructor
 */
WTimeStop::WTimeStop(int maxTime) : stop(Gecode::Search::TimeStop(maxTime)) {
    stop_ptr = &stop;
}

WTimeStop::~WTimeStop(){

}

TimeStop WTimeStop::getStop(){
    return stop;
}

TimeStop* WTimeStop::getStopPtr(){
    return stop_ptr;
}

/**
 Reset the time value of the time stop object
 */
void WTimeStop::reset(){
    stop.reset();
}

// === OPTIONS OBJECT ===

/**
 Default constructor
 */
WSearchOptions::WSearchOptions(){

}

WSearchOptions::~WSearchOptions(){

}

/**
 getter for the opts field
 */
Options WSearchOptions::getOpts(){
    return opts;
}
/**
 set the number of threads to use for parallel search
 */
int WSearchOptions::setNbThreads(int nThreads){
    opts.threads = nThreads;
    return opts.threads;
}

/**
 Set the time stopping mechanism that is to be used during the search to a certain duration in ms
 Takes a WTimeStop object as argument, and sets the WSearchOptions object's opts.stop field to the TimeStop pointer of the WTimeStop object
 */
void WSearchOptions::setTimeStop(WTimeStop* timestop){
    opts.stop = timestop->getStopPtr();
}

//=================
//= Search engine =
//=================

/*
 Branch and bound
 */
WbabEngine::WbabEngine(WSpace* sp, Options opts) {
    bab = new BAB<WSpace>(sp, opts);
}

WbabEngine::~WbabEngine() {
    delete bab;
}

/**
 Search the next solution for this search engine.
 */
WSpace* WbabEngine::next() {
    return bab->next();
}

/**
 Returns true if the search has been stopped by a search object
 */
int WbabEngine::stopped(){
    return bab->stopped();
}

/*
 Depth-first search
*/
WdfsEngine::WdfsEngine(WSpace* sp, Options opts) {
    dfs = new DFS<WSpace>(sp, opts);
}

WdfsEngine::~WdfsEngine() {
    delete dfs;
}

/**
 Search the next solution for this search engine.
 */
WSpace* WdfsEngine::next() {
    return dfs->next();
}

/**
 Returns true if the search has been stopped by a search object
 */
int WdfsEngine::stopped(){
    return dfs->stopped();
}
