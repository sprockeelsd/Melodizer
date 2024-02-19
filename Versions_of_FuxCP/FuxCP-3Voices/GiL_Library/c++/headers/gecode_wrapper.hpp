#ifndef gecode_wrapper_hpp
#define gecode_wrapper_hpp

#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

enum {
    IRT_EQ,
    IRT_NQ,
    IRT_LQ,
    IRT_LE,
    IRT_GQ,
    IRT_GR
};

enum {
    BOT_AND,
    BOT_OR,
    BOT_IMP,
    BOT_EQV,
    BOT_XOR
};

/**
 Wraps the WSpace constructor.
 */
void* computation_space();

/**
 Wraps the WSpace add_intVar method.
 */
int add_intVar(void* sp, int min, int max);

/**
 Wraps the WSpace add_intVarWithDom method.
 */
int add_intVarWithDom(void* sp, int s, int* dom);

/**
 Wraps the WSpace add_intVarArray method.
 */
int* add_intVarArray(void* sp, int n, int min, int max);

/**
 Wraps the WSpace add_intVarArrayWithDom method.
 */
int* add_intVarArrayWithDom(void* sp, int n, int s, int* dom);

/**
 Wraps the WSpace add_intVar_expr_val method.
 */
int add_intVar_expr_val(void *sp, int vid, int op, int val);

/**
 Wraps the WSpace add_intVar_expr_var method.
 */
int add_intVar_expr_var(void *sp, int vid1, int op, int vid2);

/**
 Wraps the WSpace set_as_solution_variables method.
 */
void set_solution_vars(void* sp, int n, int* vids);

/**
 Wraps the WSpace set_percent_diff method.
 */
void set_percent_diff(void* sp, int diff);

/**
 Wraps the WSpace nvars method.
 */
int nvars(void* sp);

/**
 Wraps the WSpace add_boolVar method.
 */
int add_boolVar(void* sp, int min, int max);

/**
 Wraps the WSpace add_boolVarArray method.
 */
int* add_boolVarArray(void* sp, int n, int min, int max);

/**
 Wraps the WSpace add_boolVar_expr_val method.
 */
int add_boolVar_expr_val(void* sp, int vid, int rel_type, int val);

/**
 Wraps the WSpace add_boolVar_expr_var method.
 */
int add_boolVar_expr_var(void* sp, int vid1, int rel_type, int vid2);

/**
Wraps the WSpace add_setVar method
*/
int add_setVar(void* sp, int lub_min, int lub_max, int card_min, int card_max);

/**
Wraps the WSpace add_setVarArray method
*/
int* add_setVarArray(void* sp, int n, int lub_min, int lub_max, int card_min, int card_max);

/**
 Wraps the WSpace cst_var_relr method.
 */
void var_rel(void* sp, int vid1, int rel_type, int vid2);

/**
 Wraps the WSpace cst_var_rel_reify method.
 */
void var_rel_reify(void* sp, int vid1, int rel_type, int vid2, int vid3, int mode);

/**
 Wraps the WSpace cst_val_rel_reify method.
 */
void val_rel_reify(void* sp, int vid1, int rel_type, int val, int vid2, int mode);

/**
 Wraps the WSpace cst_val_rel method.
 */
void val_rel(void* sp, int vid, int rel_type, int val);

/**
 Wraps the WSpace cst_arr_val_rel method.
 */
void arr_val_rel(void* sp, int n, int* vids, int rel_type, int val);

/**
 Wraps the WSpace cst_arr_var_rel method.
 */
void arr_var_rel(void* sp, int n, int* vids, int rel_type, int vid);

/**
 Wraps the WSpace cst_arr_rel method.
 */
void arr_rel(void* sp, int n, int* vids, int rel_type);

/**
 Wraps the WSpace cst_arr_arr_rel method.
 */
void arr_arr_rel(void* sp, int n1, int* vids1, int rel_type, int n2, int* vids2);

/**
 Wraps the WSpace cst_distinct method.
 */
void distinct(void* sp, int n, int* vids);

/**
 Wraps the WSpace cst_val_linear method.
 */
void val_linear(void* sp, int n, int* c, int* vids, int rel_type, int value);

/**
 Wraps the WSpace cst_var_linear method.
 */
void var_linear(void* sp, int n, int* c, int* vids, int rel_type, int vid);

/**
 Wraps the WSpace ite_rel method.
 */
void ite_rel(void* sp, int vid1, int vid2, int vid3, int vid4);

/**
 Wraps the WSpace cst_abs method.
 */
void arithmetics_abs(void* sp, int vid1, int vid2);

/**
 Wraps the WSpace cst_div method.
 */
void arithmetics_div(void* sp, int vid1, int vid2, int vid3);

/**
 Wraps the WSpace cst_var_mod method.
 */
void arithmetics_mod(void* sp, int vid1, int vid2, int vid3);

/**
 Wraps the WSpace cst_divmod method.
 */
void arithmetics_divmod(void* sp, int vid1, int vid2, int vid3, int vid4);

/**
 Wraps the WSpace cst_min method.
 */
void arithmetics_min(void* sp, int vid1, int vid2, int vid3);

/**
 Wraps the WSpace cst_arr_min method.
 */
void arithmetics_arr_min(void* sp, int n, int* vids, int vid);

/**
 Wraps the WSpace cst_argmin method.
 */
void arithmetics_argmin(void* sp, int n, int* vids, int vid);

/**
 Wraps the WSpace cst_max method.
 */
void arithmetics_max(void* sp, int vid1, int vid2, int vid3);

/**
 Wraps the WSpace cst_arr_max method.
 */
void arithmetics_arr_max(void* sp, int n, int* vids, int vid);

/**
 Wraps the WSpace cst_argmax method.
 */
void arithmetics_argmax(void* sp, int n, int* vids, int vid);

/**
 Wraps the WSpace cst_mult method.
 */
void arithmetics_mult(void* sp, int vid1, int vid2, int vid3);

/**
 Wraps the WSpace cst_sqr method.
 */
void arithmetics_sqr(void* sp, int vid1, int vid2);

/**
 Wraps the WSpace cst_sqrt method.
 */
void arithmetics_sqrt(void* sp, int vid1, int vid2);

/**
 Wraps the WSpace cst_pow method.
 */
void arithmetics_pow(void* sp, int vid1, int n, int vid2);

/**
 Wraps the WSpace cst_nroot method.
 */
void arithmetics_nroot(void* sp, int vid1, int n, int vid2);

/**
 Wraps the WSpace cst_dom method.
 */
void set_dom(void* sp, int vid, int n, int* d);

/**
 Wraps the WSpace cst_member method.
 */
void set_member(void* sp, int n, int* vids, int vid);

/**
 Wraps the WSpace cst_sum method.
 */
void rel_sum(void* sp, int vid, int n, int* vids);

/**
 Wraps the WSpace cst_sorted method.
 */
void rel_sorted(void* sp, int n1, int* vids1, int n2, int* vids2, int n3, int* vids3);

/**
 Wraps the WSpace cst_count_val_val method.
 */
void count_val_val(void* sp, int n, int* vids, int val1, int rel_type, int val2);

/**
 Wraps the WSpace cst_count_val_var method.
 */
void count_val_var(void* sp, int n, int* vids, int val, int rel_type, int vid);

/**
 Wraps the WSpace cst_count_var_val method.
 */
void count_var_val(void* sp, int n, int* vids, int vid, int rel_type, int val);

/**
 Wraps the WSpace cst_count_var_var method.
 */
void count_var_var(void* sp, int n, int* vids, int vid1, int rel_type, int vid2);

/**
 Wraps the WSpace cst_count_var_set_val method.
 */
void count_var_set_val(void*sp, int n, int* vids, int s, int* set, int rel_type, int val);

/**
 Wraps the WSpace cst_count_array_val method.
 */
void count_array_val(void* sp, int n, int* vids, int* c, int rel_type, int val);

/**
 Wraps the WSpace cst_count_setvararray_val method.
 */
void count_setvararray_val(void* sp, int n, int* vids1, int* vids2, int rel_type, int val);

/**
 Wraps the WSpace cst_sequence_var method.
 */
void sequence_var(void*sp, int n, int* vids, int s, int* set, int val1, int val2, int val3);

/**
 Wraps the WSpace cst_nvalues method.
 */
void nvalues(void* sp, int n, int* vids, int rel_type, int vid);

/**
 Wraps the WSpace cst_circuit method.
 */
void circuit(void* sp, int n, int* c, int* vids1, int* vids2, int vid);

/**
 Wraps the WSpace cst_precede method
*/
void precede(void* sp, int n, int* vids, int s, int u);

/**
 Wraps the WSpace cst_boolop_val method.
 */
void val_boolop(void* sp, int vid1, int bool_op, int vid2, int val);

/**
 Wraps the WSpace cst_boolop_arr_val method.
 */
void val_arr_boolop(void* sp, int bool_op, int s, int* vids, int val);

/**
 Wraps the WSpace cst_boolop_arr_var method.
 */
void var_arr_boolop(void* sp, int bool_op, int s, int* vids, int vid1);

/**
 Wraps the WSpace cst_boolop_var method.
 */
void var_boolop(void* sp, int vid1, int bool_op, int vid2, int vid3);

/**
 Wraps the WSpace cst_boolrel_val method.
 */
void val_boolrel(void* sp, int vid, int rel_type, int val);

/**
 Wraps the WSpace cst_boolrel_var method.
 */
void var_boolrel(void* sp, int vid1, int rel_type, int vid2);

/**
 Wraps the WSpace cst_setop_var method.
 */
void var_setop(void* sp, int vid1, int set_op, int vid2, int set_rel, int vid3);

/**
 Wraps the WSpace cst_setop_arr method.
 */
void arr_setop(void* sp, int set_op, int s, int* vid1, int vid2);

/**
 Wraps the WSpace cst_setrel_var method.
 */
void var_setrel(void* sp, int vid1, int rel_type, int vid2);

/**
 Wraps the WSpace cst_setrel_val method.
 */
void val_setrel(void* sp, int vid1, int rel_type, int* dom, int s);

/**
 Wraps the WSpace cst_setrel_val_reify method.
 */
void val_setrel_reify(void* sp, int vid1, int rel_type, int* dom, int s, int r, int mode);

/**
 Wraps the WSpace cst_setrel_var_reify method.
 */
void var_setrel_reify(void* sp, int vid1, int rel_type, int vid2, int r, int mode);

/**
 Wraps the WSpace cst_setdom_ints method.
 */
void ints_setdom(void* sp, int vid1, int rel_type, int i, int j);

/**
 Wraps the WSpace cst_setdom_set method.
 */
void set_setdom(void* sp, int vid1, int vid2);

/**
 Wraps the WSpace cst_set_empty method.
 */
void empty_set(void* sp, int vid1);

/**
 Wraps the WSpace cst_card_val method.
 */
void val_card(void* sp, int n, int* vids, int min_card, int max_card);

/**
 Wraps the WSpace cst_setrel_var method.
 */
void var_card(void* sp, int vid1, int vid2);

/**
 Wraps the WSpace cst_channel method.
 */
void channel_set(void* sp, int n1, int* vids1, int n2, int* vids2);

/**
Wraps the WSpace cst_channel_sb method.
*/
void channel_set_bool(void* sp, int n1, int* vids1, int vid2);

/**
Wraps the WSpace cst_setmin method.
*/
int set_min(void* sp, int vid1);

/**
Wraps the WSpace cst_setmax method.
*/
int set_max(void* sp, int vid1);

/**
Wraps the WSpace cst_setmin_reify method.
*/
void set_min_reify(void* sp, int vid1, int vid2, int r, int mode);

/**
Wraps the WSpace cst_setmax_reify method.
*/
void set_max_reify(void* sp, int vid1, int  vid2, int r, int mode);

/**
Wraps the WSpace cst_setunion method.
*/
void set_union(void* sp, int vid1, int n, int* vids);

/**
Wraps the WSpace cst_element method.
*/
void element(void* sp, int set_op, int n, int* vids, int vid1, int vid2);


/**
 Wraps the WSpace branch method.
 */
void branch(void* sp, int n, int* vids, int var_strategy, int val_strategy);

/**
 Wraps the WSpace branch_b method.
 */
void branch_b(void* sp, int n, int* vids, int var_strategy, int val_strategy);

/**
 Wraps the WSpace branch_set method.
 */
void branch_set(void* sp, int n, int* vids, int var_strategy, int val_strategy);

/**
 Wraps the WSpace cost method.
 */
void cost(void* sp, int* vid, int amount);

/**
 Wraps the WTimeStop constructor
 */
void* new_time_stop(int maxTime);

/**
 Wraps the WTimeStop reset method
 */
void reset_time_stop(void* tStop);

/**
 Wraps the WSearchOptions constructor
 */
void* new_search_options();

/**
 Wraps the WSearchOptions setNbThreads method.
 */
int set_nb_threads(void* sOpts, int nThreads);

/**
 Wraps the WSearchOptions setTimeStop method.
 */
void* set_time_stop(void* sOpts, void* tStop);

//new version
/**
 Wraps the WbabEngine constructor.
 */
void* new_bab_engine(void* sp, void* opts);

/**
 Wraps the WbabEngine next method.
 */
void* bab_next(void* se);

/**
 Wraps the WbabEngine stopped method.
 */
int bab_stopped(void* se);

/**
 Wraps the WdfsEngine constructor.
 */
void* new_dfs_engine(void* sp, void* opts);

/**
 Wraps the WdfsEngine next method.
 */
void* dfs_next(void* se);

/**
 Wraps the WdfsEngine stopped method.
 */
int dfs_stopped(void* se);

/**
 Wraps the WSpace destructor.
 */
void release(void* sp);

/**
 Wraps the WSpace value method.
 */
int get_value(void* sp, int vid);

/**
 Wraps the WSpace value method.
 */
int get_value_bool(void* sp, int vid);

/**
 Wraps the WSpace value method.
 */
int* get_value_set(void* sp, int vid, int n);

/**
 Wraps the WSpace value method.
 */
int get_value_size(void* sp, int vid);

/**
 Wraps the WSpace values method.
 */
int* get_values(void* sp, int n, int* vids);

/**
 Wraps the WSpace values_bool method.
 */
int* get_values_bool(void* sp, int n, int* vids);

/**
 Wraps the WSpace print method.
 */
void print_vars(void* sp, int n, int* vids);

#ifdef __cplusplus
};
#endif
#endif
