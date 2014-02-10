#include "z3.h"
#include <iostream>

using namespace std;

int main () {
  Z3_context ctx = Z3_mk_context (0);
  Z3_ast emptySet = Z3_mk_empty_set (ctx, Z3_mk_int_sort (ctx));
  Z3_set_ast_print_mode (ctx, Z3_PRINT_SMTLIB2_COMPLIANT);
  cout << Z3_ast_to_string (ctx, emptySet) << endl;
}
