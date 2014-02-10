#include "z3.h"
#include <iostream>

using namespace std;

void exitf(const char* message)
{
  fprintf(stderr,"BUG: %s.\n", message);
  exit(1);
}

void prove(Z3_context ctx, Z3_ast f, Z3_bool is_valid)
{
    Z3_model m;
    Z3_ast not_f;

    /* save the current state of the context */
    Z3_push(ctx);

    not_f = Z3_mk_not(ctx, f);
    Z3_assert_cnstr(ctx, not_f);

    m = 0;

    switch (Z3_check_and_get_model(ctx, &m)) {
    case Z3_L_FALSE:
        /* proved */
        printf("valid\n");
        if (!is_valid) {
            exitf("unexpected result");
        }
        break;
    case Z3_L_UNDEF:
        /* Z3 failed to prove/disprove f. */
        printf("unknown\n");
        if (m != 0) {
            /* m should be viewed as a potential counterexample. */
            printf("potential counterexample:\n%s\n", Z3_model_to_string(ctx, m));
        }
        if (is_valid) {
            exitf("unexpected result");
        }
        break;
    case Z3_L_TRUE:
        /* disproved */
        printf("invalid\n");
        if (m) {
            /* the model returned by Z3 is a counterexample */
            printf("counterexample:\n%s\n", Z3_model_to_string(ctx, m));
        }
        if (is_valid) {
            exitf("unexpected result");
        }
        break;
    }

    if (m) {
        Z3_del_model(ctx, m);
    }

    /* restore context */
    Z3_pop(ctx, 1);
}



int main () {
  Z3_context ctx = Z3_mk_context (0);
  Z3_constructor depCons = Z3_mk_constructor (ctx, Z3_mk_string_symbol (ctx, "Deposit"),
                     Z3_mk_string_symbol (ctx, "isDeposit"), 0, 0, 0, 0);
  Z3_constructor witCons = Z3_mk_constructor (ctx, Z3_mk_string_symbol (ctx, "Withdraw"),
                     Z3_mk_string_symbol (ctx, "isWithdraw"), 0, 0, 0, 0);
  Z3_constructor cons[] = {depCons, witCons};
  Z3_sort evt = Z3_mk_datatype (ctx, Z3_mk_string_symbol (ctx, "Event"), 2, cons);

  Z3_set_ast_print_mode (ctx, Z3_PRINT_SMTLIB2_COMPLIANT);
  cout << Z3_ast_to_string (ctx, Z3_sort_to_ast (ctx, evt)) << endl;

  Z3_func_decl isDep = Z3_get_datatype_sort_recognizer (ctx, evt, 0);
  cout << Z3_ast_to_string (ctx, Z3_func_decl_to_ast (ctx, isDep)) << endl;

  Z3_func_decl depCons1 = Z3_get_datatype_sort_constructor (ctx, evt, 0);
  cout << Z3_ast_to_string (ctx, Z3_func_decl_to_ast (ctx, depCons1)) << endl;
  Z3_ast deposit = Z3_mk_app (ctx, depCons1, 0, 0);

  Z3_func_decl witCons1 = Z3_get_datatype_sort_constructor (ctx, evt, 1);
  cout << Z3_ast_to_string (ctx, Z3_func_decl_to_ast (ctx, witCons1)) << endl;
  Z3_ast withdraw = Z3_mk_app (ctx, witCons1, 0, 0);


  cout << deposit << endl;
  prove (ctx, Z3_mk_eq (ctx, withdraw, deposit), Z3_FALSE);
}
