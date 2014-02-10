// This is to test the print-parse facilities over the API
// for SMT-LIB2.

#include "z3.h"
#include <iostream>

int main () {
    const char* spec = "(assert (= true (f 1)))";
    Z3_context ctx = Z3_mk_context(0);
    Z3_symbol f = Z3_mk_string_symbol (ctx, "f");
    Z3_sort domain[] = { Z3_mk_int_sort(ctx) };
    Z3_func_decl decl = Z3_mk_func_decl (ctx, f, 1, domain, Z3_mk_bool_sort(ctx));
    std::cout << spec << "\n";

    Z3_symbol decl_names[] = { f };
    Z3_func_decl decls[] = { decl };
    Z3_ast a =
        Z3_parse_smtlib2_string(ctx,
                                spec,
                                0,
                                0,
                                0,
                                1,
                                decl_names,
                                decls);

    //test_print(ctx, a);

    Z3_set_ast_print_mode(ctx, Z3_PRINT_SMTLIB2_COMPLIANT);
    std::cout << Z3_get_ast_kind (ctx, a) << std::endl;
}
