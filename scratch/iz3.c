#include <stdio.h>
#include <z3.h>

int main () {
  Z3_context ctxt = Z3_mk_interpolation_context (NULL);
  Z3_del_context (ctxt);
}
