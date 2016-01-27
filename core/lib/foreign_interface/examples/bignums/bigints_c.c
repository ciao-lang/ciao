#include <ciao_prolog.h>

void make_smart_conversion_c(ciao_term  number_in,
                             ciao_term *number_out,
                             ciao_term *how_converted) {
  int    inter_int;
  double inter_float;
  char * inter_str;

  if (ciao_fits_in_c_int(number_in)) {/* Includes the case of being a float */
    inter_int = ciao_get_c_int(number_in);
    *number_out = ciao_mk_c_int(inter_int);
    *how_converted = ciao_atom("machine_integer");
  } else
    if (ciao_is_integer(number_in)) { /* Big number */
      inter_str   = ciao_get_number_chars(number_in);
      *number_out = ciao_put_number_chars(inter_str);
      ciao_free(inter_str);
      *how_converted = ciao_atom("string");
    } else { /* Must be a float */
      inter_float = ciao_get_c_double(number_in);
      *number_out = ciao_float(inter_float);
      *how_converted = ciao_atom("float");
    }
}

void force_string_conversion_c(ciao_term  number_in, 
                               ciao_term *number_out) {
  char *inter_str;
  inter_str  = ciao_get_number_chars(number_in);
  *number_out = ciao_put_number_chars(inter_str);
  ciao_free(inter_str);
}
