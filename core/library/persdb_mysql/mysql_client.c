#include <mysql/mysql.h>

/* mysql_num_rows returns a long long integer and the foreign interface does not support them yet */

int num_rows(MYSQL_RES *res) {
  return mysql_num_rows(res);
}

/* mysql_num_fields returns a long long integer and the foreign interface does not support them yet */

int num_fields(MYSQL_RES *res) {
  return mysql_num_fields(res);
}

/* nth element of an array of strings */

char *nth_string(int i, char **array) {
  char *s = array[i];
  return s == (char *)0 ? "(null)" : s;
}

/* type of the nth field in a MYSQL_FIELD * array */

char *nth_field_type(int i, MYSQL_FIELD *field) {
  if (IS_NUM(field[i].type)) return "num"; else return "string";
}
