
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "tree-sitter/lib/include/tree_sitter/api.h"

#include <R.h>
// #include <Rinternals.h>

void *loadfile(char *file);
void print_bracket (bool open, int col);
void print_cursor(const TSTreeCursor *cursor, const char *source_code, bool col, int *brackets);
