
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <R.h>
// #include <Rinternals.h>

#include "tree-sitter/lib/include/tree_sitter/api.h"

const TSLanguage* tree_sitter_r(void);

void print_bracket (char **SExprString, bool open);
void print_cursor(const TSTreeCursor *cursor, const char *source_code, char **SExprString, bool node_brackets);
void print_content(char **SExprString, const char *content, bool node_brackets);
void appendToString(char **str, const char *appendStr);
