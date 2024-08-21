#include "tree-parser.h"

#include <R.h>
#include <Rinternals.h>

SEXP c_parse_one_file() {
    const bool colour = false;

    TSParser *parser = ts_parser_new();
    ts_parser_set_language(parser, tree_sitter_r());

    char *source_code = loadfile("junk.R");

    TSTree *tree = ts_parser_parse_string(
        parser,
        NULL,
        source_code,
        strlen(source_code));

    TSNode root_node = ts_tree_root_node(tree);

    // https://github.com/tree-sitter/py-tree-sitter/issues/33
    TSTreeCursor cursor = ts_tree_cursor_new(root_node);

    bool reached_foot = false;
    int brackets[2] = {0, 0};
    printf("(");
    while (!reached_foot) {
        print_cursor(&cursor, source_code, colour, brackets);
        if (ts_tree_cursor_goto_first_child(&cursor)) continue;
        if (ts_tree_cursor_goto_next_sibling(&cursor)) continue;

        bool retracing = true;
        while (retracing) {
            if (!ts_tree_cursor_goto_parent(&cursor)) {
                retracing = false;
                reached_foot = true;
            }
            if (ts_tree_cursor_goto_next_sibling(&cursor)) {
                // const char *field_name = ts_tree_cursor_current_field_name(&cursor);
                retracing = false;
            }
        }
    }
    printf(")\n\n");
    printf("Bracket counts: [%i, %i]\n", brackets[0], brackets[1]);

    // char *string = ts_node_string(root_node);
    // printf("Syntax tree: %s\n", string);
    // free(string);

    free(source_code);
    ts_tree_delete(tree);
    ts_parser_delete(parser);
    ts_tree_cursor_delete(&cursor);

    SEXP result;
    PROTECT(result = allocVector(STRSXP, 1)); // Allocate space for one string
    SET_STRING_ELT(result, 0, mkChar("Hello from C!")); // Set the value of the string
    UNPROTECT(1); // Unprotect the allocated memory
    return result; // Return the SEXP (Symbolic EXPression) object
}
