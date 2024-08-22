#include "tree-parser-utils.h"

void print_bracket (char **SExprString, bool open) {
    if (open) {
        appendToString(SExprString, "(");
    } else {
        appendToString(SExprString, ")");
    }
}

void appendToString(char **str, const char *appendStr) {
    size_t strLen = strlen(*str);
    size_t appendLen = strlen(appendStr);

    *str = realloc(*str, strLen + appendLen + 1); // +1 for null terminator

    if (*str == NULL) {
        error("Failed to allocate memory for string");
    }

    strcat(*str, appendStr);
}

void print_content(char **SExprString, const char *content, bool node_brackets) {
    if (node_brackets) {
        // wrap field name in brackets to make an S-expr node:
        int len = strlen(content) + 3;
        char *content_ext = malloc(len * sizeof(char));
        if (content_ext != NULL) {
            strcpy(content_ext, "(");
            strcat(content_ext, content);
            strcat(content_ext, ")");
            appendToString(SExprString, content_ext);
            free(content_ext);
        }
    } else {
        appendToString(SExprString, content);
    }
}

void print_cursor(const TSTreeCursor *cursor, const char *source_code, char **SExprString, bool node_brackets) {
    TSNode cursor_node = ts_tree_cursor_current_node(cursor);
    uint32_t n_children = ts_node_child_count(cursor_node);
    const char *field_name = ts_tree_cursor_current_field_name(cursor);
    if (field_name != NULL) {
        if (strcmp(field_name, "body") == 0 ||
            (strcmp(field_name, "rhs") == 0 && n_children > 1) ||
            strcmp(field_name, "lhs") == 0 ||
            strcmp(field_name, "consequence") == 0 ||
            strcmp(field_name, "alternative") == 0 ||
            strcmp(field_name, "arguments") == 0) {
            return;
        }
        if (strcmp(field_name, "open") == 0) {
            print_bracket(SExprString, true);
        } else if (strcmp(field_name, "close") == 0) {
            print_bracket(SExprString, false);
        } else {
            // copy source code for that node, noting that the char[] is not
            // null terminated, so can't use standard printf.
            uint32_t first_byte = ts_node_start_byte(cursor_node);
            uint32_t last_byte = ts_node_end_byte(cursor_node);
            uint32_t kBytes = last_byte - first_byte;
            if (kBytes > 0) {
                char these_bytes[kBytes];
                memcpy(these_bytes, (source_code + first_byte), kBytes);
                these_bytes[kBytes] = '\0';
                if (strcmp(field_name, "function") == 0 ||
                    strcmp(field_name, "name") == 0) {
                    print_content(SExprString, these_bytes, node_brackets);
                } else {
                    print_content(SExprString, field_name, node_brackets);
                }
            }
        }
    }
}

void test_parser (bool open) {
    TSParser *parser = ts_parser_new();

    ts_parser_set_language(parser, tree_sitter_r());
}
