#include "tree-parser-utils.h"

void *loadfile(char *file) {
    FILE *fp;
    __int32_t lSize;
    char *buffer;

    fp = fopen(file , "rb");
    if (!fp) {
        perror(file);
        exit(1);
    }

    fseek(fp , 0L , SEEK_END);
    lSize = ftell(fp);
    rewind(fp);

    buffer = calloc(1, lSize+1);
    if (!buffer) {
        fclose(fp);
        fputs("memory alloc failed", stderr);
        exit(1);
    }

    if (fread(buffer , lSize, 1 , fp) != 1) {
        fclose(fp);
        free(buffer);
        fputs("read failed", stderr);
        exit(1);
    }

    fclose(fp);
    return(buffer);
}

void print_bracket (bool open, int col) {
    if (open) {
        if (col == 0) {
            printf("(");
        } else if (col == 1) {
            printf("\\e[1;33m(\\e[0m");
        } else if (col == 2) {
            printf("\\e[1;34m(\\e[0m");
        }
    } else {
        if (col == 0) {
            printf(")");
        } else if (col == 1) {
            printf("\\e[1;33m)\\e[0m");
        } else if (col == 2) {
            printf("\\e[1;34m)\\e[0m");
        }
    }
}

void print_cursor(const TSTreeCursor *cursor, const char *source_code, bool col, int *brackets) {
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
            print_bracket(true, col);
            brackets[0]++;
        } else if (strcmp(field_name, "close") == 0) {
            print_bracket(false, col);
            brackets[1]++;
        } else {
            // copy source code for that node, noting that the char[] is not
            // null terminated, so can't use standard printf.
            uint32_t first_byte = ts_node_start_byte(cursor_node);
            uint32_t last_byte = ts_node_end_byte(cursor_node);
            uint32_t kBytes = last_byte - first_byte;
            char these_bytes[kBytes];
            memcpy(these_bytes, (source_code + first_byte), kBytes);
            // printf(" %s ", field_name);
            // printf("field_name = %s:\t", field_name);
            if (kBytes > 0) {
                if (strcmp(field_name, "function") == 0 ||
                    strcmp(field_name, "name") == 0) {
                    printf(" %.*s ", (int)sizeof(these_bytes), these_bytes);
                } else {
                    printf(" %s ", field_name);
                }
            }
        }
    }
}
