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
