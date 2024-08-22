#pragma once

#include "tree-parser-utils.h"

#include <R.h>
#include <Rinternals.h>

SEXP c_parse_one_file(SEXP source_code_, SEXP node_brackets_);
