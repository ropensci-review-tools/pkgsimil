#pragma once

#include <Rcpp.h>

Rcpp::List rcpp_bm25 (const Rcpp::DataFrame &idfs, const Rcpp::List &tokens, Rcpp::DataFrame &tokens_i);
