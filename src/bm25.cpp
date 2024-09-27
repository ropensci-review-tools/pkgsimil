#include "bm25.h"

#include<unordered_map>
#include<unordered_set>

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_bm25 (const Rcpp::DataFrame &idfs, const Rcpp::List &tokensList, Rcpp::DataFrame &these_tokens, const double ntoks_avg) {

    // Fixed parameters used in the BM25 function. See wikipedia reference for
    // these values.
    const double k = 1.2;
    const double b = 0.75;

    // Set up primary 'idf_map' to map all tokens to their IDFs over whole corpus:
    std::unordered_map <std::string, double> idf_map;
    const Rcpp::CharacterVector idf_tokens = idfs ["token"];
    const Rcpp::NumericVector idf_idf = idfs ["idf"];
    for (int i = 0; i < idfs.nrow (); i++) {
        std::string this_tok = static_cast<std::string> (idf_tokens [i]);
        idf_map.emplace (this_tok, idf_idf [i]);
    }

    const int ndocs = tokensList.size();

    // Then make a set of the input tokens:
    const Rcpp::CharacterVector these_tokens_str = these_tokens ["token"];
    std::unordered_set <std::string> these_tokens_set;
    for (auto tok: these_tokens_str) {
        these_tokens_set.emplace (static_cast<std::string> (tok));
    }

    Rcpp::NumericVector bm25 (ndocs, 0.0);

    for (int i = 0; i < ndocs; i++) {

        const Rcpp::DataFrame tokens_i = Rcpp::as <Rcpp::DataFrame> (tokensList [i]);
        const Rcpp::IntegerVector tokens_n = tokens_i ["n"];
        const Rcpp::CharacterVector tokens_tok = tokens_i ["token"];

        int len_i = 0;
        for (auto n: tokens_n) len_i += n;

        std::unordered_set <std::string> doc_tokens_set;
        for (auto tok: tokens_tok) {
            const std::string tok_str = static_cast <std::string> (tok);
            if (idf_map.find (tok_str) != idf_map.end()) {
                doc_tokens_set.emplace (tok_str);
            }
        }

        // Find out where any of 'these_tokens' are in doc_tokens_set
        int n_toks_in_these = 0;
        for (auto tok: these_tokens_set) {
            if (doc_tokens_set.find (tok) != doc_tokens_set.end ()) n_toks_in_these++;
        }

        double bm25_i = 0.0;
        if (n_toks_in_these > 0) {
            // Count numbers of instances of each token:
            std::unordered_map <std::string, int> doc_tokens_counts;
            for (auto tok: doc_tokens_set) {
                auto count_map_iter = doc_tokens_counts.find (tok);
                if (count_map_iter == doc_tokens_counts.end ()) {
                    doc_tokens_counts.emplace (tok, 1L);
                } else {
                    int n = count_map_iter -> second;
                    doc_tokens_counts.erase (count_map_iter);
                    doc_tokens_counts.emplace (tok, n + 1);
                }
            }

            // Then get numerator and denominator of BM25:
            for (auto tok: doc_tokens_set) {
                const double n_i = doc_tokens_counts.find (tok) -> second;
                const double num_i = n_i * (k + 1);
                const double denom_i = (n_i + k * (1 - b + b * len_i / ntoks_avg));
                const double idf_i = idf_map.find (tok) -> second;
                bm25_i += idf_i * num_i / denom_i;
            }
        }
        
        bm25 (i) = bm25_i;
    }

    return bm25;
}
